/*  
	 Author:        Dario Camapgna
    E-mail:			 dario.campagna@gmail.com
    WWW:				 http://www.dmi.unipg.it/dario.campagna/software/ProdProc_Modeler.html
    Copyright (C): 2011, Dario Campagna

    This file is part of ProdProc Modeler.

    ProdProc Modeler is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    ProdProc Modeler is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ProdProc Modeler; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/


:- module(draw_product_canvas, []).

:- use_module(library(pce)).
:- use_module(align).
:- use_module(messages).
:- require([ add_config/2
	   , chain_list/2
	   , default/3
	   , file_name_extension/3
	   , forall/2
	   , get_config/2
	   , ignore/1
	   , pce_shell_command/1
	   , send_list/3
	   ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class `draw_product_canvas' defines  the actual product drawing  area. 
Representing a collection  of graphicals, the  closest PCE  class is   `picture'.  In
addition  to pictures, class   draw_canvas  takes care of the  current
mode, the  current prototype, the  file (for  loading  and saving  the
image) and an editor for changing attributes of graphical objects.

For editing  the drawing,  two  variables have been added:  `mode' and
`proto'.   `Mode' is an indication of  the current  mode.  The various
gestures defined in  the file `gesture'  are only active in predefined
modes.  They can access the current mode with:

	@event?receiver?window?mode

For  modes that  create  objects,   the variable `proto'   contains  a
prototype of the object to be created.  Instances of the prototype are
created   using `Object <-clone',     except   for  links,  which  are
instantiated by creating a connection from them.

The variables <->file and <-modified are used to implement  ->save and
->load.

The attribute_editor is a reference to  an editor that allows the user
to change the attributes of the graphicals in the selection.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_product_canvas, picture, "Drawing plane of PceDraw").

variable(mode,		   name,			get,
	 "Current mode of operation").
variable(proto,		   object*,			both,
	 "Current create prototype (graphical/link)").
variable(file,		   file*,			both,
	 "Current save/load file").
variable(modified,	   bool,			both,
	 "Has the contents been modified?").
variable(auto_align_mode,   bool,			both,
	 "Autoalign graphicals after create/resize").
variable(attribute_editor, draw_product_attribute_editor*,	both,
	 "Editor handling attributes").
variable(undo_buffer,	   draw_undo_manager,	        get,
	 "Records undo actions").



		/********************************
		*           INITIALISE		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->initialise initialises the picture and custom  slots that should not
be @nil.  It also attaches  an event recogniser  to the picture.  Note
that there are two ways to attach an event recogniser to a picture.

The first  is to  attach a recogniser  using the `Object ->recogniser'
method. In this case, the object is  extended with  an interceptor and
the recogniser is attached to this interceptor.   Recognisers attached
to an interceptor are activated by the `Graphical ->event'.

The second is  to  define a  method ->event.   This method may  either
decide to  decode the events itself,  or  leave this to  a recogniser.
These    approaches are used  in the   file   shapes.pl to make shapes
sensitive to user actions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(Canvas) :->
	"Create a drawing canvas"::
	send(Canvas, send_super, initialise, 'Canvas'),
	send(Canvas, selection_feedback, handles),
	send(Canvas, slot, undo_buffer, draw_undo_manager(Canvas, 100)),
	send(Canvas, slot, modified, @off),
	send(Canvas, auto_align_mode, @off),
	send(Canvas, mode, select, @nil),
	send(Canvas, recogniser, @draw_product_canvas_recogniser).

		 /*******************************
		 *        EVENT HANDLING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The recogniser itself is  a  reusable object  (which   implies   other
instances of draw_canvas can use the same instance of the recogniser).
For this reason, it  is declared using pce_global/2.   The  first time
the recogniser reference is passed to PCE, the  interface will trap an
exception and create the object  using  the declaration in  this file.
This approach will delay the creation of the reusable object  until it
is really  necessary  and avoids conditions  in  the  code  (i.e.  `if
object does not exist then create it' just before it is used).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@draw_prodcut_canvas_popup, make_draw_canvas_popup).

make_draw_canvas_popup(P) :-
	new(Canvas, @event?receiver),
	new(P, popup),
	send_list(P, append,
		  [ menu_item(select_mode,
			      message(Canvas?frame?menu, activate_select),
			      end_group := @on),
			 menu_item(paste,
			      message(Canvas, paste,
			 	      Canvas?current_event?position))
		  ]).


:- pce_global(@draw_product_canvas_keybinding, make_draw_canvas_keybinding).

canvas_keybinding('\\C-a', select_all).

make_draw_canvas_keybinding(B) :-
	new(B, key_binding),
	forall(canvas_keybinding(Key, Method),
	       send(B, function, Key, Method)).


:- pce_global(@draw_product_canvas_recogniser, make_draw_canvas_recogniser).

% Only drawing of nodes and edges, selection gesture, and keybord
%
make_draw_canvas_recogniser(G) :-
	new(ST, handler(keyboard,
			message(@receiver, start_typing, @event))),
	new(EX, handler(area_exit,
			message(@event?window, keyboard_focus, @nil))),
	new(G, handler_group(
				  @draw_create_node_recogniser,
			     @draw_warp_select_gesture,
			     @draw_product_canvas_keybinding,
			     @draw_create_mc_recogniser,
			     popup_gesture(@draw_prodcut_canvas_popup),
			     ST, EX)).



start_typing(C, Id:event_id) :->
	"Start typing if the selection wants the keyboard"::
	get(C, keyboard_focus, @nil),
	(   get(C, selection, Selection),
	    get(Selection, size, 1),
	    get(Selection, head, Obj),
	    send(Obj, '_wants_keyboard_focus')
	->  send(C, selection, @nil),
	    send(C, keyboard_focus, Obj),
	    (   Id \== 9
	    ->  send(Selection?head, generate_event, Id)
	    ;   true
	    )
	;   Id == 9
	->  send(C, advance)
	).



		/********************************
		*            UNLINK		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The ->unlink behaviour  is called when  an object is  removed from the
PCE object base, either initiated through  `Object ->free', or through
the garbage collector.  `Object ->unlink' is responsible for unlinking
the object   from its environment.   For  example,  when  a  window is
unlinked it should inform X-windows; when a  graphical is unlinked, it
should  inform its device.  Removing an  object entails the  following
steps:

	1) Call ->unlink
	2) Reset all slots that have objects in them to @nil
	3) Reclaim the memory

Like ->initialise,   ->unlink    should invoke   the method   of   the
super-class.  Normally, it will  first do its own  part of the job and
then starts the ->unlink of the superclass.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

unlink(Canvas) :->
	(   get(Canvas, attribute_editor, Editor),
	    Editor \== @nil
	->  send(Editor, quit)
	;   true
	),
	get(Canvas, undo_buffer, UB),
	free(UB),
	send(Canvas, send_super, unlink).


		/********************************
		*         MODIFICATIONS		*
		********************************/

:- pce_group(modified).

modified(C) :->
	send(C, slot, modified, @on).

% Undo currently not supported. For the moment we keep this predicates to make 
% things work. We will use them to support undo.
open_undo_group(C) :->
	"Open a new undo group"::
	send(C?undo_buffer, open_undo_group).

close_undo_group(C) :->
	"Close the undo group"::
	send(C?undo_buffer, close_undo_group).

undo_action(C, Action:code) :->
	"Record an action for undo"::
	send(C?undo_buffer, undo_action, Action).

clear_undo_group(C) :->
	"Empty the current undo-group"::
	send(C?undo_buffer, clear_group).

simple_undo(C) :->
	"Just undo the last action"::
	get(C, undo_buffer, UB),
	send(UB, start_undo),
	send(UB, undo),
	send(UB, end_undo).

undo(C) :->
	"Start undo dialog"::
	send(C?undo_buffer, open, C?frame).



reset(C) :->
	"Trap abort"::
	send(C, send_super, reset),
	send(C?undo_buffer, reset).

		/********************************
		*           SELECTION		*
		********************************/
:- pce_group(selection).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Managing the  selection.    This is  no  different  than for  standard
picture, except that we  have to update the  attribute-editor if it is
attached.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


selection(C, Sel:'graphical|chain*') :->
	"Set the selection shape or chain"::
	send(C, send_super, selection, Sel),
	send(C, keyboard_focus, @nil),
	send(C, update_attribute_editor).


toggle_select(C, Shape:graphical) :->
	"(Un)select a shape"::
	send(Shape, toggle_selected),
	send(C, update_attribute_editor).

select_all(C) :->
	"Select all displayed objects"::
	send(C, selection, C?graphicals).



		/********************************
		*             EDIT		*
		********************************/
:- pce_group(edit).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Selection-edit operations.  Most of them are rather trivial.  Note the
use of `Chain ->for_all'  to perform operations on  all members of the
selection.   This  method  is  a lot    faster then transferring   the
selection to a Prolog list and than operating on it:

	get(Canvas, selection, Selection),
	chain_list(Selection, List),
	forall(member(Gr, List), send(Gr, free)).

The `Chain ->for_all' operation first makes an array of objects in the
chain, than invokes  the message consequtively on  each  member of the
list.   Before  sending the message,  it  validates  the object  still
exists.  This makes the  method  safe for cases  were  destroying  one
object  destroyes  related objects  that  may be  in   the chain  too.
Connections  are  an example: destroying a graphical  destroys all its
connections and therefore leaves `dangling' references.

One could   generalise from the code   below   by introducing a method
->for_selection: message, but the advantages are very small.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


edit(Canvas, Msg, Grs:'[graphical|chain]') :->
	"Perform operation on graphicals or selection"::
	default(Grs, Canvas?selection, G0),
	(   send(G0, instance_of, chain)
	->  send(G0, for_all, Msg)
	;   send(Msg, forward, G0)
	),
	send(Canvas, modified).


cut_selection(Canvas, Grs:[graphical|chain]) :->
	"Erase all members of the selection"::
	send(Canvas, copy_selection, Grs),
	send(Canvas, edit, message(@arg1, cut), Grs).



:- pce_global(@draw_clipboard, new(device)).


copy_selection(Canvas, Grs:[graphical|chain]) :->
	"Copy all members of the selection to @draw_clipboard"::
	default(Grs, Canvas?selection, ToCopy),
	get(ToCopy, clone, Copy),
	send(@draw_clipboard, clear),
	send(Copy, for_all,
	     message(@draw_clipboard, display, @arg1)),
	clean_clipboard(@draw_clipboard),
	send(@draw_clipboard, reference).


clean_clipboard(Device) :-
	new(Done, hash_table),
	send(Device?graphicals, for_some,
	     message(@prolog, clean_clipboard_connections,
		     @arg1, Device, Done)),
	send(Done, done).


clean_clipboard_connections(Gr, _CB, Done) :-
	get(Done, member, Gr, @on), !.
clean_clipboard_connections(Gr, CB, _) :-
	\+ get(Gr, is_displayed, CB, @on), !,
	send(Gr, destroy).
clean_clipboard_connections(Gr, CB, Done) :-
	send(Done, append, Gr, @on),
	get(Gr, connections, AllConnections),
	send(AllConnections, for_all,
	     message(@prolog, clean_clipboard_connections,
		     ?(@arg1, opposite, Gr), CB, Done)).


paste(Canvas, At:[point]) :->
	"Paste @draw_clipboard"::
	(   object(@draw_clipboard)
	->  (   At == @default
	    ->  (   get(@event, window, Canvas)
		->  get(@event, position, Canvas, Pos)
		;   Pos = point(0,0)
		)
	    ;   Pos = At
	    ),
	    get(@draw_clipboard?graphicals, clone, Clone),
	    send(Canvas, open_undo_group),
	    send(Clone, for_all,
		 and(if(?(@arg1, hypered, slave_of),
			new(and),
			message(@arg1, relative_move, Pos)),
		     message(Canvas, display, @arg1),
		     message(Canvas, undo_action,
			     create(message, @arg1, cut)))),
	    send(Canvas, close_undo_group),
	    send(Canvas, selection, Clone),
	    send(Clone, done),
	    send(Canvas, modified)
	;   send(Canvas, report, warning, 'Draw Clipboard is empty')
	).


		 /*******************************
		 *	 WINDOWS CLIPBOARD	*
		 *******************************/


map_format(aldus, wmf) :- !.
map_format(Fmt, Fmt).

export_win_metafile(Canvas, What:[{selection,drawing}], Format:[{wmf,emf}]) :->
	"Export to the Windows clipboard"::
	send(Canvas, keyboard_focus, @nil),
	default(What, selection, TheWhat),
	(   Format == @default
	->  get_config(draw_config:file/meta_file_format, TheFormat0),
	    map_format(TheFormat0, TheFormat)
	;   TheFormat = Format
	),
	get(Canvas, selection, OldSelection),
	send(Canvas, selection, @nil),
	(   TheWhat == selection
	->  Graphs = OldSelection
	;   get(Canvas, graphicals, Graphs)
	),
	new(MF, win_metafile),
	send(MF, draw_in, Graphs),
	send(@display, selection_owner, MF,
	     primary,			% which
	     @receiver,			% fetch object
	     message(@receiver, free),	% loose selection
	     TheFormat),
	send(Canvas, selection, OldSelection),
	send(Canvas, report, status, 'Put %s on clipboard', TheWhat).

import_win_metafile(Canvas) :->
	"Get selection as picture and import it"::
	(   get(Canvas?display, selection,
		primary, win_metafile, win_metafile, MF)
	->  new(DMF, draw_metafile),
	    send(DMF, copy, MF),
	    send(Canvas, display, DMF),
	    free(MF),
	    send(Canvas, report, status, 'Imported metafile')
	;   send(Canvas, report, warning, 'Could not get clipboard data')
	).

edit_selection(Canvas) :->
	"Start attribute editor on selection"::
	get(Canvas, attribute_editor, Editor),
	(    Editor == @nil
	->   new(A, draw_product_attribute_editor(Canvas)),
		  send(Canvas, slot, attribute_editor, A),
	     send(A, open)
	;    A = Editor,
	     send(A, show, @on),
	     send(A, expose)
	),
	send(A, client, Canvas?selection).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update  the setting   of   the attribute editor   because  either  the
selection has  changed, or the attributes of  members of the selection
has changed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

update_attribute_editor(Canvas) :->
	"Update values in attribute editor"::
	get(Canvas, attribute_editor, Editor),
	(   Editor \== @nil
	->  send(Editor, client, Canvas?selection)
	;   true
	).


clear(Canvas, Confirm:[bool]) :->
	"Clear and reset <->file attribute"::
	(   Confirm == @on,
	    \+ send(Canvas?graphicals, empty)
	->  send(@display, confirm, 'Clear drawing?')
	;   true
	),
	send(Canvas, send_super, clear),
	send(Canvas, file, @nil),
	send(Canvas, slot, modified, @off),
	send(Canvas?undo_buffer, clear),
	send(Canvas, update_attribute_editor).


		/********************************
		*           ALIGNMENT		*
		********************************/
:- pce_group(alignment).


align_with_selection(Canvas, Gr:graphical) :->
	"Align graphical (with selection)"::
	(   get(Canvas, selection, G0),
	    send(G0, delete_all, Gr),
	    \+ send(G0, empty)
	->  true
	;   get(Canvas?graphicals, copy, G0),
	    send(G0, delete_all, Gr)
	),
	get(G0, find_all, not(message(@arg1, instance_of, line)), G1),
	chain_list(G1, L1),
	align_graphical(Gr, L1).


align_selection(Canvas) :->
	"Align all elements of the selection"::
	send(Canvas, edit, message(Canvas, align_graphical, @arg1)).


align_graphical(Canvas, Gr:graphical) :->
	"Align a single graphical"::
	get(Canvas?graphicals, find_all,
	    and(not(message(@arg1, instance_of, line)),
		not(?(@arg1, hypered, supports))),
	    G0),
	send(G0, delete_all, Gr),
	chain_list(G0, L0),
	auto_adjust(resize, Gr, L0),
	align_graphical(Gr, L0).


auto_align(Canvas, Gr:graphical, How:{create,resize,move}) :->
	"Align graphical if auto_align_mode is @on"::
	(   get(Canvas, auto_align_mode, @on)
	->  ignore(auto_align(Canvas, Gr, How))
	;   true
	).


auto_align(Canvas, Gr, How) :-
	get(Canvas?graphicals, find_all,
	    and(not(message(@arg1, instance_of, line)),
		not(?(@arg1, hypered, supports))),
	    G0),
	send(G0, delete_all, Gr),
	chain_list(G0, L0),
	auto_adjust(How, Gr, L0),
	align_graphical(Gr, L0).


auto_adjust(How, Gr, L0) :-
	(How == create ; How == resize),
	\+ send(Gr, instance_of, text),
	adjust_graphical(Gr, L0), !.
auto_adjust(_, _, _).




/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Distribute spaces between graphicals evenly.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


distribute_selection(Canvas) :->
	"Distribute selected objects"::
	get(Canvas, selection, Selection),
	send(Selection, for_all,
	     if(or(message(@arg1, instance_of, connection),
		   ?(@arg1, hypered, supports)),
		message(Selection, delete, @arg1))),
	get(Selection, size, Size),
	(   Size < 3
	->  send(Canvas, report, warning, 'Need at least 3 selected objects')
	;   side_selector(Dir, Top, Bottom, Height),
	    sequence_in_dir(Selection, Dir)
	->  get(Selection?head, Top, Y0),
	    get(Selection?tail, Bottom, Y1),
	    H is Y1 - Y0,
	    new(HTotal, number(0)),
	    send(Selection, for_all, message(HTotal, plus, @arg1?Height)),
	    get(HTotal, value, HT),
	    Sep is (H - HT)/(Size-1),
	    chain_list(Selection, List),
	    send(Canvas, open_undo_group),
	    distribute(List, Y0, Dir, 0, Sep),
	    send(Canvas, close_undo_group),
	    send(Canvas, report, status, 'Distributed in %s-direction', Dir)
	;   send(Canvas, report, warning, 'Cannot determine direction')
	).

sequence_in_dir(Chain, Dir) :-
	side_selector(Dir, Sel, _, _),
	send(Chain, sort, ?(@arg1?Sel, compare, @arg2?Sel)),
	chain_list(Chain, List),
	sequence(List, Dir).

side_selector(y, top_side, bottom_side, height).
side_selector(x, left_side, right_side, width).


sequence([_], _) :- !.
sequence([H1,H2|T], y) :- !,
	get(H1, bottom_side, Bottom),
	get(H2, top_side, Top),
	Top >= Bottom,
	sequence([H2|T], y).
sequence([H1,H2|T], x) :-
	get(H1, right_side, R),
	get(H2, left_side, L),
	L >= R,
	sequence([H2|T], x).

distribute([], _, _, _, _).
distribute([H|T], V0, Sel, N, Sep) :-
	send(H, Sel, V0+N*Sep),
	(   Sel == y
	->  get(H?area, height, Me)
	;   get(H?area, width, Me)
	),
	V1 is V0+Me,
	NN is N + 1,
	distribute(T, V1, Sel, NN, Sep).




	      /********************************
	      *           LOAD/SAVE	      *
	      ********************************/
:- pce_group(file).

load(Canvas, Sheet:sheet, Clear:[bool]) :->
	"Load from named file and [clear]"::
	(    (Clear == @on,
	      \+ send(Canvas?graphicals, empty))
	->   send(Canvas, clear)
	;    true
	),
	get(Sheet, graphicals, Grs),
	send(Grs, for_all,
	     and(message(Canvas, display, @arg1),
		 message(@arg1, selected, @off))),
	send(Canvas, normalise, Grs),
	send(Canvas, slot, modified, @off),
	send(Sheet, done).


		/********************************
		*             MODES		*
		********************************/
:- pce_group(mode).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Switch the mode of the editor.  The mode determines which gestures are
active  (see `gesture.pl') and  therefore what  happens on some event.
For each mode, a cursor is defined to indicate the mode to the user.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

mode(Canvas, Mode:name, Cursor:cursor*) :->
	"Set the mode of the canvas"::
	send(Canvas, cursor, Cursor),
	send(Canvas, slot, mode, Mode),
	send(Canvas, keyboard_focus, @nil),
	send(Canvas, selection, @nil),
					% cleanup pending operations
	(   get(Canvas, focus_recogniser, R),
	    R \== @nil
	->  (   send(R, has_send_method, cancel)
	    ->	send(R, cancel)
	    ;	true
	    ),
	    send(Canvas, focus, @nil)
	;   true
	).
	
:- pce_end_class.