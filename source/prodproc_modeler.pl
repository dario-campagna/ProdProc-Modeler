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

:- use_module(library(pce)).
:- ensure_loaded(library(help_message)).
:- use_module(save_model).
:- use_module(syntax_checking).
:- use_module(validity_checking).

:- consult([ menu
			  , shapes
			  , gesture
			  , product_canvas
			  , process_canvas
			  , attribute
			  , undo
			  ]).

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 Toplevel goal: 
 	prodproc_modeler/0 creates a modeler object and display it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	
prodproc_modeler :-
	new(Modeler, modeler),
	send(Modeler, open, normalise := @on).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class `product_modeler' defines and manages  the entire tool. 
Its initialisation builds  the  entire  tool and the  resulting instance 
provide means of communication between the various parts.  The call
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(modeler, frame, "Product and process modeling tool class").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
starts  the definition of  a new class  `product_modeler' that is a subclass
of class  frame.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

variable(title, name, get, "Base-name of the program").
variable(file, file*, both, "Current save/load file").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise the product modeling tool
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(Modeler, Title:[name]) :->
	default(Title, 'ProdProc Modeler', TheTitle),
	send(Modeler, send_super, initialise, TheTitle),
	send(Modeler, slot, title, TheTitle),
	send(Modeler, done_message, message(Modeler, quit)),
	% Canvas for product modeling
	send(Modeler, append, new(ProductCanvas, draw_product_canvas)),
	% Canvas for process modeling
	send(new(_ProcessCanvas, draw_process_canvas), right, ProductCanvas),
	% Left menu (buttons)
	send(new(Menu, draw_menu), left, ProductCanvas),
	% Top menu (File, Check)
	send(new(D, dialog), above, Menu),
	send(Modeler, fill_dialog, D),
	send(Modeler, fill_menu).

unlink(Modeler) :->
	send(Modeler, send_super, unlink).
	
% Fill the top manu bar
fill_dialog(Modeler, D:dialog) :->
	"Fill the top-dialog window"::
	send(D, border, size(0,2)),
	send(D, gap, size(10,3)),
	send(D, pen, 0),
	new(ProductCanvas, Modeler?product_canvas),
	new(ProcessCanvas, Modeler?process_canvas),
	new(NonEmptyDrawingProduct, not(message(ProductCanvas?graphicals, empty))),
	new(NonEmptyDrawingProcess, not(message(ProcessCanvas?graphicals, empty))),
	%new(HasCurrentFile, ProductCanvas?file \== @nil),
	new(HasCurrentFile, Modeler?file \== @nil),
	
	send(D, append, new(MB, menu_bar(actions))),
	
	send(MB, append, new(F, popup(file))),
	send(MB, append, new(C, popup(check))),
	
	send_list(F, append,
		[ %menu_item( new,
		  %				 message(Canvas, new_model))
		%, 
		  menu_item( open,
						 message(@prolog, load_from, Modeler, ProductCanvas, ProcessCanvas))
		, menu_item( save,
						 message(@prolog, save, Modeler, ProductCanvas, ProcessCanvas),
						 @default, @off,
						 and(or(NonEmptyDrawingProduct, NonEmptyDrawingProcess), 
						 	  or(ProductCanvas?modified == @on, ProcessCanvas?modified == @on),
						 	  HasCurrentFile))
		, menu_item( save_as,
						 message(@prolog, save_as, Modeler, ProductCanvas, ProcessCanvas),
						 @default, @on, 
						 or(NonEmptyDrawingProduct, NonEmptyDrawingProcess))
		, menu_item( quit,
						 message(Modeler, quit),
						 @default, @off)
		]),
		
	send_list(C, append,
		[ menu_item( syntax,
						 message(@prolog, check_syntax, Modeler, ProductCanvas, ProcessCanvas),
						 @default, @off,
						 and(NonEmptyDrawingProduct, HasCurrentFile))
		, menu_item( validity,
						 message(@prolog, check_validity, Modeler, ProductCanvas, ProcessCanvas),
						 @default, @off, 
						 and(NonEmptyDrawingProduct, HasCurrentFile))
		]).

% Fill the (right) icon menu bar 
fill_menu(Modeler) :->
	"Fill icon menu with standard options"::
	get(Modeler, menu, M),
	send(M, proto, @nil,
			select, top_left_arrow, tag:='Select mode'),
	send(M, proto, draw_node('Node',point(0,0),34,17),
			draw_node, dotbox, tag:='Add a node'),
	send(M, proto, link(link),
			draw_edge, plus, tag:='Add an edge/loop'),
	send(M, proto, draw_mc('M. c.s',point(0,0),39,19),
			draw_mc, dotbox, tag:='Add model constraint/s'),
	send(M, proto, draw_activity('Activity',point(0,0),34,17),
			draw_activity, dotbox, tag:='Add activity'),
	send(M, proto, draw_comp_activity('C. act.',point(0,0),34,17),
			draw_comp_activity, dotbox, tag:='Add composite activity'),
	send(M, proto, draw_resource('R',point(0,0),34,17),
			draw_resource, dotbox, tag:='Add resource'),
	send(M, proto, link(link),
			draw_atomic_tc, plus, tag:='Add atomic temporal constraint'),
	send(M, proto, draw_tc(point(0,0),30,20),
			draw_tc, dotbox, tag:='Add temporal constraint'),
	send(M, proto, link(link),
			draw_rc, plus, tag:='Add a resource constraint'),
	send(M, proto, link(link),
			draw_prc, plus, tag:='Add a product related constraint'),
	send(M, proto, draw_pv('P. v.s',point(0,0),39,19),
			draw_pv, dotbox, tag:='Add process variable/s'),
	send(M, proto, draw_cc('C. c.s',point(0,0),39,19),
			draw_cc, dotbox, tag:='Add coupling constraint/s'),
	send(M, modified, @off).
	
		
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The methods below provide  access to the  various parts of the modeling
tool.  It makes  it  easier to remember  how to  access the  parts and
allows for changing the classnames without affecting too much code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

product_canvas(Modeler, C:draw_product_canvas) :<-
	"Find the product drawing canvas"::
	get(Modeler, member, draw_product_canvas, C).

process_canvas(Modeler, C:draw_process_canvas) :<-
	"Find the process drawing canvas"::
	get(Modeler, member, draw_process_canvas, C).
	
menu(Modeler, C:draw_menu) :<-
	"Find the icon menu window"::
	get(Modeler, member, draw_menu, C).
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Mode of operation
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	
select_mode(Modeler) :->
	"Switch to select mode"::
	(   get(Modeler?product_canvas, mode, select), get(Modeler?process_canvas, mode, select)
	->  true
	;   send(Modeler?menu, activate_select)
	).


mode(Modeler, Mode:name, Cursor:cursor) :->
	"Switch the mode"::
	send(Modeler?product_canvas, mode, Mode, Cursor),
	send(Modeler?process_canvas, mode, Mode, Cursor).


proto(Modeler, Proto:'graphical|link*') :->
	"Switch the current prototype"::
	send(Modeler?product_canvas, proto, Proto),
	send(Modeler?process_canvas, proto, Proto).
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Quit the product modeling tool.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
	
quit(Modeler) :->
	"Leave product modeler"::
	get(Modeler, product_canvas, Canvas1),
	get(Modeler, process_canvas, Canvas2),
	send(@prolog, save_if_modified, Modeler, Canvas1, Canvas2),
	(   object(Modeler)
	->  send(Modeler, destroy), halt
	;   true, halt
	).
						 

:- pce_end_class.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SAVE/LOAD of product and process description to/from a file
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% Save in given file product and process model graphicals and create Prolog module
save_as(Modeler, ProductCanvas, ProcessCanvas) :-
	get(@finder, file, save, tuple('ProdProc files', prp), FileName),
	new(File, file(FileName)),
	send(File, backup),
	%send(ProductCanvas, slot, file, File),
	send(Modeler, slot, file, File),
	
	% Product sheet
	send(ProductCanvas, keyboard_focus, @nil),
	new(ProductSheet, sheet(attribute(graphicals, ProductCanvas?graphicals))),
	
	% Process sheet
	send(ProcessCanvas, keyboard_focus, @nil),
	new(ProcessSheet, sheet(attribute(graphicals, ProcessCanvas?graphicals))),
	new(Sheets, chain(ProductSheet, ProcessSheet)),
	send(Sheets, save_in_file, File),
	
	% Create prolog module
	save_model(File, ProductCanvas?graphicals, ProcessCanvas?graphicals),
	
	send(ProductCanvas, slot, modified, @off),
	send(ProcessCanvas, slot, modified, @off),
	send(ProductSheet, free),
	send(ProcessSheet, free),
	send(Sheets, free).
	
% Save in current file (if it exists) product and process model graphicals and create Prolog module
save(Modeler, ProductCanvas, ProcessCanvas) :-
	%get(ProductCanvas, slot, file, SaveFile),
	get(Modeler, slot, file, SaveFile),
	
	(SaveFile \== @nil
	 ->	true, File = SaveFile
	 ;	get(@finder, file, save, tuple('ProdProc files', prp), FileName),
	   new(File, file(FileName)),
		send(ProductCanvas, file, File)
	),
	
	send(File, backup),
	
	% Product sheet
	send(ProductCanvas, keyboard_focus, @nil),
	new(ProductSheet, sheet(attribute(graphicals, ProductCanvas?graphicals))),
	
	% Process sheet
	send(ProcessCanvas, keyboard_focus, @nil),
	new(ProcessSheet, sheet(attribute(graphicals, ProcessCanvas?graphicals))),
	new(Sheets, chain(ProductSheet, ProcessSheet)),
	send(Sheets, save_in_file, File),
	
	% Create prolog module
	save_model(File, ProductCanvas?graphicals, ProcessCanvas?graphicals),
	
	send(ProductCanvas, slot, modified, @off),
	send(ProcessCanvas, slot, modified, @off),
	send(ProductSheet, free),
	send(ProcessSheet, free),
	send(Sheets, free).

% Load product and process graphicals from a file
load_from(Modeler, ProductCanvas, ProcessCanvas) :-
	get(@finder, file, open, tuple('ProdProc files', prp), FileName),
	new(File, file(FileName)),
	get(File, object, Sheets),
	get(Sheets, nth0, 0, ProductSheet),
	get(Sheets, nth0, 1, ProcessSheet),
	
	%send(ProductCanvas, slot, file, File),
	send(Modeler, slot, file, File),
	
	send(ProductCanvas, load, ProductSheet, @on),
	send(ProcessCanvas, load, ProcessSheet, @on).
	
% Open a dialog window if model has been modified but not saved when the user wants to quit the modeler
save_if_modified(Modeler, ProductCanvas, ProcessCanvas) :-
	get(ProductCanvas, slot, modified, ProdMod), get(ProcessCanvas, slot, modified, ProcMod),
	(   (ProdMod == @on ; ProcMod == @on)
	->  get(ProductCanvas, frame, Draw),
	    new(D, dialog('Drawing is modified')),
	    send(D, icon, Draw?icon_image),
	    send(D, transient_for, Draw),
	    send(D, append, label(message, 'Drawing has changed')),
	    send(D, append, button('Save & Quit',
				       message(D, return, save_and_quit))),
		 send(D, append, button(quit,
				       message(D, return, quit))),
	    send(D, append, button(cancel,
				   message(D, return, cancel))),
	    get(D, confirm_centered, Rval),
	    send(D, destroy),
	    (   Rval == save_and_quit
	    ->  send(@prolog, save, Modeler, ProductCanvas, ProcessCanvas),
	        send(Draw, destroy)
	    ;   Rval == quit
	    ->  send(Draw, destroy)
	    ;	Rval == save
	    ->	send(@prolog, save, Modeler, ProductCanvas, ProcessCanvas)
	    )
	;   true
	).
	
% Syntax checking
check_syntax(Modeler, ProductCanvas, ProcessCanvas) :-
	( ( get(ProductCanvas, slot, modified, @on) ; get(ProcessCanvas, slot, modified, @on) )
	-> save(ProductCanvas, ProcessCanvas)
	; 	true
	),
	%get(ProductCanvas, file, PrpFile),
	get(Modeler, file, PrpFile),
	get(PrpFile?name, value, PrpValue),
	sub_atom(PrpValue, 0, _, 4, ModelFile),
	atomic_concat(ModelFile, '.pl', PlModelFile),
	product_process_model(PlModelFile).
	
% Validity checking (automatic configuration)
check_validity(Modeler, _ProductCanvas, _ProcessCanvas) :-
	%get(ProductCanvas, file, PrpFile),
	get(Modeler, file, PrpFile),
	get(PrpFile?name, value, PrpValue),
	sub_atom(PrpValue, 0, _, 4, ModelFile),
	atomic_concat(ModelFile, '.pl', PlModelFile),
	validity_checking(PlModelFile).
	
validity_checking(ModelFile) :-
	new(Frame, frame('Validity checking')),
	send(Frame, append, new(D, dialog)),
	
	new(TextMin, int_item(min_node_number,@default)),
	new(TextMax, int_item(max_node_number,@default)),
	new(TextTime, int_item(max_time,@default)),
	new(TextTimeOut, int_item(time_out,@default)),
	
	new(Check, button(check, 
		message(@prolog, validity_checking, ModelFile, TextMin, TextMax, TextTime, TextTimeOut))),
	
	send_list(D, append, [TextMin, TextMax, TextTime, TextTimeOut, Check]), 
	
	send(D, append, button(quit, message(Frame, destroy))),
	
	send(Frame, open).
	
validity_checking(ModelFile, TextMin, TextMax, TextTime, TextTimeOut) :-
	get(TextMin, selection, Min), atom_to_term(Min,I1,[]),
	get(TextMax, selection, Max), atom_to_term(Max,I2,[]),
	get(TextTime, selection, Time), atom_to_term(Time,I3,[]),
	get(TextTimeOut, selection, TimeOut), atom_to_term(TimeOut, I4, []),
	
	gensym(inst_gen, Thread),
	
	(	I1 > I2
	-> error_message('The minimum value has to be lesser than the maximum value!')
	;	% Create a browser where to show messages and generation computation/results
		new(Frame, frame('Instance generation')),
		send(Frame, append, new(D, dialog)),
		new(Size, size(80,40)),
		new(Browser, browser('',Size,D)),
		new(Stop, button(stop, message(@prolog, thread_signal, Thread, halt) )),
		new(Quit, button(quit, message(Frame, destroy)) ),
		send(D, append, Browser),
		send(D, append, Stop),
		send(D, append, Quit),
		send(Frame, open),
		thread_create(generate_instance(ModelFile, I1, I2, I3, I4, Browser),_,[alias(Thread)])
	).