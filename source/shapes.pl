/*  $Id$

    Part of ProdProc Modeler
    
    Originally part of of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam
    
    Modified by: 	 Dario Camapgna (for ProdProc Modeler)
    E-mail:			 dario.campagna@gmail.com
    WWW:				 http://www.dmi.unipg.it/dario.campagna/software/ProdProc_Modeler.html
    Copyright (C): 2011, Dario Campagna

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(draw_shapes, []).

:- use_module(library(pce)).
:- use_module(library(pce_template)).
:- use_module(tagged_edge).
:- use_module(node_variable).
:- use_module(multi_box).
:- use_module(temporal_constraint).
:- require([ default/3
	   , forall/2
	   , get_config/2
	   , ignore/1
	   , member/2
	   ]).

:- multifile
	user:pce_pre_expansion_hook/2.
:- dynamic
	user:pce_pre_expansion_hook/2.

user:pce_pre_expansion_hook((:- draw_begin_shape(Name, Super,
						 Summary, Recognisers)),
	       [(:- pce_begin_class(draw_shape_class:Name, Super, Summary)),
		(:- use_class_template(draw_shape)),
		(:- pce_class_directive(draw_shapes:associate_recognisers(Recognisers)))
	       ]).
user:pce_pre_expansion_hook((:- draw_end_shape), (:- pce_end_class)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines the various shapes that can be used to construct the
diagram.   Most  of  the  shapes  are   very  close  the  PCE's  drawing
primitives.  Two things have to be added   for each of them: handles for
connecting lines (connections) and event-handling.

Programming can be done both at the class  and at the instance level.  I
decided to add them at the class  level.  As there are normally multiple
instances of the classe, this  approach   reduces  memory  cost.  A more
important  issue  is  kloning  and    saving.    These  operations  work
recursively  and  therefore  would  clone   and  save  the  object-level
extensions.  For saving, this has two   disadvantages.   The saved files
would get bigger and, more important, the   gestures -defining the UI of
the tool- would be saved too.  This leads  to a bad separation of UI and
the actual data manipulated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		/********************************
		*	  COMMON TEMPLATE	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
To facilate users to refine PceDraw for   their own needs, we designed a
very simple schema for defining  new  shapes.    A  PceDraw  shape  is a
subclass of a PCE graphical or of   another PceDraw shape.  Such classes
are defined between the braces:

	:- draw_begin_shape(Name, Super, Summary, Recognisers).

	...

	:- draw_end_shape.

The public predicate draw_begin_shape/4 creates a  new XPCE class `Name'
below `Super'.  The  class  object  itself   is  an  instance  of  class
draw_shape_class, rather then of  the  normal   XPCE  class  class.  The
reason for this is to allow for certain  definitions to be raised to the
meta-class level.

We extend the  meta-knowledge  represented   in  classes  with  `hidden'
attributes (attributes that *can*, but *are not* edited by the attribute
editor (see `draw_shape ->has_attribute') and recognisers.

NOTE:	I consider allowing for class-level recognisers anyway, avoiding
	the need for explicit event-handling methods in many cases.

First the definition of the meta-class draw_shape_class:
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_shape_class, class, "Handle class-level stuff").

variable(hidden_attributes, chain,  get, "Masked attributes").
variable(recognisers,	    chain,  get, "Event-handling recognisers").
variable(part_attributes,   sheet*, get, "Compound attribute dispatching").

initialise(Class, Name, Super) :->
	send(Class, send_super, initialise, Name, Super),
	(   get(Class, super_class, SuperClass),
	    send(SuperClass, instance_of, draw_shape_class)
	->  send(Class, slot, hidden_attributes,
		 SuperClass?hidden_attributes?copy),
	    send(Class, slot, recognisers,
		 SuperClass?recognisers?copy),
	    send(Class, slot, part_attributes,
		 SuperClass?part_attributes?clone)
	;   send(Class, slot, hidden_attributes, new(chain)),
	    send(Class, slot, recognisers, new(chain))
	).

:- pce_group(attribute).

hidden_attribute(Class, Attr:name) :->
	"Register a hidden attribute"::
	get(Class, hidden_attributes, Hidden),
	send(Hidden, add, Attr).

part_attribute(Class, Attribute:name, Part:name) :->
	"Attribute must be manipulated on part"::
	get(Class, part_attributes, A0),
	(   A0 == @nil
	->  send(Class, slot, part_attributes, new(Mapping, sheet))
	;   Mapping = A0
	),
	send(Mapping, value, Attribute, Part).

:- pce_group(handle).

delete_all_handles(Class) :->
	"Delete all registered handles"::
	(   get(Class, handles, Chain),
	    Chain \== @nil
	->  send(Chain, clear)
	;   true
	).

:- pce_group(event).

recogniser(Class, Recogniser:recogniser) :->
	"Register (prepend) a recogniser"::
	get(Class, recognisers, Recognisers),
	send(Recognisers, add, Recogniser).

:- pce_end_class.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			EXPANSION

The following fragment defines compiler expansion for:

	:- draw_begin_shape
	...
	:- draw_end_shape.

:- draw_begin_shape should create an instance of call draw_shape_class
rarther then class class.  This is achieved using the construct

	:- pce_begin_class(MetaClass:Class(...), ...)

Which tells pce_realise_class/1 that it  should   create  the  new class
using the call

	new(_, MetaClass(Class, Super))

rather then the default

	get(Super, sub_class, Class, _)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

associate_recognisers(Recognisers) :-
	forall(member(R, Recognisers),
	       send(@class, recogniser, R)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
All shape classes need to implement a  protocol to the draw_canvas class
(and  through  there  to   the   attribute    editor).    To   do  this,
draw_begin_shape/4 will include the `class  template' draw_shape in each
direct subclass of a non-PceDraw class.

Including a class template  implies  that   all  methods  defined on the
template class below class  `template'  (an   empty  class  below  class
object)  will  be  included  into  the    current  class.   Neither  the
implementation, nor the method/variable object   itself is copied: their
references are simply included  in   the  `class <-send_methods', `class
<-get_methods' or `class <-instance_variables', depending  on the object
included.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_shape, template,
		   "Common methods for PceDraw objects").

geometry(Gr, X:[int], Y:[int], W:[int], H:[int]) :->
	"Like super-method, but activate ->modified"::
	(   get(Gr, window, Window)
	->  send(Window, open_undo_group),
	    get(Gr, area, area(OX, OY, OW, OH)),
	    Msg =.. [message, Gr, do_set, OX, OY, OW, OH],
	    send(Window, undo_action, Msg),
	    send(Gr, send_super, geometry, X, Y, W, H),
	    send(Window, close_undo_group)
	;   send(Gr, send_super, geometry, X, Y, W, H)
	),
	send(Gr, modified).

cut(Gr) :->
	"Remove graphical from the drawing"::
	( (send(Gr, instance_of, multi_box) 
	  ; send(Gr, instance_of, temporal_constraint) 
	  ; send(Gr, instance_of, tagged_edge))
	->	send(Gr, unlink)
	;	true
	),
	(   get(Gr, attribute, cutting, _) % avoid recursion
	->  true
	;   send(Gr, attribute, cutting),
	    get(Gr, window, Window),
	    send(Window, open_undo_group),
	    get(Gr, device, OldDev),
	    send(Gr, device, @nil),
	    send(Window, undo_action, message(Gr, un_cut, OldDev)),
	    send(Window, close_undo_group),
	    send(Gr, delete_attribute, cutting)
	).

un_cut(Gr, Dev:device*) :->
	"Redisplay a cutted graphical"::
	send(Gr, device, Dev),
	get(Gr, window, Window),
	send(Window, open_undo_group),
	send(Window, undo_action, message(Gr, cut)),
	send(Window, close_undo_group).

:- pce_group(attribute).

draw_attribute(Gr, Att, Val) :->
	"Modify an attribute if ->has_attribute"::
	send(Gr, has_attribute, Att),
	get(Gr, draw_attribute, Att, OldVal),
	send(OldVal, lock_object, @on),
	(   catch(send(OldVal, equal, Val), _, fail)
	->  true
	;   get(Gr, window, Window),
	    send(Window, open_undo_group),
	    send(Gr, Att, Val),
	    send(Window, undo_action,
		 message(Gr, draw_attribute, Att, OldVal)),
	    send(Window, close_undo_group)
	),
	send(Gr, modified).
draw_attribute(Gr, Att, Val) :<-
	"Just completeness"::
	get(Gr, Att, Val).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->has_attribute is used by the attribute   editor to find the attributes
that can be manipulated on the currently selected object.

A name is defined an attribute if it  can both be modified and requested
(i.e.  there is send- and get-behaviour for   the  name).  The class (an
instance of draw_shape_class), defines a chain   of  attributes that are
explicitely masked.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

has_attribute(Gr, Att:name) :->
	"Test if object defines attribute"::
	send(Gr, has_send_method, Att),
	send(Gr, has_get_method, Att),
	\+ send(Gr, hidden_attribute, Att).


hidden_attribute(Gr, Att:name) :->
	"True if attibute is not editable"::
	get(Gr, class, Class),
	get(Class, hidden_attributes, Hidden),
	send(Hidden, member, Att).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If an object is ->modified, the modified   flag of the drawing should be
updated and the attribute editor should be notified.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_group(modified).

modified(Gr) :->
	"Inform <-window and update attribute editor"::
	(   get(Gr, window, Window),
	    %send(Window, modified),
	    get(Gr, selected, @on),
	    send(Window, update_attribute_editor)
	->  true
	;   true
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->event just walks through the recognisers defined on the class.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_group(event).

event(Gr, Ev:event) :->
	"Handle <-class recognisers"::
	(   send(Gr, send_super, event, Ev)
	;   get(Gr?class, recognisers, Chain),
	    get(Chain, find,
		message(@arg1, event, Ev),
		_)
	).

:- pce_group(mode).

mode(Gr, Mode:name) :<-
	"Request <-window's <-mode"::
	get(Gr, window, Window),
	get(Window, mode, Mode).


:- pce_group(edit).

undo_restack_action(Gr) :->
	"Register restack-undo action"::
	(   get(Gr, window, Canvas)
	->  send(Canvas, open_undo_group),
	    get(Gr?device, graphicals, Grs),
	    (   get(Grs, next, Gr, Next)
	    ->  send(Canvas, undo_action,
		     message(Gr, hide, Next))
	    ;   send(Canvas, undo_action,
		     message(Gr, expose))
	    ),
	    send(Canvas, close_undo_group)
	;   true
	).


hide(Gr, Behind:[graphical]) :->
	send(Gr, undo_restack_action),
	send(Gr, send_super, hide, Behind).

expose(Gr, Before:[graphical]) :->
	send(Gr, undo_restack_action),
	send(Gr, send_super, expose, Before).

restack(Gr, How:'{hide,expose}|int') :->
	"Hide one step or to background"::
	(   integer(How)
	->  get(Gr?device, graphicals, Grs),
	    get(Grs, index, Gr, Idx),
	    I is Idx + How,
	    (	get(Grs, nth1, I, Before)
	    ->  (   How < 0
		->  send(Gr, hide, Before)
		;   send(Gr, expose, Before)
		)
	    )
	;   send(Gr, How)		% hide, expose
	).

:- pce_end_class.


		/********************************
		*             NODE              *
		********************************/

:- draw_begin_shape(draw_node, box, "Node",
		    [@draw_node_recogniser]).

variable(node_name, name:='New node', both, "Name of the node").
variable(node_variables, chain, both, "Node variables").
variable(node_constraints, chain, both, "Node constraints").
		    
initialise(N,Name,Pos,W,H) :->
	send(N, send_super, initialise),
	(Name \= '' -> send(N, node_name, Name) ; true),
	send(N, width, W),
	send(N, height, H),
	send(N, center, Pos),
	send(N, node_variables, new(_, chain)),
	send(N, node_constraints, new(_, chain)).
		
handle(w/2, 0,   link, north).
handle(w/2, h,   link, south).
handle(0,   h/2, link, west).
handle(w,   h/2, link, east).

'_redraw_area'(N, A:area) :->
	get(N, area, area(X, Y, W, H)),
	send(N, draw_text, 
		N?node_name, font(helvetica, roman, 11), X, Y, W, H, center, top),
	( get(N, node_variables, Variables), send(Variables,empty)
	->	true
	;	send(N, draw_line, X, Y+20, X+99, Y+20),
		send(N, draw_text, 'Variable/s', font(helvetica, roman, 11), X, Y+22, W, H, center)
	),
	( get(N, node_constraints, Constraints), send(Constraints, empty)
	-> true
	;  send(N, draw_line, X, Y+35, X+99, Y+35),
		send(N, draw_text, 'Constraint/s', font(helvetica, roman, 11), X, Y+37, W, H, center)
	),
	send(N, send_super, '_redraw_area', A).

:- draw_end_shape.



		/********************************
		*              EDGE             *
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
An edge is an arrow between two nodes, or entering and exiting from a node
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- draw_begin_shape(constraint_ellipse, ellipse, "Constraint ellipse", []).

handle(w/2, h-15, link, center).

initialise(E) :->
	send(E, send_super, initialise, 56, 16).
	
'_redraw_area'(E, A:area) :->
	get(E, area, area(X, Y, W, H)),
	send(E, draw_text, 
		'Constraint/s', font(helvetica, roman, 8), X, Y, W, H, center, center),
	send(E, send_super, '_redraw_area', A).

:- draw_end_shape.

:- draw_begin_shape(draw_edge, tagged_edge, "Edge",
		    [@draw_edge_recogniser]).		    

handle(w/2, h/2, link, center).

variable(edge_label, name:='', both, "Edge label").

variable(edge_cardinality_name, name:='Card', both, "Cardinality name").
variable(edge_cardinality_type, {const_card,range_card}:=const_card, both, "Cardinality type, const_card or range_card").
variable(edge_cardinality_value, int:=1, both, "Constant cardinality value").
variable(edge_cardinality_min_value, int:=0, both, "Minimum cardinality value").
variable(edge_cardinality_max_value, int:=1, both, "Minimum cardinality value").
variable(edge_constraints, chain, both, "Edge constraints").

initialise(E, Label:name, F:graphical, T:graphical, L:[link], HF:[name]*, HT:[name]*) :->
	send(E, send_super, initialise, F, T, L, HF, HT),
	% Label
	(Label \= '' -> send(E, edge_label, Label) ; true),
	% Text label tag
	new(TextLabel, text(E?edge_label)),
	send(E, label_tag, TextLabel),
	% Card label tag
	card_to_text(E, Card),
	new(TextCard, text(Card)),
	send(E, card_tag, TextCard),
	% Bezier curve tag
	(F == T
	-> bezier_points(E, Start, End, C1, C2),
		new(Bz, bezier_curve(Start, End, C1, C2)),
		send(Bz, arrows, second),
		send(E, bez_tag, Bz)
	; 	send(E, arrows, second)
	),
	(   get(E, window, Window),
	    send(Window, open_undo_group),
	    send(Window, undo_action, message(E, cut)),
	    send(Window, close_undo_group)
	;   true
	),
	% Constraints
	send(E, edge_constraints, new(_, chain)).
	
'_redraw_area'(E, A:area) :->
	new(TextLabel, text(E?edge_label)),
	send(E, label_tag, TextLabel),
	card_to_text(E, Card),
	new(TextCard, text(Card)),
	send(E, card_tag, TextCard),
	(get(E, bez_tag, @nil) 
	-> true 
	;  bezier_points(E, Start, End, C1, C2),
		new(Bz, bezier_curve(Start, End, C1, C2)),
		send(Bz, arrows, second),
		send(E, bez_tag, Bz),
		send(E, pen, 0)
	),
	( get(E, edge_constraints, Constraints), send(Constraints, empty)
	-> send(E, constraints_tag, @nil)
	;	%new(C, text('Constraint/s')),
		new(C, constraint_ellipse),
		send(E, constraints_tag, C),
		new(Con, line(0,0,0,0)),
		send(E, con_tag, Con)
	),
	send(E, send_super, '_redraw_area', A).

geometry(Gr, X:[int], Y:[int], W:[int], H:[int]) :->
	"No logging needed"::
	send(Gr, send_super, geometry, X, Y, W, H).
	
bezier_points(E, Start, End, C1, C2) :-
	get(E, start, Start), get(E, end, End),
	get(Start, x, Sx), get(Start, y, Sy),
	get(End, x, Ex), get(End, y, Ey),
	get(E, from, Gr), get(Gr, x, Gx), get(Gr, y, Gy),
	( Sy = Ey, Ey = Gy
	->	new(C1, point(Sx+10,Ey-40)), new(C2, point(Ex-10,Ey-40))
	;  ( Sy = Ey
		-> new(C1, point(Sx+10,Sy+40)), new(C2, point(Ex-10,Ey+40))
		;	( Sx-Gx > 50
			->	new(C1, point(Sx+100,Sy-40)), new(C2, point(Ex+100,Ey+40))
			;  new(C1, point(Sx-100,Sy-40)), new(C2, point(Ex-100,Ey+40))
			)
		)
	).
	
cut(Gr) :->
	"Remove graphical from the drawing"::
	get(Gr, window, Window),
	send(Window, open_undo_group),
	get(Gr, from, From),
	get(Gr, to, To),
	send(Window, undo_action, message(Gr, un_cut, From, To)),
	send(Gr, unlink),
	send(Gr, relate, @nil, @nil),
	send(Window, close_undo_group).

un_cut(Gr, From:graphical, To:graphical) :->
	"Redisplay a cutted graphical"::
	send(Gr, relate, From, To),
	get(Gr, window, Window),
	send(Window, open_undo_group),
	send(Window, undo_action, message(Gr, cut)),
	send(Window, close_undo_group).

start_text(_C, _Ev:[event]) :->
	"Dummy method"::
	true.
	
card_to_text(E, Text) :-
	get(E, edge_cardinality_type, const_card)
	-> get(E, edge_cardinality_value, Text)
	;	get(E, edge_cardinality_name, Name),
		get(E, edge_cardinality_min_value, Min),
		get(E, edge_cardinality_max_value, Max),
		atomic_list_concat([Name,', [',Min,',',Max,']'], Text).

:- draw_end_shape.


		/********************************
		*       MODEL CONSTRAINTS       *
		********************************/

:- draw_begin_shape(draw_mc, ellipse, "Model constraints",
		    [@draw_mc_recogniser]).

variable(mc_set, name:='Model constraints', both, "Set of model constraints").
variable(model_constraints, chain, both, "Node constraints").
		    
initialise(MC,Name,Pos,W,H) :->
	send(MC, send_super, initialise),
	(Name \= '' -> send(MC, mc_set, Name) ; true),
	send(MC, width, W),
	send(MC, height, H),
	send(MC, center, Pos),
	send(MC, model_constraints, new(_, chain)).
		
'_redraw_area'(MC, A:area) :->
	get(MC, area, area(X, Y, W, H)),
	send(MC, draw_text, 
		MC?mc_set, font(helvetica, roman, 11), X, Y, W, H, center, center),
	send(MC, send_super, '_redraw_area', A).

:- draw_end_shape.

:- pce_begin_class(product_constraint, object, 'Constraint').

variable(constraint, (name*):=(@nil), both, 'The constraint').

initialise(Constraint, C) :->
	send(Constraint, constraint, C).

:- pce_end_class.


		/********************************
		*             ACTIVITY          *
		********************************/

:- draw_begin_shape(draw_activity, multi_box, "Activity",
		    [@draw_activity_recogniser]).

variable(activity_name, name:='New activity', both, "Name of the activity").
variable(duration_constraints, chain*, both, "Activity duration constraints").
variable(multi_instance, bool:=(@off), both, "Multiple instance flag").
variable(insts, name:='insts', both, "Variable for number of instances").
variable(min_insts, int:=1, both, "Minimum number of instances").
variable(max_insts, int:=1, both, "Maximum number of instances").
variable(is_absent, bool:=(@off), both, "is-absent (must-be-executed) flag").
variable(condition, (name*):=('none'), both, "Condition on is-absent/must-be-exeuted constraint").
		    
initialise(A,Name,Pos,W,H) :->
	send(A, send_super, initialise),
	(Name \= '' -> send(A, activity_name, Name) ; true),
	send(A, width, W),
	send(A, height, H),
	send(A, center, Pos),
	send(A, duration_constraints, new(_, chain)).
		
handle(w/2, 0,   link, north).
handle(w/2, h,   link, south).
handle(0,   h/2, link, west).
handle(w,   h/2, link, east).

'_redraw_area'(Activity, A:area) :->
	get(Activity, area, area(X, Y, W, H)),
	send(Activity, draw_text, Activity?activity_name, font(helvetica, roman, 11), X, Y, W, H, center, top),
	( get(Activity, duration_constraints, Constraints), send(Constraints, empty)
	-> true
	;  %send(Activity, draw_line, X, Y+35, X+99, Y+35),
		new(Constraint, text('Constraint/s', center, font(helvetica, roman, 11))),
		send(Activity, constraint_tag, Constraint)
	),
	( (get(Activity, condition, Cond), Cond \== @nil, Cond \== '', Cond \== 'none')
	->	new(TextCond, text(Cond, center, font(helvetica, roman, 11))),
		send(Activity, condition_tag, TextCond)
	;	send(Activity, condition_tag, @nil)
	),
	( get(Activity, multi_instance, @off)
	->	send(Activity, fill_pattern, @nil),
		send(Activity, name_tag, @nil),
		send(Activity, box_1_tag, @nil),
		send(Activity, box_2_tag, @nil)
	;	send(Activity, fill_pattern, colour(white)),
		new(Name, text(Activity?activity_name, center, font(helvetica, roman, 11))),
		send(Activity, name_tag, Name),
		new(B1, box(W, H)),
		new(B2, box(W, H)),
		
		(get(Activity, is_absent, @on)
		->	send(B1, texture, dotted), send(B2, texture, dotted)
		;	send(B1, texture, none), send(B2, texture, none)
		),
		
		send(Activity, box_1_tag, B1),
		send(Activity, box_2_tag, B2)
	),
	(get(Activity, condition, 'none')
	-> send(Activity, texture, none), send(Activity, pen, 1)
	;	(get(Activity, is_absent, @on)
		->	send(Activity, texture, dotted), send(Activity, pen, 1)
		;	send(Activity, texture, none), send(Activity, pen, 3)
		)
	),
	send(Activity, send_super, '_redraw_area', A).

:- draw_end_shape.


		/********************************
		*      COMPOSITE ACTIVITY       *
		********************************/

:- draw_begin_shape(draw_comp_activity, multi_box, "Composite activity",
		    [@draw_activity_recogniser]).

variable(activity_name, name:='New activity', both, "Name of the activity").
variable(duration_constraints, chain, both, "Activity duration constraints").
variable(multi_instance, bool:=(@off), both, "Multiple instance flag").
variable(insts, name:='insts', both, "Variable for number of instances").
variable(min_insts, int:=1, both, "Minimum number of instances").
variable(max_insts, int:=1, both, "Maximum number of instances").
variable(is_absent, bool:=(@off), both, "is-absent/must-be-executed flag").
variable(condition, (name*):=('none'), both, "Condition on is-absent/must-be-exeuted constraint").
variable(process, chain, both, "Process associated to the composite activity").
		    
initialise(A,Name,Pos,W,H) :->
	send(A, send_super, initialise),
	(Name \= '' -> send(A, activity_name, Name) ; true),
	send(A, width, W),
	send(A, height, H),
	send(A, center, Pos),
	send(A, process, new(_, chain)),
	send(A, duration_constraints, new(_, chain)).
		
handle(w/2, 0,   link, north).
handle(w/2, h,   link, south).
handle(0,   h/2, link, west).
handle(w,   h/2, link, east).

'_redraw_area'(Activity, A:area) :->
	get(Activity, area, area(X, Y, W, H)),
	send(Activity, draw_text, Activity?activity_name, font(helvetica, roman, 11), X, Y, W, H, center, top),
	( W < 50
	-> send(Activity, draw_box, X+2, Y+2, W-4, H-4)
	;	true
	),
	( get(Activity, duration_constraints, Constraints), send(Constraints, empty)
	-> true
	;	new(Constraint, text('Constraint/s', center, font(helvetica, roman, 11))),
		send(Activity, constraint_tag, Constraint)
	),
	( (get(Activity, condition, Cond), Cond \== @nil, Cond \== '', Cond \== 'none')
	->	new(TextCond, text(Cond, center, font(helvetica, roman, 11))),
		send(Activity, condition_tag, TextCond)
	;	send(Activity, condition_tag, @nil)
	),
	( get(Activity, multi_instance, @off)
	->	send(Activity, fill_pattern, @nil),
		send(Activity, name_tag, @nil),
		new(InBox, box(W-9, H-9)),
		(	get(Activity, is_absent, @on)
		->	send(InBox, texture, dotted)
		;	send(InBox, texture, none)
		),
		send(Activity, inbox_tag, InBox),
		send(Activity, box_1_tag, @nil),
		send(Activity, box_2_tag, @nil)
	;	send(Activity, fill_pattern, colour(white)),
		new(Name, text(Activity?activity_name, center, font(helvetica, roman, 11))),
		send(Activity, name_tag, Name),
		new(InBox, box(W-9, H-9)),
		new(B1, box(W, H)),
		new(B2, box(W, H)),
		(	get(Activity, is_absent, @on)
		->	send(B1, texture, dotted), send(B2, texture, dotted), send(InBox, texture, dotted)
		;	send(B1, texture, none), send(B2, texture, none), send(InBox, texture, none)
		),
		send(Activity, inbox_tag, InBox),
		send(Activity, box_1_tag, B1),
		send(Activity, box_2_tag, B2)
	),
	(get(Activity, condition, 'none')
	-> send(Activity, texture, none), send(Activity, pen, 1)
	;	(get(Activity, is_absent, @on)
		->	send(Activity, texture, dotted), send(Activity, pen, 1)
		;	send(Activity, texture, none), send(Activity, pen, 3)
		)
	),
	send(Activity, send_super, '_redraw_area', A).
	
sub_process(Activity, Proc) :-> 
	get(Proc, clone, NewProc),
	send(Activity, slot, process, NewProc).

:- draw_end_shape.


		/********************************
		*            RESOURCE           *
		********************************/

:- draw_begin_shape(draw_resource, multi_box, "Composite activity",
		    [@draw_resource_recogniser]).
		    
variable(resource_name, name:='New resource', both, "Name of the resource").
variable(quantity_min, int:=0, both, "").
variable(quantity_max, int:=0, both, "").
variable(initial_value, int:=1, both, "Activity duration constraints").

initialise(R,Name,Pos,W,H) :->
	send(R, send_super, initialise),
	(Name \= '' -> send(R, resource_name, Name) ; true),
	send(R, width, W),
	send(R, height, H),
	send(R, center, Pos).

handle(w/2, 0,   link, north).
handle(w/2, h,   link, south).
handle(0,   h/2, link, west).
handle(w,   h/2, link, east).

'_redraw_area'(R, A:area) :->
	get(R, area, area(X, Y, W, H)),
	send(R, draw_text, R?resource_name, font(helvetica, roman, 11), X, Y, W, H, center, center),
	get(R, quantity_min, Min),
	get(R, quantity_max, Max),
	atomic_list_concat(['[',Min,',',Max,']'], Text),
	send(R, draw_text, Text, font(helvetica, roman, 11), X, Y, W, H, center, top),
	send(R, draw_text, R?initial_value, font(helvetica, roman, 11), X, Y, W, H, center, bottom),
	send(R, pen, 0),
	new(Path, path(poly)),
	send(R, path_tag, Path),
	( W < 50
	-> send(R, draw_poly, chain(point(X + 1, Y + H/2), point(X + W/3, Y + 1), point(X + 2*(W/3), Y + 1),
									 point(X + W - 1, Y + H/2), point(X + 2*(W/3), Y + H - 1), point(X + W/3, Y + H - 1)), @on)
	; true
	),
	send(R, send_super, '_redraw_area', A).
		    
:- draw_end_shape.


		/********************************
		*  ATOMIC TEMPORAL CONSTRAINT   *
		********************************/

:- draw_begin_shape(draw_atomic_tc, tagged_edge, "Atomic temporal constraint",
		    [@draw_atomic_tc_recogniser]).		    

handle(w/2, h/2, link, center).

variable(atomic_constraint, name:='', both, "Atomic constraint").
variable(condition, (name*):=(@nil), both, "Condition on the constraint"). % if constraint or iff constraint

initialise(E, Label:name, F:graphical, T:graphical, L:[link], HF:[name]*, HT:[name]*) :->
	send(E, send_super, initialise, F, T, L, HF, HT),
	% Atomic constraint
	(Label \= '' -> send(E, atomic_constraint, Label) ; true),
	% Text atomic constraint
	new(TextLabel, text(E?atomic_constraint)),
	send(E, label_tag, TextLabel),
	% White arrow
	new(A, arrow(8,5,closed,@white_image)),
	send(A, pen, 1),
	send(E, second_arrow, A).
	
'_redraw_area'(E, A:area) :->
	new(TextLabel, text(E?atomic_constraint)),
	send(E, label_tag, TextLabel),
	send(E, colour, blue),
	get(E, condition, Cond),
	((Cond \== @nil, Cond \== '')
	->	new(TextCond, text(Cond)),
		send(E, card_tag, TextCond)
	;	true
	),
	send(E, send_super, '_redraw_area', A).
	
:- draw_end_shape.

		/********************************
		*     TEMPORAL CONSTRAINT       *
		********************************/

:- draw_begin_shape(draw_tc, temporal_constraint, "Temporal constraint",
		    [@draw_tc_recogniser]).		    

handle(w/2, 0,   link, north).
handle(w/2, h,   link, south).
handle(0,   h/2, link, west).
handle(w,   h/2, link, east).

variable(temporal_constraint, name:='', both, "Atomic constraint").
variable(activities, (chain*):=(@nil), both, "Activities involved in the constraint").

initialise(TC,Pos,W,H) :->
	send(TC, send_super, initialise),
	send(TC, width, W),
	send(TC, height, H),
	send(TC, center, Pos),
	send(TC, activities, new(_,chain)).

'_redraw_area'(TC, A:area) :->
	get(TC, area, area(X, Y, W, H)),
	send(TC, draw_text, 'T.C.', font(helvetica, roman, 11), X, Y, W, H, center, bottom),
	send(TC, pen, 0),
	new(Path, path(poly)),
	send(Path, colour, blue),
	send(TC, path_tag, Path),
	( W < 50
	-> send(TC, draw_poly, chain(point(X + W/2, Y), point(X + W, Y + H - 1), point(X, Y + H - 1)), @on)
	; true
	),
	get(TC, activities, Activities),
	( send(Activities, empty)
	->	send(TC, connection_tag, @nil)
	;	get(TC, device, Canvas), get(Canvas, graphicals, CGs),
		collect_activities(Activities, CGs, Graphicals),
		new(CC, chain),
		create_connections(TC, Graphicals, Connections, CC),
		send(TC, connection_tag, Connections)
	),
	send(TC, send_super, '_redraw_area', A).
	
collect_activities(Activities, CGs, Graphicals) :- 
	chain_list(Activities, LAs), 
	get(CGs, find_all, message(@arg1, instance_of, draw_activity), A),
	get(CGs, find_all, message(@arg1, instance_of, draw_comp_activity), CA),
	chain_list(A, LA), chain_list(CA, LCA), append(LA, LCA, ListActivities),
	collect_activities(LAs, ListActivities, Graphicals, []).
	
collect_activities([], _, Graphicals, Graphicals).
collect_activities([A|As], AGs, [G|Tail1], Tail2) :-
	find_activity(AGs, A, G),
	collect_activities(As, AGs, Tail1, Tail2).
	
find_activity([AG|_AGs], A, AG) :-
	get(AG, activity_name, A).
find_activity([_|AGs], A, AG) :- find_activity(AGs, A, AG).
	
create_connections(_TC, [], Connections, Connections).
create_connections(TC, [G|Gs], Connections, CC) :-
	( send(TC, connected, G)
	->	get(TC, connected, G, C)
	;	new(C, connection(TC, G))
	),
	send(C, colour, blue),
	send(CC, append, C),
	create_connections(TC, Gs, Connections, CC).


	
:- draw_end_shape.


		/********************************
		*     RESOURCE CONSTRAINT       *
		********************************/

:- draw_begin_shape(draw_rc, tagged_edge, "Resource constraint",
		    [@draw_rc_recogniser]).		    

handle(w/2, h/2, link, center).

variable(q_type, {constant, variable}:=constant, both, "").
variable(quantity, int:=0, both, "Unit produced/consumed").
variable(quantity_var, name:='q', both, "Resource quantity variable").
variable(quantity_min, int:=0, both, "").
variable(quantity_max, int:=0, both, "").
variable(time_extent, name:='FromStartToEnd', both, "Time extent").
variable(condition, (name*):=(@nil), both, "Condition on the constraint").

initialise(E, F:graphical, T:graphical, L:[link], HF:[name]*, HT:[name]*) :->
	send(E, send_super, initialise, F, T, L, HF, HT),
	% Label
	new(TextLabel, text(E?quantity_var)),
	send(E, label_tag, TextLabel). %,
	% Arrow
	%new(A, arrow(8,6,open,@nil)),
	%send(E, second_arrow, A).
	
'_redraw_area'(E, A:area) :->
	resource_constraint_label(E, Label),
	new(TextLabel, text(Label)),
	send(E, label_tag, TextLabel),
	send(E, colour, red),
	get(E, condition, C),
	(C \== @nil
	-> new(TextCond, text(C)),
		send(E, card_tag, TextCond)
	;	true
	),
	send(E, send_super, '_redraw_area', A).
	
resource_constraint_label(E, Label) :-
	( get(E, q_type, constant)
	->	get(E, quantity, Q)
	;	get(E, quantity_var, QV), get(E, quantity_min, QMin), get(E, quantity_max, QMax),
		atomic_list_concat([QV,',[',QMin,',',QMax,']'],Q)
	),
	get(E, time_extent, TE),
	atomic_list_concat([Q,',',TE], Label).
	
:- draw_end_shape.

		/********************************
		*       PROCESS VARIABLES       *
		********************************/

:- draw_begin_shape(draw_pv, box, "Process variables",
		    [@draw_pv_recogniser]).

variable(pv_set, name:='Process variables', both, "Set of process variables").
variable(process_variables, chain, both, "Process variables").
		    
initialise(PV,Name,Pos,W,H) :->
	send(PV, send_super, initialise),
	(Name \= '' -> send(PV, pv_set, Name) ; true),
	send(PV, width, W),
	send(PV, height, H),
	send(PV, center, Pos),
	send(PV, radius, 8),
	send(PV, process_variables, new(_, chain)).
		
'_redraw_area'(PV, A:area) :->
	get(PV, area, area(X, Y, W, H)),
	send(PV, draw_text, 
		PV?pv_set, font(helvetica, roman, 11), X, Y, W, H, center, center),
	send(PV, radius, 8),
	send(PV, send_super, '_redraw_area', A).

:- draw_end_shape.


		/********************************
		*  PRODUCT RELATED CONSTRAINT   *
		********************************/

:- draw_begin_shape(draw_prc, tagged_edge, "Product related constraint",
		    [@draw_prc_recogniser]).		    

handle(w/2, h/2, link, center).

variable(node_name, name:='Node name', both, "Name of the component produced/consumed").
variable(node_quantity, int:=0, both, "Number of units produced/consumed").

initialise(E, F:graphical, T:graphical, L:[link], HF:[name]*, HT:[name]*) :->
	send(E, send_super, initialise, F, T, L, HF, HT),
	% Label
	new(TextLabel, text(E?node_name)),
	send(E, label_tag, TextLabel),
	new(A, arrow(8,6,closed)),
	send(E, second_arrow, A).
	
'_redraw_area'(E, A:area) :->
	prc_label(E, Label),
	new(TextLabel, text(Label)),
	send(E, label_tag, TextLabel),
	send(E, colour, red),
	send(E, send_super, '_redraw_area', A).

prc_label(E, Label) :-
	get(E, node_name, Name),
	get(E, node_quantity, N),
	atomic_list_concat([N,',',Name], Label).

:- draw_end_shape.

		/********************************
		*     COUPLING CONSTRAINTS      *
		********************************/

:- draw_begin_shape(draw_cc, ellipse, "Coupling constraints",
		    [@draw_cc_recogniser]).

variable(cc_set, name:='Coupling constraints', both, "Set of coupling constraints").
variable(coupling_constraints, chain, both, "Coupling constraints").
		    
initialise(CC,Name,Pos,W,H) :->
	send(CC, send_super, initialise),
	(Name \= '' -> send(CC, cc_set, Name) ; true),
	send(CC, width, W),
	send(CC, height, H),
	send(CC, center, Pos),
	send(CC, coupling_constraints, new(_, chain)).
		
'_redraw_area'(CC, A:area) :->
	get(CC, area, area(X, Y, W, H)),
	send(CC, draw_text, 
		CC?cc_set, font(helvetica, roman, 11), X, Y, W, H, center, center),
	send(CC, send_super, '_redraw_area', A).

:- draw_end_shape.