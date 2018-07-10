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


:- module(draw_gesture, []).

:- use_module(library(pce)).
:- use_module(align).
:- require([ between/3
	   , send_list/3
	   , ignore/1
	   ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module defines event handling for the shapes.  Event handling for
dialog_items  is  predefined   because  the  UI  of dialog_items    is
standardised.  Event handling for  general purpose  graphicals  can be
specified by defining the method `Graphical ->event'.

The default   behaviour  of ->event (defined  at  the  level  of class
graphical)  is to   look up   the `recognisers'  slot of  the attached
interceptor  (see `Object  ->recogniser')  and test  if   any  of  the
attached interceptor is prepared to accept the event.

This implies    there are  three ways to     define event parsing  for
graphical objects:

	1) Attach a recogniser the object.
	2) Write an ->event method that parses the events.
	3) Write an ->event method that forwards the event to
	   recognisers.

For PceDraw we chose the  latter  approach for shapes.   See  also the
file canvas.pl. Provided the recognisers do not directly refer  to the
object for which they handle events as in

	send(B, recogniser, click_gesture(left, '', single,
					  message(B, inverted, @on)))

but, refer indirectly as in

	send(B, recogniser, click_gesture(left, '', single,
					  message(@receiver, inverted,
						  @on)))

recognisers can be attached to any number of graphical  objects.  This
file defines generic recognisers that are used by `Shape ->event'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		/********************************
		*      RECOGNISER OBJECTS	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Below are the declarations   of the  various recognisers.   Note  that
using  pce_global/2, the actual creation  of the recogniser is delayed
to the first time an event  occurs  on an  object that uses a specific
recogniser.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

			/* Create shapes */

:- pce_global(@draw_create_node_recogniser,
			make_create_node_recogniser).
:- pce_global(@draw_edge_gesture,
				new(draw_edge_create_gesture)).
:- pce_global(@draw_create_mc_recogniser,
			make_create_mc_recogniser).
			
:- pce_global(@draw_create_activity_recogniser,
			make_create_activity_recogniser).
:- pce_global(@draw_create_comp_activity_recogniser,
			make_create_comp_activity_recogniser).
:- pce_global(@draw_create_resource_recogniser,
			make_create_resource_recogniser).
:- pce_global(@draw_atomic_tc_gesture,
				new(draw_atomic_tc_create_gesture)).
:- pce_global(@draw_rc_gesture,
				new(draw_rc_create_gesture)).
:- pce_global(@draw_prc_gesture,
				new(draw_prc_create_gesture)).
:- pce_global(@draw_create_tc_recogniser,
			make_create_tc_recogniser).
:- pce_global(@draw_create_pv_recogniser,
			make_create_pv_recogniser).
:- pce_global(@draw_create_cc_recogniser,
			make_create_cc_recogniser).

			/* Select shapes */

:- pce_global(@draw_shape_select_recogniser,
	      make_draw_shape_select_recogniser).
:- pce_global(@draw_warp_select_gesture,
	      new(draw_warp_select_gesture)).

			/* Move/Resize shapes */

:- pce_global(@draw_move_outline_gesture,
	      new(handler_group(new(draw_move_selection_gesture),
				draw_move_selection_gesture(left),
				new(draw_move_outline_gesture)))).

			/* Combined shape recognisers */

:- pce_global(@draw_node_recogniser,
	      new(handler_group(@draw_shape_select_recogniser,
				@draw_move_outline_gesture,
				@draw_edge_gesture,
				@draw_shape_popup_gesture))).
:- pce_global(@draw_edge_recogniser,
	      new(handler_group(@draw_shape_select_recogniser,
				@draw_edge_gesture,
				@draw_shape_popup_gesture))).
:- pce_global(@draw_mc_recogniser,
		new(handler_group(@draw_shape_select_recogniser,
				@draw_move_outline_gesture,
				@draw_shape_popup_gesture))).

:- pce_global(@draw_activity_recogniser,
	      new(handler_group(@draw_shape_select_recogniser,
				@draw_move_outline_gesture,
				@draw_atomic_tc_gesture,		% Gesture for binary temporal constraint creation
				@draw_rc_gesture,				% Gesture for resource constraint creation
				@draw_prc_gesture,			% Gesture for product related constraint creation
				@draw_shape_popup_gesture))).
:- pce_global(@draw_resource_recogniser,
			new(handler_group(@draw_shape_select_recogniser,
				@draw_move_outline_gesture,
				@draw_rc_gesture, 			% Gesture for resource constraint creation				
				@draw_shape_popup_gesture))).
:- pce_global(@draw_atomic_tc_recogniser,
			new(handler_group(@draw_shape_select_recogniser,
				@draw_atomic_tc_gesture,
				@draw_shape_popup_gesture))).
:- pce_global(@draw_rc_recogniser,
			new(handler_group(@draw_shape_select_recogniser,
				@draw_rc_gesture,
				@draw_shape_popup_gesture))).
:- pce_global(@draw_prc_recogniser,
			new(handler_group(@draw_shape_select_recogniser,
				@draw_prc_gesture,
				@draw_shape_popup_gesture))).
:- pce_global(@draw_tc_recogniser,
			new(handler_group(@draw_shape_select_recogniser,
				@draw_move_outline_gesture,
				@draw_shape_popup_gesture))).
:- pce_global(@draw_pv_recogniser,
		new(handler_group(@draw_shape_select_recogniser,
				@draw_move_outline_gesture,
				@draw_shape_popup_gesture))).
:- pce_global(@draw_cc_recogniser,
		new(handler_group(@draw_shape_select_recogniser,
				@draw_move_outline_gesture,
				@draw_shape_popup_gesture))).				


		/********************************
		*            SELECT		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
When in select mode, left-click on an object makes  it  the selection,
shift-left-click   adds  or  deletes it  to/from   the   selection and
left-dragging   indicates  an   area in which   all objects  should be
selected.

Clicking  on an object is  to be defined  at the level  of the  object
itself, where the drag  version is to be  defined at  the level of the
canvas.  This is not very elegant as it implies  we have to create two
recognisers;    one for the  shapes  and   one  for the canvas.    The
alternative  would be one  recogniser at  the level of the  canvas and
find the object below  the  mouse on a  click.  It is difficult to say
which of the two approaches is better.

The  recogniser  for  shapes is  defined   below.   It  consists  of a
handler_group with two  click_gestures.  This  implementation  is  far
simpler  than defining a  new  class.   Note  the definition  of   the
obtainers before     defining the gestures   themselves.   This method
employs reusability of object and is easier to read.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_draw_shape_select_recogniser(G) :-
	new(Shape, @event?receiver),
	new(Canvas, Shape?window),
	new(SelectMode, Canvas?(mode) == select),

	new(G, handler_group(click_gesture(left, '', single,
					   message(Canvas, selection,
						   Shape),
					   SelectMode),
			     click_gesture(left, s, single,
					   message(Canvas, toggle_select,
						   Shape),
					   SelectMode))).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The `warp_gesture' allows the user to  indicate   an  area by dragging a
button and then selects all objects inside   the indicated area. It is a
rather typical example of  a   gesture  definition. The class_variable/3
declarations define the defaults that apply:   the button that activates
the gesture, the modifiers  required  (shift,   control,  meta)  and the
cursor that indicates the  gesture  is   active.  These  class variables
values are handled by the super-class gesture.

The variable `outline' keeps track of the box that is used to indicate
the area.  It can be stored here, as only one gesture can be active at
a time.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_warp_select_gesture, gesture).

class_variable(button,		button_name,	left).
class_variable(modifier,	modifier,	'').
class_variable(cursor,		cursor,		hand2).

variable(outline,	box,		get,
	 "Outline to `warp' objects").

initialise(G, B:[button_name], M:[modifier]) :->
	send(G, send_super, initialise, B, M),
	send(G, slot, outline, new(Box, box(0,0))),
	send(Box, texture, dotted).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The verify method is called to validate it is ok to start the gesture.
In this context, this implies the canvas is  in select mode  and there
are actually objects displayed.  It is  called after a  button-down of
the appropriate button with the appropriate modifier is detected.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

verify(_G, Ev:event) :->
	get(Ev, receiver, Canvas),
	get(Canvas, mode, select),
	\+ send(Canvas?graphicals, empty).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
After `Gesture ->verify'  succeeds `Gesture ->initiate'  is  called to
start  the gesture.  It  resizes  the outline  to  size(0,0) using the
`Graphical ->set'  (which  avoids creating  a  size  object) and  than
displays it at the mouse-position.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initiate(G, Ev:event) :->
	get(Ev, receiver, Canvas),
	send(G?outline, set, @default, @default, 0, 0),
	send(Canvas, display, G?outline, Ev?position).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On each drag-event,   this  method is   called.  It  just resizes  the
outline.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

drag(G, Ev:event) :->
	send(G?outline, corner, Ev?position).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On the corresponding up-event, this  method is called.  It removes the
outline from the   device and sends  `draw_canvas  ->selection' to the
canvas with a chain of all objects inside the area.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

terminate(G, Ev:event) :->
	send(G, drag, Ev),
	get(G, outline, Outline),
	get(Ev, receiver, Canvas),
	send(Outline, device, @nil),
	get(Canvas, inside, Outline?area, ToSelect),
	send(ToSelect, for_all,
	     if(not(message(@arg1?class, instance_of, draw_shape_class)),
		message(ToSelect, delete, @arg1))),
	send(Canvas, selection, ToSelect).

:- pce_end_class.



		/********************************
		*       CREATE NODE SHAPE	     *
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create shapes that have a predefined size.  The center
of   the  object   will     be at  the mouse-click      location.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_create_node_recogniser(R) :-
	new(Canvas, @event?receiver),
	new(Pos, @event?position),
	new(R, click_gesture(left, '', single, 
			message(@prolog, create_node, Canvas, Pos),
			Canvas?(mode) == draw_node)).
	
create_node(Canvas, Pos) :-
	new(Object, draw_node('',Pos,100,50)),
	%term_to_atom(Object, Name),
	%send(Object, slot, node_name, Name),
	send(Canvas, display, Object),
	send(Canvas, modified).


		/********************************
		*       CREATE M.C.s SHAPE	     *
		********************************/

make_create_mc_recogniser(R) :-
	new(Canvas, @event?receiver),
	new(Pos, @event?position),
	new(R, click_gesture(left, '', single, 
			message(@prolog, create_mc, Canvas, Pos),
			Canvas?(mode) == draw_mc)).
	
create_mc(Canvas, Pos) :-
	new(Object, draw_mc('',Pos,110,40)),
	send(Canvas, display, Object),
	send(Canvas, modified).
	
	   /********************************
		*       CREATE ACTIVITY SHAPE   *
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create shapes that have a predefined size.  The center
of   the  object   will     be at  the mouse-click      location.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_create_activity_recogniser(R) :-
	new(Canvas, @event?receiver),
	new(Pos, @event?position),
	new(R, click_gesture(left, '', single, 
			message(@prolog, create_activity, Canvas, Pos),
			Canvas?(mode) == draw_activity)).
	
create_activity(Canvas, Pos) :-
	new(Object, draw_activity('',Pos,100,50)),
	send(Canvas, display, Object),
	send(Canvas, modified).

	   /********************************
		*  CREATE COMP. ACTIVITY SHAPE  *
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create shapes that have a predefined size.  The center
of   the  object   will     be at  the mouse-click      location.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_create_comp_activity_recogniser(R) :-
	new(Canvas, @event?receiver),
	new(Pos, @event?position),
	new(R, click_gesture(left, '', single, 
			message(@prolog, create_comp_activity, Canvas, Pos),
			Canvas?(mode) == draw_comp_activity)).
	
create_comp_activity(Canvas, Pos) :-
	new(Object, draw_comp_activity('',Pos,104,54)),
	send(Canvas, display, Object),
	send(Canvas, modified).
	
	   /********************************
		* 		 CREATE RESOURCE SHAPE    *
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create shapes that have a predefined size.  The center
of   the  object   will     be at  the mouse-click      location.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_create_resource_recogniser(R) :-
	new(Canvas, @event?receiver),
	new(Pos, @event?position),
	new(R, click_gesture(left, '', single, 
			message(@prolog, create_resource, Canvas, Pos),
			Canvas?(mode) == draw_resource)).
	
create_resource(Canvas, Pos) :-
	new(Object, draw_resource('',Pos,100,50)),
	send(Canvas, display, Object),
	send(Canvas, modified).
	
	   /********************************
		*   		 CREATE TC SHAPE       *
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create shapes that have a predefined size.  The center
of   the  object   will     be at  the mouse-click      location.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_create_tc_recogniser(TC) :-
	new(Canvas, @event?receiver),
	new(Pos, @event?position),
	new(TC, click_gesture(left, '', single, 
			message(@prolog, create_tc, Canvas, Pos),
			Canvas?(mode) == draw_tc)).
	
create_tc(Canvas, Pos) :-
	new(Object, draw_tc(Pos,40,40)),
	send(Canvas, display, Object),
	send(Canvas, modified).
	
		/********************************
		*       CREATE P.V.s SHAPE	     *
		********************************/

make_create_pv_recogniser(R) :-
	new(Canvas, @event?receiver),
	new(Pos, @event?position),
	new(R, click_gesture(left, '', single, 
			message(@prolog, create_pv, Canvas, Pos),
			Canvas?(mode) == draw_pv)).
	
create_pv(Canvas, Pos) :-
	new(Object, draw_pv('',Pos,110,40)),
	send(Canvas, display, Object),
	send(Canvas, modified).

		/********************************
		*       CREATE C.C.s SHAPE	     *
		********************************/

make_create_cc_recogniser(R) :-
	new(Canvas, @event?receiver),
	new(Pos, @event?position),
	new(R, click_gesture(left, '', single, 
			message(@prolog, create_cc, Canvas, Pos),
			Canvas?(mode) == draw_cc)).
	
create_cc(Canvas, Pos) :-
	new(Object, draw_cc('',Pos,110,40)),
	send(Canvas, display, Object),
	send(Canvas, modified).

		/********************************
		*             MOVE		        *
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  move_selection gesture is active when  an object is moved that is
selected  and there  are  more objects  selected.   In this  case  all
selected objects are moved by the same  amount.   This is indicated by
showing  an outline that reflects  the  bounding  box of  all  objects
moved.

This gesture illustrates how another gesture can be  encapsulated.  It
is a  subclass  of `move_gesture' to inherit the  button  and modifier
class_variables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_move_selection_gesture, move_gesture).

variable(outline,	box,	get,
	 "Box used to indicate move").
variable(selection,	chain*, both,
	 "Stored value of device selection").
variable(origin,	point,  get,
	 "Start origin of selection").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  gesture maintains  an outline, the selection to  be moved and the
positon  where  the move orginiated.    The outline  itself is given a
normal  move_gesture to make  it move on  dragging.  This move_gesture
should operate on the same button and modifier.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(G, B:[button_name], M:[modifier]) :->
	send(G, send_super, initialise, B, M),
	send(G, slot, outline, new(Box, box(0,0))),
	send(G, slot, origin, point(0,0)),
	send(Box, texture, dotted),
	send(Box, recogniser, move_gesture(G?button, G?modifier)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Verify the object  is selected and there  is at least one  more object
selected.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

verify(_G, Ev:event) :->
	get(Ev, receiver, Receiver),
	get(Receiver, selected, @on).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initiating implies finding the  device  and the bounding  box  of  all
selected objects (= the `union' of their areas).  Next, the outline is
displayed and all events are posted to  the outline.  The move_gesture
of the outline ensures the outline is moved by the dragging events.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initiate(G, Ev:event) :->
	get(Ev?receiver, device, Dev),
	get(G, outline, Outline),
	send(G, selection, Dev?selection),
	get(G, selection, Selection),
	new(Union, area(0,0,0,0)),
	send(Selection, for_all, message(Union, union, @arg1?area)),
	send(G?origin, copy, Union?position),
	send(Outline, area, Union),
	send(Union, done),
	send(Dev, display, Outline),
	ignore(send(Ev, post, Outline)).

drag(G, Ev) :->
	send(Ev, post, G?outline).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Terminate.   First undisplay the outline.  Next  calculate by how much
the outline has been dragged and move all objects  of the selection by
this amount.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

terminate(G, Ev:event) :->
	ignore(send(G, drag, Ev)),
	get(G, outline, Outline),
	send(Outline, device, @nil),
	get(Outline?area?position, difference, G?origin, Offset),
	get(Ev, window, Canvas),
	send(Canvas, open_undo_group),
	get(G, selection, Selection),
	send(Selection, for_all, message(@arg1, relative_move, Offset)),
	(   get(Selection, size, 1)
	->  get(Selection, head, Shape),
	    send(Shape?device, auto_align, Shape, move)
	;   true
	),
	send(G, selection, @nil),
	send(Canvas, close_undo_group),
	send(Canvas, modified).

:- pce_end_class.



:- pce_begin_class(draw_move_outline_gesture, move_outline_gesture).

terminate(G, Ev:event) :->
	"Invoke auto_align"::
	get(Ev, receiver, Shape),
	get(Shape, window, Canvas),
	send(Canvas, open_undo_group),
	send(G, send_super, terminate, Ev),
	send(Shape?device, auto_align, Shape, move),
	send(Canvas, close_undo_group).

:- pce_end_class.

:- pce_begin_class(draw_move_gesture, move_gesture).

initiate(G, Ev:event) :->
	send(G, send_super, initiate, Ev),
	get(Ev, window, Canvas),
	send(Canvas, open_undo_group).

terminate(G, Ev:event) :->
	"Invoke auto_align"::
	get(Ev, receiver, Shape),
	get(Shape, window, Canvas),
	send(G, send_super, terminate, Ev),
	send(Shape?device, auto_align, Shape, move),
	send(Canvas, close_undo_group).

:- pce_end_class.

		

		/********************************
		*     CONNECT CREATE EDGE       *
		********************************/

:- pce_begin_class(draw_edge_create_gesture, gesture).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The `draw_edge_create_gesture' allows one to connect two nodes at arbitrary  
points by attaching new handles to the graphicals and creating an line 
between them. Also, it allows ont to create loops.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

variable(line,			line,		get,
	 "Line indicating link").
variable(from_indicator,	bitmap,		get,
	 "Indicator at `from' side").
variable(to_indicator,		bitmap,		get,
	 "Indicator at `to' side").
variable(to,			graphical*,	get,
	 "Graphical to connect to").

class_variable(button,   button_name, left, "Button used to connect (left)").
class_variable(modifier, modifier,    '',   "Modifier used to connect").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise the line and markers of the gesture.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(G, B:[button_name], M:[modifier]) :->
	send(G, send_super, initialise, B, M),
	send(G, slot, line, line(0,0,0,0)),
	send(G, slot, from_indicator, new(bitmap(@mark_handle_image))),
	send(G, slot, to_indicator, new(bitmap(@mark_handle_image))).


verify(_G, Ev:event) :->
	"Verify canvas is in connect_create-mode"::
	get(Ev?receiver?device, mode, draw_edge).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Indicate the  start-location  using  the <-from_indicator,   give  the
feedback-line the appropriate attributes and display it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initiate(G, Ev:event) :->
	"Start drawing line"::
	get(Ev?receiver, device, Dev),
	get(Dev, proto, Link),
	get(Ev, position, Dev, Pos),
	send(G?line, copy, Link?line),
	send(G?line, texture, dotted),
	send(G?line, start, Pos),
	send(G?line, end, Pos),
	send(Dev, display, G?line),
	send(G, indicate, Ev?receiver, Pos, G?from_indicator).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update the line, check whether the mouse points to a  valid target and
display a marker on  it.  Note how  the target  is  located  using the
method  `Chain  <-find'.  This  keeps  everything inside PCE, avoiding
interface overhead  and producing far  less garbage.  `Gesture ->drag'
should be as fast as  possible and not  produce too much garbage as it
will be called about 40 times per second while the mouse is dragged.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

drag(G, Ev:event) :->
    get(Ev, receiver, Receiver),
    get(Receiver, device, Dev),
    get(Ev, position, Dev, Pos),
    send(G?line, end, Pos),
    (   get(?(Dev, pointed_objects, Pos), find,
	    		and(Receiver \== @arg1,
				  	 G?line \== @arg1,
			  		 G?from_indicator \== @arg1,
			  		 G?to_indicator \== @arg1), 
			  	To)
    ->  	send(G, indicate, To, Pos, G?to_indicator),
			send(G, slot, to, To)
    ; ( get(?(Dev, pointed_objects, Pos), find,
	    		Receiver == @arg1, 
			  	To)
      -> send(G, indicate, To, Pos, G?to_indicator),
			send(G, slot, to, To)
    	;	send(G, slot, to, @nil),
			send(G?to_indicator, device, @nil)
		)
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If there is a  target, create unique  handles  on both sides and  link
them together.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

terminate(G, Ev:event) :->
	send(G, drag, Ev),
	send(G?line, device, @nil),
	send(G?from_indicator, device, @nil),
	send(G?to_indicator, device, @nil),
	get(G, to, To),
	(   To \== @nil
	->  send(G, slot, to, @nil),
	    get(Ev, receiver, Receiver),
	    get(Receiver?device, proto, Link),
	    get(Ev, window, Canvas),
	    send(Canvas, open_undo_group),
	    get(G, handle, Receiver, G?from_indicator?center, Link?from, FH),
	    get(G, handle, To, G?to_indicator?center, Link?to, TH),
	    new(_Edge, draw_edge('New edge', Receiver, To, Link, FH, TH)),
	    %term_to_atom(Receiver, Parent),
	    %term_to_atom(To, Child),
	    %atomic_list_concat([Parent,' ',Child], Label),
	    %send(Edge, slot, edge_label, Label),
	    send(Canvas, close_undo_group),
	    send(Canvas, modified)
	;   true
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a unique handle on a graphical at the  indicated position.  The
position of the handle is taken relative to the size of the graphical.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

handle(_G, Gr:graphical, Pos:point, Kind:name, Name) :<-
	"Attach a handle at specified position and return it's name"::
	get(Gr, x, X), get(Gr, y, Y),
	get(Gr, width, W), get(Gr, height, H),
	get(Pos, x, PX), get(Pos, y, PY),
	RX is PX - X, RY is PY - Y,
	unique_handle_name(Gr, Name),
	send(Gr, handle, handle((RX/W) * w, (RY/H) * h, Kind, Name)).


unique_handle_name(Gr, Name) :-
	between(1, 10000, N),
	atom_concat(c, N, Name),
	\+ get(Gr, handle, Name, _), !.


indicate(_G, Gr:graphical, Pos:point, Indicator:bitmap) :->
	"Display indication-marker for position"::
	send(Indicator, center, Pos),
	send(Gr?device, display, Indicator).

:- pce_end_class.

      /********************************
		*           CREATE EDGE     	  *
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The 'draw_edge_gesture' allows one to create an edge between two nodes 
dragging a line between two nodes. The line will go from one handle of the 
first node to an handle of the second node.
Drawing edges is easy but the number of edges between two nodes is limited 
by the number of handlers we create for them.

We use 'draw_connect_edge_gesture' instead of 'draw_edge_gesture' to 
allow one to create as many edges as she/he wants.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_edge_gesture, connect_gesture).

verify(G, Ev:event) :->
	"Verify canvas is in connect-mode"::
	get(Ev?receiver, device, Dev), Dev \== @nil,
	get(Dev, mode, draw_edge),
	send(G, link, Dev?proto),
	send(G, send_super, verify, Ev).

connect(_G, From:graphical, To:graphical, Link:link,
	    FH:[name], TH:[name]) :->
	"Connect the graphicals (using a draw_edge)"::
	(   get(Link, attribute, draw_connection_class, ClassName)
	->  true
	;   ClassName = draw_edge
	),
	get(From, window, Canvas),
	send(Canvas, open_undo_group),
	Term =.. [ClassName, 'New edge', From, To, Link, FH, TH],
	new(C, Term),
	send(C, start_text),
	send(Canvas, close_undo_group),
	send(Canvas, modified).

:- pce_end_class.

		/********************************
		*   CONNECT CREATE ATOMIC TC    *
		********************************/

:- pce_begin_class(draw_atomic_tc_create_gesture, gesture).

variable(line,			line,		get,
	 "Line indicating link").
variable(from_indicator,	bitmap,		get,
	 "Indicator at `from' side").
variable(to_indicator,		bitmap,		get,
	 "Indicator at `to' side").
variable(to,			graphical*,	get,
	 "Graphical to connect to").

class_variable(button,   button_name, left, "Button used to connect (left)").
class_variable(modifier, modifier,    '',   "Modifier used to connect").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise the line and markers of the gesture.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(G, B:[button_name], M:[modifier]) :->
	send(G, send_super, initialise, B, M),
	send(G, slot, line, line(0,0,0,0)),
	send(G, slot, from_indicator, new(bitmap(@mark_handle_image))),
	send(G, slot, to_indicator, new(bitmap(@mark_handle_image))).


verify(_G, Ev:event) :->
	"Verify canvas is in connect_create-mode"::
	get(Ev?receiver?device, mode, draw_atomic_tc).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Indicate the  start-location  using  the <-from_indicator,   give  the
feedback-line the appropriate attributes and display it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initiate(G, Ev:event) :->
	"Start drawing line"::
	get(Ev?receiver, device, Dev),
	get(Dev, proto, Link),
	get(Ev, position, Dev, Pos),
	send(G?line, copy, Link?line),
	send(G?line, texture, dotted),
	send(G?line, start, Pos),
	send(G?line, end, Pos),
	send(Dev, display, G?line),
	send(G, indicate, Ev?receiver, Pos, G?from_indicator).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update the line, check whether the mouse points to a  valid target and
display a marker on  it.  Note how  the target  is  located  using the
method  `Chain  <-find'.  This  keeps  everything inside PCE, avoiding
interface overhead  and producing far  less garbage.  `Gesture ->drag'
should be as fast as  possible and not  produce too much garbage as it
will be called about 40 times per second while the mouse is dragged.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

drag(G, Ev:event) :->
    get(Ev, receiver, Receiver),
    get(Receiver, device, Dev),
    get(Ev, position, Dev, Pos),
    send(G?line, end, Pos),
    (   get(?(Dev, pointed_objects, Pos), find,
	    		and(Receiver \== @arg1,
				  	 G?line \== @arg1,
			  		 G?from_indicator \== @arg1,
			  		 G?to_indicator \== @arg1), 
			  	To)
    ->  	send(G, indicate, To, Pos, G?to_indicator),
			send(G, slot, to, To)
    ; ( get(?(Dev, pointed_objects, Pos), find,
	    		Receiver == @arg1, 
			  	To)
      -> send(G, indicate, To, Pos, G?to_indicator),
			send(G, slot, to, To)
    	;	send(G, slot, to, @nil),
			send(G?to_indicator, device, @nil)
		)
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If there is a  target, create unique  handles  on both sides and  link
them together.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

terminate(G, Ev:event) :->
	send(G, drag, Ev),
	send(G?line, device, @nil),
	send(G?from_indicator, device, @nil),
	send(G?to_indicator, device, @nil),
	get(G, to, To),
	(   To \== @nil
	->  send(G, slot, to, @nil),
	    get(Ev, receiver, Receiver),
	    get(Receiver?device, proto, Link),
	    get(Ev, window, Canvas),
	    send(Canvas, open_undo_group),
	    get(G, handle, Receiver, G?from_indicator?center, Link?from, FH),
	    get(G, handle, To, G?to_indicator?center, Link?to, TH),
	    ( Receiver \== To
	    ->	new(_Edge, draw_atomic_tc('before', Receiver, To, Link, FH, TH))
	    ;		true
	    ),
	    send(Canvas, close_undo_group),
	    send(Canvas, modified)
	;   true
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a unique handle on a graphical at the  indicated position.  The
position of the handle is taken relative to the size of the graphical.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

handle(_G, Gr:graphical, Pos:point, Kind:name, Name) :<-
	"Attach a handle at specified position and return it's name"::
	get(Gr, x, X), get(Gr, y, Y),
	get(Gr, width, W), get(Gr, height, H),
	get(Pos, x, PX), get(Pos, y, PY),
	RX is PX - X, RY is PY - Y,
	unique_handle_name(Gr, Name),
	send(Gr, handle, handle((RX/W) * w, (RY/H) * h, Kind, Name)).


%unique_handle_name(Gr, Name) :-
%	between(1, 10000, N),
%	atom_concat(c, N, Name),
%	\+ get(Gr, handle, Name, _), !.


indicate(_G, Gr:graphical, Pos:point, Indicator:bitmap) :->
	"Display indication-marker for position"::
	send(Indicator, center, Pos),
	send(Gr?device, display, Indicator).

:- pce_end_class.


		/********************************
		*       CONNECT CREATE RC       *
		********************************/

:- pce_begin_class(draw_rc_create_gesture, gesture).

variable(line,			line,		get,
	 "Line indicating link").
variable(from_indicator,	bitmap,		get,
	 "Indicator at `from' side").
variable(to_indicator,		bitmap,		get,
	 "Indicator at `to' side").
variable(to,			graphical*,	get,
	 "Graphical to connect to").

class_variable(button,   button_name, left, "Button used to connect (left)").
class_variable(modifier, modifier,    '',   "Modifier used to connect").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise the line and markers of the gesture.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(G, B:[button_name], M:[modifier]) :->
	send(G, send_super, initialise, B, M),
	send(G, slot, line, line(0,0,0,0)),
	send(G, slot, from_indicator, new(bitmap(@mark_handle_image))),
	send(G, slot, to_indicator, new(bitmap(@mark_handle_image))).


verify(_G, Ev:event) :->
	"Verify canvas is in connect_create-mode"::
	get(Ev?receiver?device, mode, draw_rc).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Indicate the  start-location  using  the <-from_indicator,   give  the
feedback-line the appropriate attributes and display it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initiate(G, Ev:event) :->
	"Start drawing line"::
	get(Ev?receiver, device, Dev),
	get(Dev, proto, Link),
	get(Ev, position, Dev, Pos),
	send(G?line, copy, Link?line),
	send(G?line, texture, dotted),
	send(G?line, start, Pos),
	send(G?line, end, Pos),
	send(Dev, display, G?line),
	send(G, indicate, Ev?receiver, Pos, G?from_indicator).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update the line, check whether the mouse points to a  valid target and
display a marker on  it.  Note how  the target  is  located  using the
method  `Chain  <-find'.  This  keeps  everything inside PCE, avoiding
interface overhead  and producing far  less garbage.  `Gesture ->drag'
should be as fast as  possible and not  produce too much garbage as it
will be called about 40 times per second while the mouse is dragged.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

drag(G, Ev:event) :->
    get(Ev, receiver, Receiver),
    get(Receiver, device, Dev),
    get(Ev, position, Dev, Pos),
    send(G?line, end, Pos),
    (   get(?(Dev, pointed_objects, Pos), find,
	    		and(Receiver \== @arg1,
				  	 G?line \== @arg1,
			  		 G?from_indicator \== @arg1,
			  		 G?to_indicator \== @arg1), 
			  	To)
    ->  	send(G, indicate, To, Pos, G?to_indicator),
			send(G, slot, to, To)
    ; ( get(?(Dev, pointed_objects, Pos), find,
	    		Receiver == @arg1, 
			  	To)
      -> send(G, indicate, To, Pos, G?to_indicator),
			send(G, slot, to, To)
    	;	send(G, slot, to, @nil),
			send(G?to_indicator, device, @nil)
		)
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If there is a  target, create unique  handles  on both sides and  link
them together.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

terminate(G, Ev:event) :->
	send(G, drag, Ev),
	send(G?line, device, @nil),
	send(G?from_indicator, device, @nil),
	send(G?to_indicator, device, @nil),
	get(G, to, To),
	(   To \== @nil
	->  send(G, slot, to, @nil),
	    get(Ev, receiver, Receiver),
	    get(Receiver?device, proto, Link),
	    get(Ev, window, Canvas),
	    send(Canvas, open_undo_group),
	    get(G, handle, Receiver, G?from_indicator?center, Link?from, FH),
	    get(G, handle, To, G?to_indicator?center, Link?to, TH),
	    not_same_class(Receiver, To, NotSameClass),
	    ( Receiver \== To, NotSameClass
	    ->	new(_Edge, draw_rc(Receiver, To, Link, FH, TH))
	    ;		true
	    ),
	    send(Canvas, close_undo_group),
	    send(Canvas, modified)
	;   true
	).
	
not_same_class(A,B,false) :-
	send(A, instance_of, draw_activity),
	send(B, instance_of, draw_activity).
not_same_class(A,B,false) :-
	send(A, instance_of, draw_resource),
	send(B, instance_of, draw_resource).
not_same_class(_A, _B, true).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a unique handle on a graphical at the  indicated position.  The
position of the handle is taken relative to the size of the graphical.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

handle(_G, Gr:graphical, Pos:point, Kind:name, Name) :<-
	"Attach a handle at specified position and return it's name"::
	get(Gr, x, X), get(Gr, y, Y),
	get(Gr, width, W), get(Gr, height, H),
	get(Pos, x, PX), get(Pos, y, PY),
	RX is PX - X, RY is PY - Y,
	unique_handle_name(Gr, Name),
	send(Gr, handle, handle((RX/W) * w, (RY/H) * h, Kind, Name)).


%unique_handle_name(Gr, Name) :-
%	between(1, 10000, N),
%	atom_concat(c, N, Name),
%	\+ get(Gr, handle, Name, _), !.


indicate(_G, Gr:graphical, Pos:point, Indicator:bitmap) :->
	"Display indication-marker for position"::
	send(Indicator, center, Pos),
	send(Gr?device, display, Indicator).

:- pce_end_class.

		/********************************
		*       CONNECT CREATE PRC      *
		********************************/

:- pce_begin_class(draw_prc_create_gesture, gesture).

variable(line,			line,		get,
	 "Line indicating link").
variable(from_indicator,	bitmap,		get,
	 "Indicator at `from' side").
variable(to_indicator,		bitmap,		get,
	 "Indicator at `to' side").
variable(to,			graphical*,	get,
	 "Graphical to connect to").

class_variable(button,   button_name, left, "Button used to connect (left)").
class_variable(modifier, modifier,    '',   "Modifier used to connect").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Initialise the line and markers of the gesture.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(G, B:[button_name], M:[modifier]) :->
	send(G, send_super, initialise, B, M),
	send(G, slot, line, line(0,0,0,0)),
	send(G, slot, from_indicator, new(bitmap(@mark_handle_image))),
	send(G, slot, to_indicator, new(bitmap(@mark_handle_image))).


verify(_G, Ev:event) :->
	"Verify canvas is in connect_create-mode"::
	get(Ev?receiver?device, mode, draw_prc).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Indicate the  start-location  using  the <-from_indicator,   give  the
feedback-line the appropriate attributes and display it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initiate(G, Ev:event) :->
	"Start drawing line"::
	get(Ev?receiver, device, Dev),
	get(Dev, proto, Link),
	get(Ev, position, Dev, Pos),
	send(G?line, copy, Link?line),
	send(G?line, texture, dotted),
	send(G?line, start, Pos),
	send(G?line, end, Pos),
	send(Dev, display, G?line),
	send(G, indicate, Ev?receiver, Pos, G?from_indicator).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update the line, check whether the mouse points to a  valid target and
display a marker on  it.  Note how  the target  is  located  using the
method  `Chain  <-find'.  This  keeps  everything inside PCE, avoiding
interface overhead  and producing far  less garbage.  `Gesture ->drag'
should be as fast as  possible and not  produce too much garbage as it
will be called about 40 times per second while the mouse is dragged.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

drag(G, Ev:event) :->
    get(Ev, receiver, Receiver),
    get(Receiver, device, Dev),
    get(Ev, position, Dev, Pos),
    send(G?line, end, Pos),
    (   get(?(Dev, pointed_objects, Pos), find,
	    		and(Receiver \== @arg1,
				  	 G?line \== @arg1,
			  		 G?from_indicator \== @arg1,
			  		 G?to_indicator \== @arg1), 
			  	To)
    ->  	send(G, indicate, To, Pos, G?to_indicator),
			send(G, slot, to, To)
    ; ( get(?(Dev, pointed_objects, Pos), find,
	    		Receiver == @arg1, 
			  	To)
      -> send(G, indicate, To, Pos, G?to_indicator),
			send(G, slot, to, To)
    	;	send(G, slot, to, @nil),
			send(G?to_indicator, device, @nil)
		)
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
If there is a  target, create unique  handles  on both sides and  link
them together.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

terminate(G, Ev:event) :->
	send(G, drag, Ev),
	send(G?line, device, @nil),
	send(G?from_indicator, device, @nil),
	send(G?to_indicator, device, @nil),
	get(G, to, To),
	(   To \== @nil
	->  send(G, slot, to, @nil),
	    get(Ev, receiver, Receiver),
	    get(Receiver?device, proto, Link),
	    get(Ev, window, Canvas),
	    send(Canvas, open_undo_group),
	    get(G, handle, Receiver, G?from_indicator?center, Link?from, FH),
	    get(G, handle, To, G?to_indicator?center, Link?to, TH),
	    ( Receiver \== To
	    ->	new(_Edge, draw_prc(Receiver, To, Link, FH, TH))
	    ;		true
	    ),
	    send(Canvas, close_undo_group),
	    send(Canvas, modified)
	;   true
	).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a unique handle on a graphical at the  indicated position.  The
position of the handle is taken relative to the size of the graphical.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

handle(_G, Gr:graphical, Pos:point, Kind:name, Name) :<-
	"Attach a handle at specified position and return it's name"::
	get(Gr, x, X), get(Gr, y, Y),
	get(Gr, width, W), get(Gr, height, H),
	get(Pos, x, PX), get(Pos, y, PY),
	RX is PX - X, RY is PY - Y,
	unique_handle_name(Gr, Name),
	send(Gr, handle, handle((RX/W) * w, (RY/H) * h, Kind, Name)).


%unique_handle_name(Gr, Name) :-
%	between(1, 10000, N),
%	atom_concat(c, N, Name),
%	\+ get(Gr, handle, Name, _), !.


indicate(_G, Gr:graphical, Pos:point, Indicator:bitmap) :->
	"Display indication-marker for position"::
	send(Indicator, center, Pos),
	send(Gr?device, display, Indicator).

:- pce_end_class.


		/********************************
		*          SHAPE POPUP		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The  code of this section  attaches a popup-menu  to the shapes.  On a
mouse-right-down  event,  the  shape on which   the  down  occurred is
selected  to indicate on which  object the operation  will take place.
Next, the menu is shown.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@draw_shape_popup_gesture, make_draw_shape_popup_gesture).

make_draw_shape_popup_gesture(G) :-
	new(Gr, @event?receiver),
	new(Canvas, Gr?device),

	new(P, popup),
	send_list(P, append,
		  [
		    menu_item(cut,
			      message(Canvas, cut_selection,
				      create(chain, @arg1)),
			      @default, @off),
		    menu_item(copy,
			      message(Canvas, copy_selection,
				      create(chain, @arg1)),
			      @default, @on),
		    menu_item(edit_attributes,
			      and(message(Canvas, selection, Gr),
				  		 message(Canvas, edit_selection)),
			      @default, @on)
		  ]),

	new(G, draw_draw_shape_popup_gesture(P)).


:- pce_begin_class(draw_draw_shape_popup_gesture, popup_gesture).

variable(old_selected,	bool*,	both, "Was graphical selected").

verify(G, Ev:event) :->
	get(Ev?receiver, device, Dev),
	Dev \== @nil,
	(send(Dev?class, is_a, draw_product_canvas) ; send(Dev?class, is_a, draw_process_canvas)),
	send(G, send_super, verify, Ev).


initiate(G, Ev:event) :->
	get(Ev, receiver, Receiver),
	send(G, old_selected, Receiver?selected),
	send(Receiver, selected, @on),
	send(G, send_super, initiate, Ev).


terminate(G, Ev:event) :->
	get(G, context, Gr),
	send(Gr, selected, G?old_selected),
	send(G, send_super, terminate, Ev).

:- pce_end_class.
