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

:- module(sub_process_modeler, []).

:- use_module(library(pce)).
:- ensure_loaded(library(help_message)).

:- consult([ menu
			  , shapes
			  , gesture
			  , process_canvas
			  , undo
			  ]).


:- pce_begin_class(sub_process_modeler, frame, "Sub-process modeler class").


variable(title, name, get, "Base-name of the program").


initialise(Modeler, Activity, Canvas, Title:[name]) :->
	default(Title, 'Composite activity process', TheTitle),
	send(Modeler, send_super, initialise, TheTitle),
	send(Modeler, slot, title, TheTitle),
	send(Modeler, done_message, message(Modeler, quit)),
	% Canvas for process modeling
	send(Modeler, append, new(ProcessCanvas, draw_process_canvas)),
	get(Activity, process, Proc),
	send(Proc, for_all,
	     and(message(ProcessCanvas, display, @arg1),
		 		message(@arg1, selected, @off))),
	send(ProcessCanvas, normalise, Proc),
	send(ProcessCanvas, slot, modified, @off),
	% Left menu (buttons)
	send(new(_Menu, draw_menu), left, ProcessCanvas),
	send(Modeler, fill_menu),
	% Apply and quit button
	send(new(D, dialog), below, ProcessCanvas),
	send(D, append, button(apply,
			and( message(Activity, sub_process, ProcessCanvas?graphicals),
				  message(Canvas, modified)))),
	send(D, append, button(quit,
			message(Modeler, quit))).

unlink(Modeler) :->
	send(Modeler, send_super, unlink).

 
fill_menu(Modeler) :->
	"Fill icon menu with standard options"::
	get(Modeler, menu, M),
	send(M, proto, @nil,
			select, top_left_arrow, tag:='Select mode'),
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
	
		

process_canvas(Modeler, C:draw_process_canvas) :<-
	"Find the process drawing canvas"::
	get(Modeler, member, draw_process_canvas, C).
	
menu(Modeler, C:draw_menu) :<-
	"Find the icon menu window"::
	get(Modeler, member, draw_menu, C).
	
	
select_mode(Modeler) :->
	"Switch to select mode"::
	(   get(Modeler?process_canvas, mode, select)
	->  true
	;   send(Modeler?menu, activate_select)
	).


mode(Modeler, Mode:name, Cursor:cursor) :->
	"Switch the mode"::
	send(Modeler?process_canvas, mode, Mode, Cursor).


proto(Modeler, Proto:'graphical|link*') :->
	"Switch the current prototype"::
	send(Modeler?process_canvas, proto, Proto).
	
	
quit(Modeler) :->
	"Leave product modeler"::
	(   object(Modeler)
	->  send(Modeler, destroy)
	;   true
	).
						 

:- pce_end_class.