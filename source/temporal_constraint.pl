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

:- module(temporal_constraint, []).
:- use_module(library(pce)).
:- require([ forall/2
	   ]).

:- pce_begin_class(temporal_constraint, box,
		   "Triangle with connections").

variable(path_tag, graphical*, both, "").
variable(connection_tag, chain*, both, "").

path_tag(C, Tag:graphical*) :->
	"Associate (new) tag with the connection"::
	get(C, path_tag, Old),
	(   Old == Tag
	->  true
	;   (	Old \== @nil
	    ->	send(Old, free)
	    ;	true
	    ),
	    send(C, slot, path_tag, Tag),
	    update_path_tag(C, _All)
	).
	
connection_tag(C, Tag:chain*) :->
	"Associate (new) tag with the connection"::
	get(C, connection_tag, Old),
	(   Old == Tag
	->  true
	;   (	Old \== @nil
	    ->	send(Old, free)
	    ;	true
	    ),
	    send(C, slot, connection_tag, Tag)
	).

unlink(C) :->
	"Destroy tags"::
	send(C, send_super, unlink),
	get(C, path_tag, PTag),
	(   PTag \== @nil
	->  free(PTag)
	;   true
	),
	get(C, connection_tag, CTag),
	(   CTag \== @nil
	->  free(CTag)
	;   true
	).
	

tag_attribute(device).
tag_attribute(displayed).


update_path_tag(C,_) :-
	get(C, path_tag, @nil), !.
update_path_tag(C, What) :-
	get(C, path_tag, Tag),
	forall(tag_attribute(What), send(Tag, What, C?What)),
	get(C, width, W), get(C, height, H), get(C, x, X), get(C, y, Y),
	send(Tag, points, chain(point(X + W/2, Y), point(X + W, Y + H - 1), point(X, Y + H - 1), point(X + W/2, Y))),
	send(Tag, hide).

:- pce_end_class.