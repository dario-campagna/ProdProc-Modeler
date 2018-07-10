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

:- module(multi_box, []).
:- use_module(library(pce)).
:- require([ forall/2
	   ]).

:- pce_begin_class(multi_box, box,
		   "Overlapped boxes").

variable(name_tag, graphical*, both, "").
variable(constraint_tag, graphical*, both, "").
variable(condition_tag, graphical*, both, "").
variable(inbox_tag, graphical*, both, "").
variable(box_1_tag,	graphical*,	both, "").
variable(box_2_tag,	graphical*, both, "").
variable(path_tag, graphical*, both, "").

name_tag(C, Tag:graphical*) :->
	"Associate (new) tag with the connection"::
	get(C, name_tag, Old),
	(   Old == Tag
	->  true
	;   (	Old \== @nil
	    ->	send(Old, free)
	    ;	true
	    ),
	    send(C, slot, name_tag, Tag),
	    update_name_tag(C, _All)
	).
	
constraint_tag(C, Tag:graphical*) :->
	"Associate (new) tag with the connection"::
	get(C, constraint_tag, Old),
	(   Old == Tag
	->  true
	;   (	Old \== @nil
	    ->	send(Old, free)
	    ;	true
	    ),
	    send(C, slot, constraint_tag, Tag),
	    update_constraint_tag(C, _All)
	).
	
condition_tag(C, Tag:graphical*) :->
	"Associate (new) tag with the connection"::
	get(C, condition_tag, Old),
	(   Old == Tag
	->  true
	;   (	Old \== @nil
	    ->	send(Old, free)
	    ;	true
	    ),
	    send(C, slot, condition_tag, Tag),
	    update_condition_tag(C, _All)
	).
	
inbox_tag(C, Tag:graphical*) :->
	"Associate (new) tag with the connection"::
	get(C, inbox_tag, Old),
	(   Old == Tag
	->  true
	;   (	Old \== @nil
	    ->	send(Old, free)
	    ;	true
	    ),
	    send(C, slot, inbox_tag, Tag),
	    update_inbox_tag(C, _All)
	).

box_1_tag(C, Tag:graphical*) :->
	"Associate (new) tag with the connection"::
	get(C, box_1_tag, Old),
	(   Old == Tag
	->  true
	;   (	Old \== @nil
	    ->	send(Old, free)
	    ;	true
	    ),
	    send(C, slot, box_1_tag, Tag),
	    update_box_1_tag(C, _All)
	).
	
box_2_tag(C, Tag:graphical*) :->
	"Associate (new) tag with the connection"::
	get(C, box_2_tag, Old),
	(   Old == Tag
	->  true
	;   (	Old \== @nil
	    ->	send(Old, free)
	    ;	true
	    ),
	    send(C, slot, box_2_tag, Tag),
	    update_box_2_tag(C, _All)
	).

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

unlink(C) :->
	"Destroy tags"::
	send(C, send_super, unlink),
	get(C, name_tag, NTag),
	(   NTag \== @nil
	->  free(NTag)
	;   true
	),
	get(C, constraint_tag, CTag),
	(   CTag \== @nil
	->  free(CTag)
	;   true
	),
	get(C, condition_tag, CoTag),
	(   CoTag \== @nil
	->  free(CoTag)
	;   true
	),
	get(C, inbox_tag, ITag),
	(   ITag \== @nil
	->  free(ITag)
	;   true
	),
	get(C, box_1_tag, B1Tag),
	(   B1Tag \== @nil
	->  free(B1Tag)
	;   true
	),
	get(C, box_2_tag, B2Tag),
	(   B2Tag \== @nil
	->  free(B2Tag)
	;   true
	),
	get(C, path_tag, PTag),
	(   PTag \== @nil
	->  free(PTag)
	;   true
	).
	

tag_attribute(device).
tag_attribute(displayed).

update_name_tag(C,_) :-
	get(C, name_tag, @nil), !.
update_name_tag(C, What) :-
	get(C, name_tag, Tag),
	forall(tag_attribute(What), send(Tag, What, C?What)),
	send(Tag, center, point(C?center_x, C?center_y-18)).

update_constraint_tag(C,_) :-
	get(C, constraint_tag, @nil), !.
update_constraint_tag(C, What) :-
	get(C, constraint_tag, Tag),
	forall(tag_attribute(What), send(Tag, What, C?What)),
	send(Tag, center, point(C?center_x, C?center_y+18)).

update_condition_tag(C,_) :-
	get(C, condition_tag, @nil), !.
update_condition_tag(C, What) :-
	get(C, condition_tag, Tag),
	forall(tag_attribute(What), send(Tag, What, C?What)),
	send(Tag, center, point(C?center_x, C?center_y)).
	
update_inbox_tag(C,_) :-
	get(C, inbox_tag, @nil), !.
update_inbox_tag(C, What) :-
	get(C, inbox_tag, Tag),
	forall(tag_attribute(What), send(Tag, What, C?What)),
	send(Tag, center, C?center).

update_box_1_tag(C,_) :-
	get(C, box_1_tag, @nil), !.
update_box_1_tag(C, What) :-
	get(C, box_1_tag, Tag),
	forall(tag_attribute(What), send(Tag, What, C?What)),
	send(Tag, fill_pattern, colour(white)),
	send(Tag, center, point(C?center_x + 2, C?center_y + 2)),
	send(Tag, hide, C).
	
update_box_2_tag(C,_) :-
	get(C, box_2_tag, @nil), !.
update_box_2_tag(C, What) :-
	get(C, box_2_tag, Tag),
	forall(tag_attribute(What), send(Tag, What, C?What)),
	send(Tag, fill_pattern, colour(white)),
	send(Tag, center, point(C?center_x + 4, C?center_y + 4)),
	send(Tag, hide).
	
update_path_tag(C,_) :-
	get(C, path_tag, @nil), !.
update_path_tag(C, What) :-
	get(C, path_tag, Tag),
	forall(tag_attribute(What), send(Tag, What, C?What)),
	get(C, width, W), get(C, height, H), get(C, x, X), get(C, y, Y),
	send(Tag, points, chain(point(X, Y + H/2), point(X + W/3, Y), point(X + 2*(W/3), Y),
									point(X + W, Y + H/2), point(X + 2*(W/3), Y + H), point(X + W/3, Y + H),
									point(X, Y + H/2))),
	send(Tag, hide).

:- pce_end_class.
