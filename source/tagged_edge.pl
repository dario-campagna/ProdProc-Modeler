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

:- module(tagged_edge, []).
:- use_module(library(pce)).
:- require([ forall/2
	   ]).

:- pce_begin_class(tagged_edge, connection,
		   "Connection with centered tag").

variable(label_tag,	graphical*,	both, "Associated label tag").
variable(card_tag,	graphical*, both, "Associated cardinality tag").
variable(bez_tag,    graphical*, both, "Associated bezier curve tag").
variable(constraints_tag, graphical*, both, "Associated constraints tag").
variable(con_tag, graphical*, both, "").

label_tag(C, Tag:graphical*) :->
	"Associate (new) tag with the connection"::
	get(C, label_tag, Old),
	(   Old == Tag
	->  true
	;   (	Old \== @nil
	    ->	send(Old, free)
	    ;	true
	    ),
	    send(C, slot, label_tag, Tag),
	    update_label_tag(C, _All)
	).
	
card_tag(C, Tag:graphical*) :->
	"Associate (new) tag with the connection"::
	get(C, card_tag, Old),
	(   Old == Tag
	->  true
	;   (	Old \== @nil
	    ->	send(Old, free)
	    ;	true
	    ),
	    send(C, slot, card_tag, Tag),
	    update_card_tag(C, _All)
	).
	
bez_tag(C, Tag:graphical*) :->
	"Associate (new) tag with the connection"::
	get(C, bez_tag, Old),
	(   Old == Tag
	->  true
	;   (	Old \== @nil
	    ->	send(Old, free)
	    ;	true
	    ),
	    send(C, slot, bez_tag, Tag),
	    update_bez_tag(C, _All)
	).

constraints_tag(C, Tag:graphical*) :->
	"Associate (new) tag with the connection"::
	get(C, constraints_tag, Old),
	(   Old == Tag
	->  true
	;   (	Old \== @nil
	    ->	send(Old, free)
	    ;	true
	    ),
	    send(C, slot, constraints_tag, Tag),
	    update_constraints_tag(C, _All)
	).
	
con_tag(C, Tag:graphical*) :->
	"Associate (new) tag with the connection"::
	get(C, con_tag, Old),
	(   Old == Tag
	->  true
	;   (	Old \== @nil
	    ->	send(Old, free)
	    ;	true
	    ),
	    send(C, slot, con_tag, Tag),
	    update_con_tag(C, _All)
	).

unlink(C) :->
	"Destroy tags"::
	send(C, send_super, unlink),
	get(C, label_tag, LTag),
	(   LTag \== @nil
	->  free(LTag)
	;   true
	),
	get(C, card_tag, CTag),
	(   CTag \== @nil
	->  free(CTag)
	;   true
	),
	get(C, bez_tag, BTag),
	(   BTag \== @nil
	->  free(BTag)
	;   true
	),
	get(C, constraints_tag, CsTag),
	(   CsTag \== @nil
	->  free(CsTag)
	;   true
	),
	get(C, con_tag, ConTag),
	(   ConTag \== @nil
	->  free(ConTag)
	;   true
	).
	

tag_attribute(device).
tag_attribute(displayed).

update_label_tag(C, _) :-
	get(C, label_tag, @nil), !.
update_label_tag(C, What) :-
	get(C, label_tag, Tag),
	forall(tag_attribute(What), send(Tag, What, C?What)),
	get(C, bez_tag, Bz),
	(Bz = @nil
	->	send(Tag, center, C?center)
	;	send(Tag, center, Bz?center)
	).
	
update_card_tag(C, _) :-
	get(C, card_tag, @nil), !.
update_card_tag(C, What) :-
	get(C, card_tag, Tag),
	forall(tag_attribute(What), send(Tag, What, C?What)),
	get(C, bez_tag, Bz),
	(Bz = @nil
	->	send(Tag, center, point(C?center_x, C?center_y + 10))
	;	send(Tag, center, point(Bz?center_x, Bz?center_y + 10))
	).
	
update_bez_tag(C, _) :-
	get(C, bez_tag, @nil), !.
update_bez_tag(C, What) :-
	get(C, bez_tag, Tag),
	forall(tag_attribute(What), send(Tag, What, C?What)).
	
update_constraints_tag(C, _) :-
	get(C, constraints_tag, @nil), !.
update_constraints_tag(C, What) :-
	get(C, constraints_tag, Tag),
	forall(tag_attribute(What), send(Tag, What, C?What)),
	get(C, bez_tag, Bz),
	(Bz = @nil
	->	send(Tag, center, point(C?center_x, C?center_y + 25))
	;	send(Tag, center, point(Bz?center_x, Bz?center_y + 30))
	).
	
update_con_tag(C, _) :-
	get(C, con_tag, @nil), !.
update_con_tag(C, What) :-
	get(C, con_tag, Tag),
	forall(tag_attribute(What), send(Tag, What, C?What)),
	get(C, constraints_tag, CE),
	get(C, bez_tag, Bz),
	(	Bz = @nil
	->	send(Tag, start_x, C?center_x),
		send(Tag, start_y, C?center_y),
		send(Tag, end_x, CE?center_x),
		send(Tag, end_y, CE?center_y-8)
	;	send(Tag, start_x, Bz?start?x),
		send(Tag, start_y, Bz?start?y),
		send(Tag, end_x, CE?center_x),
		send(Tag, end_y, CE?center_y-8)
	).

:- pce_end_class.
