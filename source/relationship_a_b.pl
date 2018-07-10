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

:- module(relationship_a_b, [relationship_a_b/6]).

:- use_module(transitivity_table).
:- use_module(check_tc_condition).

% NOTE: A set of activities and of temporal constraints on them can be viewed as an undirected graph 
% where nodes are activities and edges temporal constraints


% Compute a temporal relationship between two activities
relationship_a_b(A, B, TemporalConstraints, ProcVars, Strings, TC) :-
	% If A and B are related by a temporal constraint we return it
	member(CTC, TemporalConstraints), %temporal_constraint(TC),
	is_true_cond_tc(CTC, ProcVars, Strings, TC1),
	( (TC1 =.. [Op, A, B], TC = TC1) 
	; (TC1 =.. [Op, B, A], inverse_tc(TC1, TC)) 
	; (TC1 =.. [or|_], disj_two_acts(A, B, TC1, TC)) 
	).
relationship_a_b(A, B, TemporalConstraints, ProcVars, Strings, TC) :-	
	% If A and B are NOT related by a temporal constraint we compute the paths going from A to B
	paths_A_B(A, B, TemporalConstraints, ProcVars, Strings, Paths),
	paths_to_relation_lists(Paths, RelationPaths),
	% Then, we compute the transitivity constraint on each path and interstect them
	rel_a_b(A, B, RelationPaths, TCList),
	list_to_disj(TCList, TC).

% Check if a condition of a temporal constraint (if any) is true
is_true_cond_tc(CondTC, ProcVars, Strings, TC) :-
	( CondTC = cond(Cond, TC)
	-> check_condition_tf(Cond, ProcVars, Strings, 1)
	;	TC = CondTC
	).	

% Compute paths going form A to B
paths_A_B(A, B, TemporalConstraints, ProcVars, Strings, Paths) :-
	% Compute an association list (Reached) that associates each node 
	% with a list of paths (list of edges) going from A to the node
	put_assoc(A, t, [[]], InitReached),
	sub_net_A_B_q([A], B, TemporalConstraints, ProcVars, Strings, Reached, InitReached),
	get_paths(B, Reached, Paths).
	
get_paths(B, Reached, Paths) :-
	get_assoc(B, Reached, Paths).
get_paths(B, Reached, []) :-
	\+ get_assoc(B, Reached, _).
	
sub_net_A_B_q([], _, _, _, _, Reached, Reached).
sub_net_A_B_q([A|As], End, TemporalConstraints, ProcVars, Strings, Reached, AuxReached) :-
	% Find all the (undirected) edges exiting from A
	edges_form_A(A, TemporalConstraints, ProcVars, Strings, FromA),
	% Add to the queue of nodes to examine all the node reached form A that have not been examined yet
	add_to_queue(FromA, End, AuxReached, NewAs, As),
	get_assoc(A, AuxReached, SubPathsA),
	add_sub_paths(SubPathsA, FromA, [End|NewAs], NewAuxReached, AuxReached),
	sub_net_A_B_q(NewAs, End, TemporalConstraints, ProcVars, Strings, Reached, NewAuxReached).

% Find edges exiting from a node
edges_form_A(A, TemporalConstraints, ProcVars, Strings, FromA) :-
	findall(B-TC,
				(  member(CTC, TemporalConstraints), %temporal_constraint(TC1),
					is_true_cond_tc(CTC, ProcVars, Strings, TC1),
					( (TC1 =.. [Op, A, B], TC = TC1)
					; (TC1 =.. [Op, B, A], inverse_tc(TC1,TC))
					; (TC1 =.. [or|_], disj_two_acts(A, B, TC1, TC)) 
					)	
				),
			  FromA).

% Determine if a constraint is adisjunction of atmic constraints on A and B
disj_two_acts(A, B, TC, TC) :-
	TC =.. [Op, A, B], Op \= or.
disj_two_acts(A, B, TC1, TC) :-
	TC1 =.. [Op, B, A], Op \= or, inverse_tc(TC1, TC).
disj_two_acts(A, B, or(TC1, TC2), or(NewTC1, NewTC2)) :-
	disj_two_acts(A, B, TC1, NewTC1),
	disj_two_acts(A, B, TC2, NewTC2).
	
% Compute the inverse of a temporal constraint
inverse_tc(TC, InvTC) :-
	TC =.. [Op,B,A],
	inv_tc(Op,InvOp),
	InvTC =.. [InvOp, A, B].

inv_tc(before, after).
inv_tc(after, before).
inv_tc(during, includes).
inv_tc(includes, during).
inv_tc(overlaps, overlapped_by).
inv_tc(overlapped_by, overlaps).
inv_tc(meets, met_by).
inv_tc(met_by, meets).
inv_tc(starts, started_by).
inv_tc(started_by, starts).
inv_tc(finishes, finished_by).
inv_tc(finished_by, finishes).
inv_tc(equals, equals).
	
% Add to the queue nodes not examined yet
add_to_queue([], _, _, Q, Q).
add_to_queue([B-_|Bs], End, Reached, [B|Tail1], Tail2) :-
	B \= End,
	\+ get_assoc(B, Reached, _),
	add_to_queue(Bs, End, Reached, Tail1, Tail2).
add_to_queue([B-_|Bs], B, Reached, Q, Tail) :- 
	add_to_queue(Bs, B, Reached, Q, Tail).
add_to_queue([B-_|Bs], End, Reached, Q, Tail) :- 
	B \= End,
	get_assoc(B, Reached, _),
	add_to_queue(Bs, End, Reached, Q, Tail).

% Update the paths in the association list
add_sub_paths([], _, _, Reached, Reached).
add_sub_paths([SubA|SubsA], FromA, Q, Reached, AuxReached) :-
	add_s_p(SubA, FromA, Q, NewAuxReached, AuxReached),
	add_sub_paths(SubsA, FromA, Q, Reached, NewAuxReached).

% Update the paths reaching a node
add_s_p(_, [], _, Reached, Reached).
add_s_p(SubA, [B-TC|FAs], Q, Reached, AuxReached) :-
	get_assoc(B, AuxReached, SubPathsB), member(B, Q),
	append(SubA, [TC], SubB),
	put_assoc(B, AuxReached, [SubB|SubPathsB], NewAuxReached),
	add_s_p(SubA, FAs, Q, Reached, NewAuxReached).
add_s_p(SubA, [B-TC|FAs], Q, Reached, AuxReached) :-
	\+ get_assoc(B, AuxReached, _), member(B, Q),
	append(SubA, [TC], SubB),
	put_assoc(B, AuxReached, [SubB], NewAuxReached),
	add_s_p(SubA, FAs, Q, Reached, NewAuxReached).
add_s_p(SubA, [B-_|FAs], Q, Reached, AuxReached) :-
	\+ member(B, Q),
	add_s_p(SubA, FAs, Q, Reached, AuxReached).


% Transform paths into lists of disjuncitons of atomic temporal constraints (lists of atomic temporal constraints)
paths_to_relation_lists(Paths, RelationPaths) :- paths_to_relation_lists(Paths, RelationPaths, []).
paths_to_relation_lists([], RelationPaths, RelationPaths).
paths_to_relation_lists([P|Ps], [RelationPath|Tail1], Tail2) :-
	path_to_relations(P, RelationPath, []),
	paths_to_relation_lists(Ps, Tail1, Tail2).

path_to_relations([], RelationPath, RelationPath).
path_to_relations([E|Es], [[E]|Tail1], Tail2) :-
	E =.. [Op|_], Op \= or,
	path_to_relations(Es, Tail1, Tail2).
path_to_relations([E|Es], [Disj|Tail1], Tail2) :-
	E =.. [Op|_], Op = or,
	disj_rel(E, Disj),
	path_to_relations(Es, Tail1, Tail2).

disj_rel(TC, [TC]) :- TC =.. [Op|_], Op \= or.
disj_rel(or(TC1, TC2), Disj) :-
	disj_rel(TC1, D1),
	disj_rel(TC2, D2),
	append(D1, D2, Disj).
	
	
% Compute a temporal relationship between two activities
rel_a_b(A, B, RelationPaths, TC) :- 
	R = [
			after(A,B), before(A,B), during(A,B), equals(A,B),
			finished_by(A,B), finishes(A,B), includes(A,B),
			meets(A,B), met_by(A,B), overlapped_by(A,B), overlaps(A,B), 
			started_by(A,B), starts(A,B)
	    ],
	rel_a_b(RelationPaths, TC, R).

rel_a_b([], TC, TC).
rel_a_b([RelPath|RelPaths], TC, R) :-
	RelPath = [E|Es],
	constraint_along_path(Es, CPath, E),
	ord_intersection(R, CPath, NewR),
	rel_a_b(RelPaths, TC, NewR).

constraint_along_path([], CPath, CPath).
constraint_along_path([E|Es], CPath, R) :-
	constraints(R, E, NewR, []),
	constraint_along_path(Es, CPath, NewR).

constraints([], _, C, C).
constraints([R1|RR1], RR2, C, Aux) :-
	cs(R1, RR2, T, []),
	ord_union(Aux, T, NewAux),
	constraints(RR1, RR2, C, NewAux).

cs(_, [], C, C).
cs(R1, [R2|RR2], C, Aux) :-
	t(R1, R2, T),
	ord_union(Aux, T, NewAux),
	cs(R1, RR2, C, NewAux).
	

% Transform a list of temporal constraints into a disjunction of temporal constraints
list_to_disj([], true).
list_to_disj([TC], TC).
list_to_disj([TC|TCs], or(TC,DisjTCs)) :- 
	TCs \= [],
	list_to_disj(TCs, DisjTCs).