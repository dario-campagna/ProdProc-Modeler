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

:- module(check_tc_condition, [check_condition_tf/4, fd_condition/4]).

:- use_module(library(clpfd)).

% Check condition of a temporal constraint
check_condition_tf(true,_,_,_).
check_condition_tf(iff(C), ProcVars, B, TF) :-
	fd_condition(C, ProcVars, B, FDC),
	TF #<==> FDC.
check_condition_tf(if(C), ProcVars, B, TF) :- % CHOICE POINT?
	fd_condition(C, ProcVars, B, FDC),
	( TF = 1
	->	FDC
	;	#\ FDC
	).

% Obtaint a fd contraint from a temporal constraint condition
fd_condition(Var, ProcVars, _, FDVar) :-
	get_assoc(Var, ProcVars, FDVar-_).
fd_condition(Int, _, _, Int) :- integer(Int).
fd_condition(String, _, B, IntString) :-
	atom(String),
	get_assoc(String, B, IntString).

fd_condition(C, ProcVars, B, FDC) :-
	C =.. [Op,C1,C2],
	member((Op,FDOp),[(plus,+),(minus,-),(times,*),(frac,/),(mod,mod),(lt,#<),(leq,#=<),(eq,#=),(geq,#>=),(gt,#>),(neq,#\=)]),
	fd_condition(C1,ProcVars, B, FDC1),
	fd_condition(C2,ProcVars, B, FDC2),
	FDC =.. [FDOp,FDC1,FDC2].
fd_condition(minus_sign(Exp), ProcVars, B, - FDExp) :-
	fd_condition(Exp, ProcVars, B, FDExp).
fd_condition(C, ProcVars, B, FDC) :-
	C =.. [Op,C1,C2],
	member((Op,FDOp),[(and,#/\),(or,#\/),(impl,#==>)]),
	fd_condition(C1,ProcVars, B, FDC1),
	fd_condition(C2,ProcVars, B, FDC2),
	FDC =.. [FDOp,FDC1,FDC2].	
fd_condition(not(Con),ProcVars, B, FDC) :-
	fd_condition(Con,ProcVars, B, FDC).