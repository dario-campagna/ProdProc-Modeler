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

:- module(transitivity_table, [t/3]).

% Transitivity table

% before T before -> before
t(before(A,B), before(B,C), [before(A,C)]).
% before T after -> no info
t(before(A,B), after(B, C), T) :-
	T = [
			after(A,C), before(A,C), during(A,C), equals(A,C),
			finished_by(A,C), finishes(A,C), includes(A,C),
			meets(A,C), met_by(A,C), overlapped_by(A,C), overlaps(A,C), 
			started_by(A,C), starts(A,C) 
	    ].
% before T during -> before, overlaps, meets, during, starts
t(before(A,B), during(B,C), T) :-
	T = [
			before(A,C), during(A,C), meets(A,C), 
			overlaps(A,C), starts(A,C)
		  ].
% before T includes -> before
t(before(A,B), includes(B,C), [before(A,C)]).
% before T overlaps -> before
t(before(A,B), overlaps(B,C), [before(A,C)]).
% before T overlapped_by -> before, overlaps, meets, during, starts
t(before(A,B), overlapped_by(B,C), T) :-
	T = [
			before(A,C), during(A,C), meets(A,C), 
			overlaps(A,C), starts(A,C)
		  ].
% before T meets -> before
t(before(A,B), meets(B,C), [before(A,C)]).
% before T met_by -> before, overlaps, meets, duting, starts
t(before(A,B), met_by(B,C), T) :-
	T = [
			before(A,C), during(A,C), meets(A,C), 
			overlaps(A,C), starts(A,C)
		  ].
% before T starts -> before
t(before(A,B), starts(B,C), [before(A,C)]).
% before T started_by -> before
t(before(A,B), started_by(B,C), [before(A,C)]).
% before T finishes -> before, overlaps, meets, during, starts
t(before(A,B), finishes(B,C), T) :-
	T = [
			before(A,C), during(A,C), meets(A,C), 
			overlaps(A,C), starts(A,C)
		  ].
% before T finished_by -> before
t(before(A,B), finished_by(B,C), [before(A,C)]).
% before T equals -> before
t(before(A,B), equals(B,C), [before(A,C)]).

% after T before -> no info
t(after(A,B), before(B, C), T) :-
	T = [
			after(A,C), before(A,C), during(A,C), equals(A,C),
			finished_by(A,C), finishes(A,C), includes(A,C),
			meets(A,C), met_by(A,C), overlapped_by(A,C), overlaps(A,C), 
			started_by(A,C), starts(A,C) 
	    ].
% after T after -> after
t(after(A,B), after(B,C), [after(A,C)]).
% after T during -> after, overlapped_by, met_by, during, finishes
t(after(A,B), during(B, C), T) :-
	T = [
			after(A,C), during(A,C), finishes(A,C), met_by(A,C), overlapped_by(A,C)
	    ].
% after T includes -> after
t(after(A,B), includes(B,C), [after(A,C)]).
% after T overlaps -> after, overlapped_by, met_by, during, finishes
t(after(A,B), overlaps(B, C), T) :-
	T = [
			after(A,C), during(A,C), finishes(A,C), met_by(A,C), overlapped_by(A,C)
	    ].
% after T overlapped_by -> after
t(after(A,B), overlapped_by(B,C), [after(A,C)]).
% after T meets -> after, overlapped_by, met_by, during, finishes
t(after(A,B), meets(B, C), T) :-
	T = [
			after(A,C), during(A,C), finishes(A,C), met_by(A,C), overlapped_by(A,C)
	    ].
% after T met_by -> after
t(after(A,B), met_by(B,C), [after(A,C)]).
% after T starts -> after, overlapped_by, met_by, during, finishes
t(after(A,B), starts(B, C), T) :-
	T = [
			after(A,C), during(A,C), finishes(A,C), met_by(A,C), overlapped_by(A,C)
	    ].
% after T started_by -> after
t(after(A,B), started_by(B,C), [after(A,C)]).
% after T finishes -> after
t(after(A,B), finishes(B,C), [after(A,C)]).
% after T finished_by -> after
t(after(A,B), finished_by(B,C), [after(A,C)]).
% after T equals ->after
t(after(A,B), equals(B,C), [after(A,C)]).

% during T before -> before
t(during(A,B), before(B,C), [before(A,C)]).
% during T after -> afer
t(during(A,B), after(B,C), [after(A,C)]).
% during T during -> during
t(during(A,B), during(B,C), [during(A,C)]).
% during T includes -> no info
t(during(A,B), includes(B, C), T) :-
	T = [
			after(A,C), before(A,C), during(A,C), equals(A,C),
			finished_by(A,C), finishes(A,C), includes(A,C),
			meets(A,C), met_by(A,C), overlapped_by(A,C), overlaps(A,C), 
			started_by(A,C), starts(A,C) 
	    ].
% during T overlaps -> before, overlaps, meets, during, starts
t(during(A,B), overlaps(B, C), T) :-
	T = [
			before(A,C), during(A,C), meets(A,C), overlaps(A,C), starts(A,C) 
	    ].
% during T overlapped_by -> after, overlapped_by, met_by, during, finishes
t(during(A,B), overlapped_by(B, C), T) :-
	T = [
			after(A,C), during(A,C), finishes(A,C), met_by(A,C), overlapped_by(A,C)
	    ].
% during T meets -> before
t(during(A,B), meets(B,C), [before(A,C)]).
% during T met_by -> after
t(during(A,B), met_by(B,C), [after(A,C)]).
% during T starts -> during
t(during(A,B), starts(B,C), [during(A,C)]).
% during T started_by -> after, overlapped_by, met_by, during, finishes
t(during(A,B), started_by(B, C), T) :-
	T = [
			after(A,C), during(A,C), finishes(A,C), met_by(A,C), overlapped_by(A,C)
	    ].
% during T finishes -> during
t(during(A,B), finishes(B,C), [during(A,C)]).
% during T finished_by -> before, overlaps, meets, during, starts
t(during(A,B), finished_by(B, C), T) :-
	T = [
			before(A,C), during(A,C), meets(A,C), overlaps(A,C), starts(A,C) 
	    ].
% during T equals -> during
t(during(A,B), equals(B,C), [during(A,C)]).

% includes T before -> before, overlaps, meets, includes, finished_by
t(includes(A,B), before(B,C), T) :-
	T = [
			before(A,C), finished_by(A,C), includes(A,C), meets(A,C), overlaps(A,C)
	    ].
% includes T after -> after, overlapped_by, includes, met_by, started_by
t(includes(A,B), after(B,C), T) :-
	T = [
			after(A,C), includes(A,C), met_by(A,C), overlapped_by(A,C),	started_by(A,C)
	    ].
% includes T during -> overlaps, overlapped_by, during, includes, equals
t(includes(A,B), during(B,C), T) :-
	T = [
			during(A,C), equals(A,C), includes(A,C), overlapped_by(A,C), overlaps(A,C)
	    ].
% includes T includes -> includes
t(includes(A,B), includes(B,C), [includes(A,C)]).
% includes T overlaps -> overlaps, includes, finished_by
t(includes(A,B), overlaps(B,C), T) :-
	T = [
			finished_by(A,C), includes(A,C), overlaps(A,C)
	    ].
% includes T overlapped_by -> overlapped_by, inludes, started_by
t(includes(A,B), overlapped_by(B,C), T) :-
	T = [
			includes(A,C), overlapped_by(A,C), started_by(A,C)
	    ].
% includes T meets -> overlaps, includes, finished_by
t(includes(A,B), meets(B,C), T) :-
	T = [
			finished_by(A,C), includes(A,C), overlaps(A,C)
	    ].
% includes T met_by -> overlapped_by, inludes, started_by
t(includes(A,B), met_by(B,C), T) :-
	T = [
			includes(A,C), overlapped_by(A,C), started_by(A,C)
	    ].
% includes T starts -> overlaps, includes, finished_by
t(includes(A,B), starts(B,C), T) :-
	T = [
			finished_by(A,C), includes(A,C), overlaps(A,C)
	    ].
% includes T started_by -> includes
t(includes(A,B), started_by(B,C), [includes(A,C)]).
% includes T finishes -> overlapped_by, inludes, started_by
t(includes(A,B), finishes(B,C), T) :-
	T = [
			includes(A,C), overlapped_by(A,C), started_by(A,C)
	    ].
% includes T finished_by -> includes
t(includes(A,B), finished_by(B,C), [includes(A,C)]).
% includes T equals -> includes
t(includes(A,B), equals(B,C), [includes(A,C)]).

% overlaps T before -> before
t(overlaps(A,B), before(B,C), [before(A,C)]).
% overlaps T after -> after, overlapped_by, includes, met_by, started_by
t(overlaps(A,B), after(B,C), T) :-
	T = [
			after(A,C), includes(A,C), met_by(A,C), overlapped_by(A,C),	started_by(A,C)
	    ].
% overlaps T during -> overlaps, during, starts
t(overlaps(A,B), during(B,C), T) :-
	T = [
			during(A,C), overlaps(A,C), starts(A,C) 
	    ].
% overlaps T includes -> before, overlaps, meets, includes, finished_by
t(overlaps(A,B), includes(B,C), T) :-
	T = [
			before(A,C), finished_by(A,C), includes(A,C), meets(A,C), overlaps(A,C)
	    ].
% overlaps T overlaps -> before, overlaps, meets
t(overlaps(A,B), overlaps(B,C), T) :-
	T = [
			before(A,C), meets(A,C), overlaps(A,C)
	    ].
% overlaps T overlapped_by -> overlaps, overlapped_by, during, includes, equals
t(overlaps(A,B), overlapped_by(B,C), T) :-
	T = [
			during(A,C), equals(A,C), includes(A,C), overlapped_by(A,C), overlaps(A,C)
	    ].
% overlaps T meets -> before
t(overlaps(A,B), meets(B,C), [before(A,C)]).
% overlaps T met_by -> overlapped_by, includes, started_by
t(overlaps(A,B), met_by(B,C), T) :-
	T = [
			includes(A,C), overlapped_by(A,C), started_by(A,C)
	    ].
% overlaps T starts -> overlaps
t(overlaps(A,B), starts(B,C), [overlaps(A,C)]).
% overlaps T started_by -> includes, finished_by, overlaps
t(overlaps(A,B), started_by(B,C), T) :-
	T = [
			finished_by(A,C), includes(A,C), overlaps(A,C)
	    ].
% overlaps T finishes -> during, starts, overlaps
t(overlaps(A,B), finishes(B,C), T) :-
	T = [
			during(A,C), overlaps(A,C), starts(A,C) 
	    ].
% overlaps T finished_by -> before, overlaps, meets
t(overlaps(A,B), finished_by(B,C), T) :-
	T = [
			before(A,C), meets(A,C), overlaps(A,C)
	    ].
% overlaps T equals -> overlaps
t(overlaps(A,B), equals(B,C), [overlaps(A,C)]).

% overlapped_by T before -> before, overlaps, meets, includes, finished_by
t(overlapped_by(A,B), before(B,C), T) :-
	T = [
			before(A,C), finished_by(A,C), includes(A,C), meets(A,C), overlaps(A,C)
	    ].
% overlapped_by T after -> after
t(overlapped_by(A,B), after(B,C), [after(A,C)]).
% overlapped_by T during -> overlapped_by, during, finishes
t(overlapped_by(A,B), during(B,C), T) :-
	T = [
			during(A,C), finishes(A,C), overlapped_by(A,C)
	    ].
% overlapped_by T includes -> after, overlapped_by, met_by, includes, started_by
t(overlapped_by(A,B), includes(B,C), T) :-
	T = [
			after(A,C), includes(A,C), met_by(A,C), overlapped_by(A,C),	started_by(A,C)
	    ].
% overlapped_by T overlaps -> overlaps, overlapped_by, during, includes, equals
t(overlapped_by(A,B), overlaps(B,C), T) :-
	T = [
			during(A,C), equals(A,C), includes(A,C), overlapped_by(A,C), overlaps(A,C)
	    ].
% overlapped_by T overlapped_by -> after, overlapped_by, met_by
t(overlapped_by(A,B), overlapped_by(B,C), T) :-
	T = [
			after(A,C), met_by(A,C), overlapped_by(A,C)
	    ].
% overlapped_by T meets -> overlaps, includes, finished_by
t(overlapped_by(A,B), meets(B,C), T) :-
	T = [
			finished_by(A,C), includes(A,C), overlaps(A,C)
	    ].
% overlapped_by T met_by -> after
t(overlapped_by(A,B), met_by(B,C), [after(A,C)]).
% overlapped_by T starts -> overlapped_by, during, finishes
t(overlapped_by(A,B), starts(B,C), T) :-
	T = [
			during(A,C), finishes(A,C), overlapped_by(A,C)
	    ].
% overlapped_by T started_by -> overlapped_by, after, met_by
t(overlapped_by(A,B), started_by(B,C), T) :-
	T = [
			after(A,C), met_by(A,C), overlapped_by(A,C)
	    ].
% overlapped_by T finishes -> overlapped_by
t(overlapped_by(A,B), finishes(B,C), [overlapped_by(A,C)]).
% overlapped_by T finished_by -> overlapped_by, includes, started_by
t(overlapped_by(A,B), finished_by(B,C), T) :-
	T = [
			includes(A,C), overlapped_by(A,C), started_by(A,C)
	    ].
% overlapped_by T equals -> overlapped_by
t(overlapped_by(A,B), equals(B,C), [overlapped_by(A,C)]).

% meets T before -> before
t(meets(A,B), before(B,C), [before(A,C)]).
% meets T after -> after, overlapped_by, met_by, includes, started_by
t(meets(A,B), after(B, C), T) :-
	T = [
			after(A,C), includes(A,C), met_by(A,C), overlapped_by(A,C), started_by(A,C)
	    ].
% meets T during -> overlaps, during, starts
t(meets(A,B), during(B, C), T) :-
	T = [
			during(A,C), overlaps(A,C), starts(A,C) 
	    ].
% meets T includes -> before
t(meets(A,B), includes(B,C), [before(A,C)]).
% meets T overlaps -> before
t(meets(A,B), overlaps(B,C), [before(A,C)]).
% meets T overlapped_by -> overlaps, during, starts
t(meets(A,B), overlapped_by(B, C), T) :-
	T = [
			during(A,C), overlaps(A,C), starts(A,C) 
	    ].			
% meets T meets -> before
t(meets(A,B), meets(B,C), [before(A,C)]).
% meets T met_by -> finishes, finished_by, equals
t(meets(A,B), met_by(B, C), T) :-
	T = [
			equals(A,C), finished_by(A,C), finishes(A,C) 
	    ].
% meets T starts -> meets
t(meets(A,B), starts(B,C), [meets(A,C)]).
% meets T started_by -> meets
t(meets(A,B), started_by(B,C), [meets(A,C)]).
% meets T finishes -> during, starts, overlaps
t(meets(A,B), finishes(B, C), T) :-
	T = [
			during(A,C), overlaps(A,C), starts(A,C) 
	    ].
% meets T finished_by -> before
t(meets(A,B), finished_by(B,C), [before(A,C)]).
% meets T equals -> meets
t(meets(A,B), equals(B,C	), [meets(A,C)]).

% met_by T before -> before, overlaps, meets, includes, finished_by
t(met_by(A,B), before(B,C), T) :-
	T = [
			before(A,C), finished_by(A,C), includes(A,C), meets(A,C), overlaps(A,C)
	    ].
% met_by T after -> after
t(met_by(A,B), after(B,C), [after(A,C)]).
% met_by T during -> overlapped_by, during, finishes
t(met_by(A,B), during(B,C), T) :-
	T = [
			during(A,C), finishes(A,C), overlapped_by(A,C)
	    ].
% met_by T includes -> after
t(met_by(A,B), includes(B,C), [after(A,C)]).
% met_by T overlaps -> overlapped_by, during, finishes
t(met_by(A,B), overlaps(B,C), T) :-
	T = [
			during(A,C), finishes(A,C), overlapped_by(A,C)
	    ].
% met_by T overlapped_by -> after
t(met_by(A,B), overlapped_by(B,C), [after(A,C)]).
% met_by T meets -> starts, started_by, equals
t(met_by(A,B), meets(B,C), T) :-
	T = [
			equals(A,C), started_by(A,C), starts(A,C)
		 ].
% met_by T met_by -> after
t(met_by(A,B), met_by(B,C), [after(A,C)]).
% met_by T starts -> during, finishes, overlapped_by
t(met_by(A,B), starts(B,C), T) :-
	T = [
			during(A,C), finishes(A,C), overlapped_by(A,C)
	    ].
% met_by T started_by -> after
t(met_by(A,B), started_by(B,C), [after(A,C)]).
% met_by T finishes -> met_by
t(met_by(A,B), finishes(B,C), [met_by(A,C)]).
% met_by T finished_by -> met_by
t(met_by(A,B), finished_by(B,C), [met_by(A,C)]).
% met_by T equals -> met_by
t(met_by(A,B), equals(B,C), [met_by(A,C)]).

% starts T before -> before
t(starts(A,B), before(B,C), [before(A,C)]).
% starts T after -> after
t(starts(A,B), after(B,C), [after(A,C)]).
% starts T during -> during
t(starts(A,B), during(B,C), [during(A,C)]).
% starts T includes -> before, overlaps, meets, includes, finished_by
t(starts(A,B), includes(B,C), T) :-
	T = [
			before(A,C), finished_by(A,C), includes(A,C), meets(A,C), overlaps(A,C) 
	    ].
% starts T overlaps -> before, overlaps, meets
t(starts(A,B), overlaps(B,C), T) :-
	T = [
			before(A,C), meets(A,C), overlaps(A,C)
	    ].
% starts T overlapped_by -> overlapped_by, during, finishes
t(starts(A,B), overlapped_by(B,C), T) :-
	T = [
			during(A,C), finishes(A,C), overlapped_by(A,C)
	    ].
% starts T meets -> before
t(starts(A,B), meets(B,C), [before(A,C)]).
% starts T met_by -> met_by
t(starts(A,B), met_by(B,C), [met_by(A,C)]).
% starts T starts -> starts
t(starts(A,B), starts(B,C), [starts(A,C)]).
% starts T started_by -> starts, started_by, equals
t(starts(A,B), started_by(B,C), T) :-
	T = [
			equals(A,C), started_by(A,C), starts(A,C) 
	    ].
% starts T finishes -> during
t(starts(A,B), finishes(B,C), [during(A,C)]).
% starts T finished_by -> before, meets, overlaps
t(starts(A,B), finished_by(B,C), T) :-
	T = [
			before(A,C), meets(A,C), overlaps(A,C)
	    ].
% starts T equals -> starts
t(starts(A,B), equals(B,C), [starts(A,C)]).

% started_by T before -> before, overlaps, meets, includes, finished_by
t(started_by(A,B), before(B,C), T) :-
	T = [
			before(A,C), finished_by(A,C), includes(A,C), meets(A,C), overlaps(A,C)
	    ].
% started_by T after -> after
t(started_by(A,B), after(B,C), [after(A,C)]).
% started_by T during -> overlapped_by, during, finishes
t(started_by(A,B), during(B,C), T) :-
	T = [
			during(A,C), finishes(A,C), overlapped_by(A,C)
	    ].
% started_by T includes -> includes
t(started_by(A,B), includes(B,C), [includes(A,C)]).
% started_by T overlaps -> overlaps, includes, finished_by
t(started_by(A,B), overlaps(B,C), T) :-
	T = [
			finished_by(A,C), includes(A,C), overlaps(A,C)
	    ].
% started_by T overlapped_by -> overlapped_by
t(started_by(A,B), overlapped_by(B,C), [overlapped_by(A,C)]).
% started_by T meets -> overlaps, includes, finished_by
t(started_by(A,B), meets(B,C), T) :-
	T = [
			finished_by(A,C), includes(A,C), overlaps(A,C)
	    ].
% started_by T met_by -> met_by
t(started_by(A,B), met_by(B,C), [met_by(A,C)]).
% started_by T starts -> starts, started_by, equals
t(started_by(A,B), starts(B,C), T) :-
	T = [
			equals(A,C), started_by(A,C), starts(A,C) 
	    ].
% started_by T started_by -> started_by
t(started_by(A,B), started_by(B,C), [started_by(A,C)]).
% started_by T finishes -> overlapped_by
t(started_by(A,B), finishes(B,C), [overlapped_by(A,C)]).
% started_by T finished_by -> includes
t(started_by(A,B), finished_by(B,C), [includes(A,C)]).
% started_by T equals -> started_by
t(started_by(A,B), equals(B,C), [started_by(A,C)]).

% finishes T before -> before
t(finishes(A,B), before(B,C), [before(A,C)]).
% finishes T after -> after
t(finishes(A,B), after(B,C), [after(A,C)]).
% finishes T during -> during
t(finishes(A,B), during(B,C), [during(A,C)]).
% finishes T includes -> after, overlapped_by, met_by, includes, started_by
t(finishes(A,B), includes(B,C), T) :-
	T = [
			after(A,C), includes(A,C), met_by(A,C), overlapped_by(A,C),	started_by(A,C)
	    ].
% finishes T overlaps -> overlaps, during, starts
t(finishes(A,B), overlaps(B,C), T) :-
	T = [
			during(A,C), overlaps(A,C), starts(A,C) 
	    ].
% finishes T overlapped_by -> after, overlapped_by, met_by
t(finishes(A,B), overlapped_by(B,C), T) :-
	T = [
			after(A,C), met_by(A,C), overlapped_by(A,C)
	    ].
% finishes T meets -> meets
t(finishes(A,B), meets(B,C), [meets(A,C)]).
% finishes T met_by -> after
t(finishes(A,B), met_by(B,C), [after(A,C)]).
% finishes T starts -> during
t(finishes(A,B), starts(B,C), [during(A,C)]).
% finishes T started_by -> after, overlapped_by, met_by
t(finishes(A,B), started_by(B,C), T) :-
	T = [
			after(A,C), met_by(A,C), overlapped_by(A,C)
	    ].
% finishes T finishes -> finishes
t(finishes(A,B), finishes(B,C), [finishes(A,C)]).
% finishes T finished_by -> finishes, finished_by, equals
t(finishes(A,B), finished_by(B,C), T) :-
	T = [
			equals(A,C), finished_by(A,C), finishes(A,C)
	    ].
% finishes T equals -> finishes
t(finishes(A,B), equals(B,C), [finishes(A,C)]).

% finished_by T before -> before
t(finished_by(A,B), before(B,C), [before(A,C)]).
% finished_by T after -> after, overlapped_by, met_by, includes, started_by
t(finished_by(A,B), after(B,C), T) :-
	T = [
			after(A,C), includes(A,C), met_by(A,C), overlapped_by(A,C),	started_by(A,C)
	    ].
% finished_by T during -> overlaps, during, starts
t(finished_by(A,B), during(B,C), T) :-
	T = [
			during(A,C), overlaps(A,C), starts(A,C) 
	    ].
% finished_by T includes -> includes
t(finished_by(A,B), includes(B,C), [includes(A,C)]).
% finished_by T overlaps -> overlaps
t(finished_by(A,B), overlaps(B,C), [overlaps(A,C)]).
% finished_by T overlapped_by -> overlapped_by, includes, started_by
t(finished_by(A,B), overlapped_by(B,C), T) :-
	T = [
			includes(A,C), overlapped_by(A,C), started_by(A,C)
	    ].
% finished_by T meets -> meets
t(finished_by(A,B), meets(B,C), [meets(A,C)]).
% finished_by T met_by -> started_by, overlapped_by, includes
t(finished_by(A,B), met_by(B,C), T) :-
	T = [
			includes(A,C), overlapped_by(A,C), started_by(A,C)
	    ].
% finished_by T starts -> overlaps
t(finished_by(A,B), starts(B,C), [overlaps(A,C)]).
% finished_by T started_by -> includes
t(finished_by(A,B), started_by(B,C), [includes(A,C)]).
% finished_by T finishes -> finishes, finished_by, equals
t(finished_by(A,B), finishes(B,C), T) :-
	T = [
			equals(A,C), finished_by(A,C), finishes(A,C)
	    ].
% finished_by T finished_by -> finshed_by
t(finished_by(A,B), finished_by(B,C), [finished_by(A,C)]).
% finished_by T equals -> finished_by
t(finished_by(A,B), equals(B,C), [finished_by(A,C)]).

% equals T before -> before
t(equals(A,B), before(B,C), [before(A,C)]).
% equals T after -> after
t(equals(A,B), after(B,C), [after(A,C)]).
% equals T during -> during
t(equals(A,B), during(B,C), [during(A,C)]).
% equals T includes -> includes
t(equals(A,B), includes(B,C), [includes(A,C)]).
% equals T overlaps -> overlaps
t(equals(A,B), overlaps(B,C), [overlaps(A,C)]).
% equals T overlapped_by -> overlapped_by
t(equals(A,B), overlapped_by(B,C), [overlapped_by(A,C)]).
% equals T meets -> meets
t(equals(A,B), meets(B,C), [meets(A,C)]).
% equals T met_by -> met_by
t(equals(A,B), met_by(B,C), [met_by(A,C)]).
% equals T starts -> starts
t(equals(A,B), starts(B,C), [starts(A,C)]).
% equals T started_by -> started_by
t(equals(A,B), started_by(B,C), [started_by(A,C)]).
% equals T finishes -> finishes
t(equals(A,B), finishes(B,C), [finishes(A,C)]).
% equals T finished_by -> finished_by
t(equals(A,B), finished_by(B,C), [finished_by(A,C)]).
% equals T equals -> equals
t(equals(A,B), equals(B,C), [equals(A,C)]).