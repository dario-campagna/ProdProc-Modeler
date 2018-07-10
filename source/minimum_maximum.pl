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

:- module(minimum_maximum, [minimum/2, maximum/2]).

:- use_module(library(clpfd)).

% minimum(+Min, +List)
% Min is the minimum element in the list of expressions List
minimum(Min,[L|Ls]):-
	minimum(Ls,L,Min).

minimum([],Min,Min).
minimum([L|Ls],Acc,Min):-
	Acc1 #= min(Acc,L),
   minimum(Ls,Acc1,Min).

% maximum(+Max, +List)
% Max is the maximum element in the list of expressions List   
maximum(Max,[L|Ls]):-
	maximum(Ls,L,Max).

maximum([],Max,Max).
maximum([L|Ls],Acc,Max):-
	Acc1 #= max(Acc,L),
   maximum(Ls,Acc1,Max).