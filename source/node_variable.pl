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

:- module(node_variable, []).
:- use_module(library(pce)).

:- pce_begin_class(node_variable, object, "Node variable").

variable(var_name, (name*):='', both, "Variable name").
variable(var_type, ({integer,string}*):=integer, both, "Variable type, string or integer").
variable(var_min_value, (int*):=0, both, "Variable domain min value").
variable(var_max_value, (int*):=0, both, "Variable domain max value").
variable(var_values, chain*, both, "Variable domain values").

initialise(Var, Name, Type, Min, Max, Values) :->
	send(Var, var_name, Name),
	send(Var, var_type, Type),
	send(Var, var_min_value, Min),
	send(Var, var_max_value, Max),
	send(Var, var_values, Values).

:- pce_end_class.