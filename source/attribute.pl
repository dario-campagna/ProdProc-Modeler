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


:- module(draw_attribute, []).
:- use_module(library(pce)).
:- use_module(messages).
:- use_module(sub_process_modeler).
:- require([ between/3
	   , chain_list/2
	   , default/3
	   , forall/2
	   , get_config/2
	   , get_object/4
	   , listen/3
	   , member/2
	   , send_list/3
	   , set_config/2
	   , unlisten/1
	   ]).

:- pce_autoload(font_item, library(pce_font_item)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
draw_product_attribute_editor defines a  separate frame that allows the user 
to set the values of attributes (pen, font, etc.) of  product model elements.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_product_attribute_editor, frame).

variable(editor,	object,		get,
	 "Editor I'm attached too").
variable(client,	chain*,		get,
	 "Objects I'm editing the attributes for").
variable(blocked,	int := 0,	get,
	 "Blocked count to avoid quadratic behaviour").

attribute(node_name, node_name).
attribute(edge_label, edge_label).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create the attribute  window.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(A, Draw:object, Label:[name]) :->
	default(Label, 'Attributes', Lbl),
	send(A, send_super, initialise, Lbl),
	send(A, done_message, message(A, quit)),
	send(A, append, new(dialog)),
	send(A, slot, editor, Draw),
	send(A, fill_dialog),
	listen(A,
	       %set_config(draw_config:resources/_, _),
	       send(A, config_changed)).


config_changed(A) :->
	get(A, member, dialog, D),
	send(D, clear),
	send(D, fill_dialog),
	send(D, layout),
	send(D, fit),
	(   get(A, client, Client), Client \== @nil
	->  send(A, client, Client)
	;   true
	).


open(A, Pos:[point]) :->
	"Open at position from config database"::
	(   Pos == @default,
	    get(A, editor, Draw), Draw \== @nil %,
	    %get_config(draw_config:history/geometry/attributes, Diff)
	->  get(Draw?area, position, Pos1),
		 Pos1 = Pos	
	    %send(Pos1, plus, Diff)
	;   Pos1 = Pos
	),
	send(A, send_super, open, Pos1, normalise := @on).


unlink(A) :->
	"Save position in config database"::
	(   get(A, editor, Draw), Draw \== @nil %,
	    %get(Draw?area, position, PDraw),
	    %get(A?area, position, PA),
	    %get_object(PA, difference, PDraw, Diff),
	    %set_config(draw_config:history/geometry/attributes, Diff)
	->  true
	;   true
	),
	unlisten(A),
	send(A, send_super, unlink).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fill the dialog with the various menus.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fill_dialog(A) :->
	get(A, member, dialog, D),
	get(A, editor, Canvas),
	
	make_text_item(Text),
	
	send(D, append, Text),
	
	( send(@arg1, has_attribute, mc_set)
	-> true
	;	make_varCard(Canvas,VarCard),
		send_list(D, append, VarCard)
	),
	
	make_constraint(Canvas,Constraint),
	
	send_list(D, append, Constraint),
	
	send(D, append, button(apply, message(D, apply))),

	send(D, append, button(quit, message(A, quit))).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Text dialog for changing node names and edge labels
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_text_item(Text) :-
	send(@arg1, has_attribute, mc_set)
	-> get(@arg1, mc_set, CurrentMCS),
		new(Text,
			text_item(model_constraint_set,
					CurrentMCS,
					message(@receiver?frame, client_attribute, mc_set, @arg1)))
	;(	send(@arg1, has_attribute, node_name)
	 ->	get(@arg1, node_name, CurrentName),
			new(Text, 
			 	text_item(node_name,
					  CurrentName,
					  message(@receiver?frame, client_attribute, node_name, @arg1)))
	 ; 	get(@arg1, edge_label, CurrentLabel),
			new(Text, 
				text_item(edge_label,
					  CurrentLabel,
					  message(@receiver?frame, client_attribute, edge_label, @arg1)))
	 ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create elements for setting variables/cardinality
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_varCard(Canvas,VarCard) :-
	send(@arg1, has_attribute, node_name)
	->	get(@arg1, node_variables, Variables),
		(Variables = @nil
		->	VarCard = []
		;	chain_list(Variables, ListVars),
			text_vars(ListVars, ListTextVars, []),
			new(Vars, list_browser(@default, 70)),
			send(Vars, multiple_selection, @off),
			send_list(Vars, append, ListTextVars),
			new(AddVar, button(add_variable, 
					message(@prolog, add_variable, Vars, Variables, Canvas))),
			new(DelVar, button(delete_variable,
					message(@prolog, delete_variable, Vars, Variables, Canvas))),
			new(ModVar, button(modify_variable,
					message(@prolog, modify_variable, Vars, Variables, Canvas))),
			VarCard = [Vars, AddVar, DelVar, ModVar]
		)
	;	get(@arg1, edge_cardinality_value, Val),
		get(@arg1, edge_cardinality_name, Name),
		get(@arg1, edge_cardinality_min_value, Min),
		get(@arg1, edge_cardinality_max_value, Max),
		new(TextName, text_item(cardinality_name, '',
			message(@receiver?frame, client_attribute, edge_cardinality_name, @arg1))),
		new(TextVal, int_item(cardinality_value, 1, 
			message(@receiver?frame, client_attribute, edge_cardinality_value, @arg1), 0)),
		new(TextMax, int_item),
		new(TextMin, int_item(cardinality_min_value, 0,
			message(@prolog, update_cardinality_min_value, @receiver?frame, edge_cardinality_min_value, @arg1, TextMax), 0)),
		send(TextMax, initialise, cardinality_max_value, 1,
			message(@prolog, update_cardinality_max_value, @receiver?frame, edge_cardinality_max_value, @arg1, TextMin), 0),
		new(CardType, menu(cardinality_type)),
		new(Constant, menu_item(constant, 
			and(
				message(@receiver?frame, client_attribute, edge_cardinality_type, const_card),
				message(TextMax, editable, @off), message(TextMin, editable, @off), message(TextName, editable, @off),
				message(TextVal, editable, @on),
				message(TextVal, displayed_value, Val),
				message(TextName, displayed_value, '')%, message(TextMin, displayed_value, ''), message(TextMax, displayed_value, '')
			))),
		new(Range, menu_item(range, 
			and(
				message(@receiver?frame, client_attribute, edge_cardinality_type, range_card),
				message(TextMax, editable, @on), message(TextMin, editable, @on), message(TextName, editable, @on),
				message(TextVal, editable, @off),
				%message(TextVal, displayed_value, ''),
				message(TextName, displayed_value, Name), message(TextMin, displayed_value, Min), message(TextMax, displayed_value, Max)
				))),
		send_list(CardType, append, [Constant, Range]),	
		VarCard = [CardType, TextName, TextVal, TextMin, TextMax],
		( get(@arg1, edge_cardinality_type, const_card)
		->	send(Constant, selected, @on),
			send(TextName, editable, @off), send(TextMin, editable, @off), send(TextMax, editable, @off),
			send(TextVal, displayed_value, Val)
		;	send(Range, selected, @on),
			send(TextVal, editable, @off),
			send(TextName, displayed_value, Name), send(TextMin, displayed_value, Min), send(TextMax, displayed_value, Max)
		).
		
% Update cardinality value or show an error message
update_cardinality_min_value(Rec, Arg, Value, TextMax) :-
	get(TextMax, selection, Max),
	Max > Value
	->	send(Rec, client_attribute, Arg, Value)
	;	error_message('Min value must be lesser than max value!').

update_cardinality_max_value(Rec, Arg, Value, TextMin) :-
	get(TextMin, selection, Min),
	Min < Value
	->	send(Rec, client_attribute, Arg, Value)
	;	error_message('Max value must be greater than min value!').
		
% Create text strings from node variables
text_vars([],Tail,Tail).
text_vars([V|Vs],[TV|Tail1],Tail2) :-
	text_var(V,TV),
	text_vars(Vs, Tail1, Tail2).
	
text_var(V, DI) :-
	get(V, var_name, Name),
	( get(V, var_type, integer), get(V, var_values, @nil)
	-> get(V, var_min_value, Min), get(V, var_max_value, Max),
		atomic_list_concat([Name,', [',Min,',',Max,']'], TV)
	;  get(V, var_values, Values),
		chain_list(Values, ListValues),
		atomic_list_concat(ListValues, ',', AV),
		atomic_list_concat([Name, ', {', AV, '}'],TV)
	),
	new(DI, dict_item(@default, TV, V)).

% Frame for adding variables
add_variable(Vars, Variables, Canvas) :-
	new(Frame, frame('Variable creation')),
	send(Frame, append, new(D, dialog)),
	
	new(Var, node_variable(@nil,integer,@nil,@nil,@nil)),
	
	new(TextName, text_item(variable_name,'',
		message(@prolog, update_var, Var, var_name, @arg1, 'Var'))),
	new(TextMin, int_item(min_value,@default,
		message(@prolog, update_var, Var, var_min_value, @arg1, @nil))),
	new(TextMax, int_item(max_value,@default,
		message(@prolog, update_var, Var, var_max_value, @arg1, @nil))),
	new(TextValues, text_item(values,'',
		message(@prolog, add_var_values, Var, @arg1))),
	send(TextValues, editable, @off),
		
	new(DomType, menu(integer_domain_format)),
	
	new(VarType, menu(variable_type)),
	new(Integer, menu_item(integer,
		and(
			message(Var, var_type, integer),
			message(@prolog, editable_texts, DomType, TextMin, TextMax, TextValues),	
			message(TextValues, clear),
			message(DomType, all_on)
			))),
	new(String, menu_item(string,
		and(
			message(Var, var_type, string),
			message(TextValues, editable, @on),
			message(TextMin, editable, @off),
			message(TextMin, clear),
			message(TextMax, editable, @off),
			message(TextMax, clear),
			message(TextValues, clear),
			message(DomType, all_off)
			))),
	send_list(VarType, append, [Integer, String]),
	send(Integer, selected, @on),
	
	new(Interval, menu_item(interval, 
		and(
			message(TextMin, editable, @on),
			message(TextMax, editable, @on),
			message(TextValues, editable, @off),
			message(TextValues, clear),
			message(Integer, selected, @on) ,
			message(Var, var_values, @nil)
			))),
	new(Set, menu_item(set,
		and(
			message(TextMin, editable, @off),
			message(TextMin, clear),
			message(TextMax, editable, @off),
			message(TextMax, clear),
			message(TextValues, editable, @on)
		))),
	send_list(DomType, append, [Interval, Set]),
	send(Interval, selected, @on),
	
	send_list(D, append, [VarType, DomType, TextName, TextMin, TextMax, TextValues]),

	new(Add, button(add,
		and(
		 message(D, apply),
		 message(@prolog, add_new_variable, TextName, TextMin, TextMax, TextValues, Integer, Interval, Var, Variables, Vars, Canvas)
		))),
	
	send(D, append, Add), 
	
	send(D, append, button(quit, message(Frame, destroy))),
	
	send(Frame, open).

% Update the value of argument Arg of variable Var (Text is the content of a text_item)
update_var(Var, Arg, Text, Default) :-
	( Text \= ''
	->	send(Var, Arg, Text)
	;	send(Var, Arg, Default)
	).

% Determine wich text_item are editable	
editable_texts(DomType, TextMin, TextMax, TextValues) :-
	(get(DomType, selected, interval, @on)
	->	send(TextMin, editable, @on),
		send(TextMax, editable, @on),
		send(TextValues, editable, @off),
		send(TextValues, clear)
	;	send(TextMin, editable, @off),
		send(TextMin, clear),
		send(TextMax, editable, @off),
		send(TextMax, clear),
		send(TextValues, editable, @on)
	).

% Convert a list of comma separeted values into a chain, and add it to variable Var	
add_var_values(Var, Values) :-
	Values \= ''
	->	new(Chain, chain),
		atom_to_term(Values, TermValues, []),
		value_chain(TermValues, Chain),
		send(Var, var_values, Chain)
	;	send(Var, var_values, @nil).

value_chain(Values, Chain) :-
	Values = (V1,Vs)
	->	send(Chain, append, V1),
		value_chain(Vs, Chain)
	;	send(Chain, append, Values).

% Add the created variable to a node, or show an error message	
add_new_variable(TextVar, TextMin, TextMax, TextValues, Integer, Interval, Var, Variables, Vars, Canvas) :-
	( get(TextVar, selection, '')
	->	error_message('Variable name (and maybe something else) is missing!')
	;	( get(Integer, selected, @on)
		->	( get(Interval, selected, @on)
			-> ( (get(TextMin, selection, '') ; get(TextMax, selection, '') ;
					%(get(TextMin, selection, Min), atom_to_term(Min,I1,[]), \+ integer(I1)) ; 
					%(get(TextMax, selection, Max), atom_to_term(Max,I2,[]), \+ integer(I2)) 
					\+ get(TextMin, selection, _) ; \+ get(TextMax, selection, _)
				  )
				-> error_message('Min or/and max value is/are missing!')%, or is/are not integer/s!')
				; 	( get(TextMin, selection, Min), atom_to_term(Min,I1,[]),
					  get(TextMax, selection, Max), atom_to_term(Max,I2,[]),
					  I1 >= I2
					-> error_message('The minimum value has to be lesser than the maximum value!')
					;	append_var(Variables, Var), send(@prolog, update_var_list, Vars, Variables),
						send(Canvas, slot, modified, @on)
					)
				)
			;	( get(TextValues, selection, '')
				-> error_message('Values are missing!')
				; ( check_integer(Var)
				  -> append_var(Variables, Var), send(@prolog, update_var_list, Vars, Variables),
				  	  send(Canvas, slot, modified, @on)
				  ; error_message('Values must be integers!')
				  )
				)
			)
		;	( get(TextValues, selection, '')
			-> error_message('Values are missing!')
			; append_var(Variables, Var), send(@prolog, update_var_list, Vars, Variables),
			  send(Canvas, slot, modified, @on)
			)
		)
	).
	
% Create a new variable identical to the one just created and add it to the node variables
% In this way we can keep the variabel creation frame opened and use it to create as many variable we want
append_var(Variables, Var) :-
	get(Var, var_name, N),
	get(Var, var_type, T),
	get(Var, var_min_value, Min),
	get(Var, var_max_value, Max),
	get(Var, var_values, Vs),
	new(V, node_variable(N,T,Min,Max,Vs)),
	send(Variables, append, V).

% Update the node variable list browser	
update_var_list(Vars, Variables) :-
	chain_list(Variables, ListVars),
	text_vars(ListVars, ListTextVars, []),
	send(Vars, clear),
	send_list(Vars, append, ListTextVars).

% Check if the value chain of a variable contains only integers
check_integer(Var) :- 
	get(Var, var_values, Values),
	chain_list(Values, List),
	forall(member(X, List), integer(X)).

% Delete variable selected in variable list browser
delete_variable(Vars, Variables, Canvas) :-
	get(Vars, selection, SelV),
	get(SelV, object, V),
	send(Variables, delete, V),
	update_var_list(Vars, Variables),
	send(Canvas, slot, modified, @on).
	
% Modify variable selected in variable list browser
modify_variable(Vars, Variables, Canvas) :-
	get(Vars, selection, SelV),
	get(SelV, object, V),
	mod_var(V, Vars, Variables, Canvas).
	
mod_var(Var, Vars, Variables, Canvas) :-
	new(Frame, frame('Modify variable')),
	send(Frame, append, new(D, dialog)),
	
	get(Var, var_name, Name),
	get(Var, var_type, Type),
	get(Var, var_values, Values),
	
	( Type = integer, Values = @nil
	->	get(Var, var_min_value, Min), get(Var, var_max_value, Max), Vs = '',
		I = @on, S = @off
	;	Min = @default, Max = @default, chain_to_text(Values, Vs),
		S = @on, I = @off
	),
	
	(Type = integer -> IType = @on ; IType = @off),
	
	new(TextName, text_item(variable_name,Name,
		message(@prolog, update_var, Var, var_name, @arg1, Name))),
	new(TextMin, int_item(min_value,Min,
		message(@prolog, update_var, Var, var_min_value, @arg1, @nil))),
	send(TextMin, editable, I),
	new(TextMax, int_item(max_value,Max,
		message(@prolog, update_var, Var, var_max_value, @arg1, @nil))),
	send(TextMax, editable, I),
	new(TextValues, text_item(values,Vs,
		message(@prolog, add_var_values, Var, @arg1))),
	send(TextValues, editable, S),
	
	new(DomType, menu(integer_domain_format)),
	
	new(VarType, menu(variable_type)),
	new(Integer, menu_item(integer,
		and(
			message(Var, var_type, integer),
			message(@prolog, editable_texts, DomType, TextMin, TextMax, TextValues),	
			message(TextValues, clear),
			message(DomType, all_on)
			))),
	new(String, menu_item(string,
		and(
			message(Var, var_type, string),
			message(TextValues, editable, @on),
			message(TextMin, editable, @off),
			message(TextMax, editable, @off),
			message(TextValues, clear),
			message(DomType, all_off)
			))),
	(IType = @on -> send(Integer, selected, @on) ; send(String, selected, @on)),
	send_list(VarType, append, [Integer, String]),
	
	new(Interval, menu_item(interval, 
		and(
			message(TextMin, editable, @on),
			message(TextMax, editable, @on),
			message(TextValues, editable, @off),
			message(Integer, selected, @on),
			message(Var, var_values, @nil)
			))),
	new(Set, menu_item(set,
		and(
			message(TextMin, editable, @off),
			message(TextMax, editable, @off),
			message(TextValues, editable, @on)
		))),
	(I = @on -> send(Integer, selected, @on) ; send(Set, selected, @on)),
	send_list(DomType, append, [Interval, Set]),
	(IType = @on -> send(DomType, all_on) ; send(DomType, all_off)),
	
	send_list(D, append, [VarType, DomType, TextName, TextMin, TextMax, TextValues]),
	
	new(Mod, button(modify, 
		message(@prolog, apply_modifications, D, Integer, Interval, TextName, TextMin, TextMax, TextValues, Vars, Variables, Canvas))),
	
	send(D, append, Mod), 
	
	send(D, append, button(quit, message(Frame, destroy))),
	send(Frame, open).
	
chain_to_text(Values, Vs) :-
	chain_list(Values, List),
	atomic_list_concat(List, ',', Vs).
	
apply_modifications(D, Integer, Interval, TextName, TextMin, TextMax, TextValues, Vars, Variables, Canvas) :-
	( get(TextName, selection, '')
	->	error_message('Variable name (and maybe something else) is missing!')
	;	( get(Integer, selected, @on)
		->	( get(Interval, selected, @on)
			-> ( (get(TextMin, selection, '') ; get(TextMax, selection, '') ;
					%(get(TextMin, selection, Min), atom_to_term(Min,I1,[]), \+ integer(I1)) ; 
					%(get(TextMax, selection, Max), atom_to_term(Max,I2,[]), \+ integer(I2)) ;
					\+ get(TextMin, selection, _) ; \+ get(TextMax, selection, _)
				  )
				-> error_message('Min or/and max value is/are missing!')%, or is/are not integer/s!')
				; 	( get(TextMin, selection, Min), atom_to_term(Min,I1,[]),
					  get(TextMax, selection, Max), atom_to_term(Max,I2,[]),
					  I1 >= I2
					-> error_message('The minimum value has to be lesser than the maximum value!')
					;	send(D, apply, @on), send(@prolog, update_var_list, Vars, Variables),
						send(Canvas, slot, modified, @on)
					)
				)
			;	( get(TextValues, selection, '')
				-> error_message('Values are missing!')
				; ( get(TextValues, selection, Values), atom_to_term(Values, TermVs, []),
					 value_list(TermVs,LVs), forall(member(X,LVs), integer(X))
				  -> send(D, apply, @on), send(@prolog, update_var_list, Vars, Variables),
				  	  send(Canvas, slot, modified, @on)
				  ; error_message('Values must be integers!')
				  )
				)
			)
		;	( get(TextValues, selection, '')
			-> error_message('Values are missing!')
			; send(D, apply, @on), send(@prolog, update_var_list, Vars, Variables),
			  send(Canvas, slot, modified, @on)
			)
		)
	).
	
value_list(TermVs, List) :-
	TermVs = (V1,Vs)
	->	List = [V1|Tail1],
		value_list(Vs, Tail1)
	;	List = [TermVs].
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create elements for setting constriaints
Currently, constraint creation has no support. We may implement something
similar to the constraint creation procedure/interface presented in 
Schiavinato thesis.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

make_constraint(Canvas,Constraint) :-
	( send(@arg1, has_attribute, node_name)
	->	( get(@arg1, node_variables, Vs), send(Vs,empty)
		->	C = no
		;  get(@arg1, node_constraints, Constraints), C = yes
		)
	;	( send(@arg1, has_attribute, edge_label), get(@arg1, edge_cardinality_type, range_card)
		-> get(@arg1, edge_constraints, Constraints), C = yes
		;  ( send(@arg1, has_attribute, mc_set)
			-> get(@arg1, model_constraints, Constraints), C = yes
			;	C = no
			)
		)
	),
	( C = yes
	->	chain_list(Constraints, ListConstraints),
		dict_constraints(ListConstraints, DictConstraints, []),
		new(ConstraintLB, list_browser(@default, 70)),
		send(ConstraintLB, multiple_selection, @off),
		send_list(ConstraintLB, append, DictConstraints),
		new(AddConstraint, button(add_constraint, 
			message(@prolog, add_constraint, ConstraintLB, Constraints, Canvas))),
		new(DelConstraint, button(delete_constraint,
			message(@prolog, delete_constraint, ConstraintLB, Constraints, Canvas))),
		new(ModConstraint, button(modify_constraint,
			message(@prolog, modify_constraint, ConstraintLB, Constraints, Canvas))),
		Constraint = [ConstraintLB, AddConstraint, DelConstraint, ModConstraint]
	;	Constraint = []
	).

dict_constraints([], Dict, Dict).
dict_constraints([C|Cs], [D|List1], List2) :-
	get(C, constraint, CC),
	new(D, dict_item(@default, CC, C)),
	dict_constraints(Cs, List1, List2).

add_constraint(ConstraintLB, Constraints, Canvas) :-
	new(Frame, frame('Constraint creation')),
	send(Frame, append, new(D, dialog)),
	
	send(D, append, text_item(constraint, '', 
			message(@prolog, add_c, Constraints, @arg1, Canvas) 
		)),
	
	send(D, append, button(add, 
		and(
			message(D, apply),
			message(@prolog, update_constraint_list, ConstraintLB, Constraints)
			))),
	
	send(D, append, button(quit, message(Frame, destroy))),
	
	send(Frame, open).
	
add_c(Constraints, C, Canvas) :-
	new(PC, product_constraint(C)),
	send(Constraints, append, PC),
	send(Canvas, slot, modified, @on).

delete_constraint(ConstraintLB, Constraints, Canvas) :-
	get(ConstraintLB, selection, SelC),
	get(SelC, object, C),
	send(Constraints, delete, C),
	update_constraint_list(ConstraintLB, Constraints),
	send(Canvas, slot, modified, @on).
	
update_constraint_list(ConstraintLB, Constraints) :-
	chain_list(Constraints, ListConstraints),
	dict_constraints(ListConstraints, DictConstraints, []),
	send(ConstraintLB, clear),
	send_list(ConstraintLB, append, DictConstraints).

modify_constraint(ConstraintLB, Constraints, Canvas) :-
	get(ConstraintLB, selection, SelC),
	
	get(SelC, label, CText),
	get(SelC, object, C),
	
	new(Frame, frame('Modify constraint')),
	send(Frame, append, new(D, dialog)),
	
	send(D, append, text_item(constraint, CText, 
			message(C, constraint, @arg1)
		)),
	
	send(D, append, button(modify, 
		and(
			message(D, apply),
			message(@prolog, update_constraint_list, ConstraintLB, Constraints),
			message(Canvas, slot, modified, @on)
			))),
	
	send(D, append, button(quit, message(Frame, destroy))),
	
	send(Frame, open).



		/********************************
		*              QUIT		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
For a secondary  window like  this  attribute  editor,  it might be  a
useful idea  not to destroy the window  if the user  hits  `quit', but
just to unmap it from the display using `Frame ->show: @off'.  In this
case, it can  be remapped on the  display very  quickly   and when the
window has certain status  information attached to  it,  this  will be
maintained.   For the   case of this editor,  this  only concernes the
coordinates of the window.

To control between  actual  destruction  and   just unmapping it,   an
optional   boolean   argument has been   attached.  This  approach has
several advantages.  If  the caller wants to  descriminate, it can  do
so.  For all cases where the caller does not want  to discriminate, we
have one central place to change the default behaviour.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

quit(A, ShowOff:[bool]) :->
	(   ShowOff == @on
	->  send(A, show, @off)
	;   send(A?editor, attribute_editor, @nil),
	    send(A, free)
	).


		/********************************
		*     CLIENT COMMUNICATION	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->fill_items fills and  (de)activates all  dialog items.  The argument
is a chain of shapes (normally the <-selection of the canvas).  If one
of the elements of the selection  has the specified attribute, it will
be activated and the ->selection of the menu will be set accordingly.

If more than one object   in the  selection  has some  attribute,  the
->selection  of the  item  will  be the attribute  value of  the first
object in the chain that is has the attibute.  This is a rather simple
way of handling this case, but what else can we do?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fill_items(A, Client:chain) :->
	"Fill the dialog items from chain of shapes"::
	get(A, member, dialog, Dialog),
	(   attribute(Label, Selector),
	    get(Dialog, member, Label, Menu),
	    (   get(Client, find,
		    and(message(@arg1, has_send_method, has_attribute),
			message(@arg1, has_attribute, Selector)),
		    Proto),
		get(Proto, draw_attribute, Selector, Value)
	    ->  send(Menu, active, @on),
		set_selection(Menu, Value)
	    ;   send(Menu, active, @off)
	    ),
	    fail
	;   true
	).

set_selection(Menu, Value) :-
	send(Menu, instance_of, menu), !,
	(   get(Menu, member, Value, Item)
	->  send(Menu, selection, Item)
	;   get(Menu, attribute, equal_predicates, PredChain),
	    chain_list(PredChain, Preds),
	    member(Pred, Preds),
	    get(Menu?members, find,
		message(@prolog, Pred, @arg1?value, Value),
		Item)
	->  send(Menu, selection, Item)
	;   true
	).
set_selection(Menu, Value) :-
	send(Menu, selection, Value).


block(A) :->
	"<-blocked++"::
	get(A, blocked, B0),
	B1 is B0 + 1,
	send(A, slot, blocked, B1).

unblock(A) :->
	"<-blocked--"::
	get(A, blocked, B0),
	B1 is B0 - 1,
	send(A, slot, blocked, B1).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the chain of shapes for which we are editing the attributes.  Note
that if the window is not shown, we won't update the contents.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

client(A, Client:chain*) :->
	"Set the graphical I'm editing"::
	get(A, member, dialog, Dialog),
	(   get(A, blocked, B), B == 0
	->  (    Client == @nil
	    ->   send(Dialog?graphicals, for_some,
		      message(@arg1, active, @off))
	    ;    send(A, fill_items, Client)
	    )
	;   true
	),
	send(A, slot, client, Client).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the value of an attribute for the clients.  The value is set for
each shape that accepts ->has_attribute.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

client_attribute(A, Selector:name, Val:any) :->
	"Set attribute of client object"::
	(   get(A, client, Chain),
	    Chain \== @nil,
	    get(Chain, head, Head)
	->  send(A, block),
	    get(Head, window, Window),
	    send(Window, open_undo_group),
	    send(A?client, for_some,
		 if(message(@arg1, has_attribute, Selector),
		    message(@arg1, draw_attribute, Selector, Val))),
	    send(Window, close_undo_group),
	    send(A, unblock)
	;   true
	).

:- pce_end_class.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
draw_process_attribute_editor defines a  separate frame that allows the user 
to set the values of attributes of  process model elements.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(draw_process_attribute_editor, frame).

variable(editor,	object,		get,
	 "Editor I'm attached too").
variable(client,	chain*,		get,
	 "Objects I'm editing the attributes for").
variable(blocked,	int := 0,	get,
	 "Blocked count to avoid quadratic behaviour").
	 
%attribute(node_name, node_name).
%attribute(edge_label, edge_label).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create the attribute  window.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

initialise(A, Draw:object, Label:[name]) :->
	default(Label, 'Attributes', Lbl),
	send(A, send_super, initialise, Lbl),
	send(A, done_message, message(A, quit)),
	send(A, append, new(dialog)),
	send(A, slot, editor, Draw),
	send(A, fill_dialog),
	listen(A, send(A, config_changed)).


config_changed(A) :->
	get(A, member, dialog, D),
	send(D, clear),
	send(D, fill_dialog),
	send(D, layout),
	send(D, fit),
	(   get(A, client, Client), Client \== @nil
	->  send(A, client, Client)
	;   true
	).


open(A, Pos:[point]) :->
	"Open at position from config database"::
	(   Pos == @default,
	    get(A, editor, Draw), Draw \== @nil %,
	->  get(Draw?area, position, Pos1),
		 Pos1 = Pos	
	;   Pos1 = Pos
	),
	send(A, send_super, open, Pos1, normalise := @on).


unlink(A) :->
	"Save position in config database"::
	(   get(A, editor, Draw), Draw \== @nil
	->  true
	;   true
	),
	unlisten(A),
	send(A, send_super, unlink).
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fill the dialog with the various menus.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fill_dialog(A) :->
	get(A, member, dialog, D),
	get(A, editor, Canvas),
	
	( send(@arg1, has_attribute, atomic_constraint)
	->	get(@arg1, atomic_constraint, AC), get(@arg1, condition, Cond),
		make_menu_atomic_constraint(AC,Cond,Menu),
		DialogType = none
	;	true
	),
	
	( send(@arg1, has_attribute, resource_name)
	->	get(@arg1, resource_name, RN), get(@arg1, initial_value, IV),
		get(@arg1, quantity_min, RMin), get(@arg1, quantity_max, RMax), 
		make_menu_resource(RN, RMin, RMax, IV, Menu),
		DialogType = resource
	;	true
	),
	
	( send(@arg1, has_attribute, q_type)
	->	get(@arg1, q_type, QT), get(@arg1, quantity, Q), get(@arg1, quantity_var, QV),
		get(@arg1, quantity_min, QMin), get(@arg1, quantity_max, QMax),
		get(@arg1, time_extent, TE), get(@arg1, condition, Cond),
		get(@arg1, from, From), get(@arg1, to, To),
		make_menu_resource_constraint(QT, Q, QV, QMin, QMax, TE, Cond, From, To, Menu),
		DialogType = resource_constraint
	;	true
	),
	
	( (send(@arg1, has_attribute, activity_name), \+ send(@arg1, has_attribute, process))
	->	get(@arg1, activity_name, Name), get(@arg1, multi_instance, MI),
		get(@arg1, insts, Insts), get(@arg1, min_insts, MinInsts), get(@arg1, max_insts, MaxInsts),
		get(@arg1, is_absent, IA), get(@arg1, condition, Cond),
		make_menu_activity(Canvas, Name, MI, Insts, MinInsts, MaxInsts, IA, Cond, Menu),
		DialogType = activity
	;	true),
	
	( (send(@arg1, has_attribute, activity_name), send(@arg1, has_attribute, process))
	->	get(@arg1, activity_name, Name), get(@arg1, multi_instance, MI),
		get(@arg1, insts, Insts), get(@arg1, min_insts, MinInsts), get(@arg1, max_insts, MaxInsts),
		get(@arg1, is_absent, IA), get(@arg1, condition, Cond),
		make_menu_composite_activity(Canvas, Name, MI, Insts, MinInsts, MaxInsts, IA, Cond, Menu),
		DialogType = activity
	;	true),
	
	( send(@arg1, has_attribute, node_quantity)
	->	get(@arg1, node_name, Node), get(@arg1, node_quantity, N),
		make_menu_product_related_constraint(Node, N, Menu),
		DialogType = none
	;	true
	),
	
	( send(@arg1, has_attribute, temporal_constraint)
	->	get(@arg1, temporal_constraint, TC), get(@arg1, activities, As),
		make_menu_temporal_constraint(TC, As, Canvas, Menu),
		DialogType = none
	;	true
	),
	
	( send(@arg1, has_attribute, pv_set)
	->	get(@arg1, pv_set, SetName), get(@arg1, process_variables, PVs),
		make_menu_process_variables(SetName, PVs, Canvas, Menu),
		DialogType = none
	;	true
	),
	
	( send(@arg1, has_attribute, cc_set)
	->	get(@arg1, cc_set, SetName), get(@arg1, coupling_constraints, CCs),
		make_menu_coupling_constraints(SetName, CCs, Canvas, Menu),
		DialogType = none
	;	true
	),
	
	send_list(D, append, Menu),
	
	send(D, append, button(apply, 
			and(message(D, apply), message(@prolog, check_min_max, DialogType, D)))),

	send(D, append, button(quit, message(A, quit))).
	
check_min_max(resource, D) :-
	get(D, member, minimum_quantity, IntMin),
	get(D, member, maximum_quantity, IntMax),
	get(D, member, initial_value, IntIV),
	get(IntMin, selection, Min),
	get(IntMax, selection, Max),
	get(IntIV, selection, IV),
	( Min =< Max
	->	( (Min =< IV, IV =< Max)
		->	true
		;	error_message('Initial value not in the quantity range!')
		)
	;	error_message('Minimum quantity must be less or equal to maximum quantity!')
	).
check_min_max(activity, D) :-
	get(D, member, multiple_instance, MultipleInstance),
	( get(MultipleInstance, selected, yes, @on)
	->	get(D, member, min_instances, IntMin),
		get(D, member, max_instances, IntMax),
		get(IntMin, selection, Min), get(IntMax, selection, Max),
		( Min >= Max
		->	error_message('Minimum number of instances must be less than maximum number of instances!')
		;	true
		)
	;	true
	).
check_min_max(resource_constraint, D) :-
	get(D, member, quantity_type, QT),
	( get(QT, selected, variable, @on)
	->	get(D, member, minimum_quantity, IntMin),
		get(D, member, maximum_quantity, IntMax),
		get(IntMin, selection, Min), get(IntMax, selection, Max),
		( Min >= Max
		->	error_message('Minimum quantity must be less than maximum quantity!')
		;	true
		)
	;	true
	).
check_min_max(none, _).

% Controls for changing atomic constraint attributes
make_menu_atomic_constraint(AC,C,[ACs,Cond]) :-
	new(ACs, menu(atomic_constraint, cycle,
		message(@receiver?frame, client_attribute, atomic_constraint, @arg1))),
	send_list(ACs, append,
		[	'before', 	'after', 
			'meets', 	'met_by',
			'overlaps',	'overlapped_by',
			'during',	'includes',
			'starts',	'started_by',
			'finishes',	'finished_by',
			'equals',
			'not_co_existent_with',
			'succeeded_by'
		]),
	send(ACs, selection, AC),
	(C \== @nil -> CurrentCond = C ; CurrentCond = ''),
	new(Cond, text_item(condition, CurrentCond,
		message(@receiver?frame, client_attribute, condition, @arg1))).
	
% Controls for changing resource attributes
make_menu_resource(RN, RMin, RMax, IV, [Name, RMinVal, RMaxVal, InitVal]) :-
	new(Name, text_item(resource_name, RN,
		message(@receiver?frame, client_attribute, resource_name, @arg1))),
	new(RMinVal, int_item(minimum_quantity, RMin,
		message(@receiver?frame, client_attribute, quantity_min, @arg1))),
	new(RMaxVal, int_item(maximum_quantity, RMax,
		message(@receiver?frame, client_attribute, quantity_max, @arg1))),
	new(InitVal, int_item(initial_value, IV,
		message(@receiver?frame, client_attribute, initial_value, @arg1))).
		
% Controls for changing resource constraint attributes
make_menu_resource_constraint(QT, Q, QV, QMin, QMax, TE, C, From, To, Menu) :- 
	( send(From, instance_of, draw_activity)
	->	Resource = To
	;	Resource = From
	),
	get(Resource, quantity_min, RMin),
	get(Resource, quantity_max, RMax),

	new(QType, menu(quantity_type)),
	
	new(IntVal, int_item(quantity, Q,
		message(@receiver?frame, client_attribute, quantity, @arg1), RMin - RMax, RMax)),
	new(VarVal, text_item(quantity_variable, QV,
		message(@receiver?frame, client_attribute, quantity_var, @arg1))),
	new(VarMin, int_item(minimum_quantity, QMin,
		message(@receiver?frame, client_attribute, quantity_min, @arg1), RMin - RMax, RMax)),
	new(VarMax, int_item(maximum_quantity, QMax,
		message(@receiver?frame, client_attribute, quantity_max, @arg1), RMin - RMax, RMax)),
		
	new(Constant, menu_item(constant,
		and(
			message(@prolog, editable_rc_attributes, QType, IntVal, VarVal, VarMin, VarMax),	
			message(@receiver?frame, client_attribute, q_type, constant)
			))),
	new(Variable, menu_item(variable,
		and(
			message(@prolog, editable_rc_attributes, QType, IntVal, VarVal, VarMin, VarMax),
			message(@receiver?frame, client_attribute, q_type, variable)
			))),
	send_list(QType, append, [Constant, Variable]),
	
	( QT = constant
	->	send(VarMin, editable, @off),
		send(VarMax, editable, @off),
		send(VarVal, editable, @off),
		send(IntVal, editable, @on),
		send(QType, selected, constant, @on)
	;	send(VarMin, editable, @on),
		send(VarMax, editable, @on),
		send(VarVal, editable, @on),
		send(IntVal, editable, @off),
		send(QType, selected, variable, @on)
	),
		
	new(TimeExt, menu(time_extent, cycle)),
	send_list(TimeExt, append, ['FromStartToEnd', 'AfterStart', 'AfterEnd',
										 'BeforeStart', 'BeforeEnd', 'Always']),
	send(TimeExt, selection, TE),
	(C \== @nil -> CurrentCond = C ; CurrentCond = ''),
	new(Cond, text_item(condition, CurrentCond,
		message(@receiver?frame, client_attribute, condition, @arg1))),
	Menu = [QType, IntVal, VarVal, VarMin, VarMax, TimeExt, Cond].
	
editable_rc_attributes(QType, Q, QV, QMin, QMax) :-
	(get(QType, selected, constant, @on)
	->	send(QMin, editable, @off),
		send(QMax, editable, @off),
		send(QV, editable, @off),
		send(Q, editable, @on)
	;	send(QMin, editable, @on),
		send(QMax, editable, @on),
		send(QV, editable, @on),
		send(Q, editable, @off)
	).
	
% Controls for changing activity attributes
make_menu_activity(Canvas, Name, MI, Insts, MinInsts, MaxInsts, IA, Cond, Menu) :-
	new(TextName, text_item(activity_name, Name,
			message(@receiver?frame, client_attribute, activity_name, @arg1))),
			
	( (Cond \== @nil, Cond \== 'none') -> CurrentCond = Cond ; CurrentCond = ''),
	new(TextCond, text_item(condition, CurrentCond, 
			message(@receiver?frame, client_attribute, condition, @arg1))),
			
	new(TextInsts, text_item(instance_number_variable, Insts,
			message(@receiver?frame, client_attribute, insts, @arg1))),
	new(IntMin, int_item(min_instances, MinInsts,
			message(@receiver?frame, client_attribute, min_insts, @arg1))),
	new(IntMax, int_item(max_instances, MaxInsts,
			message(@receiver?frame, client_attribute, max_insts, @arg1))),		
			
	new(Multi, menu(multiple_instance)),
	new(MultiYes, menu_item(yes, 
			and(
				message(@receiver?frame, client_attribute, multi_instance, @on),
				message(@prolog, editable_insts_attributes, TextInsts, IntMin, IntMax, @on)
				))),
	new(MultiNo, menu_item(no, 
			and(
				message(@receiver?frame, client_attribute, multi_instance, @off),
				message(@prolog, editable_insts_attributes, TextInsts, IntMin, IntMax, @off)
				))),
	send_list(Multi, append, [MultiYes, MultiNo]),
	
	( MI == @off
	->	send(MultiYes, selected, @off), send(MultiNo, selected, @on),
		send(TextInsts, editable, @off), send(IntMin, editable, @off), send(IntMax, editable, @off)
	;	send(MultiYes, selected, @on), send(MultiNo, selected, @off),
		send(TextInsts, editable, @on), send(IntMin, editable, @on), send(IntMax, editable, @on)
	),
			
	new(Absent, menu('Unary constraint')),
	new(IsAbsent, menu_item(is_absent,
			and(message(@receiver?frame, client_attribute, is_absent, @on),
			    message(@receiver?frame, client_attribute, condition, ''),
			    message(@prolog, editable_condition, TextCond, @on)
			))),
	new(MustBe, menu_item(must_be_executed,
			and(message(@receiver?frame, client_attribute, is_absent, @off),
			    message(@receiver?frame, client_attribute, condition, ''),
			    message(@prolog, editable_condition, TextCond, @on)
			))),
	new(None, menu_item(none,
			and(message(@receiver?frame, client_attribute, condition, 'none'),
				 message(@prolog, editable_condition, TextCond, @off)
			))),
	send_list(Absent, append, [IsAbsent, MustBe, None]),
	
	( Cond == 'none'
	-> send(IsAbsent, selected, @off), send(MustBe, selected, @off), send(None, selected, @on), send(TextCond, editable, @off)
	;	( IA == @off
		->	send(IsAbsent, selected, @off), send(MustBe, selected, @on), send(None, selected, @off), send(TextCond, editable, @on)
		;	send(IsAbsent, selected, @on), send(MustBe, selected, @off), send(None, selected, @off), send(TextCond, editable, @on)
		)
	),
	
	make_duration_constraints(Canvas, Constraints),
	append([TextName, Multi, TextInsts, IntMin, IntMax, Absent, TextCond], Constraints, Menu).
	
editable_condition(TextCond, C) :- send(TextCond, editable, C).

editable_insts_attributes(TextInsts, IntMin, IntMax, MI) :-
	( MI == @off
	->	send(TextInsts, editable, @off), send(IntMin, editable, @off), send(IntMax, editable, @off)
	;	send(TextInsts, editable, @on), send(IntMin, editable, @on), send(IntMax, editable, @on)
	).
	
make_duration_constraints(Canvas,Constraint) :-
	get(@arg1, duration_constraints, Constraints),
	chain_list(Constraints, ListConstraints),
	dict_constraints(ListConstraints, DictConstraints, []),
	new(ConstraintLB, list_browser(@default, 90)),
	send(ConstraintLB, multiple_selection, @off),
	send_list(ConstraintLB, append, DictConstraints),
	new(AddConstraint, button(add_constraint, 
		message(@prolog, add_constraint, ConstraintLB, Constraints, Canvas))),
	new(DelConstraint, button(delete_constraint,
		message(@prolog, delete_constraint, ConstraintLB, Constraints, Canvas))),
	new(ModConstraint, button(modify_constraint,
		message(@prolog, modify_constraint, ConstraintLB, Constraints, Canvas))),
	Constraint = [ConstraintLB, AddConstraint, DelConstraint, ModConstraint].
	
% Controls for changing composite activity attributes
make_menu_composite_activity(Canvas, Name, MI, Insts, MinInsts, MaxInsts, IA, Cond, Menu) :-
	make_menu_activity(Canvas, Name, MI, Insts, MinInsts, MaxInsts, IA, Cond, MenuAct),
	get(Canvas?selection, head, Activity),
	new(Process, button(process, 
			message(@prolog, menu_sub_process, Activity, Canvas))),	% Controls for sub-process creation/modification
	Menu = [MenuAct, Process].
	
menu_sub_process(Activity, Canvas) :-
	new(Modeler, sub_process_modeler(Activity, Canvas)),
	send(Modeler, open, normalise := @on).
	
	
% Controls for changing product related constraint attributes
make_menu_product_related_constraint(Node, N, Menu) :-
	new(TextName, text_item(node_name, Node,
			message(@receiver?frame, client_attribute, node_name, @arg1))),
	new(NodeN, int_item(quantity, N,
			message(@receiver?frame, client_attribute, node_quantity, @arg1))),
	Menu = [TextName, NodeN].
	
% Controls for changing temporal constraint attributes
make_menu_temporal_constraint(TC, As, Canvas, Menu) :-
	new(TextConstraint, text_item(temporal_constraint, TC,
			message(@receiver?frame, client_attribute, temporal_constraint, @arg1))),
	new(Activities, list_browser(@default, 50)),
	send(Activities, multiple_selection, @off),
	chain_list(As, ListAs),
	send_list(Activities, append, ListAs),
	new(AddAct, button(add_activity, 
			message(@prolog, add_activity, Activities, As, Canvas))),
	Menu = [TextConstraint, Activities, AddAct].
	
add_activity(Activities, As, Canvas) :-
	new(Frame, frame('Add activity')),
	send(Frame, append, new(D, dialog)),
	
	send(D, append, text_item(activity_name, '', 
			message(@prolog, add_a, As, @arg1, Canvas) 
		)),
	
	send(D, append, button(add, 
		and(
			message(D, apply),
			message(@prolog, update_activity_list, Activities, As)
			))),
	
	send(D, append, button(quit, message(Frame, destroy))),
	
	send(Frame, open).
	
add_a(As, A, Canvas) :-
	send(As, append, A),
	send(Canvas, slot, modified, @on).
	
update_activity_list(Activities, As) :-
	send(Activities, clear),
	chain_list(As, ListAs),
	send_list(Activities, append, ListAs).
	

% Controls for changing process variable set attributes
make_menu_process_variables(SetName, PVs, Canvas, Menu) :-
	new(TextName, text_item(set_name, SetName,
			message(@receiver?frame, client_attribute, pv_set, @arg1))),
	new(Variables, list_browser(@default, 50)),
	send(Variables, multiple_selection, @off),
	chain_list(PVs, ListPVs),
	text_vars(ListPVs, ListTextVars, []),
	send_list(Variables, append, ListTextVars),
	new(Add, button(add,
			message(@prolog, add_variable, Variables, PVs, Canvas))),
	new(Modify, button(modify,
			message(@prolog, modify_variable, Variables, PVs, Canvas))),
	new(Delete, button(delete,
			message(@prolog, delete_variable, Variables, PVs, Canvas))),
	Menu = [TextName, Variables, Add, Modify, Delete].
	

% Controls for changing coupling constraint set attributes
make_menu_coupling_constraints(SetName, Constraints, Canvas, Menu) :-
	new(TextName, text_item(set_name, SetName,
			message(@receiver?frame, client_attribute, cc_set, @arg1))),
	chain_list(Constraints, ListConstraints),
	dict_constraints(ListConstraints, DictConstraints, []),
	new(ConstraintLB, list_browser(@default, 70)),
	send(ConstraintLB, multiple_selection, @off),
	send_list(ConstraintLB, append, DictConstraints),
	new(AddConstraint, button(add_constraint, 
			message(@prolog, add_constraint, ConstraintLB, Constraints, Canvas))),
	new(DelConstraint, button(delete_constraint,
			message(@prolog, delete_constraint, ConstraintLB, Constraints, Canvas))),
	new(ModConstraint, button(modify_constraint,
			message(@prolog, modify_constraint, ConstraintLB, Constraints, Canvas))),
	Menu = [TextName,ConstraintLB, AddConstraint, DelConstraint, ModConstraint].

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Quit the dialog
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
quit(A, ShowOff:[bool]) :->
	(   ShowOff == @on
	->  send(A, show, @off)
	;   send(A?editor, attribute_editor, @nil),
	    send(A, free)
	).
	
		/********************************
		*     CLIENT COMMUNICATION	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->fill_items fills and  (de)activates all  dialog items.  The argument
is a chain of shapes (normally the <-selection of the canvas).  If one
of the elements of the selection  has the specified attribute, it will
be activated and the ->selection of the menu will be set accordingly.

If more than one object   in the  selection  has some  attribute,  the
->selection  of the  item  will  be the attribute  value of  the first
object in the chain that is has the attibute.  This is a rather simple
way of handling this case, but what else can we do?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

fill_items(A, Client:chain) :->
	"Fill the dialog items from chain of shapes"::
	get(A, member, dialog, Dialog),
	(   attribute(Label, Selector),
	    get(Dialog, member, Label, Menu),
	    (   get(Client, find,
		    and(message(@arg1, has_send_method, has_attribute),
			message(@arg1, has_attribute, Selector)),
		    Proto),
		get(Proto, draw_attribute, Selector, Value)
	    ->  send(Menu, active, @on),
		set_selection(Menu, Value)
	    ;   send(Menu, active, @off)
	    ),
	    fail
	;   true
	).

block(A) :->
	"<-blocked++"::
	get(A, blocked, B0),
	B1 is B0 + 1,
	send(A, slot, blocked, B1).

unblock(A) :->
	"<-blocked--"::
	get(A, blocked, B0),
	B1 is B0 - 1,
	send(A, slot, blocked, B1).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the chain of shapes for which we are editing the attributes.  Note
that if the window is not shown, we won't update the contents.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

client(A, Client:chain*) :->
	"Set the graphical I'm editing"::
	get(A, member, dialog, Dialog),
	(   get(A, blocked, B), B == 0
	->  (    Client == @nil
	    ->   send(Dialog?graphicals, for_some,
		      message(@arg1, active, @off))
	    ;    send(A, fill_items, Client)
	    )
	;   true
	),
	send(A, slot, client, Client).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the value of an attribute for the clients.  The value is set for
each shape that accepts ->has_attribute.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

client_attribute(A, Selector:name, Val:any) :->
	"Set attribute of client object"::
	(   get(A, client, Chain),
	    Chain \== @nil,
	    get(Chain, head, Head)
	->  send(A, block),
	    get(Head, window, Window),
	    send(Window, open_undo_group),
	    send(A?client, for_some,
		 if(message(@arg1, has_attribute, Selector),
		    message(@arg1, draw_attribute, Selector, Val))),
	    send(Window, close_undo_group),
	    send(A, unblock)
	;   true
	).

:- pce_end_class.