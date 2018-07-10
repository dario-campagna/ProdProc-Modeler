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

:- module(save_model, [save_model/3]).

:- use_module(library(pce)).
:- use_module(shapes).
:- use_module(library(assoc)).

:- set_prolog_flag(double_quotes,string).

save_model(PrpFile, ProductGraphicals, ProcessGraphicals) :-
	pl_model_name(PrpFile, PlModelFile),
	open(PlModelFile,write,StreamPlModel),
	set_output(StreamPlModel),
	write_heading(PrpFile),
	chain_list(ProductGraphicals, ProductGrsList),
	chain_list(ProcessGraphicals, ProcessGrsList),
	reset_gensym('type_'),
	% Product
	product_predicates_arguments( ProductGrsList, 
											ProductTypes, t, 
											Nodes, [], 
											Edges, [], 
											ModelConstraints, []),									
	% Process
	process_predicates_arguments(	ProcessGrsList, main,
										  	TypesAssoc, ProductTypes,
										  	ProcessVariables, [],
										  	Activities, [],
										  	Resources, [],
										  	ResourceConstraints, [],
										  	TemporalConstraints, [],
										  	ProducesForConstraints, [],
										  	CouplingConstraints, []),
	% Write predicates
	assoc_to_list(TypesAssoc, Types),
	write_product_predicates(Types, Nodes, Edges, ModelConstraints), nl, nl,
	write_process_predicates(ProcessVariables, Activities, Resources, ResourceConstraints, TemporalConstraints,
									 ProducesForConstraints, CouplingConstraints),
	close(StreamPlModel).

% Create prolog file name for the model
pl_model_name(PrpFile, PlModelFile) :-
	get(PrpFile?name, value, PrpValue),
	sub_atom(PrpValue, 0, _, 4, ModelFile),
	atomic_concat(ModelFile, '.pl', PlModelFile).
	
% Write prolog model heading
write_heading(PrpFile) :-
	get(PrpFile?base_name, value, BaseName),
	sub_atom(BaseName, 0, _, 4, ModelName),
	atomic_list_concat([':- module(',ModelName,','], ModuleDecl),
	writeln(ModuleDecl),
	writeln('\t[type/3,'),
	writeln('\t node/3,'),
	writeln('\t edge/4,'),
	writeln('\t edge/5,'),
	%writeln('\t globalConstraint/1,'),
	writeln('\t nodeModelConstraint/1,'),
	writeln('\t aggConstraint/4,'),
	writeln('\t allDifferentValues/1,'),
	writeln('\t cardModelConstraint/1,'), 
	writeln('\t process_variable/2,'),
	writeln('\t activity/5,'),
	writeln('\t res/3,'),
	writeln('\t resource_constraint/5,'),
	writeln('\t temporal_constraint/1,'),
	writeln('\t coupling_constraint/1,'),
	writeln('\t produces_for/4]).'),
	nl,	
	%writeln(':- [dynamic_predicates].'),
	atomic_list_concat([':- dynamic ',ModelName,':edge/4.'], DynamicHP4),
	atomic_list_concat([':- dynamic ',ModelName,':edge/5.'], DynamicHP5),
	%atomic_list_concat([':- dynamic ',ModelName,':globalConstraint/1.'], DynamicGC),
	atomic_list_concat([':- dynamic ',ModelName,':nodeModelConstraint/1.'], DynamicGC),
	atomic_list_concat([':- dynamic ',ModelName,':aggConstraint/4.'], DynamicAG),
	atomic_list_concat([':- dynamic ',ModelName,':allDifferentValues/1.'], DynamicAD),
	atomic_list_concat([':- dynamic ',ModelName,':cardModelConstraint/1.'], DynamicCM),
	atomic_list_concat([':- dynamic ',ModelName,':process_variable/2.'], DynamicPV),
	atomic_list_concat([':- dynamic ',ModelName,':res/3.'], DynamicRes),
	atomic_list_concat([':- dynamic ',ModelName,':resource_constraint/5.'], DynamicRC),
	atomic_list_concat([':- dynamic ',ModelName,':temporal_constraint/1.'], DynamicTC),
	atomic_list_concat([':- dynamic ',ModelName,':coupling_constraint/1.'], DynamicCC),
	atomic_list_concat([':- dynamic ',ModelName,':produces_for/4.'], DynamicPF),
	atomic_list_concat([':- dynamic ',ModelName,':type/3.'], DynamicType), 
	writeln(DynamicHP4),
	writeln(DynamicHP5),
	writeln(DynamicGC),
	writeln(DynamicAG),
	writeln(DynamicAD),
	writeln(DynamicCM),
	writeln(DynamicPV),
	writeln(DynamicRes),
	writeln(DynamicRC),
	writeln(DynamicTC),
	writeln(DynamicCC),
	writeln(DynamicPF),
	writeln(DynamicType), nl, nl.

% Create predicate arguments for types, nodes, edges and model constraints
product_predicates_arguments([], TypesAssoc, TypesAssoc, Nodes, Nodes, Edges, Edges, ModelConstraints, ModelConstraints).% :-
%	assoc_to_list(TypesAssoc, Types).
product_predicates_arguments([Gr|Grs], Types, TypesAssoc, Nodes, NTail, Edges, ETail, ModelConstraints, MCTail2) :-
	\+ get(Gr, class_name, draw_node), \+ get(Gr, class_name, draw_edge), \+ get(Gr, class_name, draw_mc),
	product_predicates_arguments(Grs, Types, TypesAssoc, Nodes, NTail, Edges, ETail, ModelConstraints, MCTail2).
product_predicates_arguments([Gr|Grs], Types, TypesAssoc, Nodes, NTail2, Edges, ETail2, ModelConstraints, MCTail2) :-
	( send(Gr, has_attribute, node_name)
	-> node_pred(Gr, TypesAssoc, NewTypesAssoc, N),
		Nodes = [N|NTail1], Edges = ETail1, ModelConstraints = MCTail1
	; ( send(Gr, has_attribute, edge_label)
	  -> edge_pred(Gr, TypesAssoc, NewTypesAssoc, E),
	  	  Nodes = NTail1, Edges = [E|ETail1], ModelConstraints = MCTail1
	  ;  model_constraint_preds(Gr, MCs),
	  	  Nodes = NTail1, Edges = ETail1, append(MCs, MCTail1, ModelConstraints),
	  	  NewTypesAssoc = TypesAssoc
	  )
	),
	product_predicates_arguments(Grs, Types, NewTypesAssoc, NTail1, NTail2, ETail1, ETail2, MCTail1, MCTail2).
	
%type_preds([], Types, Types).
%type_preds([TD-(TT-TN)|Ts], [T|Tail1], Tail2) :-
%	atomic_list_concat(['type(',TN,',',TT,',',TD,').'],T),
%	type_preds(Ts, Tail1, Tail2).

% Create data for node predicate and update type association list
node_pred(Gr, TypesAssoc, NewTypesAssoc, N) :-
	get(Gr, node_name, Name),
	get(Gr, node_variables, VarChain),
	chain_list(VarChain, Variables),
	get(Gr, node_constraints, ConstrChain),
	chain_list(ConstrChain, Constraints),
	var_type_couples(Variables, TypesAssoc, NewTypesAssoc, VarTypeList, []),
	N = Name-VarTypeList-Constraints.

var_type_couples([], TypesAssoc, TypesAssoc, VarTypeList, VarTypeList).
var_type_couples([V|Vs], TypesAssoc, NewTypesAssoc, [VarName-TypeName|Tail1], Tail2) :-
	get(V, var_name, VarName),
	get(V, var_type, Type),
	get(V, var_values, Values),
	( Type = integer, Values = @nil
	->	get(V, var_min_value, Min), get(V, var_max_value, Max),
		TT = int-Min-Max
	;	chain_list(Values, Vals),
		(Type = integer
		->	TT = int-Vals
		;	TT = string-Vals
		)
	),
	add_type(TT, TypesAssoc, NewTypesAssoc1, TypeName),
	var_type_couples(Vs, NewTypesAssoc1, NewTypesAssoc, Tail1, Tail2).

add_type(TT, TypesAssoc, NewTypesAssoc, TypeName) :-
	(get_assoc(TT, TypesAssoc, TypeName)
	->	NewTypesAssoc = TypesAssoc
	;	gensym('type_',TypeName),
		put_assoc(TT, TypesAssoc, TypeName, NewTypesAssoc)
	).
	
% Create data for edge predicate and update type association list
edge_pred(Gr, TypesAssoc, NewTypesAssoc, E) :-
	get(Gr, edge_label, Label),
	get(Gr?from, node_name, Parent),
	get(Gr?to, node_name, Child),
	get(Gr, edge_cardinality_type, CT),
	( CT = const_card
	->	get(Gr, edge_cardinality_value, CValue),
		E = Label-Parent-Child-CValue,
		NewTypesAssoc = TypesAssoc
	;	get(Gr, edge_cardinality_name, CName),
		get(Gr, edge_cardinality_min_value, CMin),
		get(Gr, edge_cardinality_max_value, CMax),
		get(Gr, edge_constraints, Constraints),
		chain_list(Constraints, ConstrList),
		add_type(int-CMin-CMax, TypesAssoc, NewTypesAssoc, TypeName),
		E = Label-Parent-Child-CName-TypeName-ConstrList
	).

% Create data for model constraint predicate
model_constraint_preds(Gr, ModelConstraints) :-
	get(Gr, model_constraints, MCs),
	chain_list(MCs, ModelConstraints).


% Create predicate arguments for process elements
process_predicates_arguments( [], _,
										Types, Types, 
										ProcessVariables, ProcessVariables,										
										Activities, Activities,
										Resources, Resources,
										ResourceConstraints, ResourceConstraints,
										TemporalConstraints, TemporalConstraints,
										ProducesForConstraints, ProducesForConstraints,
										CouplingConstraints, CouplingConstraints).
% Other graphical
process_predicates_arguments(	[G|ProcessGrsList], Proc,
										Types, AuxTypes,
										ProcessVariables, PVTail,
										Activities, ATail,
										Resources, RTail,
										ResourceConstraints, RCTail,
										TemporalConstraints, TCTail,
										ProducesForConstraints, PFTail,
										CouplingConstraints, CCTail) :-
	\+ send(G, instance_of, draw_activity),
	\+ send(G, instance_of, draw_comp_activity),
	\+ send(G, instance_of, draw_pv),
	\+ send(G, instance_of, draw_resource),
	\+ send(G, instance_of, draw_atomic_tc),
	\+ send(G, instance_of, draw_tc),
	\+ send(G, instance_of, draw_rc),
	\+ send(G, instance_of, draw_cc),
	\+ send(G, instance_of, draw_prc),
	process_predicates_arguments(ProcessGrsList, Proc,
											Types, AuxTypes,
											ProcessVariables, PVTail,
											Activities, ATail,
											Resources, RTail,
											ResourceConstraints, RCTail,
											TemporalConstraints, TCTail,
											ProducesForConstraints, PFTail,
											CouplingConstraints, CCTail).
% Process variables											
process_predicates_arguments(	[G|ProcessGrsList], Proc,
										Types, AuxTypes,
										ProcessVariables, PVTail2,
										Activities, ATail2,
										Resources, RTail2,
										ResourceConstraints, RCTail2,
										TemporalConstraints, TCTail2,
										ProducesForConstraints, PFTail2,
										CouplingConstraints, CCTail2) :-
	send(G, instance_of, draw_pv),
	pv_pred(G, AuxTypes, NewAuxTypes, PVs),
	append(PVs,PVTail1,ProcessVariables),
	process_predicates_arguments( ProcessGrsList, Proc,
											Types, NewAuxTypes,
											PVTail1, PVTail2,
											Activities, ATail2,
											Resources, RTail2,
											ResourceConstraints, RCTail2,
											TemporalConstraints, TCTail2,
											ProducesForConstraints, PFTail2,
											CouplingConstraints, CCTail2).
% Activity										
process_predicates_arguments(	[G|ProcessGrsList], Proc,
										Types, AuxTypes,
										ProcessVariables, PVTail2,
										Activities, ATail2,
										Resources, RTail2,
										ResourceConstraints, RCTail2,
										TemporalConstraints, TCTail2,
										ProducesForConstraints, PFTail2,
										CouplingConstraints, CCTail2) :-
	send(G, instance_of, draw_activity),
	activity_pred(G, Proc, AuxTypes, NewAuxTypes, A, TC),
	Activities = [A|ATail1],
	TemporalConstraints = [TC|TCTail1],
	process_predicates_arguments( ProcessGrsList, Proc,
											Types, NewAuxTypes,
											ProcessVariables, PVTail2,
											ATail1, ATail2,
											Resources, RTail2,
											ResourceConstraints, RCTail2,
											TCTail1, TCTail2,
											ProducesForConstraints, PFTail2,
											CouplingConstraints, CCTail2).
% Composite activity
process_predicates_arguments(	[G|ProcessGrsList], Proc,
										Types, AuxTypes,
										ProcessVariables, PVTail2,
										Activities, ATail2,
										Resources, RTail2,
										ResourceConstraints, RCTail2,
										TemporalConstraints, TCTail2,
										ProducesForConstraints, PFTail2,
										CouplingConstraints, CCTail2) :-
	send(G, instance_of, draw_comp_activity),
	comp_activity_pred(G, Proc, AuxTypes, NewAuxTypes, A, TC,
							 PVs, As, Rs, RCs, TCs, PRCs, CCs),
	append([A|As],ATail1,Activities),
	append([TC|TCs],TCTail1,TemporalConstraints),
	append(PVs, PVTail1, ProcessVariables),
	append(Rs, RTail1, Resources),
	append(RCs, RCTail1, ResourceConstraints),
	append(PRCs, PFTail1, ProducesForConstraints),
	append(CCs, CCTail1, CouplingConstraints),
	process_predicates_arguments( ProcessGrsList, Proc,
											Types, NewAuxTypes,
											PVTail1, PVTail2,
											ATail1, ATail2,
											RTail1, RTail2,
											RCTail1, RCTail2,
											TCTail1, TCTail2,
											PFTail1, PFTail2,
											CCTail1, CCTail2).
% Resource											
process_predicates_arguments(	[G|ProcessGrsList], Proc,
										Types, AuxTypes,
										ProcessVariables, PVTail2,
										Activities, ATail2,
										[R|RTail1], RTail2,
										ResourceConstraints, RCTail2,
										TemporalConstraints, TCTail2,
										ProducesForConstraints, PFTail2,
										CouplingConstraints, CCTail2) :-
	send(G, instance_of, draw_resource),
	resource_pred(G, AuxTypes, NewAuxTypes, R),
	process_predicates_arguments( ProcessGrsList, Proc,
											Types, NewAuxTypes,
											ProcessVariables, PVTail2,
											Activities, ATail2,
											RTail1, RTail2,
											ResourceConstraints, RCTail2,
											TemporalConstraints, TCTail2,
											ProducesForConstraints, PFTail2,
											CouplingConstraints, CCTail2).%,
	%( member(R, RTail1)
	%->	Resources = RTail1
	%;	Resources = [R|RTail1]
	%).
% Resource constraint											
process_predicates_arguments(	[G|ProcessGrsList], Proc,
										Types, AuxTypes,
										ProcessVariables, PVTail2,
										Activities, ATail2,
										Resources, RTail2,
										ResourceConstraints, RCTail2,
										TemporalConstraints, TCTail2,
										ProducesForConstraints, PFTail2,
										CouplingConstraints, CCTail2) :-
	send(G, instance_of, draw_rc),
	rc_pred(G, AuxTypes, NewAuxTypes, RC),
	ResourceConstraints = [RC|RCTail1],
	process_predicates_arguments( ProcessGrsList, Proc,
											Types, NewAuxTypes,
											ProcessVariables, PVTail2,
											Activities, ATail2,
											Resources, RTail2,
											RCTail1, RCTail2,
											TemporalConstraints, TCTail2,
											ProducesForConstraints, PFTail2,
											CouplingConstraints, CCTail2).											
% Temporal constraint											
process_predicates_arguments(	[G|ProcessGrsList], Proc,
										Types, AuxTypes,
										ProcessVariables, PVTail2,
										Activities, ATail2,
										Resources, RTail2,
										ResourceConstraints, RCTail2,
										TemporalConstraints, TCTail2,
										ProducesForConstraints, PFTail2,
										CouplingConstraints, CCTail2) :-
	( send(G, instance_of, draw_atomic_tc) ; send(G, instance_of, draw_tc)),
	tc_pred(G, TC),
	TemporalConstraints = [TC|TCTail1],
	process_predicates_arguments( ProcessGrsList, Proc,
											Types, AuxTypes,
											ProcessVariables, PVTail2,
											Activities, ATail2,
											Resources, RTail2,
											ResourceConstraints, RCTail2,
											TCTail1, TCTail2,
											ProducesForConstraints, PFTail2,
											CouplingConstraints, CCTail2).
% Produces for constraint											
process_predicates_arguments(	[G|ProcessGrsList], Proc,
										Types, AuxTypes,
										ProcessVariables, PVTail2,
										Activities, ATail2,
										Resources, RTail2,
										ResourceConstraints, RCTail2,
										TemporalConstraints, TCTail2,
										ProducesForConstraints, PFTail2,
										CouplingConstraints, CCTail2) :-
	send(G, instance_of, draw_prc),
	prc_pred(G, PRC),
	ProducesForConstraints = [PRC|PRCTail1],
	process_predicates_arguments( ProcessGrsList, Proc,
											Types, AuxTypes,
											ProcessVariables, PVTail2,
											Activities, ATail2,
											Resources, RTail2,
											ResourceConstraints, RCTail2,
											TemporalConstraints, TCTail2,
											PRCTail1, PFTail2,
											CouplingConstraints, CCTail2).
% Coupling constraints										
process_predicates_arguments(	[G|ProcessGrsList], Proc,
										Types, AuxTypes,
										ProcessVariables, PVTail2,
										Activities, ATail2,
										Resources, RTail2,
										ResourceConstraints, RCTail2,
										TemporalConstraints, TCTail2,
										ProducesForConstraints, PFTail2,
										CouplingConstraints, CCTail2) :-
	send(G, instance_of, draw_cc),
	cc_pred(G, CCs),
	append(CCs,CCTail1,CouplingConstraints),
	process_predicates_arguments( ProcessGrsList, Proc,
											Types, AuxTypes,
											ProcessVariables, PVTail2,
											Activities, ATail2,
											Resources, RTail2,
											ResourceConstraints, RCTail2,
											TemporalConstraints, TCTail2,
											ProducesForConstraints, PFTail2,
											CCTail1, CCTail2).

% Process variables
pv_pred(G, TypesAssoc, NewTypesAssoc, PVs) :-
	get(G, process_variables, VarChain),
	chain_list(VarChain, Variables),
	var_type_couples(Variables, TypesAssoc, NewTypesAssoc, PVs, []).

% Activity
activity_pred(G, Proc, TypesAssoc, NewTypesAssoc, A, TC) :-
	get(G, activity_name, Name),
	get(G, duration_constraints, DurationConstraints),
	chain_list(DurationConstraints, DCs),
	get(G, multi_instance, MI),
	( MI == @off
	->	NewTypesAssoc = TypesAssoc,
		InstsVar = nil, AType = atomic
	;	get(G, insts, Insts), get(G, min_insts, Min), get(G, max_insts, Max),
		add_type(int-Min-Max, TypesAssoc, NewTypesAssoc, Type),
		InstsVar = Insts-Type, AType = multi
	),
	get(G, is_absent, IS),
	get(G, condition, Cond),
	( IS == @off
	->	AC = must_be_executed
	; 	AC = is_absent
	),
	( Cond == @nil
	->	TC = AC-Name
	;	TC = AC-Name-Cond
	),
	A = Name-AType-Proc-InstsVar-DCs.

% Composite activity
comp_activity_pred(G, Proc, TypesAssoc, NewTypesAssoc, A, TC,
						 ProcessVariables, Activities, Resources, ResourceConstraints,
						 TemporalConstraints, ProducesForConstraints, CouplingConstraints) :-
	get(G, activity_name, Name),
	get(G, duration_constraints, DurationConstraints),
	chain_list(DurationConstraints, DCs),
	get(G, multi_instance, MI),
	( MI == @off
	->	NewTypesAssoc1 = TypesAssoc,
		InstsVar = nil, AType = comp
	;	get(G, insts, Insts), get(G, min_insts, Min), get(G, max_insts, Max),
		add_type(int-Min-Max, TypesAssoc, NewTypesAssoc1, Type),
		InstsVar = Insts-Type, AType = multicomp
	),
	get(G, is_absent, IS),
	get(G, condition, Cond),
	( IS == @off
	->	AC = must_be_executed
	; 	AC = is_absent
	),
	( Cond == @nil
	->	TC = AC-Name
	;	TC = AC-Name-Cond
	),
	A = Name-AType-Proc-InstsVar-DCs,
	get(G, process, Process),
	chain_list(Process, ProcessGrsList),
	process_predicates_arguments(	ProcessGrsList, 
											Name,
										  	NewTypesAssoc, NewTypesAssoc1,
										  	ProcessVariables, [],
										  	Activities, [],
										  	Resources, [],
										  	ResourceConstraints, [],
										  	TemporalConstraints, [],
										  	ProducesForConstraints, [],
										  	CouplingConstraints, []).

% Resource
resource_pred(G, TypesAssoc, NewTypesAssoc, R) :-
	get(G, resource_name, Name),
	get(G, quantity_min, Min), get(G, quantity_max, Max),
	get(G, initial_value, IV),
	add_type(int-Min-Max, TypesAssoc, NewTypesAssoc, Type),
	R = Name-Type-IV.

% Resource constraint
rc_pred(G, TypesAssoc, NewTypesAssoc, RC) :-
	get(G, from, From), get(G, to, To),
	( send(From, instance_of, draw_resource)
	->	get(From, resource_name, Resource)
	;	get(From, activity_name, Activity)
	),
	( send(To, instance_of, draw_resource)
	->	get(To, resource_name, Resource)
	;	get(To, activity_name, Activity)
	),
	( get(G, q_type, constant)
	->	get(G, quantity, Q), NewTypesAssoc = TypesAssoc
	;	get(G, quantity_var, QVar), get(G, quantity_min, QMin), get(G, quantity_max, QMax),
		add_type(int-QMin-QMax, TypesAssoc, NewTypesAssoc, Type),
		Q = QVar-Type
	),
	get(G, time_extent, TE),
	get(G, condition, Cond),
	( Cond == @nil
	-> Condition = true
	;	Condition = Cond
	),
	RC = Activity-Resource-Q-TE-Condition.
	
% Temporal constraint
tc_pred(G, TC) :-
	( send(G, instance_of, draw_tc)
	->	get(G, temporal_constraint, TC)
	;	get(G, atomic_constraint, AC), get(G, condition, Cond),
		get(G, from, From), get(G, to, To),
		get(From, activity_name, A), get(To, activity_name, B),
		( Cond == @nil
		->	TC = AC-A-B
		;	TC = AC-A-B-Cond
		)
	).

% Product related constraint
prc_pred(G, PRC) :-
	get(G, node_name, Node), get(G, node_quantity, N),
	get(G, from, From), get(G, to, To),
	get(From, activity_name, A), get(To, activity_name, B),
	PRC = A-N-Node-B.

% Coupling constraints
cc_pred(G, CCs) :-
	get(G, coupling_constraints, Chain),
	chain_list(Chain, CCs).


% Write predicates for types, nodes, edges and model constrints in prolog model file
write_product_predicates(Types, Nodes, Edges, ModelConstraints) :-
	write_types(Types), nl,
	write_nodes(Nodes), nl,
	write_edges(Edges), nl,
	write_mcs(ModelConstraints).
	
write_types(Types) :- forall(member(T, Types), write_type(T)).

write_type(int-Min-Max-TypeName) :- 
	write('type('), write(TypeName), write(',int,('),
	write(Min), write(','), write(Max), write(')).'), nl.
write_type(int-Values-TypeName) :-
	atomic_list_concat(Values, ',', Vals),
	write('type('), write(TypeName), write(',int,['),
	write(Vals), write(']).'), nl.
write_type(string-Values-TypeName) :-
	atomic_list_concat(Values, ',', Vals),
	write('type('), write(TypeName), write(',string,['),
	write(Vals), write(']).'), nl.

write_nodes(Nodes) :- forall(member(N, Nodes), write_node(N)).

write_node(NodeName-Variables-Constraints) :-
	write("node('"), write(NodeName), write("',["),
	write_variables(Variables),
	write('],['),
	write_constraints(Constraints),
	write(']).'), nl.

write_variables([V]) :- write_var(V).
write_variables([V|Vs]) :- write_var(V), write(','), write_variables(Vs).
write_variables([]).

write_var(V-T) :- write("('"), write(V), write("',"), write(T), write(')').

write_constraints([C]) :- get(C,constraint,Constr), write(Constr).
write_constraints([C|Cs]) :- get(C,constraint,Constr), write(Constr), write(','), write_constraints(Cs).
write_constraints([]).

write_edges(Edges) :- forall(member(E,Edges), write_edge(E)).


write_edge(Label-Parent-Child-CName-TypeName-Constraints) :-
	write("edge('"), write(Label), write("','"),
	write(Parent), write("','"), write(Child), write("',('"),
	write(CName), write("',"), write(TypeName), write('),['),
	write_constraints(Constraints), write(']).'), nl.
write_edge(Label-Parent-Child-CVal) :-
	write("edge('"), write(Label), write("','"),
	write(Parent), write("','"), write(Child), write("',"),
	write(CVal), write(',[]).'), nl.
	
write_mcs(Constraints) :- forall(member(C,Constraints), write_mc(C)).

write_mc(C) :- %write('globalConstraint('), 
					write_constraints([C]), write('.'), nl.


% Write predicates for process elements in prolog model file
write_process_predicates(ProcessVariables, Activities, Resources, ResourceConstraints, 
								 TemporalConstraints, ProducesForConstraints, CouplingConstraints) :-
	write_process_variables(ProcessVariables), nl,
	write_activities(Activities), nl,
	write_resources(Resources), nl,
	write_resource_constraints(ResourceConstraints), nl,
	write_temporal_constraints(TemporalConstraints), nl,
	write_produces_for_constraints(ProducesForConstraints), nl,
	write_coupling_constraints(CouplingConstraints).

% Process variables
write_process_variables(ProcessVariables) :-	
	forall(member(PV, ProcessVariables), write_pv(PV)).

write_pv(Name-Type) :- 
	write("process_variable('"), write(Name), write("',"), write(Type), write(').'), nl.

% Activities
write_activities(Activities) :-
	forall(member(A, Activities), write_activity(A)).
	
write_activity(Name-AType-Proc-Insts-DCs) :-
	write("activity('"), write(Name), write("',"), write(AType), write(",'"),
	write(Proc), write("',"),
	( Insts = InstsVar-Type
	-> write("('"), write(InstsVar), write("',"), write(Type), write(')')
	;	write(Insts)
	),
	write(',['), write_constraints(DCs), write(']).'), nl.

% Resources
write_resources(Resources) :-
	remove_duplicates(Resources, Rs),
	forall(member(R, Rs), write_resource(R)).
	
remove_duplicates(List, NewList) :- remove_duplicates(List, NewList, []).
remove_duplicates([], NewList, NewList).
remove_duplicates([L|Ls], [L|Tail1], Tail2) :-
	delete(Ls, L, NewLs),
	remove_duplicates(NewLs, Tail1, Tail2).
	
write_resource(Name-Type-IV) :-
	write("res('"), write(Name), write("',"), write(Type), write(','), write(IV), write(').'), nl.

% Resource constraints
write_resource_constraints(ResourceConstraints) :-
	forall(member(RC, ResourceConstraints), write_rc(RC)).

write_rc(A-R-Q-TE-Cond) :-
	write("resource_constraint('"), write(A), write("','"), write(R), write("',"),
	( Q = Var-Type
	->	write("('"), write(Var), write("',"), write(Type), write("),'")
	;	write(Q), write(",'")
	),
	write(TE), write("',"), write(Cond), write(').'), nl.

% Temporal constraints
write_temporal_constraints(TemporalConstraints) :-
	forall(member(TC, TemporalConstraints), write_tc(TC,TemporalConstraints)).

write_tc(AC-A-B-Cond,_) :-
	write('temporal_constraint('), write('cond('), write(Cond), write(','),
	write(AC), write("('"),
	write(A), write("','"), write(B), write("')))."), nl.
write_tc(AC-A-Cond, _TCs) :-
	( AC = must_be_executed ; AC = is_absent),
	( Cond = 'none'
	->	true
	;	write('temporal_constraint('), write('cond('), write(Cond), write(','),
		write(AC), write("('"), write(A), write("')))."), nl
	).
write_tc(AC-A-B, _) :-
	write('temporal_constraint('), write(AC), write("('"),
	write(A), write("','"), write(B), write("'))."), nl.
write_tc(AC-A, _TCs) :-
	( AC = must_be_executed ; AC = is_absent),
	write('temporal_constraint('), write(AC), write("('"), write(A), write("'))."), nl.
write_tc(TC, _) :- 
	write('temporal_constraint('), write(TC), write(').'), nl.

% Product related constraints
write_produces_for_constraints(ProducesForConstraints) :-
	forall(member(PRC, ProducesForConstraints), write_prc(PRC)).

write_prc(A-N-Node-B) :-
	write("produces_for('"), write(A), write("',"), write(N), write(",'"),
	write(Node), write("','"), write(B), write("')."), nl.

% Coupling constraints
write_coupling_constraints(CouplingConstraints) :-
	forall(member(CC, CouplingConstraints), write_cc(CC)).
	
write_cc(CC) :- 
	get(CC, constraint, C),
	write('coupling_constraint('), write(C), write(').'), nl.