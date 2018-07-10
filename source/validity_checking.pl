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

:- module(validity_checking,
			 [generate_instance/6,
			  involved_activities/2,
			  check_condition_tf/4]).

:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(record)).
:- use_module(library(pce)).

:- use_module(syntax_checking).
:- use_module(minimum_maximum).
:- use_module(check_tc_condition).
:- use_module(relationship_a_b).


% Record for a product model tree
:- record productModel(pmTypes,pmNodes,pmHasPartRelations,pmGlobalConstraints).
% Recodr for product model global constraints
:- record pmGlobalConstraints(pmGlobal,pmAggregate,pmAllDifferentValues,pmCardConstraints).
% Record for an instance tree
:- record iTree(iNodes,iHasPartRelations).
% Record for a CSP
:- record csp(cspVariables,cspConstraints).


% generate_instance/3 generates an instance of a given product model with a number of nodes in [MinNodes,MaxNodes]
generate_instance(ProductModel, MinNodes, MaxNodes, MaxTime, TimeLimit, Browser) :-
	use_module(ProductModel),
	
	% Create a browser where to show messages and generation computation/results
	%new(Browser, browser('Instance generation')),
	%send(Browser, open),
	
	send(Browser, append, '\nInstantiating tree'),

	% Create a bijection B: string -> int
	createBijection(B),
	% We first create a product instance
	
	% Create a record for the product model tree
	createPM(B,PM),
	% Instantiate the instance tree considering only constant cardinality has-part relations
	instantiateITree(PM,ITree,LastNodeInstance),
	% Check if the number of nodes in the instantiated tree is less than MinNodes, if it is the case report a message
	checkMinNodes(ITree,MinNodes,Browser),
	% Create the CSP corresponding to the instantiated instance tree
	createCSP(PM,ITree,B,CSP),
	%nl, writeln('Instantiated tree'),
	send(Browser, append, '\nInstantiated tree'),
	printITree(ITree, Browser),
	% Check if there are relations that can be used to add new nodes to the instance tree
	( csp_cspVariables(CSP,_-t)
	  ->  labelNodeVariables(CSP,ITree,[],[],PM,B,MinNodes,MaxNodes,Browser)
	  ;	(% Propagate the constraints of the CSP
	 		propagate_constraints_no_bt(CSP)
	 		->	(% Instantiate the node pool
				instantiateNodePool(ITree,PM,NodePool), !,
				%nl, write('Instantiated node pool: '), writeln(NodePool),
				% Create a product and process instance
				csp_cspVariables(CSP,_-CardVariables),
				% Find all temporal constraints
 			   findall(TC, temporal_constraint(TC), ModelTemporalConstraints),
		   	% Compute process hierarchy
		      process_hierarchy(ProcHierarchy),
				generate_product_process(ITree, CardVariables, NodePool, [], LastNodeInstance, PM, B, MinNodes, MaxNodes, 
												 ProcHierarchy, ModelTemporalConstraints, MaxTime, TimeLimit, [], norm, Browser)
				)
	 		;	send(Browser, append, '\nThe search algorithm is not able to obtain an instance tree whose number of nodes is in [minNodes,maxNodes], and that can be produced within the given maximum time.')
			)
	),
	unload_file(ProductModel).

generate_product_process(ITree, CardVariables, NodePool, LastAction, LastNodeInstance, PM, B, MinNodes, MaxNodes, 
								 ProcHierarchy, ModelTemporalConstraints, MaxTime, TimeLimit, States, BFlag, Browser) :-
	% Add new nodes to the instantiated tree in order to obtain an instance with a number of nodes in [MinNodes,MaxNodes]
	(addNodes(ITree,CardVariables,NodePool,LastAction,LastNodeInstance,PM,B,MinNodes,MaxNodes,States,BFlag,Browser,
			   FinalTree,FinalCSP,LastStates,LastCurrentLastAction)
	->% At this point we have a produt instance, we can generate the corresponding production process
		send(Browser, append, '\nSearching for a process.\n'),
		( generate_process_instance(FinalTree, FinalCSP, PM, B, ProcHierarchy, ModelTemporalConstraints, MaxTime, TimeLimit, [],
		 									 Browser)
		->	true
		;	restoreState(LastStates,LastCurrentLastAction,NewStates,RITree-RLastNodeInstance-RCardVariables-RNodePool-RLastAction),
			(NewStates \= []
			->	generate_product_process(RITree, RCardVariables, RNodePool, RLastAction, RLastNodeInstance, PM, B, MinNodes, 
				MaxNodes, ProcHierarchy, ModelTemporalConstraints, MaxTime, TimeLimit, NewStates, back,Browser)
			; false
			)
		)
	; send(Browser, append, '\nThe search algorithm is not able to obtain an instance tree whose number of nodes is in [minNodes,maxNodes], and that can be produced within the given maximum time.')
	).

% createBijection/1 creates a bijection B: string -> int
createBijection(B) :-
	findall(StringSet,type(_,string,StringSet),StringSets),
	createBijection(StringSets,0,t,B).

createBijection([],_,B,B).
createBijection([StringSet|Sets],LastInt,AccB,B) :-
	addConversion(StringSet,LastInt,NewInt,AccB,NewAccB),
	createBijection(Sets,NewInt,NewAccB,B).
	
addConversion([],LastInt,LastInt,B,B).
addConversion([S|Strings],LastInt,Int,CurrentB,B) :-
	(get_assoc(S,CurrentB,_)
	 ->	addConversion(Strings,LastInt,Int,CurrentB,B)
	 ;		NewInt is LastInt + 1,
	 		put_assoc(S,CurrentB,NewInt,NewB),
	 		addConversion(Strings,NewInt,Int,NewB,B)
	).
	
% --------------------------------------------------------------------------- %	
%                Predicates for product instance generation						   %
% --------------------------------------------------------------------------- %

% createPM/2 creates a record of type productModel for the product model tree, the argument of the record are: an association list for types, 
% an association list for nodes and an association list for has-part relations
createPM(B,PM) :- 
	% Create an association list for product model types
	typesAssocList(B,TypesAssocList),
	% Create an association list for product model nodes
	nodesAssocList(NodesAssocList),
	% Create an association list for product model has-part relations
	hasPartRelationsAssocList(HasPartRelationsAssocList),
	% Create a record for product model global constraints
	globalConstraintsRecord(GlobalConstraints),
	% Create record for product model tree
	make_productModel([pmTypes(TypesAssocList),pmNodes(NodesAssocList),pmHasPartRelations(HasPartRelationsAssocList),pmGlobalConstraints(GlobalConstraints)],PM).

% typeAssocList/2 creates an association list TypesAssocList with types as keys and finite domains as values
typesAssocList(B,TypesAssocList) :-	
	findall(Type-Sort-Values,type(Type,Sort,Values),TypesList),
	typesFiniteDomains(TypesList,B,[],TypesAssocList).
	
typesFiniteDomains([],_,Acc,TypesAssocList) :-
	list_to_assoc(Acc,TypesAssocList).
typesFiniteDomains([Type-int-[Int|Values]|Types],B,Acc,TypesAssocList) :-
	intList_to_FD(Values,Int,FiniteDomain),
	typesFiniteDomains(Types,B,[Type-FiniteDomain|Acc],TypesAssocList).
typesFiniteDomains([Type-int-(Min,Max)|Types],B,Acc,TypesAsssocList) :-
	typesFiniteDomains(Types,B,[(Type-Min..Max)|Acc],TypesAsssocList).
typesFiniteDomains([Type-string-[S|Strings]|Types],B,Acc,TypesAsssocList) :-
	get_assoc(S,B,SInt),
	stringList_to_FD(Strings,B,SInt,FiniteDomain),
	typesFiniteDomains(Types,B,[Type-FiniteDomain|Acc],TypesAsssocList).
	
intList_to_FD([],FD,FD).
intList_to_FD([Int|Values],Acc,FD) :-
	 NewAcc = Acc \/ Int,
	 intList_to_FD(Values,NewAcc,FD).
	
stringList_to_FD([],_,FD,FD).
stringList_to_FD([S|Strings],B,Acc,FD) :-
	get_assoc(S,B,SInt),
	NewAcc = Acc \/ SInt,
	stringList_to_FD(Strings,B,NewAcc,FD).

% nodesAssocList/1 creates an association list NodesAssocList with node names as keys and couples composed by an association list NV and a list of
% node constraints as values. The association list NV has node variables as keys and variable types as values
nodesAssocList(NodesAssocList) :-
	findall((NodeName,NodeVars,NodeConstrs),node(NodeName,NodeVars,NodeConstrs),NodesList),
	nodeVarAssocList(NodesList,t,NodesAssocList).

nodeVarAssocList([],NodeAssocList,NodeAssocList).
nodeVarAssocList([(NodeName,NodeVars,NodeConstrs)|Nodes],Acc,NodeAssocList) :-
	varAssocList(NodeVars,t,NodeVarsAssocList),
	put_assoc(NodeName,Acc,NodeVarsAssocList-NodeConstrs,NewAcc),
	nodeVarAssocList(Nodes,NewAcc,NodeAssocList).

varAssocList([],NV,NV).
varAssocList([(V,T)|Vs],Acc,NV) :-
	put_assoc(V,Acc,T,NAcc),
	varAssocList(Vs,NAcc,NV).
	
% hasPartRelationAssocList/1 creates an association list with triples RelationName-ParentNode-ChildNode as keys and couples
% RelationCard-RelationConstrs as values
hasPartRelationsAssocList(HasPartRelationsAssocList) :-
	findall(((RelationName-ParentNode-ChildNode)-(RelationCard-RelationConstrs)),hasPartRelationModel(RelationName,ParentNode,ChildNode,RelationCard,RelationConstrs),HasPartRelationsList),
	list_to_assoc(HasPartRelationsList,HasPartRelationsAssocList).

hasPartRelationModel('',B,C,D,E) :- edge(B,C,D,E).
hasPartRelationModel(A,B,C,D,E) :- edge(A,B,C,D,E).

% globalConstraintsList/1 creates a record containing a list of product model global constraints, a list of product model aggregate constraints 
% and a list of product model all different constraints
globalConstraintsRecord(GlobalConstraints) :-
	%findall(G,globalConstraint(G),Global),
	findall(G,nodeModelConstraint(G),Global),
	findall(AggregationFunction-Variables-RelOp-Integer,aggConstraint(AggregationFunction,Variables,RelOp,Integer),Aggregate),
	findall(AllDifferentVariables,allDifferentValues(AllDifferentVariables),AllDifferentValues),
	findall(C,cardModelConstraint(C),CardConstrs),
	make_pmGlobalConstraints([pmGlobal(Global),pmAggregate(Aggregate),pmAllDifferentValues(AllDifferentValues),pmCardConstraints(CardConstrs)],GlobalConstraints).
	
% rootNode/3 finds the root of the product model tree	
rootNode(PMNodes,PMHasPartRelations,RootNode) :- once((gen_assoc(RootNode,PMNodes,_),\+ gen_assoc(_-_-RootNode,PMHasPartRelations,_))).

% instantiateITree/3 generates a tree of node instances represented with a list of couples Node-NodeID and a list of has-part relations 
% RelName-Parent-PID-Child-CID considering only constant cardinality has-part relations
instantiateITree(PM,ITree,LastNodeInstance) :-
	productModel_pmNodes(PM,PMNodes),
	productModel_pmHasPartRelations(PM,PMHasPartRelations),
	rootNode(PMNodes,PMHasPartRelations,RootNode),
	list_to_assoc([RootNode-1],AccLastNodeInstance),
	instantiateITree([RootNode-1],[RootNode-1],ITreeNodes,AccLastNodeInstance,LastNodeInstance,PMHasPartRelations,[],ITreeHasPartRelations),
	make_iTree([iNodes(ITreeNodes),iHasPartRelations(ITreeHasPartRelations)],ITree).

instantiateITree([],ITreeNodes,ITreeNodes,LastNodeInstance,LastNodeInstance,_,ITreeHasPartRelations,ITreeHasPartRelations).
instantiateITree([N-NID|NodesInstancesQueue],NodesAcc,ITreeNodes,AccLastNodeInstance,LastNodeInstance,PMHasPartRelations,RelationsAcc,ITreeHasPartRelations) :-
	findall(RelationName-N-ChildNode-RelationCard,
			  (gen_assoc(RelationName-N-ChildNode,PMHasPartRelations,RelationCard-_), integer(RelationCard)),
			  ConstantHasPartRelations
			 ),
	(ConstantHasPartRelations \= []
	 -> generateITreeNodesConstantRelations(N,NID,ConstantHasPartRelations,AccLastNodeInstance,NewLastNodeInstance,[],NewNodeInstances,[],NewHasPartRelations),
		 append(NodesInstancesQueue,NewNodeInstances,NewNodesInstancesQueue),
		 append(NodesAcc,NewNodeInstances,NewNodesAcc),
		 append(RelationsAcc,NewHasPartRelations,NewRelationsAcc)
	 ;	 NewNodesInstancesQueue = NodesInstancesQueue,
		 NewNodesAcc = NodesAcc,
		 NewLastNodeInstance = AccLastNodeInstance,
		 NewRelationsAcc = RelationsAcc
	),
	instantiateITree(NewNodesInstancesQueue,NewNodesAcc,ITreeNodes,NewLastNodeInstance,LastNodeInstance,PMHasPartRelations,NewRelationsAcc,ITreeHasPartRelations).

% generateITreeNodesConstantRelations/9 generates node instances and relations for children of a node instance given constant has-part relations
generateITreeNodesConstantRelations(_,_,[],LastNodeInstance,LastNodeInstance,NewNodeInstances,NewNodeInstances,NewHasPartRelations,NewHasPartRelations).
generateITreeNodesConstantRelations(N,NID,[RelationName-N-ChildNode-RelationCard|ConstantHasPartRelations],LastNodeInstance,NewLastNodeInstance,INodesAcc,NewNodeInstances,IRelationsAcc,NewHasPartRelations) :-
	generateInstances(ChildNode,LastNodeInstance,NewLNI,RelationCard,[],ChildNodeInstances),
	generateHasPartRelations(RelationName,N,NID,ChildNodeInstances,[],NewChildRelations),
	append(INodesAcc,ChildNodeInstances,NewINodesAcc),
	append(IRelationsAcc,NewChildRelations,NewIRelationsAcc),
	generateITreeNodesConstantRelations(N,NID,ConstantHasPartRelations,NewLNI,NewLastNodeInstance,NewINodesAcc,NewNodeInstances,NewIRelationsAcc,NewHasPartRelations).

% createInstances/6 creates instances of a child
generateInstances(_,NewLastNodeInstance,NewLastNodeInstance,0,NewChildInstances,NewChildInstances).		
generateInstances(ChildNode,LastNodeInstance,NewLastNodeInstance,RelationCard,ChildAcc,NewChildInstances) :-
	generateChildInstance(ChildNode,LastNodeInstance,NewLNI,ChildID),
	NewRelationCard is RelationCard - 1, NewRelationCard >= 0,
	append(ChildAcc,[ChildNode-ChildID],NewChildAcc),
	generateInstances(ChildNode,NewLNI,NewLastNodeInstance,NewRelationCard,NewChildAcc,NewChildInstances).

% generateChildInstance/4 creates an instance of a child	
generateChildInstance(ChildNode,LastNodeInstance,NewLastNodeInstance,NewLastChildID) :-
	get_assoc(ChildNode,LastNodeInstance,LastChildID),
	NewLastChildID is LastChildID + 1,
	put_assoc(ChildNode,LastNodeInstance,NewLastChildID,NewLastNodeInstance).
generateChildInstance(ChildNode,LastNodeInstance,NewLastNodeInstance,1) :-
	\+ get_assoc(ChildNode,LastNodeInstance,_),
	put_assoc(ChildNode,LastNodeInstance,1,NewLastNodeInstance).
	
% createHPRelations/6 creates has-part relation between a node instance and its children	
generateHasPartRelations(_,_,_,[],NewHasPartRelations,NewHasPartRelations).
generateHasPartRelations(RelationName,N,NID,[ChildNode-ChildID|Children],RelationsAcc,NewHasPartRelations) :-
	append(RelationsAcc,[RelationName-N-NID-ChildNode-ChildID],NewRelationsAcc),
	generateHasPartRelations(RelationName,N,NID,Children,NewRelationsAcc,NewHasPartRelations).
	
% checkMinNodes/2 checks if MinNodes is greater than the number of nodes in the instantiated instance tree. Prints a message if it is not the case
checkMinNodes(ITree,MinNodes,Browser) :-
	iTree_iNodes(ITree,INodes),
	length(INodes,Nodes),
	( MinNodes > Nodes
	  -> 	true
	  ;	atomic_list_concat(['The minimum number of nodes (',MinNodes,') is less than or equal to the number of nodes in the instantiated tree.\n'], Msg),
	  		send(Browser, append, Msg)
	).
	
% createCSP/4 create a record for the CSP corresponding to a given instance tree (it does not compute constraints related to performed actions)
createCSP(PM,ITree,B,CSP) :-
	once(createCSP(PM,ITree,FDNodeVariablesAssocList,B,FDCardVariablesAssocList,CSPConstraintsList)),
	make_csp([cspVariables(FDNodeVariablesAssocList-FDCardVariablesAssocList),cspConstraints(CSPConstraintsList)],CSP).
	
% createCSPConstaints/6 creates a list of constraint involving variables of node instances
createCSP(PM,ITree,FDNodeVariablesAssocList,B,FDCardVariablesAssocList,CSPConstraintsLists) :-
	iTree_iNodes(ITree,INodes),
	iTree_iHasPartRelations(ITree,IHasPartRelations),
	productModel_pmNodes(PM,PMNodesAssocList),
	productModel_pmHasPartRelations(PM,PMHasPartRelations),
	productModel_pmTypes(PM,PMTypesAssocList),
	% Create a list of domains, nodes and cardinality constraints, an association list for finite domain node variables, and an association list
	% for cardinality variables
	domainNodeCardConstraints(INodes,PMNodesAssocList,PMHasPartRelations,IHasPartRelations,t,FDNodeVariablesAssocList,PMTypesAssocList,B,[],Constraints,t,FDCardVariablesAssocList),
	% Create a list of global constraints
	productModel_pmGlobalConstraints(PM,PMGlobalConstraints),
	pmGlobalConstraints_pmGlobal(PMGlobalConstraints,PMGlobal),
	findall(GC,(member(GC,PMGlobal),pathsInvolved(GC,NodesPaths),instanceExists(NodesPaths,INodes,IHasPartRelations)),GCs),
	globalConstraints(GCs,INodes,IHasPartRelations,FDNodeVariablesAssocList,B,[],GlobalConstraints),
	% Create a list of aggregate constraints
	pmGlobalConstraints_pmAggregate(PMGlobalConstraints,PMAggregate),
	aggregateConstraints(PMAggregate,INodes,IHasPartRelations,FDNodeVariablesAssocList,[],AggConstraints),
	% Create a list of all different constraints
	pmGlobalConstraints_pmAllDifferentValues(PMGlobalConstraints,PMAllDifferentValues),
	allDifferentValuesConstraints(PMAllDifferentValues,INodes,IHasPartRelations,FDNodeVariablesAssocList,[],AllDiffConstraints),
	% Create a list of cardinality model constraints
	pmGlobalConstraints_pmCardConstraints(PMGlobalConstraints,PMCardConstrs),
	cardModelConstraints(PMCardConstrs,INodes,FDCardVariablesAssocList,B,[],CardModelConstrs),
	append([GlobalConstraints,AggConstraints,AllDiffConstraints,CardModelConstrs],CSPGlobalConstraintsList),
	CSPConstraintsLists = Constraints-CSPGlobalConstraintsList.
	
% domainNodeCardConstraints/12 generates a list of CSP constraints given a list of node instances
domainNodeCardConstraints([],_,_,_,FDNodeVariablesAssocList,FDNodeVariablesAssocList,_,_,Constraints,Constraints,FDCardVariablesAssocList,FDCardVariablesAssocList).
domainNodeCardConstraints([Node-NodeID|Nodes],PMNodesAssocList,PMHasPartRelations,IHasPartRelations,AccFDNodeVariablesAssocList,FDNodeVariablesAssocList,PMTypesAssocList,B,ConstraintsAcc,Constraints,AccFDCardAssocList,FDCardVariablesAssocList) :-
	get_assoc(Node,PMNodesAssocList,NodeVariables-NodeConstrs),
	% Finite domain variables for node variables
	assoc_to_list(NodeVariables,NodeVariablesList),
	createNFDVars(Node,NodeID,NodeVariablesList,PMTypesAssocList,AccFDNodeVariablesAssocList,NewAccFDNodeVariablesAssocList,[],FDVariablesDomains),
	ancestors(Node,NodeID,IHasPartRelations,[],Ancestors),
	% Domain constraints for node variables
	domainConstraints(FDVariablesDomains,[],NodeVariablesDomainConstraints),
	% Node constraints
	(NodeConstrs \= []
	 -> instConstraints(NodeConstrs,Node,NodeID,NodeVariables,Ancestors,IHasPartRelations,NewAccFDNodeVariablesAssocList,B,[],_,[],InstNodeConstraints)
	 ; InstNodeConstraints = []
	),
	% Cardinality constraints
	findall(RelationName-Node-NodeID-ChildNode-RelationCard-CardConstraints,
			  (gen_assoc(RelationName-Node-ChildNode,PMHasPartRelations,RelationCard-CardConstraints), \+ integer(RelationCard)),
			  NodeRelations
	),
	cardConstraints(NodeRelations,Node,NodeID,NodeVariables,Ancestors,IHasPartRelations,NewAccFDNodeVariablesAssocList,B,PMTypesAssocList,AccFDCardAssocList,NewFDCardAssocList,[],InstCardConstrs),
	append([ConstraintsAcc,NodeVariablesDomainConstraints,InstNodeConstraints,InstCardConstrs],NewConstraintsAcc),
	domainNodeCardConstraints(Nodes,PMNodesAssocList,PMHasPartRelations,IHasPartRelations,NewAccFDNodeVariablesAssocList,FDNodeVariablesAssocList,PMTypesAssocList,B,NewConstraintsAcc,Constraints,NewFDCardAssocList,FDCardVariablesAssocList).
domainNodeCardConstraints([Node-NodeID|Nodes],PMNodesAssocList,PMHasPartRelations,IHasPartRelations,AccFDNodeVariablesAssocList,FDNodeVariablesAssocList,PMTypesAssocList,B,ConstraintsAcc,Constraints,AccFDCardAssocList,FDCardVariablesAssocList) :-
	get_assoc(Node,PMNodesAssocList,NodeVariables-[]),
	% Finite domain variables for node variables
	assoc_to_list(NodeVariables,NodeVariablesList),
	createNFDVars(Node,NodeID,NodeVariablesList,PMTypesAssocList,AccFDNodeVariablesAssocList,NewAccFDNodeVariablesAssocList,[],FDVariablesDomains),
	ancestors(Node,NodeID,IHasPartRelations,[],Ancestors),
	% Domain constraints for node variables
	domainConstraints(FDVariablesDomains,[],NodeVariablesDomainConstraints),
	% Cardinality constraints
	findall(RelationName-Node-NodeID-ChildNode-RelationCard-CardConstraints,
			  (gen_assoc(RelationName-Node-ChildNode,PMHasPartRelations,RelationCard-CardConstraints), \+ integer(RelationCard)),
			  NodeRelations
	),
	cardConstraints(NodeRelations,Node,NodeID,NodeVariables,Ancestors,IHasPartRelations,NewAccFDNodeVariablesAssocList,B,PMTypesAssocList,AccFDCardAssocList,NewFDCardAssocList,[],InstCardConstrs),
	append([ConstraintsAcc,NodeVariablesDomainConstraints,InstCardConstrs],NewConstraintsAcc),
	domainNodeCardConstraints(Nodes,PMNodesAssocList,PMHasPartRelations,IHasPartRelations,NewAccFDNodeVariablesAssocList,FDNodeVariablesAssocList,PMTypesAssocList,B,NewConstraintsAcc,Constraints,NewFDCardAssocList,FDCardVariablesAssocList).

% 	createNFDVars/8 add new associations to the finite domain node variables association list
createNFDVars(_,_,[],_,FDNodeVariablesAssocList,FDNodeVariablesAssocList,FDVariablesDomains,FDVariablesDomains).
createNFDVars(Node,NodeID,[VariableName-VariableType|Variables],PMTypesAssocList,FDNodeVariablesAssocListAcc, FDNodeVariablesAssocList,FDAcc,FDVariablesDomains) :-
	get_assoc(VariableType,PMTypesAssocList,FD),
	put_assoc(Node-NodeID-VariableName,FDNodeVariablesAssocListAcc,V-FD,NewFDNodeVariablesAssocListAcc),
	createNFDVars(Node,NodeID,Variables,PMTypesAssocList,NewFDNodeVariablesAssocListAcc,FDNodeVariablesAssocList,[V-FD|FDAcc],FDVariablesDomains).
	
% ancestors/5 computes the ancestors of a node instance in the instance tree	
ancestors(Node,NodeID,IHasPartRelations,Acc,Ancestors) :- ancestors(Node,NodeID,[],IHasPartRelations,Acc,Ancestors).
ancestors(Node,NodeID,Path,IHasPartRelations,Acc,Ancestors) :-
	member(RelationName-Parent-ParentID-Node-NodeID,IHasPartRelations),
	ancestors(Parent,ParentID,[RelationName|Path],IHasPartRelations,[Parent-ParentID-[RelationName|Path]|Acc],Ancestors).
ancestors(Node,NodeID,_,IHasPartRelations,Ancestors,Ancestors) :-
	\+ member(_-_-_-Node-NodeID,IHasPartRelations).
	
% domainConstraints/3 creates a list of domain constraints for a given list of couple FDVariable-FD
domainConstraints([],DomainConstraints,DomainConstraints).
domainConstraints([FDVariable-FD|VariablesDomains],Acc,DomainConstraints) :-
	domainConstraints(VariablesDomains,[FDVariable in FD|Acc],DomainConstraints).
	
% instConstraints/12 instantiate node constraints on node instance variables
instConstraints([],_,_,_,_,_,_,_,_,_,InstConsts,InstConsts).
instConstraints([C|Cs],Node,NodeID,NodeVariables,Ancestors,IHasPartRelations,FDNodeVariablesAssocList,B,Card,FDCard,Acc,InstConstrs) :-
	( applicable(C,Node,NodeID,Ancestors,IHasPartRelations)
	  ->	instantiate(C,Node,NodeID,NodeVariables,Ancestors,B,Card,FDCard,FDNodeVariablesAssocList,InstC),
	 		append(Acc,[InstC],NewAcc),
	 		instConstraints(Cs,Node,NodeID,NodeVariables,Ancestors,IHasPartRelations,FDNodeVariablesAssocList,B,Card,FDCard,NewAcc,InstConstrs)
	  ;
	 		instConstraints(Cs,Node,NodeID,NodeVariables,Ancestors,IHasPartRelations,FDNodeVariablesAssocList,B,Card,FDCard,Acc,InstConstrs)
	).
	
% cardConstraints/13 instantiate cardinality constraints of relation having Node as parent node
cardConstraints([],_,_,_,_,_,_,_,_,FDCardAssocList,FDCardAssocList,CardConstrs,CardConstrs).
cardConstraints([RelationName-ParentNode-PID-ChildNode-(RelationCard,CardType)-RelationConstrs|NodeRelations],Node,NodeID,NodeVariables,Ancestors,IHasPartRelations,FDNodeVariablesAssocList,B,PMTypesAssocList,AccFDCardAssocList,FDCardAssocList,AccCardConstrs,CardConstrs) :-
	instConstraints(RelationConstrs,Node,NodeID,NodeVariables,Ancestors,IHasPartRelations,FDNodeVariablesAssocList,B,RelationCard,FDCard,[],InstCardConstraints),
	get_assoc(CardType,PMTypesAssocList,FD),
	put_assoc(RelationName-ParentNode-PID-ChildNode-RelationCard,AccFDCardAssocList,FDCard-FD,NewAccFDCardAssocList),
	append(AccCardConstrs,InstCardConstraints,NewAccCardConstrs),
	cardConstraints(NodeRelations,Node,NodeID,NodeVariables,Ancestors,IHasPartRelations,FDNodeVariablesAssocList,B,PMTypesAssocList,NewAccFDCardAssocList,FDCardAssocList,[FDCard in FD|NewAccCardConstrs],CardConstrs).

% applicable/5 checks if a constraint can be instantiated 
applicable(C,Node,NodeID,Ancestors,IHasPartRelations) :-
	pathsInvolved(C,Paths),
	forall( member((A,P),Paths),
			  (member(A-AID-_,Ancestors), checkPath(A,AID,P,Node,NodeID,IHasPartRelations))
			).
	
path(N,NID,N,NID,_).
path(N,NID,M,MID,IHasPartRelations) :- member(_-N-NID-M-MID,IHasPartRelations).
path(N,NID,M,MID,IHasPartRelations) :- member(_-N-NID-O-OID,IHasPartRelations), path(O,OID,M,MID,IHasPartRelations).
	
checkPath(N,NID,[],N,NID,_).
checkPath(A,AID,[RName|Path],NName,NID,IHasPartRelations) :-
	member(RName-A-AID-C-CID,IHasPartRelations),
	checkPath(C,CID,Path,NName,NID,IHasPartRelations).
checkPath(A,AID,[has_part|Path],NName,NID,IHasPartRelations) :- 
	member(_-A-AID-M-MID,IHasPartRelations),
	checkPath(M,MID,Path,NName,NID,IHasPartRelations).
checkPath(A,AID,['*'|Path],NName,NID,IHasPartRelations) :-
	path(A,AID,M,MID,IHasPartRelations),
	checkPath(M,MID,Path,NName,NID,IHasPartRelations).
	
% globalConstraints/7 generates CSP-constraints form global constraints
globalConstraints([],_,_,_,_,GlobalConstraints,GlobalConstraints).
globalConstraints([GC|GCs],INodes,IHasPartRelations,FDNodeVariablesAssocList,B,GCAcc,GlobalConstraints) :-
	pathsInvolved(GC,NodesPaths),
	instancesTuples(NodesPaths,INodes,IHasPartRelations,InstancesTuples),
	instGlobalConstraint(GC,InstancesTuples,FDNodeVariablesAssocList,B,[],InstGCs),
	append(GCAcc,InstGCs,NewAcc),
	globalConstraints(GCs,INodes,IHasPartRelations,FDNodeVariablesAssocList,B,NewAcc,GlobalConstraints).

instanceExists(NodesPaths,INodes,IHasPartRelations) :-
	forall(	member((N,P),NodesPaths),
			(member(N-NID,INodes),
			 checkPath(_,_,P,N,NID,IHasPartRelations)
			)
	).
	
instancesTuples(NodesPaths,INodes,IHasPartRelations,InstanceTuples) :-
	findall(T,tuple(NodesPaths,INodes,IHasPartRelations,[],T),InstanceTuples).

tuple([],_,_,T,T).
tuple([(N,P)|NsPs],INodes,IHasPartRelations,T,FT) :-
	member(N-ID,INodes),
	checkPath(_,_,P,N,ID,IHasPartRelations),
	tuple(NsPs,INodes,IHasPartRelations,[(N,ID,P)|T],FT).
	
% instGlobalConstraint/6 instantiates a gloabl constraint on node instance variables
instGlobalConstraint(_,[],_,_,InstGCs,InstGCs).
instGlobalConstraint(GC,[T|Tuples],FDNodeVariablesAssocList,B,Acc,InstGCs) :-
	instantiate(GC,glob,_,t,T,B,[],_,FDNodeVariablesAssocList,InstGC),
	append(Acc,[InstGC],NewAcc),
	instGlobalConstraint(GC,Tuples,FDNodeVariablesAssocList,B,NewAcc,InstGCs).
	
% aggregateConstraints/6 generates CSP-constraints from aggregate consrtaints
aggregateConstraints([],_,_,_,AggConstraints,AggConstraints).
aggregateConstraints([AggFun-Vars-ROp-Num|ACs],INodes,IHasPartRelations,FDNodeVariablesAssocList,Acc,AggConstraints) :-
	collectVarsInvolvedInstances(Vars,INodes,IHasPartRelations,FDNodeVariablesAssocList,[],VarInvInsts),
	member((ROp,FDOp),[(lt,#<),(leq,#=<),(eq,#=),(geq,#>=),(gt,#>),(neq,#\=)]),
	aggregateConstraint(AggFun,VarInvInsts,FDOp,Num,AggConst),
	append(Acc,[AggConst],NewAcc),
	aggregateConstraints(ACs,INodes,IHasPartRelations,FDNodeVariablesAssocList,NewAcc,AggConstraints).
	
collectVarsInvolvedInstances([],_,_,_,VarInvInsts,VarInvInsts).
collectVarsInvolvedInstances([(V,N,P)|Vars],INodes,IHasPartRelations,FDNodeVariablesAssocList,Acc,VarInvInsts) :-
	findall(N-ID,
			(member(N-ID,INodes),checkPath(_,_,P,N,ID,IHasPartRelations)),
			Insts),
	collectInstVariables(V,Insts,FDNodeVariablesAssocList,[],VarInsts),
	append(Acc,VarInsts,NewAcc),
	collectVarsInvolvedInstances(Vars,INodes,IHasPartRelations,FDNodeVariablesAssocList,NewAcc,VarInvInsts).
	
collectInstVariables(_,[],_,VarInsts,VarInsts).
collectInstVariables(V,[N-ID|Insts],FDNodeVariablesAssocList,Acc,VarInsts) :-
	get_assoc(N-ID-V,FDNodeVariablesAssocList,FDVar-_),
	collectInstVariables(V,Insts,FDNodeVariablesAssocList,[FDVar|Acc],VarInsts).
	
aggregateConstraint(sum,Vars,FDOp,Num,AggConst) :-
	AggConst = sum(Vars,FDOp,Num).
	
aggregateConstraint(avg,[V|Vars],FDOp,Num,AggConst) :-
	varSum(Vars,V,VarSum),
	length([V|Vars],NElem),
	AggConst =.. [FDOp,(VarSum) / NElem,Num].
	
varSum([],VarSum,VarSum).
varSum([V|Vs],Acc,VarSum) :-
	NewAcc = Acc + V,
	varSum(Vs,NewAcc,VarSum).
	
% allDifferentValues/6 generates CSP-constraints from all-different constraints
allDifferentValuesConstraints([],_,_,_,AllDiffConstraints,AllDiffConstraints).
allDifferentValuesConstraints([ADV|AllDiffVars],INodes,IHasPartRelations,FDNodeVariablesAssocList,Acc,AllDiffConstraints) :-
	collectVarsInvolvedInstances(ADV,INodes,IHasPartRelations,FDNodeVariablesAssocList,[],InstVars),
	append(Acc,[all_different(InstVars)],NewAcc),
	allDifferentValuesConstraints(AllDiffVars,INodes,IHasPartRelations,FDNodeVariablesAssocList,NewAcc,AllDiffConstraints).
	
% cardModelConstraints/6 generate CSP-constraints from cardinality model constraints
cardModelConstraints([],_,_,_,CardModelConstraints,CardModelConstraints).
cardModelConstraints([C|CardConstrs],INodes,FDCardVariablesAssocList,B,Acc,CardModelConstrs) :-
	nodeInvolved(C,[N]),
	findall(N-ID,member(N-ID,INodes),Insts),
	instCardModelConstraint(C,Insts,FDCardVariablesAssocList,B,InstCs,[]),
	append(Acc,InstCs,NewAcc),
	cardModelConstraints(CardConstrs,INodes,FDCardVariablesAssocList,B,NewAcc,CardModelConstrs).

instCardModelConstraint(_,[],_,_,List,List).	
instCardModelConstraint(C,[N-ID|Insts],FDCardVariablesAssocList,B,[InstC|Tail1],Tail2) :-
	instantiate(C,N,ID,cardModel,_,B,_,_,FDCardVariablesAssocList,InstC),
	instCardModelConstraint(C,Insts,FDCardVariablesAssocList,B,Tail1,Tail2).	
	
% findNode/4 given the ancestors of a node instance I, finds the id of the instance of the ancestor N such that the path connecting it with I
% is the shortest one matching a given path P (evry path match [])
findNode(N,P,Ancestors,NID) :-
	findall(N-AID-PA,member(N-AID-PA,Ancestors),NAncestors),
	(P = []
	 ->	shortestPath(NAncestors,NID)
	 ;		matchingPath(NAncestors,P,MatchingAncestors), shortestPath(MatchingAncestors,NID)
	).

shortestPath([_-AID-Path|Ancestors],NID) :-
	length(Path,L),
	shortestPath(Ancestors,L,AID,NID).
	
shortestPath([],_,AID,AID).
shortestPath([_-AID-Path|Ancestors],CL,CID,NID) :-
	length(Path,L),
	(L < CL
	 ->	shortestPath(Ancestors,L,AID,NID)
	 ;		shortestPath(Ancestors,CL,CID,NID)
	).
	
matchingPath(Ancestors,P,MatchingAncestors) :-
	findall(A-AID-PA,
		     (member(A-AID-PA,Ancestors),matchPath(P,PA)),
			  MatchingAncestors
			 ).

matchPath([],[]).
matchPath(['*'],_).
matchPath([E|P],[E|PA]) :- matchPath(P,PA).
matchPath([has_part|P],[_|PA]) :- matchPath(P,PA).
matchPath(['*',E|P],[F|PA]) :-
	(E = F
	 ->	matchPath(P,PA)
	 ;		matchPath(['*',E|P],PA)
	).

% instantiate/10 instantiates a given constraint on finite domain variables
instantiate((Label,N,ChildNode,Card),N,ID,cardModel,_,_,_,_,FDCardVariablesAssocList,FDVar) :-
	get_assoc(Label-N-ID-ChildNode-Card,FDCardVariablesAssocList,FDVar-_).
instantiate((Var,N,P),glob,_,_,Tuple,_,_,_,FDNodeVariables,FDVar) :-
	member((N,NID,P),Tuple),
	get_assoc(N-NID-Var,FDNodeVariables,FDVar-_).
%instantiate((Var,N,[]),N,NID,_,Ancestors,_,_,_,FDNodeVariables,FDVar) :-	
%	\+ member(N-NID-_,Ancestors),
%	get_assoc(N-NID-Var,FDNodeVariables,FDVar-_).
instantiate((Var,N,P),_,_,_,Ancestors,_,_,_,FDNodeVariables,FDVar) :-
	((member('*',P) ; member(has_part,P) ; P = [])
	 ->	findNode(N,P,Ancestors,NID)
	 ;		member(N-NID-P,Ancestors)
	),
	get_assoc(N-NID-Var,FDNodeVariables,FDVar-_).
instantiate(Var,N,NID,NV,_,_,_,_,FDNodeVariables,FDVar) :-
	get_assoc(Var,NV,_),
	get_assoc(N-NID-Var,FDNodeVariables,FDVar-_).
instantiate(Card,_,_,NV,_,_,Card,CardFD,_,CardFD) :-
	NV \= cardModel,
	\+ get_assoc(Card,NV,_).
instantiate(T,_,_,_,_,_,_,_,_,T) :- integer(T).
instantiate(T,glob,_,_,_,B,_,_,_,IntT) :-
	atom(T), get_assoc(T,B,IntT).
instantiate(T,_,_,cardModel,_,B,_,_,_,IntT) :-
	atom(T), get_assoc(T,B,IntT).
instantiate(T,_,_,NV,_,B,_,_,_,IntT) :-
	atom(T),
	\+ get_assoc(T,NV,_),
	get_assoc(T,B,IntT).
	
instantiate(C,N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,InstC) :-
	C =.. [Op,C1,C2], \+ (C2 =.. [valid_tuples,_,_]),  \+ (C1 =.. [valid_tuples,_,_]),
	member((Op,FDOp),[(plus,+),(minus,-),(times,*),(frac,/),(mod,mod),(lt,#<),(leq,#=<),(eq,#=),(geq,#>=),(gt,#>),(neq,#\=)]),
	instantiate(C1,N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,InstC1),
	instantiate(C2,N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,InstC2),
	InstC =.. [FDOp,InstC1,InstC2].
instantiate(abs(Exp),N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,abs(InstExp)) :-
	\+ Exp =.. [valid_tuples,_,_],
	instantiate(Exp,N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,InstExp).
instantiate(minus_sign(Exp),N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,- InstExp) :-
	\+ Exp =.. [valid_tuples,_,_],
	instantiate(Exp,N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,InstExp).
instantiate(C,N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,InstC) :-
	C =.. [Op,C1,C2],
	member((Op,FDOp),[(and,#/\),(or,#\/),(impl,#==>)]),
	instantiate(C1,N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,InstC1),
	instantiate(C2,N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,InstC2),
	InstC =.. [FDOp,InstC1,InstC2].	
instantiate(not(Con),N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,#\ InstC) :-
	instantiate(Con,N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,InstC).

instantiate(valid_tuples(Vars,Tuples),N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,InstC) :-
	instList(Vars,N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,[],InstVars),
	instTuples(Tuples,N,NID,Ancestors,B,Card,CardVal,FDNodeVariables,[],InstTuples),
	InstC = tuples_in([InstVars],InstTuples).
	
instList([],_,_,_,_,_,_,_,_,InstList,InstList).
instList([L|Ls],N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,Acc,InstList) :-
	instantiate(L,N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,InstL),
	instList(Ls,N,NID,NV,Ancestors,B,Card,CardVal,FDNodeVariables,[InstL|Acc],InstList).
	
instTuples([],_,_,_,_,_,_,_,InstTuples,InstTuples).
instTuples([T|Tuples],N,NID,Ancestors,B,Card,CardVal,FDNodeVariables,Acc,InstTuples) :-
	instTuple(T,B,[],InstT),
	instTuples(Tuples,N,NID,Ancestors,B,Card,CardVal,FDNodeVariables,[InstT|Acc],InstTuples).
	
instTuple([],_,InstT,InstT).
instTuple([T|Tuple],B,Acc,InstT) :-
	((integer(T), IntT = T)
	 ;
	 (atom(T), get_assoc(T,B,IntT))
	),
	instTuple(Tuple,B,[IntT|Acc],InstT).
	
% propagate_constraints_no_bt/1 posts the constraints of a CSP, message in case of failure (empty domain/s)
propagate_constraints_no_bt(CSP) :-
	csp_cspConstraints(CSP,NodeConstraints-GlobalConstraints),
	append(NodeConstraints,GlobalConstraints,CSPConstraints),
	(propagate_constraints(CSPConstraints)
	 ->	true
	 ;		false
	).

% propagate_constraints_bt/8 posts the constraints of a CSP, backtrack in case of failure (empty domain/s)
propagate_constraints_bt(CurrentCSP,ITree,States,CurrentLastAction,PM,B,MinNodes,MaxNodes,ExitStatus,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction) :-
	send(Browser, append, '\nConstraint propagation\n'),
	csp_cspConstraints(CurrentCSP,NodeConstraints-GlobalConstraints),
	% Compute constraints related to performed action
	lastActionConstraints(CurrentCSP,States,CurrentLastAction,ActionConstraints),
	append([NodeConstraints,GlobalConstraints,ActionConstraints],Constraints),
	(propagate_constraints(Constraints)
	 ->	%true, ExitStatus = 0
			(tryToLabel(ITree,PM,B,States,CurrentLastAction)
			 ->	true, ExitStatus = 0
			 ;		send(Browser, append, '\nBacktrack after try to label failure!!!\n'),
					backtrack(States,CurrentLastAction,PM,B,MinNodes,MaxNodes,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction), ExitStatus = 1		
			)
	 ;		send(Browser, append, '\nBacktrack after propagation failure!!!\n'),
			backtrack(States,CurrentLastAction,PM,B,MinNodes,MaxNodes,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction), ExitStatus = 1
	).

% tryToLabel/5 computes the CSP corresponding to the current instance tree and try to label its variables
% This predicate allows to prune earlier the search tree during instance construction. The computation of the CSP allows to obtain
% a CSP equivalent to the one passed as arguments to propagate_constraints_bt but on different variables, that can be labeled without affecting
% successive computation steps
tryToLabel(ITree,PM,B,States,LastAction) :-
	createCSP(PM,ITree,B,CSP),
	csp_cspConstraints(CSP,NodeConstraints-GlobalConstraints),
	lastActionConstraints(CSP,States,LastAction,ActionConstraints),
	% Compute constraint related to not yet considered relations
	notYetConstraint(CSP,States,LastAction,NotYet),
	append([NodeConstraints,GlobalConstraints,ActionConstraints,NotYet],Constraints),
	once(propagate_constraints(Constraints)),
	csp_cspVariables(CSP,NodeVariables-CardVariables),
	assoc_to_values(NodeVariables,NVFD),
	assoc_to_values(CardVariables,CVFD),
	extractVariables(NVFD,[],NV),
	extractVariables(CVFD,[],CV),
	append(NV,CV,V),
	labeling([ff],V).
	
% labelNodeVariables/7 labels node variables to obtain an instance, backtracks in case of failure
labelNodeVariables(CSP,ITree,States,LastAction,PM,B,MinNodes,MaxNodes,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction) :-
	csp_cspConstraints(CSP,NodeConstraints-GlobalConstraints),
	csp_cspVariables(CSP,NodeVariables-CardVariables),
	% Compute constraints related to performed action
	lastActionConstraints(CSP,States,LastAction,ActionConstraints),
	% Compute constraints related to relations whose cardinalities has to be equal to zero in order to obtain an instance
	cardZeroConstraints(CardVariables,ITree,States,LastAction,CardZeroConstraints),
	!,
	append([NodeConstraints,GlobalConstraints,ActionConstraints,CardZeroConstraints],Constraints),
	send(Browser, append, '\nConstraint propagation and labeling\n'),
	(propagate_constraints(Constraints)
	 ->	(	labelNodeVariables(NodeVariables)
			 ->	printITree(ITree,Browser), printAssignments(NodeVariables,PM,B,Browser), 			
			 		printCardinalities(CardVariables,Browser), FinalTree = ITree, FinalCSP = CSP,
			 		LastStates=States, LastCurrentLastAction=LastAction
			 ;		send(Browser, append, '\nBacktrack after labeling failure!!!\n'),
			 		backtrack(States,LastAction,PM,B,MinNodes,MaxNodes,Browser,FinalTree, FinalCSP)
			)
	 ;		send(Browser, append, '\nBacktrack after propagation failure!!!\n'),
			backtrack(States,LastAction,PM,B,MinNodes,MaxNodes,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction)
	).
	
labelNodeVariables(NodeVariables) :-
	assoc_to_values(NodeVariables,FDVariablesDomains),
	extractVariables(FDVariablesDomains,[],Variables),
	labeling([ff],Variables).
	
extractVariables([],Variables,Variables).
extractVariables([FDV-_|FDVariablesDomains],Acc,Variables) :-
	integer(FDV),
	extractVariables(FDVariablesDomains,Acc,Variables).
extractVariables([FDV-_|FDVariablesDomains],Acc,Variables) :- 
	\+ integer(FDV),
	extractVariables(FDVariablesDomains,[FDV|Acc],Variables).

% lastActionConstraints/4 computes constraints (of the form FDCardVariable #= CardValue) related to performed actions
lastActionConstraints(_,_,noaction,[]).
lastActionConstraints(CSP,States,LastAction,ActionConstraints) :-
	csp_cspVariables(CSP,_-CardVariables),
	lastActionConstraintsAux(CardVariables,LastAction,AC),
	lastActionConstraints(States,CardVariables,[],ACs),
	append(AC,ACs,ActionConstraints).
	
lastActionConstraints([],_,ACs,ACs).
lastActionConstraints([_-_-_-_-LastAction|States],CardVariables,Acc,ACs) :-
	lastActionConstraintsAux(CardVariables,LastAction,AC),
	append(AC,Acc,NewAcc),
	lastActionConstraints(States,CardVariables,NewAcc,ACs).

lastActionConstraintsAux(_,noaction,[]).
lastActionConstraintsAux(_,[],[]).
lastActionConstraintsAux(CardVariables,Node-NodeID-RelationName-ChildNode-CardValue,[FDCVar #= CardValue]) :-
	gen_assoc(RelationName-Node-NodeID-ChildNode-_,CardVariables,FDCVar-_).
	
% notYetConstraint/4 compute a constraint of the form CardVariables1 #> 0 #\/ ... #\/ CardVariablesN #> 0 involving variables related to
% cardinalities of not yet considered relations
notYetConstraint(CSP,States,LastAction,[NotYet]) :-
	csp_cspVariables(CSP,_-CardVariables),
	assoc_to_list(CardVariables,CardVariablesList),
	notYetConstraintAux(CardVariablesList,States,LastAction,NotYet).
	
notYetConstraintAux([],_,_,1 #= 1).
notYetConstraintAux([RelationName-Node-NodeID-ChildNode-_-_|CardVariablesList],States,Node-NodeID-RelationName-ChildNode-_,NotYet) :-
		notYetConstraintAux(CardVariablesList,States,Node-NodeID-RelationName-ChildNode-_,NotYet).
notYetConstraintAux([RName-N-NID-CNode-_-(FDV-_)|CardVariablesList],States,Node-NodeID-RelationName-ChildNode-_,NotYet) :-
	(RName \= RelationName; N \= Node; NID \= NodeID; CNode \= ChildNode),
	((integer(FDV) ; member(_-_-_-_-(N-NID-RName-CNode-_),States))
	 ->	notYetConstraintAux(CardVariablesList,States,Node-NodeID-RelationName-ChildNode-_,NotYet)
	 ;		notYetConstraint(CardVariablesList,States,Node-NodeID-RelationName-ChildNode-_,FDV #> 0,NotYet)
	).
notYetConstraintAux(CardVariablesList,[],[],NotYet) :- notYetConstraintAux(CardVariablesList,[],[],1#=1,NotYet).
notYetConstraintAux([],[],[],NotYet,NotYet).
notYetConstraintAux([_-_-_-_-_-(FDV-_)|CardVariablesList],[],[],Acc,NotYet) :-
	notYetConstraintAux(CardVariablesList,[],[],Acc #\/ FDV #> 0,NotYet).
	
notYetConstraint([],_,_,NotYet,NotYet).
notYetConstraint([RelationName-Node-NodeID-ChildNode-_-_|CardVariablesList],States,Node-NodeID-RelationName-ChildNode-_,Acc,NotYet) :-
	notYetConstraint(CardVariablesList,States,Node-NodeID-RelationName-ChildNode-_,Acc,NotYet).
notYetConstraint([RName-N-NID-CNode-_-(FDV-_)|CardVariablesList],States,Node-NodeID-RelationName-ChildNode-_,Acc,NotYet) :-
	(RName \= RelationName; N \= Node; NID \= NodeID; CNode \= ChildNode),
	((integer(FDV) ; member(_-_-_-_-(N-NID-RName-CNode-_),States))
	 ->	notYetConstraint(CardVariablesList,States,Node-NodeID-RelationName-ChildNode-_,Acc,NotYet)
	 ;		notYetConstraint(CardVariablesList,States,Node-NodeID-RelationName-ChildNode-_,Acc #\/ FDV #> 0,NotYet)
	).

% cardZeroConstraints/4 computes constraints (of the form FDCardVariable #= 0) related to relations whose cardinality has to be equal to zero in
% order to obtain an instance
cardZeroConstraints(CardVariables,ITree,States,LastAction,CardZeroConstraints) :-
	assoc_to_list(CardVariables,CardVariablesList),
	iTree_iHasPartRelations(ITree,IHasPartRelations),
	cardZeroConstraints(CardVariablesList,IHasPartRelations,States,LastAction,[],CardZeroConstraints).
	
cardZeroConstraints([],_,_,_,CardZeroConstraints,CardZeroConstraints).
cardZeroConstraints([RelationName-Node-NodeID-ChildNode-_-_|CardVariablesList],IHasPartRelations,States,Node-NodeID-RelationName-ChildNode-_,Acc,CardZeroConstraints) :-
	cardZeroConstraints(CardVariablesList,IHasPartRelations,States,Node-NodeID-RelationName-ChildNode-_,Acc,CardZeroConstraints).
cardZeroConstraints([RelationName-Node-NodeID-ChildNode-_-(FDV-_)|CardVariablesList],IHasPartRelations,States,LastAction,Acc,CardZeroConstraints) :-
	(member(RelationName-Node-NodeID-ChildNode-_,IHasPartRelations)
		->	cardZeroConstraints(CardVariablesList,IHasPartRelations,States,LastAction,Acc,CardZeroConstraints)
		;	cardZeroConstraints(CardVariablesList,IHasPartRelations,States,LastAction,[FDV #= 0|Acc],CardZeroConstraints)
	).
/*
cardZeroConstraints([RName-N-NID-CNode-_-(FDV-_)|CardVariablesList],IHasPartRelations,States,Node-NodeID-RelationName-ChildNode-_,Acc,CardZeroConstraints) :-
	(RName \= RelationName; N \= Node; NID \= NodeID; CNode \= ChildNode),
	((integer(FDV) ; member(_-_-_-_-(N-NID-RName-CNode-_),States))
	 ->	cardZeroConstraints(CardVariablesList,IHasPartRelations,States,Node-NodeID-RelationName-ChildNode-_,Acc,CardZeroConstraints)
	 ;		cardZeroConstraints(CardVariablesList,IHasPartRelations,States,Node-NodeID-RelationName-ChildNode-_,[FDV #= 0|Acc],CardZeroConstraints)
	).
cardZeroConstraints([RName-N-NID-CNode-_-(FDV-_)|CardVariablesList],IHasPartRelations,States,[],Acc,CardZeroConstraints) :-
	((integer(FDV) ; member(_-_-_-_-(N-NID-RName-CNode-_),States))
	 ->	cardZeroConstraints(CardVariablesList,IHasPartRelations,States,[],Acc,CardZeroConstraints)
	 ;		cardZeroConstraints(CardVariablesList,IHasPartRelations,States,[],[FDV #= 0|Acc],CardZeroConstraints)
	).
*/			
% backtrack/6 backtracks to the first state in state stack whose node pool is not empty after value removal
backtrack(States,CurrentLastAction,PM,B,MinNodes,MaxNodes,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction) :-
	restoreState(States,CurrentLastAction,NewStates,ITree-LastNodeInstance-CardVariables-NodePool-LastAction),
	(NewStates \= []
	 -> addNodes(ITree,CardVariables,NodePool,LastAction,LastNodeInstance,PM,B,MinNodes,MaxNodes,NewStates,back,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction)
	 ;  false
	).
		
% restoreState/4 retrives first state whose node pool is not empty after value removal
restoreState([],_,[],_).
restoreState([_-_-_-_-LastAction|States],LastAction,NewStates,State) :-
	restoreState(States,LastAction,NewStates,State).
restoreState([ITree-LastNodeInstance-CardVariables-NodePool-LastAction|States],CurrentLastAction,NewStates,State) :-
	removeFromPool(NodePool,CurrentLastAction,[],NewNodePool),
	( NewNodePool \= []
	  ->	State = ITree-LastNodeInstance-CardVariables-NewNodePool-LastAction,
			NewStates = [State|States]
	  ;	restoreState(States,LastAction,NewStates,State)
	).

% removeFromPool/4 removes from the node pool the element/s of the choice that lead to a failure
removeFromPool([],_,NewNodePool,NewNodePool).	
removeFromPool([N-NID-RelationsGtZero-RelationsGeqZero|NodePool],Node-NodeID-RelationName-ChildNode-CardValue,Acc,NewNodePool) :-
	(N \= Node; NID \= NodeID),
	append(Acc,[N-NID-RelationsGtZero-RelationsGeqZero],NewAcc),
	removeFromPool(NodePool,Node-NodeID-RelationName-ChildNode-CardValue,NewAcc,NewNodePool).
removeFromPool([Node-NodeID-RelationsGtZero-RelationsGeqZero|NodePool],Node-NodeID-RelationName-ChildNode-CardValue,Acc,NewNodePool) :-
	(member(RelationName-Node-ChildNode-_-_,RelationsGtZero)
	 ->	updateRelationsGtZero(RelationsGtZero,Node-NodeID-RelationName-ChildNode-CardValue,[],NewRelationsGtZero),
			NewRelationsGeqZero = RelationsGeqZero
	 ;	updateRelationsGeqZero(RelationsGeqZero,RelationsGtZero,Node-NodeID-RelationName-ChildNode-CardValue,NewRelationsGtZero,[],NewRelationsGeqZero)
	),
	( NewRelationsGtZero = [], NewRelationsGeqZero = []
	  ->	append(Acc,NodePool,NewAcc),
			removeFromPool([],_,NewAcc,NewNodePool)
	  ;	append(Acc,[Node-NodeID-NewRelationsGtZero-NewRelationsGeqZero],AuxAcc),
			append(AuxAcc,NodePool,NewAcc),
			removeFromPool([],_,NewAcc,NewNodePool)
	).
	
updateRelationsGtZero([],_,NewRelationsGtZero,NewRelationsGtZero).
updateRelationsGtZero([RName-N-C-Card-CurrentCardFD|RelationsGtZero],Node-NodeID-RelationName-ChildNode-CardValue,Acc,NewRelationsGtZero) :-
	(RName \= RelationName ; N \= Node ; C \= ChildNode),
	append(Acc,[RName-N-C-Card-CurrentCardFD],NewAcc),
	updateRelationsGtZero(RelationsGtZero,Node-NodeID-RelationName-ChildNode-CardValue,NewAcc,NewRelationsGtZero).
updateRelationsGtZero([RelationName-Node-ChildNode-Card-CurrentCardFD|RelationsGtZero],Node-_-RelationName-ChildNode-CardValue,Acc,NewRelationsGtZero) :-
	( removeFromDomain(CurrentCardFD,CardValue,NewCardFD)
	  ->	append(Acc,[RelationName-Node-ChildNode-Card-NewCardFD],AuxAcc),
			append(AuxAcc,RelationsGtZero,NewAcc),
			updateRelationsGtZero([],_,NewAcc,NewRelationsGtZero)
	  ;	append(Acc,RelationsGtZero,NewAcc),
			updateRelationsGtZero([],_,NewAcc,NewRelationsGtZero)
	).
	
updateRelationsGeqZero([],NewRelationsGtZero,_,NewRelationsGtZero,NewRelationsGeqZero,NewRelationsGeqZero).
updateRelationsGeqZero([RName-N-C-Card-CurrentCardFD|RelationsGeqZero],AccGt,Node-NodeID-RelationName-ChildNode-CardValue,NewRelationsGtZero,AccGeq,NewRelationsGeqZero) :-
	(RName \= RelationName ; N \= Node ; C \= ChildNode),
	append(AccGeq,[RName-N-C-Card-CurrentCardFD],NewAccGeq),
	updateRelationsGeqZero(RelationsGeqZero,AccGt,Node-NodeID-RelationName-ChildNode-CardValue,NewRelationsGtZero,NewAccGeq,NewRelationsGeqZero).
updateRelationsGeqZero([RelationName-Node-ChildNode-Card-CurrentCardFD|RelationsGeqZero],AccGt,Node-_-RelationName-ChildNode-CardValue,NewRelationsGtZero,AccGeq,NewRelationsGeqZero) :-
	( removeFromDomain(CurrentCardFD,CardValue,NewCardFD)
	  ->	(	zeroInFD(NewCardFD)
				->	( NewCardFD \= 0..0
					  ->	append(AccGeq,[RelationName-Node-ChildNode-Card-NewCardFD],AuxAcc),
							append(AuxAcc,RelationsGeqZero,NewAccGeq),
							updateRelationsGeqZero([],AccGt,_,NewRelationsGtZero,NewAccGeq,NewRelationsGeqZero)
					  ;   append(AccGeq,RelationsGeqZero,NewAccGeq),
							updateRelationsGeqZero([],AccGt,_,NewRelationsGtZero,NewAccGeq,NewRelationsGeqZero)
					)
				;	append(AccGt,[RelationName-Node-ChildNode-Card-NewCardFD],NewAccGt),
					append(AccGeq,RelationsGeqZero,NewAccGeq),
					updateRelationsGeqZero([],NewAccGt,_,NewRelationsGtZero,NewAccGeq,NewRelationsGeqZero)
			)
	  ;	append(AccGeq,RelationsGeqZero,NewAccGeq),
			updateRelationsGeqZero([],AccGt,_,NewRelationsGtZero,NewAccGeq,NewRelationsGeqZero)
	).
	
removeFromDomain(FD,Value,NewFD) :-
	X in FD,
	X #\= Value,
	fd_dom(X,NewFD).

% propagate_constraints/1 posts constraints contained in a list
propagate_constraints([]).
propagate_constraints([C|Constraints]) :-
	C,
	propagate_constraints(Constraints).
	
% instantiateNodePool/3 instantiates the node pool (list of Node-NodeID-RelationsGtZero-RelationsGeqZero)
instantiateNodePool(ITree,PM,NodePool) :-
	iTree_iNodes(ITree,INodes),
	productModel_pmTypes(PM,PMTypesAssocList),
	productModel_pmHasPartRelations(PM,PMHasPartRelations),
	instantiateNodePool(INodes,PMHasPartRelations,PMTypesAssocList,[],NodePool).

instantiateNodePool([],_,_,NodePool,NodePool).
instantiateNodePool([Node-NodeID|INodes],PMHasPartRelations,PMTypesAssocList,Acc,NodePool) :-
	findall(RelationName-Node-ChildNode-Card-CardFD,
			  (gen_assoc(RelationName-Node-ChildNode,PMHasPartRelations,(Card,CardType)-_),
			   get_assoc(CardType,PMTypesAssocList,CardFD),
			   \+ zeroInFD(CardFD)
			  ),
			  RelationsGtZero),
	findall(RelationName-Node-ChildNode-Card-CardFD,
			  (gen_assoc(RelationName-Node-ChildNode,PMHasPartRelations,(Card,CardType)-_),
			   get_assoc(CardType,PMTypesAssocList,CardFD),
			   zeroInFD(CardFD)
		     ),
			  RelationsGeqZero),
	((RelationsGtZero \= [] ; RelationsGeqZero \= [])
	 -> instantiateNodePool(INodes,PMHasPartRelations,PMTypesAssocList,[Node-NodeID-RelationsGtZero-RelationsGeqZero|Acc],NodePool)
	 ;  instantiateNodePool(INodes,PMHasPartRelations,PMTypesAssocList,Acc,NodePool)
	).
	
zeroInFD(FD) :-
	X in FD,
	fd_inf(X,0).
	
% addToPool/4 adds new nodes to the node pool
addToPool(NodePool,NewNodes,PM,NewNodePool) :-
	productModel_pmTypes(PM,PMTypesAssocList),
	productModel_pmHasPartRelations(PM,PMHasPartRelations),
	instantiateNodePool(NewNodes,PMHasPartRelations,PMTypesAssocList,NodePool,NewNodePool).
	
% updatePool/3 updates the content of the node pool according to cardinality variable domains
updatePool(NodePool,CardVariables,NewNodePool) :-
	%csp_cspVariables(CSP,_-CardVariables),
	updatePool(NodePool,CardVariables,[],NewNodePool).
	
updatePool([],_,NewNodePool,NewNodePool).
updatePool([Node-NodeID-RelationsGtZero-RelationsGeqZero|NodePool],CardVariables,Acc,NewNodePool) :-
	updateRelationsCardFD(NodeID,CardVariables,RelationsGtZero,RelationsGeqZero,[],NewRelationsGtZero,[],NewRelationsGeqZero),
	( (NewRelationsGtZero = [], NewRelationsGeqZero = [])
	  -> updatePool(NodePool,CardVariables,Acc,NewNodePool)
	  ;  updatePool(NodePool,CardVariables,[Node-NodeID-NewRelationsGtZero-NewRelationsGeqZero|Acc],NewNodePool)
	).
	
updateRelationsCardFD(_,_,[],[],NewRelationsGtZero,NewRelationsGtZero,NewRelationsGeqZero,NewRelationsGeqZero).
updateRelationsCardFD(NodeID,CardVariables,[RelationName-Node-ChildNode-Card-CurrentCardFD|RelationsGtZero],[],Acc,NewRelationsGtZero,NewRelationsGeqZero,NewRelationsGeqZero) :-
	get_assoc(RelationName-Node-NodeID-ChildNode-Card,CardVariables,FDV-_), %fd_dom(FDV,CardFD),
	( \+ integer(FDV)
	  ->	updateRelationsCardFD(NodeID,CardVariables,RelationsGtZero,[],[RelationName-Node-ChildNode-Card-CurrentCardFD|Acc],NewRelationsGtZero,NewRelationsGeqZero,NewRelationsGeqZero)
	  ; updateRelationsCardFD(NodeID,CardVariables,RelationsGtZero,[],Acc,NewRelationsGtZero,NewRelationsGeqZero,NewRelationsGeqZero)
	).
updateRelationsCardFD(NodeID,CardVariables,RelationsGtZero,[RelationName-Node-ChildNode-Card-CurrentCardFD|RelationsGeqZero],AccGt,NewRelationsGtZero,AccGeq,NewRelationsGeqZero) :-
	get_assoc(RelationName-Node-NodeID-ChildNode-Card,CardVariables,FDV-_), %fd_dom(FDV,CurrentCardFD),
	( \+ integer(FDV), \+ CurrentCardFD = 0..0
	  -> ( zeroInFD(CurrentCardFD)
			 -> updateRelationsCardFD(NodeID,CardVariables,RelationsGtZero,RelationsGeqZero,AccGt,NewRelationsGtZero,[RelationName-Node-ChildNode-Card-CurrentCardFD|AccGeq],NewRelationsGeqZero)
			; updateRelationsCardFD(NodeID,CardVariables,RelationsGtZero,RelationsGeqZero,[RelationName-Node-ChildNode-Card-CurrentCardFD|AccGt],NewRelationsGtZero,AccGeq,NewRelationsGeqZero)
			)
		; updateRelationsCardFD(NodeID,CardVariables,RelationsGtZero,RelationsGeqZero,AccGt,NewRelationsGtZero,AccGeq,NewRelationsGeqZero)
	).
	
% addNodes/11 adds nodes to the instantiated instance tree to obtain an instance having a number of nodes in [MinNodes,MaxNodes]
addNodes(ITree,CardVariables,NodePool,LastAction,LastNodeInstance,PM,B,MinNodes,MaxNodes,States,From,Browser,FinalTree, FinalCSP, LastStates, LastCurrentLastAction) :-
	% Compute a list of relations whose cardinality has been assigned
	(From \= back
	 ->	assigned_cardinalities(CardVariables,ITree,AssignedCardinalities)
	 ;		AssignedCardinalities = []
	),
	term_to_atom(AssignedCardinalities,AC),
	atomic_concat('\nAssigned cardinalities: ',AC,ACMsg),
	send(Browser, append, ACMsg),
	(AssignedCardinalities \= []
 	 ->	send(Browser,append,'\nAdding mandatory nodes\n'),
			% Add new nodes considering assigned cardinality relations and constant cardinality relations related to them
			addAssignedCardinalitiesNodes(AssignedCardinalities,ITree,PM,LastNodeInstance,NewITree,NewNodes,NewLastNodeInstance),
			!,
			iTreeNodesNumber(NewITree,NNumb), atomic_concat('\nNumber of nodes in new tree: ',NNumb,Msg),
			send(Browser, append, Msg),
			(NNumb < MinNodes
			 -> % Create the CSP corresponding to the new instance tree
				 %updateCSP(NewNodes,NewITree,PM,B,_,NewCSP),
				 createCSP(PM,NewITree,B,NewCSP),
				 %writeln('New instance tree'), printITree(NewITree),
				 % Propagate constraints of the new CSP, backtrack in case of failure
				 (propagate_constraints_bt(NewCSP,NewITree,States,LastAction,PM,B,MinNodes,MaxNodes,ExitStatus,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction)
			     ->	(ExitStatus = 0
						 ->	% Add new nodes to the node pool
				 				addToPool(NodePool,NewNodes,PM,NewNodePool), %write('New Pool: '), writeln(NewNodePool),
				 				% Add more nodes
								csp_cspVariables(NewCSP,_-NewCardVariables),
			 	 				addNodes(NewITree,NewCardVariables,NewNodePool,LastAction,NewLastNodeInstance,PM,B,MinNodes,MaxNodes,States,norm,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction)
						 ;		true
						)
				  ;	false
				 )
			 ;	(NNumb =< MaxNodes
	 			 -> % Create the CSP corresponding to the new instance tree
					 %updateCSP(NewNodes,NewITree,PM,B,_,NewCSP),
					 createCSP(PM,NewITree,B,NewCSP),
					 % Label node variables, backtrack in case of failure					 	
					 (labelNodeVariables(NewCSP,NewITree,States,LastAction,PM,B,MinNodes,MaxNodes,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction)
					  ->	true
					  ;	false
					 )
	  		    ;  (send(Browser, append, '\nToo many nodes. Backtrack!!!\n'), backtrack(States,LastAction,PM,B,MinNodes,MaxNodes,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction)
					  ->	true
					  ;	false
					 )
				)
			)
 	 ;		% Update node pool (if this is not a call from backtrack)
			(From \= back
			 ->	updatePool(NodePool,CardVariables,UpdatedPool)%, write('Updated Pool: '), writeln(UpdatedPool)
			 ;		NodePool = UpdatedPool%, writeln('Pool: '), writeln(NodePool)
			),
			(UpdatedPool \= []
			 ->	send(Browser, append, '\nAdding non mandatory nodes\n'),
					% Save current state
					saveState(States,ITree,LastNodeInstance,CardVariables,UpdatedPool,LastAction,NewStates),
					iTreeNodesNumber(ITree,NNumb), atomic_concat('\nNumber of nodes in the tree: ',NNumb,Msg),
					send(Browser, append, Msg),
					(NNumb < MinNodes
			 		 -> % Add n non mandatory node instances
				 		 addNonMandatoryNodes(ITree,PM,LastNodeInstance,UpdatedPool,NewITree,NewNodes,NewLastNodeInstance,NewLastAction),
				 		 !,
				 		 term_to_atom(NewLastAction, NLA),
				 		 atomic_concat('\nAction: ',NLA,ActMsg), send(Browser, append, ActMsg),
				 		 iTreeNodesNumber(NewITree,NewNNumb), 
				 		 atomic_concat('\nNumber of nodes in new tree: ', NewNNumb, NewMsg),
				 		 send(Browser, append, NewMsg),
				 		 (NewNNumb < MinNodes
				  			-> % Create the CSP corresponding to the new instance tree
					  			%updateCSP(NewNodes,NewITree,PM,B,_,NewCSP),
					 			createCSP(PM,NewITree,B,NewCSP),
					  			% Propagate constraints of the new CSP, backtrack in case of failure
				     			(propagate_constraints_bt(NewCSP,NewITree,NewStates,NewLastAction,PM,B,MinNodes,MaxNodes,ExitStatus,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction)
					  			 ->	(ExitStatus = 0
										 ->	% Add new nodes to the node pool
				     							addToPool(UpdatedPool,NewNodes,PM,NewNodePool), %write('New Pool: '), writeln(NewNodePool),
					  							% Add more nodes
												csp_cspVariables(NewCSP,_-NewCardVariables),
				 	  							addNodes(NewITree,NewCardVariables,NewNodePool,NewLastAction,NewLastNodeInstance,PM,B,MinNodes,MaxNodes,NewStates,norm,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction)
										 ;		true
										)
								 ;		false
								)
				  			;  (NewNNumb =< MaxNodes
				 	   			-> % Create the CSP corresponding to the new instance tree										
										%updateCSP(NewNodes,NewITree,PM,B,_,NewCSP),
										createCSP(PM,NewITree,B,NewCSP),
										% Label node variables, backtrack in case of failure
			    						(labelNodeVariables(NewCSP,NewITree,NewStates,NewLastAction,PM,B,MinNodes,MaxNodes,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction)
										 ->	true
										 ;		false
										)
				 	   			;  (send(Browser, append, '\nToo many nodes. Backtrack!!!\n'), backtrack(NewStates,NewLastAction,PM,B,MinNodes,MaxNodes,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction)
										 -> true
										 ;	 false
										)
				 	  			)
				  		 )
			 		 ; % Create the CSP corresponding to the current instance tree
						%updateCSP(_,ITree,PM,B,_,CSP),
						createCSP(PM,ITree,B,CSP),
			    		(labelNodeVariables(CSP,ITree,NewStates,LastAction,PM,B,MinNodes,MaxNodes,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction)
						 ->	true
						 ;		false
					   )
				   )
		    ; (send(Browser, append, '\nEmpty node pool. Backtrack!!!\n'),
		    	 backtrack(States,LastAction,PM,B,MinNodes,MaxNodes,Browser,FinalTree, FinalCSP,LastStates, LastCurrentLastAction)
					-> true
					;	false
				)
			)
	).

% iTreeNodesNumber/2 returns the number of nodes in the instance tree
iTreeNodesNumber(ITree,NNumb) :-
	iTree_iNodes(ITree,INodes),
	length(INodes,NNumb).
	
% assigned cardinalities/3 computes a list of relations whose cardinality variable domain consists of a single element
assigned_cardinalities(CardinalityVariables,ITree,AdjAssignedCardinalities) :-
	%csp_cspVariables(CSP,_-CardinalityVariables),
	findall(RelationName-ParentNode-PID-ChildNode-RelationCard-CardValue,
			  (gen_assoc(RelationName-ParentNode-PID-ChildNode-RelationCard,CardinalityVariables,FDV-_), integer(FDV), FDV = CardValue, CardValue \= 0),
			  AssignedCardinalities
	),
	% Adjust assigned cardinality list
	iTree_iHasPartRelations(ITree,IHasPartRelations),
	adjustAssignedCardinalities(AssignedCardinalities,IHasPartRelations,[],AdjAssignedCardinalities).

adjustAssignedCardinalities([],_,AdjAssignedCardinalities,AdjAssignedCardinalities).
adjustAssignedCardinalities([RelationName-ParentNode-PID-ChildNode-Card-CardValue|AssignedCardinalities],IHasPartRelations,Acc,AdjAssignedCardinalities) :-
	member(RelationName-ParentNode-PID-ChildNode-_,IHasPartRelations)
	-> adjustAssignedCardinalities(AssignedCardinalities,IHasPartRelations,Acc,AdjAssignedCardinalities) 
	; adjustAssignedCardinalities(AssignedCardinalities,IHasPartRelations,[RelationName-ParentNode-PID-ChildNode-Card-CardValue|Acc],AdjAssignedCardinalities).
	
% addAssignedCardinalitiesNodes/7 adds node instances related to relations whose cardinality has become assigned
addAssignedCardinalitiesNodes(AssignedCardinalities,ITree,PM,LastNodeInstance,NewITree,NewNodes,NewLastNodeInstance) :-
	% Generate new nodes considering relations whose cardinality has been assigned
	addAssignedCardinalitiesNodesAux(AssignedCardinalities,LastNodeInstance,NewACLastNodeInstance,[],NewACNodes,[],NewACRelations),
	% Generate new nodes considering constant has part relations for new nodes
	productModel_pmHasPartRelations(PM,PMHasPartRelations),
	instantiateITree(NewACNodes,NewACNodes,NewNodes,NewACLastNodeInstance,NewLastNodeInstance,PMHasPartRelations,NewACRelations,NewRelations),
	% Make the new instance tree
	iTree_iNodes(ITree,INodes),
	iTree_iHasPartRelations(ITree,IHasPartRelations),
	append(INodes,NewNodes,NewINodes),
	append(IHasPartRelations,NewRelations,NewIHasPartRelations),
	make_iTree([iNodes(NewINodes),iHasPartRelations(NewIHasPartRelations)],NewITree).

addAssignedCardinalitiesNodesAux([],LastNodeInstance,LastNodeInstance,NewNodes,NewNodes,NewRelations,NewRelations).
addAssignedCardinalitiesNodesAux([RelationName-ParentNode-PID-ChildNode-_-CardValue|AssignedCardinalities],AccLastNodeInstance,LastNodeInstance,AccNodes,NewNodes,AccRelations,NewRelations) :-
	generateITreeNodesConstantRelations(ParentNode,PID,[RelationName-ParentNode-ChildNode-CardValue],AccLastNodeInstance,NewLastNodeInstance,[],NewNodeInstances,[],NewHasPartRelations),
	append(AccNodes,NewNodeInstances,NewAccNodes),
	append(AccRelations,NewHasPartRelations,NewAccRelations),
	addAssignedCardinalitiesNodesAux(AssignedCardinalities,NewLastNodeInstance,LastNodeInstance,NewAccNodes,NewNodes,NewAccRelations,NewRelations).
	
% addNonMandatoryNodes/8 adds node instances selecting a node from the node pool, a relation (relations whose cardinality is strictly greater
% than 0 are considered first) and a value for its cardinality
addNonMandatoryNodes(ITree,PM,LastNodeInstance,[Node-NodeID-RelationsGtZero-RelationsGeqZero|_],NewITree,NewNodes,NewLastNodeInstance,NewLastAction) :-
	% Generate new nodes considering a node, a relations and a cardinality value
	( RelationsGtZero = []
	  ->	addNonMandatoryNodesAux(Node,NodeID,RelationsGeqZero,LastNodeInstance,NewNLastNodeInstance,NewNodeInstances,NewHasPartRelations,NewLastAction)
	  ;	addNonMandatoryNodesAux(Node,NodeID,RelationsGtZero,LastNodeInstance,NewNLastNodeInstance,NewNodeInstances,NewHasPartRelations,NewLastAction)
	),
	% Generate new nodes considering constant has part relations for new nodes
	productModel_pmHasPartRelations(PM,PMHasPartRelations),
	instantiateITree(NewNodeInstances,NewNodeInstances,NewNodes,NewNLastNodeInstance,NewLastNodeInstance,PMHasPartRelations,NewHasPartRelations,NewRelations),
	iTree_iNodes(ITree,INodes),
	iTree_iHasPartRelations(ITree,IHasPartRelations),
	append(INodes,NewNodes,NewINodes),
	append(IHasPartRelations,NewRelations,NewIHasPartRelations),
	make_iTree([iNodes(NewINodes),iHasPartRelations(NewIHasPartRelations)],NewITree).
	
addNonMandatoryNodesAux(Node,NodeID,Relations,LastNodeInstance,NewLastNodeInstance,NewNodeInstances,NewHasPartRelations,Action) :-
	Relations = [RelationName-Node-ChildNode-_-CardFD|_],
	CardValue in CardFD, CardValue #> 0, indomain(CardValue),
	Action = Node-NodeID-RelationName-ChildNode-CardValue,
	generateITreeNodesConstantRelations(Node,NodeID,[RelationName-Node-ChildNode-CardValue],LastNodeInstance,NewLastNodeInstance,[],NewNodeInstances,[],NewHasPartRelations).
		
% saveState/7 adds to the state stack a new state (if a non mandatory node has been added)
saveState(States,ITree,LastNodeInstance,CardVariables,NodePool,LastAction,NewStates) :-
	(LastAction = []
	 -> 	NewStates = [ITree-LastNodeInstance-CardVariables-NodePool-noaction|States] %States
	 ; 	NewStates = [ITree-LastNodeInstance-CardVariables-NodePool-LastAction|States]
	).
	
% Output predicates
printITree(ITree, Browser) :-
	send(Browser, append, '\nInstance tree\n'),
	iTree_iNodes(ITree,INodes),
	iTree_iHasPartRelations(ITree,IHasPartRelations),
	forall(	member(Node-NodeID,INodes),
				( atomic_list_concat(['Node ',Node,', ID ',NodeID,'\n'], Msg),				  
				  send(Browser, append, Msg) )
	),
	forall(	member(RelationName-ParentNode-ParentID-ChildNode-ChildID,IHasPartRelations),
				( atomic_list_concat(['Relation ',RelationName,' between ',
											 ParentNode,',',ParentID,' and ',ChildNode,',',ChildID,'\n'], Msg),
				  send(Browser, append, Msg) )
	).

printAssignments(NodeVariables,PM,B,Browser) :-
	productModel_pmNodes(PM,PMNodes),
	send(Browser, append, '\nAssignments\n'),
	forall(gen_assoc(Node-ID-Var,NodeVariables,FDVar-_),
			 (get_assoc(Node,PMNodes,Vars-_), get_assoc(Var,Vars,Type), type(Type,string,_)
			   -> gen_assoc(String,B,FDVar), 
			   	atomic_list_concat([Node,',',ID,',',Var,' = ',String,'\n'],Msg),
			   	send(Browser, append, Msg)
				;  atomic_list_concat([Node,',',ID,',',Var,' = ',FDVar,'\n'],Msg),
			   	send(Browser, append, Msg)
			 )
	).
	
printCardinalities(CardVariables,Browser) :-
	send(Browser, append, '\nCardinalities\n'),
	forall(gen_assoc(RelationName-PNode-PID-Child-CVar,CardVariables,FDCVar-_), 
		( atomic_list_concat([RelationName,',',PNode,',',PID,',',Child,',',CVar,' = ',FDCVar,'\n'], Msg),
		  send(Browser, append, Msg)
		)		
	).
	

/*
	NOTE: Currently CSPs are always computed from scratch. Can we compute them incrementally without incurring in problems with previously assigned
	values (e.g.: values assigned through action constraints)?
*/	


% --------------------------------------------------------------------------- %	
%                Predicates for process instance generation						   %
% --------------------------------------------------------------------------- %

generate_process_instance(FinalTree, FinalCSP, PM, B, ProcHierarchy, ModelTemporalConstraints,
								  MaxTime, TimeLimit, OldSolutions, Browser) :-
	% 1. Evaluate coupling constraints to instantiate process variables and multiple instance number variables.
   %	  Create a CSP from process variables and coupling constraints, then label process variables (CHOICE POINT)
   csp_cspVariables(FinalCSP,FinalNodeVariables-FinalCardVariables),
   % Create fd variables for process variables
   fd_process_variables(PM, FDProcVarsAssocList),
   % Create fd variables for multiple instance number variables
   fd_multiple_instance_variables(PM, FDMInstVarsAssocList),
   % Create CLP(FD) constraitns from coupling constraints
	coupling_constraints(FDProcVarsAssocList, FDMInstVarsAssocList, FinalNodeVariables, FinalCardVariables, FinalTree, B, CCs),
 	% Create domain constraints for process variables and multiple instance variables
 	assoc_to_values(FDProcVarsAssocList, FDProcVars),
 	domainConstraints(FDProcVars,[],ProcVarDomConstraints),
 	assoc_to_values(FDMInstVarsAssocList, FDInstVars),
 	domainConstraints(FDInstVars, [], InstVarDomConstraints),
 	!,
 	% Propagate coupling and domain constraints and label process variables and instance number variables
 	(prop_label_process_variables(FDProcVars, FDInstVars, ProcVarDomConstraints, InstVarDomConstraints, CCs, 
 										  NewOldSolutions, OldSolutions)
 	->(
		( instantiate_and_solve(MaxTime, FDProcVarsAssocList, FDMInstVarsAssocList, B, FinalTree, ProcHierarchy, 			
										ModelTemporalConstraints, TimeLimit, Browser)
		-> true
		;	send(Browser, append, '\nSearching for a process.'), 
			remove_product_related_constraints(FinalTree),
			remove_before_constraints(FDMInstVarsAssocList),
		   generate_process_instance(FinalTree, FinalCSP, PM, B, ProcHierarchy, ModelTemporalConstraints, MaxTime, TimeLimit, NewOldSolutions, Browser)
		)
	)
	;	remove_product_related_constraints(FinalTree),
		send(Browser, append, '\nNo process found, searching for a new product.'), false
	).
	
instantiate_and_solve(MaxTime, FDProcVarsAssocList, FDMInstVarsAssocList, B, FinalTree, ProcHierarchy, ModelTemporalConstraints,
							 TimeLimit, Browser) :-
	% 2 - 4. Create a set of activity instances
	activity_instances(MaxTime, FDProcVarsAssocList, FDMInstVarsAssocList, B, ActivityInstancesAssoc, t),
 	add_before_constraints(FDMInstVarsAssocList, NewModelTemporalConstraints, ModelTemporalConstraints),	
	% 5. Encode product related constraints in resource and other constraints
  	encode_product_related_constraints(FinalTree),
  	createPM(B, NewPM),
	% 6. At this point we have a set of activity instance, we instantiate temporal constraints
   instantiate_temporal_constraints(FDProcVarsAssocList, ActivityInstancesAssoc, TemporalConstraints),
   implicit_temporal_constraints(ActivityInstancesAssoc, ImpTemporalConstraints),
   !,
   % 7. Instantiate resource constraints
   send(Browser, append, '\nOperations on resource constraints\n'),
  	instantiate_resource_constraints(TimeLimit, NewPM, FDProcVarsAssocList, ActivityInstancesAssoc, B,
   											ProcHierarchy, NewModelTemporalConstraints,
   											QuantityVarsAssoc, StartEndVars, ResourceConstraints,Browser),
   
	% 8. Instantiate duration constraints
  	once(instantiate_duration_constraints(FDProcVarsAssocList, ActivityInstancesAssoc, QuantityVarsAssoc, DurationConstraints)),
   
   % 9. Propagtion and labeling, in case of failure backtrack
	once(prc_q_constraints(QuantityVarsAssoc, PRCqConstraints)),
	send(Browser, append, '\nProcess generation\n'),		
	(prop_label_process(TimeLimit, ActivityInstancesAssoc, QuantityVarsAssoc, StartEndVars,
							 TemporalConstraints, ImpTemporalConstraints, ResourceConstraints, DurationConstraints, PRCqConstraints,
							 [], Browser)
	->	print_process_assignments(FDProcVarsAssocList, ActivityInstancesAssoc, QuantityVarsAssoc, Browser)
	;	false
	).
	
add_before_constraints(InstVarsAssocList, NewModelTemporalConstraints, ModelTemporalConstraints) :-
	assoc_to_list(InstVarsAssocList,InstVars),
	add_before(InstVars, NewModelTemporalConstraints, ModelTemporalConstraints).

add_before([], NewModelTemporalConstraints, NewModelTemporalConstraints).
add_before([_-(V-_)|InstVs], List, Tail) :-
	V =< 1,
	add_before(InstVs, List, Tail).
add_before([Var-(V-_)|InstVs], [before(A,A)|Tail1], Tail2) :-
	V > 1,
	activity(A,_,_,(Var,_),_),
	assert(temporal_constraint(before(A,A))),
	add_before(InstVs, Tail1, Tail2).
	
remove_before_constraints(InstVarsAssocList) :-
	assoc_to_list(InstVarsAssocList,InstVars),
	remove_before(InstVars).	
	
remove_before([]).
remove_before([_-(V-_)|InstVs]) :-
	V < 1,
	remove_before(InstVs).
remove_before([Var-(V-_)|InstVs]) :-
	V > 1,
	activity(A,_,_,(Var,_),_),
	retract(temporal_constraint(before(A,A))),
	remove_before(InstVs).

% Create finite domain variables for process variables
fd_process_variables(PM,FDProcVarsAssocList) :-
	findall(Var-Type, process_variable(Var,Type), ProcVars),
	productModel_pmTypes(PM,PMTypesAssocList),
	fd_process_variables(ProcVars, FDProcVarsAssocList, t, PMTypesAssocList).

fd_process_variables([], FDProcVarsAssocList, FDProcVarsAssocList, _).
fd_process_variables([Var-Type|ProcVars], FDProcVarsAssocList, AuxAssoc, PMTypesAssocList) :-
	get_assoc(Type, PMTypesAssocList, FD),
	put_assoc(Var, AuxAssoc, _-FD, NewAuxAssoc),
	fd_process_variables(ProcVars, FDProcVarsAssocList, NewAuxAssoc, PMTypesAssocList).
	
% Create finite domain variables for multiple instance variables
fd_multiple_instance_variables(PM, FDMInstVarsAssocList) :-
	findall(Var-Type, activity(_,multi,_,(Var,Type),_), Vars1),
	findall(Var-Type, activity(_,multicomp,_,(Var,Type),_), Vars2),
	append(Vars1, Vars2, Vars),
	productModel_pmTypes(PM,PMTypesAssocList),
	fd_multiple_instance_variables(Vars, FDMInstVarsAssocList, t, PMTypesAssocList).

fd_multiple_instance_variables([], FDMInstVarsAssocList, FDMInstVarsAssocList, _).
fd_multiple_instance_variables([Var-Type|Vars], FDMInstVarsAssocList, AuxAssoc, PMTypesAssocList) :-
	get_assoc(Type, PMTypesAssocList, FD),
	put_assoc(Var, AuxAssoc, _-FD, NewAuxAssoc),
	fd_multiple_instance_variables(Vars, FDMInstVarsAssocList, NewAuxAssoc, PMTypesAssocList).
	
% Create CLP(FD) constraints form coupling constraints
coupling_constraints(FDProcVarsAssocList, FDMInstVarsAssocList, NodeVariables, CardVariables, Tree, B, FDCCs) :-
	findall(CC, coupling_constraint(CC), CouplingConstraints),
	coupling_constraints(CouplingConstraints, 
								FDProcVarsAssocList, FDMInstVarsAssocList, NodeVariables, CardVariables, Tree, B, 
								FDCCs, []).

coupling_constraints([], _, _, _, _, _, _, FDCCs, FDCCs).
coupling_constraints([CC|CCs], ProcVars, InstVars, NodeVars, CardVars, Tree, B, List, Tail2) :-
	pathsCardInvolved(CC,NodesPathsCard),
	iTree_iNodes(Tree,INodes),
	iTree_iHasPartRelations(Tree,IHasPartRelations),
	instancesTuplesC(NodesPathsCard,INodes,IHasPartRelations,InstanceTuples),
	( InstanceTuples = [] -> ITuples = [_] ; ITuples = InstanceTuples),
	fd_coupling_constraint_tuples(CC, ProcVars, InstVars, NodeVars, CardVars, ITuples, B, InstCCs, []),
	append(InstCCs, Tail1, List),
	coupling_constraints(CCs, ProcVars, InstVars,NodeVars, CardVars, Tree, B, Tail1, Tail2).
	
pathsCardInvolved((L,P,C,Card),[(L,P,C,Card)]) :- atom(L), atom(P), atom(C), atom(Card).	
pathsCardInvolved((_,Node,P),[(Node,P)]).
pathsCardInvolved(T,[]) :- integer(T) ; atom(T).

pathsCardInvolved(E,Paths) :-
	E =.. [Op,E1,E2], Op \= valid_tuples, Op \= ',',
	pathsCardInvolved(E1,P1), pathsCardInvolved(E2,P2),
	union(P1,P2,Paths).

pathsCardInvolved(minus_sign(T),Paths) :-
	pathsCardInvolved(T,Paths).
	
pathsCardInvolved(abs(T),Paths) :-
	pathsCardInvolved(T,Paths).

pathsCardInvolved(not(C1),Paths) :-
	pathsInvolved(C1,Paths).
	
instancesTuplesC(NodesPaths,INodes,IHasPartRelations,InstanceTuples) :-
	findall(T,tupleC(NodesPaths,INodes,IHasPartRelations,[],T),InstanceTuples).

tupleC([],_,_,T,T).
tupleC([(_, P, _, _)|Ls],INodes,IHasPartRelations,T,FT) :-
	member(P-ID,INodes),
	tupleC(Ls,INodes,IHasPartRelations,[(P,ID)|T],FT).
tupleC([(N,P)|NsPs],INodes,IHasPartRelations,T,FT) :-
	member(N-ID,INodes),
	checkPath(_,_,P,N,ID,IHasPartRelations),
	tupleC(NsPs,INodes,IHasPartRelations,[(N,ID,P)|T],FT).

fd_coupling_constraint_tuples(_, _, _, _, _, [], _, InstCCs, InstCCs).
fd_coupling_constraint_tuples(CC, ProcVars, InstVars, NodeVars, CardVars, [Nodes|Tuples], B, [InstCC|Tail1], Tail2) :-
	fd_coupling_constraint(CC, ProcVars, InstVars, NodeVars, CardVars, Nodes, B, InstCC),
	fd_coupling_constraint_tuples(CC, ProcVars, InstVars, NodeVars, CardVars, Tuples, B, Tail1, Tail2).

fd_coupling_constraint((Label, P, C, Card), _, _, _, CardVars, Nodes, _, FDCard) :-
	member((P,N),Nodes),
	get_assoc(Label-P-N-C-Card,CardVars,FDCard-_).
fd_coupling_constraint((Var,Node,Path), _, _, NodeVars, _, Nodes, _, FDNodeVar) :-
	member((Node,ID,Path), Nodes),
	get_assoc(Node-ID-Var, NodeVars, FDNodeVar-_).
fd_coupling_constraint(ProcVar, ProcVars, _, _, _, _, _, FDProcVar) :-
	get_assoc(ProcVar, ProcVars, FDProcVar-_).
fd_coupling_constraint(InstVar, _, InstVars, _, _, _, _, FDInstVar) :-
	get_assoc(InstVar, InstVars, FDInstVar-_).
fd_coupling_constraint(Int, _, _, _, _, _, _, Int) :- integer(Int).
fd_coupling_constraint(String, _,_,_,_,_, B, IntString) :-
	atom(String),
	get_assoc(String, B, IntString).

fd_coupling_constraint(C, ProcVars, InstVars, NodeVars, CardVars, Nodes, B, FDC) :-
	C =.. [Op,E1,E2],
	member((Op,FDOp),[(plus,+),(minus,-),(times,*),(frac,/),(mod,mod),(eq,#=)]),
	fd_coupling_constraint(E1, ProcVars, InstVars, NodeVars, CardVars, Nodes, B, FDE1),
	fd_coupling_constraint(E2, ProcVars, InstVars, NodeVars, CardVars, Nodes, B, FDE2),
	FDC =.. [FDOp,FDE1,FDE2].
fd_coupling_constraint(minus_sign(Exp), ProcVars, InstVars, NodeVars, CardVars, Nodes, B, - FDExp) :-
	fd_coupling_constraint(Exp, ProcVars, InstVars, NodeVars, CardVars, Nodes, B, FDExp).
	
% Propagate coupling and domain constraints and label process variables
prop_label_process_variables(FDProcVars, FDInstVars, ProcVarDomConstraints, InstVarDomConstraints, CCs, [Sol|OldSol], OldSol) :-
	append([FDProcVars, FDInstVars], FDVars),
	old_sol_constraints(FDVars, OldSol, OSCs, []),
	append([ProcVarDomConstraints, InstVarDomConstraints, CCs, OSCs], Constraints),
	propagate_constraints(Constraints),
	extractVariables(FDVars, [], Vars),
	labeling([ff],Vars),
	computed_solution(FDVars, Sol, []).

old_sol_constraints(_, [], Cs, Cs).
old_sol_constraints([V-_|FDVars], [[N|OS]|OldSol], [C|Tail1], Tail2) :-
	old_sol_c(FDVars, OS, C, V #\= N),
	old_sol_constraints([V-_|FDVars], OldSol, Tail1, Tail2).

old_sol_c([],[],C,C).
old_sol_c([V-_|FDVars], [N|OS], C, Aux) :-
	NewAux = (Aux #/\ V #\= N),
	old_sol_c(FDVars, OS, C, NewAux).

computed_solution([], Sol, Sol).
computed_solution([V-_|Vs], [V|Tail1], Tail2) :- computed_solution(Vs, Tail1, Tail2).

activity_instances(MaxTime, FDProcVarsAssocList, FDMInstVarsAssocList, B, ActivityInstancesAssoc, AuxAssoc) :-
	% 2. Consider has-to-be-executed and is-absent constraints and create activity instances for activities that have to be 
   %     executed. For multiple instance activities with instance variable not fixed start with minimum value (CHOICE POINT)
   instantiateActivityInstanceSet(MaxTime, FDProcVarsAssocList, FDMInstVarsAssocList, B, ActivityInstancesAssoc_1, AuxAssoc),
   fix_point_activities(FDProcVarsAssocList, B, MaxTime, FDMInstVarsAssocList, ActivityInstancesAssoc, ActivityInstancesAssoc_1).
   
fix_point_activities(FDProcVarsAssocList, B, MaxTime, FDMInstVarsAssocList, Assoc, ActivityInstancesAssoc_1) :-
	% 3. Consider not-co-existent constraints and remove activities if necessary (CHOICE POINT)
   remove_not_coexistent(ActivityInstancesAssoc_2, ActivityInstancesAssoc_1),
   % 4. Consider succeeded_by constraint and add activities if necessary, then repeat step 3
	succeeded_by_activities(FDProcVarsAssocList, B, MaxTime, FDMInstVarsAssocList, ActivityInstancesAssoc_3, ActivityInstancesAssoc_2),
  	( ActivityInstancesAssoc_3 \= ActivityInstancesAssoc_2
   ->	fix_point_activities(FDProcVarsAssocList, B, MaxTime, FDMInstVarsAssocList, Assoc, ActivityInstancesAssoc_3)
   ;	Assoc = ActivityInstancesAssoc_1
	).
	
% Instantiate activity instance set considering is_absent and must_be_executed constraints
instantiateActivityInstanceSet(MaxTime, ProcVars, InstVars, B, ActivityInstancesAssoc, AuxAssoc) :-
	%existing_instances(AuxAssoc, ExistingInstances),
	findall(Activity, temporal_constraint(must_be_executed(Activity)), MustActivities),
	findall(Cond-Activity, temporal_constraint(cond(Cond,must_be_executed(Activity))), CondMustActivities),
	findall(Cond-Activity, temporal_constraint(cond(Cond,is_absent(Activity))), CondIsAbsentActivities),
	to_be_executed(CondMustActivities, 1, ProcVars, B, TrueMustActivities, [], FalseMustActivities, []),
	to_be_executed(CondIsAbsentActivities, 0, ProcVars, B, FalseIsAbsentActivities, [], TrueIsAbsentActivities, []),
	append([MustActivities, TrueMustActivities, FalseIsAbsentActivities], InstActivities),
	append(FalseMustActivities, TrueIsAbsentActivities, _NoInstActivities),
	create_activity_instances(MaxTime, normal, InstActivities, InstActivities, 'main', 0, InstVars, ActivityInstancesAssoc, AuxAssoc).
	
% Check conditions on is_absent and must_be_exeuted constraints and create list of activities to be executed
to_be_executed([], _, _, _, InstActivities, InstActivities, NoInstActivities, NoInstActivities).
to_be_executed([Cond-A|As], TF, ProcVars, B, Activities, Tail2, NoActivities, NTail2) :-
	( check_condition_tf(Cond, ProcVars, B, TF)
	->	Activities = [A|Tail1], NoActivities = NTail1
	;  Activities = Tail1, NoActivities = [A|NTail1]
	),
	to_be_executed(As, TF, ProcVars, B, Tail1, Tail2, NTail1, NTail2).
	
% Create variables for start, end, duration, exec of activities that can/have to be executed
create_activity_instances(_,_,[], _, _, _, _, ActivityInstancesAssoc, ActivityInstancesAssoc).
create_activity_instances(MaxTime, Flag, [A|As], Activities, Proc, IP, InstVars, ActivityInstancesAssoc, AuxAssoc) :-
	Flag = normal,
	( gen_assoc(A-_-_-_, AuxAssoc, _) ; gen_assoc(A, AuxAssoc, _) ),
	create_activity_instances(MaxTime, Flag, As, Activities, Proc, IP, InstVars, ActivityInstancesAssoc, AuxAssoc).
create_activity_instances(MaxTime, Flag, [A|As], Activities, Proc, IP, InstVars, ActivityInstancesAssoc, AuxAssoc) :-
	activity(A,atomic,Proc,_,_),
	Vars = _Start-0..MaxTime-_End-0..MaxTime-_Duration-0..MaxTime-_Exec-1..1,
	put_assoc(A-1-Proc-IP, AuxAssoc, Vars, NewAuxAssoc),
	create_activity_instances(MaxTime, Flag, As, Activities, Proc, IP, InstVars, ActivityInstancesAssoc, NewAuxAssoc).
create_activity_instances(MaxTime, Flag, [A|As], Activities, Proc, IP, InstVars, ActivityInstancesAssoc, AuxAssoc) :-
	activity(A,comp,Proc,_,_),
	Vars = _Start-0..MaxTime-_End-0..MaxTime-_Duration-0..MaxTime-_Exec-1..1,
	put_assoc(A-1-Proc-IP, AuxAssoc, Vars, NewAuxAssoc_1),
	( activity(_, _, A, _, _)
	->	create_activity_instances(MaxTime, Flag, As, Activities, A, 1, InstVars, NewAuxAssoc, NewAuxAssoc_1)
	;	NewAuxAssoc = NewAuxAssoc_1
	),
	create_activity_instances(MaxTime, Flag, As, Activities, Proc, IP, InstVars, ActivityInstancesAssoc, NewAuxAssoc).
create_activity_instances(MaxTime, Flag, [A|As], Activities, Proc, IP, InstVars, ActivityInstancesAssoc, AuxAssoc) :-
	activity(A,multi,Proc,(Var,_),_),
	get_assoc(Var, InstVars, V-FD),
	(
		integer(V), NInst = V
	;
		FD = NInst.._
	),
	create_n_instances(MaxTime, A, NInst, Proc, IP, NewAuxAssoc, AuxAssoc),
	create_activity_instances(MaxTime, Flag, As, Activities, Proc, IP, InstVars, ActivityInstancesAssoc, NewAuxAssoc).
create_activity_instances(MaxTime, Flag, [A|As], Activities, Proc, IP, InstVars, ActivityInstancesAssoc, AuxAssoc) :-
	activity(A,multicomp,Proc,(Var,_),_),
	get_assoc(Var, InstVars, V-FD),
	(
		integer(V), NInst = V
	;
		FD = NInst.._
	),
	create_n_instances(MaxTime, A, NInst, Proc, IP, NewAuxAssoc_1, AuxAssoc),
	findall( A-IA-Proc-IP, gen_assoc(A-IA-Proc-IP, NewAuxAssoc_1, _), MCAs),
	sub_activities(As, A, SubActs, []),
	mc_sub(MaxTime, MCAs, SubActs, SubActs, InstVars, NewAuxAssoc, NewAuxAssoc_1),
	remove_sub_acts(SubActs, As, NewAs),
	create_activity_instances(MaxTime, Flag, NewAs, Activities, Proc, IP, InstVars, ActivityInstancesAssoc, NewAuxAssoc).
create_activity_instances(MaxTime, Flag, [A|As], Activities, 'main', IP, InstVars, ActivityInstancesAssoc, AuxAssoc) :-
	activity(A,_,ProcA,_,_), member(ProcA, Activities),
	%ProcA \= Proc,
	append(As, [A], NewAs),
	create_activity_instances(MaxTime, Flag, NewAs, Activities, 'main', IP, InstVars, ActivityInstancesAssoc, AuxAssoc).
create_activity_instances(MaxTime, Flag, [A|As], Activities, Proc, IP, InstVars, ActivityInstancesAssoc, AuxAssoc) :-
	activity(A,_,ProcA,_,_), ProcA \= Proc, % \+ member(ProcA, Activities),
	%Vars = _Exec-0..0,
	%put_assoc(A, AuxAssoc, Vars, NewAuxAssoc),
	NewAuxAssoc = AuxAssoc, 
	create_activity_instances(MaxTime, Flag, As, Activities, Proc, IP, InstVars, ActivityInstancesAssoc, NewAuxAssoc).
	
remove_sub_acts(SubActs, Acts, NewActs) :-
	list_to_ord_set(SubActs, S1),
	list_to_ord_set(Acts, S2),
	ord_subtract(S2, S1, NewActs).

create_n_instances(_, _A, 0, _, _, ActivityInstancesAssoc, ActivityInstancesAssoc).
create_n_instances(MaxTime, A, 1, Proc, IP, ActivityInstancesAssoc, AuxAssoc) :-
	Vs = _Start-0..MaxTime-_End-0..MaxTime-_Duration-0..MaxTime-_Exec-1..1,
	put_assoc(A-1-Proc-IP, AuxAssoc, Vs, ActivityInstancesAssoc).
create_n_instances(MaxTime, A, NInst, Proc, IP, ActivityInstancesAssoc, AuxAssoc) :-
	NInst > 0,
	Vs = _Start-0..MaxTime-_End-0..MaxTime-_Duration-0..MaxTime-_Exec-1..1,
	put_assoc(A-NInst-Proc-IP, AuxAssoc, Vs, NewAuxAssoc),
	NewNInst is NInst - 1,
	create_n_instances(MaxTime, A, NewNInst, Proc, IP, ActivityInstancesAssoc, NewAuxAssoc).
	
sub_activities([], _, SubActs, SubActs).
sub_activities([B|Bs], A, List, Tail2) :-
	( 	activity(B,_,A,_,_)
	->	List = [B|Tail1]
	; 	List = Tail1
	),
	sub_activities(Bs, A, Tail1, Tail2).
	
mc_sub(_,[], _, _, _, Assoc, Assoc).
mc_sub(MaxTime, [A-IA-_-_|MCAs], As, Activities, InstVars, Assoc, AuxAssoc) :-
	create_activity_instances(MaxTime, multicomp, As, Activities, A, IA, InstVars, NewAuxAssoc, AuxAssoc),
	mc_sub(MaxTime, MCAs, As, Activities, InstVars, Assoc, NewAuxAssoc).

% Remove activity intances considering not_co_existent constraitns
remove_not_coexistent(ActivityInstancesAssoc_2, ActivityInstancesAssoc_1) :-
	findall(A-B, temporal_constraint(not-co-existent-with(A,B)), NCE),
	remove_not_coexistent(NCE, ActivityInstancesAssoc_2, ActivityInstancesAssoc_1).

remove_not_coexistent([], ActivityInstancesAssoc, ActivityInstancesAssoc).
remove_not_coexistent([A-B|NCE], ActivityInstancesAssoc_2, ActivityInstancesAssoc_1) :-
	get_assoc(A-_-_-_, ActivityInstancesAssoc_1, _),
	get_assoc(B-_-_-_, ActivityInstancesAssoc_1, _),
	(temporal_constraint(cond(_,is_absent(A)))
	->	remove_activity(A, ActivityInstancesAssoc_1, NewAssoc)
	;	( temporal_constraint(cond(_,is_absent(B)))
		->	remove_activity(B, ActivityInstancesAssoc_1, NewAssoc)
		;	false
		)
	),
	remove_not_coexistent(NCE, ActivityInstancesAssoc_2, NewAssoc).
remove_not_coexistent([_|NCE], ActivityInstancesAssoc_2, ActivityInstancesAssoc_1) :-
	remove_not_coexistent(NCE, ActivityInstancesAssoc_2, ActivityInstancesAssoc_1).

remove_activity(A, Assoc, NewAssoc) :-
	assoc_to_list(Assoc, ListAssoc),
	rec_delete([A], ListAssoc, NewListAssoc),
	list_to_assoc(NewListAssoc, NewAssoc).

rec_delete([], List, List).
rec_delete([A|Acts], Aux, List) :-
	findall(B, member(B-_-A-_-_, List), AActs),
	delete(Aux, A-_-_-_-_, NewAux),
	append(Acts, AActs, NewActs),
	rec_delete(NewActs, NewAux, List).

% Add activity instances considering succeeded_by constraints
succeeded_by_activities(FDProcVarsAssocList, BS, MaxTime, InstVars, NewAssoc, Assoc) :-
	assoc_to_keys(Assoc, ExistingInstances),	
	findall(A-B, temporal_constraint(succeeded_by(A,B)), Succ_1),
	findall(A-B, 
		(
			temporal_constraint(cond(Cond, succeeded_by(A,B))),
			check_condition_tf(Cond, FDProcVarsAssocList, BS, 1)
		),
		Succ_2
	),
	append(Succ_1, Succ_2, Succ),
	successor_activities(Succ, ExistingInstances, SuccActs, []),
	create_succ_instances(MaxTime, ExistingInstances, SuccActs, InstVars, NewAssoc, Assoc).

successor_activities([], _, Acts, Acts).
successor_activities([A-B|Succs], Existing, List, Tail2) :-
	findall(Proc-IP, member(A-_-Proc-IP, Existing), Procs),
	( Procs \= []
	-> List = [B-Procs|Tail1]
	;  List = Tail1
	),
	successor_activities(Succs, Existing, Tail1, Tail2).
	
create_succ_instances(_, _, [], _, Assoc, Assoc).
create_succ_instances(MaxTime, ExistingInstances, [A-Procs|SuccActs], InstVars, Assoc, AuxAssoc) :-
	create_succ_instances(MaxTime, NewExisting, ExistingInstances, Procs, A, InstVars, NewAuxAssoc, AuxAssoc),
	create_succ_instances(MaxTime, NewExisting, SuccActs, InstVars, Assoc, NewAuxAssoc).
	
create_succ_instances(_, NEI, NEI, [], _, _, Assoc, Assoc).
create_succ_instances(MaxTime, NEI, ExistingInstances, [Proc-IP|Procs], A, InstVars, Assoc, AuxAssoc) :-
	( member(A-_-Proc-IP, ExistingInstances)
	-> NewAuxAssoc = AuxAssoc, NewExisting = ExistingInstances
	;	create_activity_instances(MaxTime, multicomp, [A], [], Proc, IP, InstVars, NewAuxAssoc, AuxAssoc),
		assoc_to_keys(NewAuxAssoc, NewExisting)
	),
	create_succ_instances(MaxTime, NEI, NewExisting, Procs, A, InstVars, Assoc, NewAuxAssoc).

% Instantiate temporal constraints
instantiate_temporal_constraints(FDProcVarAssocList, ActivityInstancesAssoc, TemporalConstraints) :-
	findall(TC, 
			 (
			 	temporal_constraint(TC),
			 	TC \= must_be_executed(_),
			 	TC \= is_absent(_),
			 	TC \= cond(_,must_be_executed(_)),
			 	TC \= cond(_,is_absent(_))
			 ), 
			 TCs),
	instantiate_temporal_constraints(TCs, FDProcVarAssocList, ActivityInstancesAssoc, TemporalConstraints, []).

instantiate_temporal_constraints([], _, _, TemporalConstraints, TemporalConstraints).
instantiate_temporal_constraints([before(A,A)|TCs], FDProcVarAssocList, ActivityInstancesAssoc, List, Tail2) :-
	activity_tuples_aa(A, ActivityInstancesAssoc, Tuples),
	instantiate_tc_t(before(A,A), Tuples, FDProcVarAssocList, ActivityInstancesAssoc, InstTCs, []),
	append(InstTCs, Tail1, List),
	instantiate_temporal_constraints(TCs, FDProcVarAssocList, ActivityInstancesAssoc, Tail1, Tail2).
instantiate_temporal_constraints([TC|TCs], FDProcVarAssocList, ActivityInstancesAssoc, List, Tail2) :-
	once(involved_activities(TC, Activities)),
	is_inst_tc(Activities, ActivityInstancesAssoc),
	activity_tuples(Activities, ActivityInstancesAssoc, Tuples),
	instantiate_tc_t(TC, Tuples, FDProcVarAssocList, ActivityInstancesAssoc, InstTCs, []),
	append(InstTCs, Tail1, List),
	instantiate_temporal_constraints(TCs, FDProcVarAssocList, ActivityInstancesAssoc, Tail1, Tail2).
instantiate_temporal_constraints([TC|TCs], FDProcVarAssocList, ActivityInstancesAssoc, List, Tail) :-
	involved_activities(TC, Activities),
	\+ is_inst_tc(Activities, ActivityInstancesAssoc),
	instantiate_temporal_constraints(TCs, FDProcVarAssocList, ActivityInstancesAssoc, List, Tail).

is_inst_tc(Acts, ActsAssoc) :- forall( member(A, Acts), gen_assoc(A-_-_-_, ActsAssoc, _)).

instantiate_tc_t(_, [], _, _, InstTCs, InstTCs).
instantiate_tc_t(TC, [T|Ts], FDProcVarAssocList, ActivityInstancesAssoc, [ITC|Tail1], Tail2) :-
	instantiate_tc(TC, T, FDProcVarAssocList, ActivityInstancesAssoc, ITC),
	instantiate_tc_t(TC, Ts, FDProcVarAssocList, ActivityInstancesAssoc, Tail1, Tail2).

% Generate tuples of activity instances
activity_tuples(Acts, ActivityAssoc, Tuples) :-
	Acts = [A|_],
	activity(A,_,Proc,_,_),
	( Proc = 'main'
	-> SubProcs = ['main'-0]
	;	findall( Proc-IP, gen_assoc(Proc-IP-_-_, ActivityAssoc, _), SubProcs)
	),
	act_tuple(SubProcs, Acts, ActivityAssoc, Tuples, []).

act_tuple([], _, _, T, T).
act_tuple([Proc-IP|Procs], Acts, Assoc, List, Tail2) :-
	findall(T, a_tuple(Acts, Proc, IP, Assoc, T, []), Tuples),
	append(Tuples, Tail1, List),
	act_tuple(Procs, Acts, Assoc, Tail1, Tail2).

a_tuple([], _, _, _, T, T).
a_tuple([A|As], Proc, IP, Assoc, [A-I-Proc-IP|Tail1], Tail2) :-
	gen_assoc(A-I-Proc-IP, Assoc, _),
	a_tuple(As, Proc, IP, Assoc, Tail1, Tail2).
	
activity_tuples_aa(A, ActivityAssoc, Tuples) :-
	activity(A,_,Proc,_,_),
	( Proc = 'main'
	-> SubProcs = ['main'-0]
	;	findall( Proc-IP, gen_assoc(Proc-IP-_-_, ActivityAssoc, _), SubProcs)
	),
	act_tuple_aa(SubProcs, A, ActivityAssoc, Tuples, []).

act_tuple_aa([], _, _, Tuples, Tuples).
act_tuple_aa([Proc-IP|Procs], A, ActivityAssoc, List, Tail2) :-
	findall(A-IA-Proc-IP, gen_assoc(A-IA-Proc-IP, ActivityAssoc, _), InstsA),
	a_tuple_aa(InstsA, T, []),
	append(T, Tail1, List),
	act_tuple_aa(Procs, A, ActivityAssoc, Tail1, Tail2).
	
a_tuple_aa([], T, T).
a_tuple_aa([_], T, T).
a_tuple_aa([A1-IA1-Proc-IP, A2-IA2-Proc-IP|As], [[A1-IA1-Proc-IP, A2-IA2-Proc-IP]|Tail1], Tail2) :-
	a_tuple_aa([A2-IA2-Proc-IP|As], Tail1, Tail2).

% Activities involved in a temporal constraint
involved_activities(before(A,B), As) :- list_to_ord_set([A,B], As).
involved_activities(after(A,B), As) :- list_to_ord_set([A,B], As).
involved_activities(meets(A,B), As) :- list_to_ord_set([A,B], As).
involved_activities(met_by(A,B), As) :- list_to_ord_set([A,B], As).
involved_activities(overlaps(A,B), As) :- list_to_ord_set([A,B], As).
involved_activities(overlapped_by(A,B), As) :- list_to_ord_set([A,B], As).
involved_activities(during(A,B), As) :- list_to_ord_set([A,B], As).
involved_activities(includes(A,B), As) :- list_to_ord_set([A,B], As).
involved_activities(starts(A,B), As) :- list_to_ord_set([A,B], As).
involved_activities(started_by(A,B), As) :- list_to_ord_set([A,B], As).
involved_activities(finishes(A,B), As) :- list_to_ord_set([A,B], As).
involved_activities(finished_by(A,B), As) :- list_to_ord_set([A,B], As).
involved_activities(equals(A,B), As) :- list_to_ord_set([A,B], As).
involved_activities(succeeded_by(A,B), As) :- list_to_ord_set([A,B], As).

involved_activities(TC, Acts) :-
	TC =.. [Op, C1, C2],
	member(Op, [and, or]),
	involved_activities(C1, A1),
	involved_activities(C2, A2),
	ord_union(A1, A2, Acts).
involved_activities(cond(_,TC), Acts) :- involved_activities(TC, Acts).

% Instantiate a temporal constraint
instantiate_tc(TC, Tuple, _, ActivityInstancesAssoc, InstTC) :-
	( TC = before(A,B) ; TC = after(B,A)),
	member(A-IA-Proc-IP, Tuple),
	member(B-IB-Proc-IP, Tuple),
	(A = B -> IA \= IB ; true),
	get_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, TSA-_-TEA-_-_-_-_-_),
	get_assoc(B-IB-Proc-IP, ActivityInstancesAssoc, TSB-_-_-_-_-_-_-_),
	InstTC = (TSA #< TSB #/\ TEA #< TSB).
instantiate_tc(TC, Tuple, _, ActivityInstancesAssoc, InstTC) :-
	( TC = meets(A,B) ; TC = met_by(B,A) ),
	member(A-IA-Proc-IP, Tuple),
	member(B-IB-Proc-IP, Tuple),
	get_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, TSA-_-TEA-_-_-_-_-_),
	get_assoc(B-IB-Proc-IP, ActivityInstancesAssoc, TSB-_-_-_-_-_-_-_),
	InstTC = (TSA #< TSB #/\ TEA #= TSB).
instantiate_tc(TC, Tuple, _, ActivityInstancesAssoc, InstTC) :-
	( TC = overlaps(A,B) ; TC = overlapped_by(B,A) ),
	member(A-IA-Proc-IP, Tuple),
	member(B-IB-Proc-IP, Tuple),
	get_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, TSA-_-TEA-_-_-_-_-_),
	get_assoc(B-IB-Proc-IP, ActivityInstancesAssoc, TSB-_-TEB-_-_-_-_-_),
	InstTC = (TSA #< TSB #/\ TEA #> TSB #/\ TEA #< TEB).
instantiate_tc(TC, Tuple, _, ActivityInstancesAssoc, InstTC) :-
	( TC = during(A,B) ; TC = includes(B,A) ),
	member(A-IA-Proc-IP, Tuple),
	member(B-IB-Proc-IP, Tuple),
	get_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, TSA-_-TEA-_-_-_-_-_),
	get_assoc(B-IB-Proc-IP, ActivityInstancesAssoc, TSB-_-TEB-_-_-_-_-_),
	InstTC = (TSA #> TSB #/\ TSA #< TEB #/\ TEA #< TEB).
instantiate_tc(TC, Tuple, _, ActivityInstancesAssoc, InstTC) :-
	( TC = starts(A,B) ; TC = started_by(B,A) ),
	member(A-IA-Proc-IP, Tuple),
	member(B-IB-Proc-IP, Tuple),
	get_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, TSA-_-TEA-_-_-_-_-_),
	get_assoc(B-IB-Proc-IP, ActivityInstancesAssoc, TSB-_-TEB-_-_-_-_-_),
	InstTC = (TSA #= TSB #/\ TEA #< TEB).
instantiate_tc(TC, Tuple, _, ActivityInstancesAssoc, InstTC) :-
	( TC = finishes(A,B) ; TC = finished_by(B,A) ),
	member(A-IA-Proc-IP, Tuple),
	member(B-IB-Proc-IP, Tuple),
	get_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, TSA-_-TEA-_-_-_-_-_),
	get_assoc(B-IB-Proc-IP, ActivityInstancesAssoc, TSB-_-TEB-_-_-_-_-_),
	InstTC = (TSA #> TSB #/\ TSA #< TEB #/\ TEA #= TEB).
instantiate_tc(equals(A,B), Tuple, _, ActivityInstancesAssoc, InstTC) :-
	member(A-IA-Proc-IP, Tuple),
	member(B-IB-Proc-IP, Tuple),
	get_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, TSA-_-TEA-_-_-_-_-_),
	get_assoc(B-IB-Proc-IP, ActivityInstancesAssoc, TSB-_-TEB-_-_-_-_-_),
	InstTC = (TSA #= TSB #/\ TEA #= TEB).
instantiate_tc(succeeded_by(A,B), Tuple, _, ActivityInstancesAssoc, InstTC) :-
	member(A-IA-Proc-IP, Tuple),
	member(B-IB-Proc-IP, Tuple),
	get_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, _-_-TEA-_-_-_-_-_),
	get_assoc(B-IB-Proc-IP, ActivityInstancesAssoc, TSB-_-_-_-_-_-_-_),
	InstTC = (TSB #> TEA).

instantiate_tc(and(TC1,TC2), Tuple, FDProcVarAssocList, ActivityInstancesAssoc, InstTC) :-
	instantiate_tc(TC1, Tuple, FDProcVarAssocList, ActivityInstancesAssoc, InstTC1),
	instantiate_tc(TC2, Tuple, FDProcVarAssocList, ActivityInstancesAssoc, InstTC2),
	InstTC = (InstTC1 #/\ InstTC2).
instantiate_tc(or(TC1,TC2), Tuple, FDProcVarAssocList, ActivityInstancesAssoc, InstTC) :-
	instantiate_tc(TC1, Tuple, FDProcVarAssocList, ActivityInstancesAssoc, InstTC1),
	instantiate_tc(TC2, Tuple, FDProcVarAssocList, ActivityInstancesAssoc, InstTC2),
	InstTC = (InstTC1 #\/ InstTC2).
instantiate_tc(cond(if(Cond),TC), Tuple, FDProcVarAssocList, ActivityInstancesAssoc, InstTC) :-
	fd_condition(Cond, FDProcVarAssocList, InstCond),
	instantiate_tc(TC, Tuple, FDProcVarAssocList, ActivityInstancesAssoc, ITC),
	InstTC = (InstCond #==> ITC).
instantiate_tc(cond(iff(Cond),TC), Tuple, FDProcVarAssocList, ActivityInstancesAssoc, InstTC) :-
	fd_condition(Cond, FDProcVarAssocList, InstCond),
	instantiate_tc(TC, Tuple, FDProcVarAssocList, ActivityInstancesAssoc, ITC),
	InstTC = (InstCond #<==> ITC).

% Instantiate duration constraints
instantiate_duration_constraints(FDProcVarsAssocList, ActivityInstancesAssoc, QuantityVarsAssoc, DurationConstraints) :-
	findall(A-DCs, activity(A,_,_,_,DCs), ActivityDCs),
	instantiate_duration_constraints(ActivityDCs, FDProcVarsAssocList, ActivityInstancesAssoc, QuantityVarsAssoc, DurationConstraints, []).

instantiate_duration_constraints([], _, _, _, DurationConstraints, DurationConstraints).
instantiate_duration_constraints([A-DCs|ActivityDCs], FDProcVarsAssocList, ActivityInstancesAssoc, QuantityVarsAssoc, 
											DurationConstraints, Tail2) :-
	findall(A-IA-Proc-IP, gen_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, _), Acts),
	instantiate_dcs_a(Acts, DCs, ActivityInstancesAssoc, FDProcVarsAssocList, QuantityVarsAssoc, InstDCs, []),
	append(InstDCs, Tail1, DurationConstraints),
	instantiate_duration_constraints(ActivityDCs, FDProcVarsAssocList, ActivityInstancesAssoc, QuantityVarsAssoc, Tail1, Tail2).

instantiate_dcs_a([], _, _, _, _, InstDCs, InstDCs).
instantiate_dcs_a([A-IA-Proc-IP|ActsD], DCs, ActivityInstancesAssoc, FDProcVarsAssocList, QuantityVarsAssoc, InstDCs, Tail2) :-
	inst_dcs_a(DCs, A, IA, Proc, IP, ActivityInstancesAssoc, FDProcVarsAssocList, QuantityVarsAssoc, IDCs, []),
	append(IDCs, Tail1, InstDCs),
	instantiate_dcs_a(ActsD, DCs, ActivityInstancesAssoc, FDProcVarsAssocList, QuantityVarsAssoc, Tail1, Tail2).

inst_dcs_a([], _, _, _, _, _, _, _, InstDCs, InstDCs).
inst_dcs_a([DC|DCs], A, IA, Proc, IP, ActivityInstancesAssoc, FDProcVarsAssocList, QuantityVarsAssoc, [InstDC|Tail1], Tail2) :-
	inst_dc(DC, A-IA-Proc-IP, ActivityInstancesAssoc, FDProcVarsAssocList, QuantityVarsAssoc, InstDC),
	inst_dcs_a(DCs, A, IA, Proc, IP, ActivityInstancesAssoc, FDProcVarsAssocList, QuantityVarsAssoc, Tail1, Tail2).

% Instantiate a duration constraint
inst_dc(DVar, A-IA-Proc-IP, ActivityInstancesAssoc, _, _, D) :-
	atomic_list_concat(['d','_',A],DVar), 
	get_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, _-_-_-_-D-_-_-_).
inst_dc(ProcVar, _, _, ProcVarsAssoc, _, FDProcVar) :-
	get_assoc(ProcVar, ProcVarsAssoc, FDProcVar-_).
inst_dc(QVar, A-IA-Proc-IP, _, _, QuantityVarsAssoc, FDQVar) :-
	gen_assoc(A-IA-Proc-IP-_-QVar, QuantityVarsAssoc, FDQVar-_).
inst_dc(Int, _, _, _, _, Int) :- integer(Int).

inst_dc(DC,A-IA-Proc-IP,D,ProcVarsAssoc,QuantityVarsAssoc,InstDC) :-
	DC =.. [Op,C1,C2],
	member((Op,FDOp),[(plus,+),(minus,-),(times,*),(frac,/),(mod,mod),(lt,#<),(leq,#=<),(eq,#=),(geq,#>=),(gt,#>),(neq,#\=)]),
	inst_dc(C1,A-IA-Proc-IP,D,ProcVarsAssoc,QuantityVarsAssoc,InstC1),
	inst_dc(C2,A-IA-Proc-IP,D,ProcVarsAssoc,QuantityVarsAssoc,InstC2),
	InstDC =.. [FDOp,InstC1,InstC2].
inst_dc(abs(Exp),A-IA-Proc-IP,D,ProcVarsAssoc,QuantityVarsAssoc,abs(InstExp)) :-
	inst_dc(Exp,A-IA-Proc-IP,D,ProcVarsAssoc,QuantityVarsAssoc,InstExp).
inst_dc(minus_sign(Exp),A-IA-Proc-IP,D,ProcVarsAssoc,QuantityVarsAssoc,- InstExp) :-
	inst_dc(Exp,A-IA-Proc-IP,D,ProcVarsAssoc,QuantityVarsAssoc,InstExp).
inst_dc(DC,A-IA-Proc-IP,D,ProcVarsAssoc,QuantityVarsAssoc,InstDC) :-
	DC =.. [Op,C1,C2],
	member((Op,FDOp),[(and,#/\),(or,#\/),(impl,#==>)]),
	inst_dc(C1,A-IA-Proc-IP,D,ProcVarsAssoc,QuantityVarsAssoc,InstC1),
	inst_dc(C2,A-IA-Proc-IP,D,ProcVarsAssoc,QuantityVarsAssoc,InstC2),
	InstDC =.. [FDOp,InstC1,InstC2].	
inst_dc(not(DC),A-IA-Proc-IP,D,ProcVarsAssoc,QuantityVarsAssoc,#\ InstDC) :-
	inst_dc(DC,A-IA-Proc-IP,D,ProcVarsAssoc,QuantityVarsAssoc,InstDC).
	
% Implicit temporal constraints
implicit_temporal_constraints(ActivityInstancesAssoc, ImpTemporalConstraints) :-
	assoc_to_list(ActivityInstancesAssoc, ActList),
	implicit_temporal_constraints(ActList, ActivityInstancesAssoc, ImpTemporalConstraints, []).

implicit_temporal_constraints([], _, TCs, TCs).
implicit_temporal_constraints([A|As], ActivityInstancesAssoc, List, Tail2) :-
	A = AName-IA-_-_-(TS-_-TE-_-D-_-_-_),
	( resource_constraint(AName, _, _, _, _)
	->	Cs = [TE #> TS, D #= TE - TS]
	;	Cs = [TE #>= TS, D #= TE - TS]
	),
	activity(AName, Type, _, _, _), 
	( ( Type = comp ; Type = multicomp )
	->	comp_tc(AName, IA, TS, TE, ActivityInstancesAssoc, CompTCs),
		append([Cs, CompTCs], CTCs),
		append(CTCs, Tail1, List)
	;	append(Cs, Tail1, List)
	),
	implicit_temporal_constraints(As, ActivityInstancesAssoc, Tail1, Tail2).
implicit_temporal_constraints([_A|As], ActivityInstancesAssoc, List, Tail2) :-
	implicit_temporal_constraints(As, ActivityInstancesAssoc, List, Tail2).

% We need to keep track (e.g., in activity instance assoc. list) of which comp. activity instance an activity belongs to
comp_tc(A, IA, TS, TE, ActAssoc, CompTCs) :-
	findall(B, activity(B,_,A,_,_), InA),
	times_in_A(InA, A, IA, ActAssoc, StartList, [], EndList, []),
	( StartList \= [], EndList \= []
	->	CompTCs = [minimum(TS,StartList), maximum(TE,EndList)]
	; CompTCs = []
	).
	
times_in_A([], _, _, _, Start, Start, End, End).
times_in_A([B|Bs], A, IA, ActAssoc, [TS|Tail1S], Tail2S, [TE|Tail1E], Tail2E) :-
	gen_assoc(B-_-A-IA, ActAssoc, TS-_-TE-_-_-_-_-_),
	times_in_A(Bs, A, IA, ActAssoc, Tail1S, Tail2S, Tail1E, Tail2E).
times_in_A([B|Bs], A, IA, ActAssoc, Tail1S, Tail2S, Tail1E, Tail2E) :-
	\+ gen_assoc(B-_-A-IA, ActAssoc, _TS-_-_TE-_-_-_-_-_),
	times_in_A(Bs, A, IA, ActAssoc, Tail1S, Tail2S, Tail1E, Tail2E).
	
	
% Instantiate resource constraints and create fd variables for resource quantity variables
instantiate_resource_constraints(TimeLimit, PM, FDProcVarAssocList, ActivityInstancesAssoc, B, ProcHierarchy, TemporalConstraints,
											QuantityVarsAssoc, StartEndVars, ResourceConstraints,Browser) :-										
	productModel_pmTypes(PM,PMTypesAssocList),
	% Find all resources
	findall(R-T-IV, res(R,T,IV), Resources),
	resource_constraints_CSPs(Resources, PMTypesAssocList, FDProcVarAssocList, ActivityInstancesAssoc, B,
										ProcHierarchy, TemporalConstraints,
										QuantityVarsAssoc, t, StartEndVars, t,
										CSPs, []),
	list_to_ord_set(CSPs, OrdCSPs),
	catch(
		call_with_time_limit( TimeLimit, 
				sequential_labeling(OrdCSPs, ActivityInstancesAssoc, QuantityVarsAssoc, StartEndVars, [], ResourceConstraints, [],
				Browser) ),
		time_limit_exceeded,
		(send(Browser, append, '\nTime limit exceeded!\n'), false)).
		
		
quantity_variables([], _, _, QuantityVars, QuantityVars, ActInsts, ActInsts).
quantity_variables([A-_-Q-_|Cs], PMTypesAssocList, ActivityInstancesAssoc, QuantityVars, AssocAux, ActivityInstances, Tail2) :-
	integer(Q),
	findall(A-IA-Proc-IP, gen_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, _), InstsA),
	append(InstsA, Tail1, ActivityInstances),
	quantity_variables(Cs, PMTypesAssocList, ActivityInstancesAssoc, QuantityVars, AssocAux, Tail1, Tail2).
quantity_variables([A-R-Q-_|Cs], PMTypesAssocList, ActivityInstancesAssoc, QuantityVars, AssocAux, ActivityInstances, Tail2) :-
	Q = (V,T),
	get_assoc(T, PMTypesAssocList, FD),
	findall(A-IA-Proc-IP, gen_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, _), InstsA),
	append(InstsA, Tail1, ActivityInstances),
	quantity_variables(InstsA, R, V, FD, NewAssocAux, AssocAux),
	quantity_variables(Cs, PMTypesAssocList, ActivityInstancesAssoc, QuantityVars, NewAssocAux, Tail1, Tail2).
	
quantity_variables([], _, _, _, Assoc, Assoc).
quantity_variables([A-IA-Proc-IP|InstA], R, V, FD, Assoc, AuxAssoc) :-
	put_assoc(A-IA-Proc-IP-R-V, AuxAssoc, _-FD, NewAuxAssoc),
	quantity_variables(InstA, R, V, FD, Assoc, NewAuxAssoc).
	
start_end_quantity([], _, _, Vars, Vars).
start_end_quantity([A-IA-Proc-IP|As], R, FD, Assoc, AuxAssoc) :-
	put_assoc(A-IA-Proc-IP-R, AuxAssoc, _-_-FD, NewAuxAssoc),
	start_end_quantity(As, R, FD, Assoc, NewAuxAssoc).


%%%%%%%
% Generate CSPs (one for each resource) for deteemining qualitative scheduling of activities on resources
resource_constraints_CSPs([], _, _, _, _, _, _,
											QuantityVarsAssoc, QuantityVarsAssoc, 
											StartEndVars, StartEndVars,
											CSPs, CSPs).
resource_constraints_CSPs([R-T-IV|Resources], PMTypesAssocList, FDProcVarAssocList, ActivityInstancesAssoc, B,
											ProcHierarchy, TemporalConstraints,
											QuantityVarsAssoc, AssocAux,
											StartEndVarsAssoc, StartEndAux,
											List, Tail2) :-
	% Find all resource constraints on R whose condition is verified
	findall(A-R-Q-TE, 
			  (
			  	resource_constraint(A, R, Q, TE, Cond),
			  	check_condition_tf(Cond, FDProcVarAssocList, B, 1)
			  ),
			  ConstraintsR
			 ),
	% Generate quantity variables for instances of activities involved in resource constraints
	quantity_variables(ConstraintsR, PMTypesAssocList, ActivityInstancesAssoc, NewAssocAux, AssocAux, ActivityInstances, []),
	( ActivityInstances \= []
	->	% Generate variables for quantity of resource R at start and end of activities
		get_assoc(T, PMTypesAssocList, FD),
		start_end_quantity(ActivityInstances, R, FD, NewStartEndAux, StartEndAux),
		length(ActivityInstances, K), TwoTimesK is 2 * K,
		resource_constraints_CSP(ActivityInstancesAssoc, ActivityInstances, ProcHierarchy, 
										 TemporalConstraints, FDProcVarAssocList, B, TwoTimesK, CSP),
		List = [TwoTimesK-R-FD-IV-ConstraintsR-ActivityInstances-CSP|Tail1],
		% Generate CSP for qualitative scheduling of activities on R
		resource_constraints_CSPs(Resources, PMTypesAssocList, FDProcVarAssocList, ActivityInstancesAssoc, B,
												ProcHierarchy, TemporalConstraints,
											   QuantityVarsAssoc, NewAssocAux, StartEndVarsAssoc, NewStartEndAux, Tail1, Tail2)
	;	resource_constraints_CSPs(Resources, PMTypesAssocList, FDProcVarAssocList, ActivityInstancesAssoc, B,
												ProcHierarchy, TemporalConstraints,
											   QuantityVarsAssoc, AssocAux, StartEndVarsAssoc, StartEndAux, List, Tail2)
	).

% Generate CSP for computing qualitative scheduling of activities on R
resource_constraints_CSP(ActivityInstancesAssoc, ActivityInstances, ProcHierarchy, TemporalConstraints, ProcVars, B, 
								 TwoTimesK, CSP) :-
	% Find constraints on activities in ActivityInstances
	tran_cl_activities(ActivityInstances, TemporalConstraints, ProcVars, B, TranTCs, []),
	find_constraints(ActivityInstances, ProcVars, B, TCsIF),
	append(TCsIF, TranTCs, TCsTranIF),
	% Generate variables for start and end of activities in ActivityInstances
	generate_I_F_vars(ActivityInstances, IFs, []),
	DomIF = 1..TwoTimesK,
	domains_I_F(IFs, DomIF, DomainsIFs, []),
	basic_constraints_I_F(IFs, BasicConstraintsIFs, []),
	if_symmetries(IFs, SymConstraints),
	temporal_constraints_I_F(ActivityInstances, TCsTranIF, ActivityInstancesAssoc, IFs, 
									 ProcHierarchy, TemporalConstraints, ProcVars, B, TemporalConstraintsIFs),
	append([DomainsIFs, BasicConstraintsIFs, SymConstraints, TemporalConstraintsIFs], CSPConstraints),
	csp_variables(IFs, CSPVars, []),
	CSP = IFs-CSPVars-CSPConstraints.
	
sequential_labeling([], _, _, _, _, Constraints, Constraints,_).
sequential_labeling([CSP|CSPs], ActivityInstancesAssoc, QuantityVars, StartEndVars, 
						  PreviousCSPs, [Cond, Cons|Tail1], Tail2, Browser) :-
	CSP = TwoTimesK-R-FD-IV-ConstraintsR-ActivityInstances-(IFs-CSPVars-CSPConstraints),
	% Compute constraints for previous CSPs
	list_to_ord_set(ActivityInstances, OrdActivityInstances),
	once(previous_CSP_constraints(PreviousCSPs, OrdActivityInstances, IFs, PrevConstraints, [])),
	append(CSPConstraints, PrevConstraints, CSPPrevConstraints),
	%length(ActivityInstances, K), TwoTimesK is 2 * K,
	% Post CSPs constraints
	atomic_list_concat(['\n', R, '\n'], ResS),
	send(Browser, append, ResS),
	K is TwoTimesK / 2,
	atomic_list_concat([K, ' activitie/s\n'],ActK),
	send(Browser, append, ActK),
	length(CSPPrevConstraints, NCs), atomic_list_concat([NCs, ' constraint/s\n'],NCsC), 
	send(Browser, append, NCsC),
	once(post(CSPPrevConstraints)),
	% Compute a qualitative distribution of activities
	labeling([ffc, up, enum], CSPVars),
	% Compute an implication from the solution
	once(implied_res_constr(IFs, [CSPVars], TwoTimesK,
							 R, FD, IV, ConstraintsR, StartEndVars, ActivityInstancesAssoc, QuantityVars,
							 [Cond #==> Cons], [])),
	% Check if Cons is satisfiable
	once(check_cons(Cons, StartEndVars, QuantityVars)),
	% Solve next CSP
	sequential_labeling(CSPs, ActivityInstancesAssoc, QuantityVars, StartEndVars, 
							  [OrdActivityInstances-IFs|PreviousCSPs], Tail1, Tail2, Browser).
							  
previous_CSP_constraints([], _, _, Constraints, Constraints).
previous_CSP_constraints([Acts-_|PrevSolutions], ActivityInstances, IFs, List, Tail) :-
	ord_intersection(Acts, ActivityInstances, I),
	( I = [] ; I = [_]),
	previous_CSP_constraints(PrevSolutions, ActivityInstances, IFs, List, Tail).
previous_CSP_constraints([Acts-PrevIFs|PrevSolutions], ActivityInstances, IFs, List, Tail2) :-
	ord_intersection(Acts, ActivityInstances, I),
	I \= [], I \= [_],
	prev_constraints(I, PrevIFs, IFs, Constraints, []),
	append(Constraints,Tail1,List),
	previous_CSP_constraints(PrevSolutions, ActivityInstances, IFs, Tail1, Tail2).

prev_constraints([], _, _, Constraints, Constraints).	
prev_constraints([A-IA-P-IP|Activities], PrevIFs, IFs, List, Tail2) :-
	member(A-IA-P-IP-IN-FN, PrevIFs),
	member(A-IA-P-IP-I-F, IFs),
	prev_constraints(IN-FN, I-F, Activities, PrevIFs, IFs, ACs, []),
	append(ACs, Tail1, List),
	prev_constraints(Activities, PrevIFs, IFs, Tail1, Tail2).
	
prev_constraints(_, _, [], _, _, ACs, ACs).
prev_constraints(IN-FN, I-F, [B-IdB-P-IP|Activities], PrevIFs, IFs, List, Tail2) :-
	member(B-IdB-P-IP-INB-FNB, PrevIFs),
	member(B-IdB-P-IP-IB-FB, IFs),
	if_constraint(IN, INB, I, IB, II),
	if_constraint(FN, FNB, F, FB, FF),
	if_constraint(FN, INB, F, IB, FI),
	if_constraint(IN, FNB, I, FB, IF),
	List = [II, FF, FI, IF|Tail1],
	prev_constraints(IN-FN, I-F, Activities, PrevIFs, IFs, Tail1, Tail2).

if_constraint(N1, N2, V1, V2, V1 #> V2) :- N1 > N2.
if_constraint(N1, N2, V1, V2, V1 #< V2) :- N1 < N2.
if_constraint(N, N, V1, V2, V1 #= V2).
	
							  
check_cons(Cons, StartEndVars, QuantityVarsAssoc) :-
	start_end_variables(StartEndVars, SEVars),
	assoc_to_values(QuantityVarsAssoc, QVars),
	append([SEVars, QVars], VarFDs),
	domainConstraints(VarFDs, [], DomainConstraints),
	copy_term([Cons|DomainConstraints], [NewCons|NewDomainConstraints]),
	term_variables(NewCons, Vars),
	post([NewCons|NewDomainConstraints]),
	labeling([leftmost, up, enum], Vars).
%%%%%%%

tran_cl_activities([], _, _, _, TranTCs, TranTCs).							 
tran_cl_activities([A-_-_-_|ActivityInstances], TemporalConstraints, ProcVars, B, List, Tail2) :-
	tran_cl_acts(A, ActivityInstances, TemporalConstraints, ProcVars, B, TCA, []),
	append(TCA, Tail1, List),
	tran_cl_activities(ActivityInstances, TemporalConstraints, ProcVars, B, Tail1, Tail2).

tran_cl_acts(_, [], _, _, _, TCs, TCs).
tran_cl_acts(A, [B-_-_-_|Bs], TemporalConstraints, ProcVars, Strings, [TC|Tail1], Tail2) :-
	relationship_a_b(A, B, TemporalConstraints, ProcVars, Strings, TC), TC \= true,
	tran_cl_acts(A, Bs, TemporalConstraints, ProcVars, Strings, Tail1, Tail2).
tran_cl_acts(A, [_|Bs], TemporalConstraints, ProcVars, Strings, Tail1, Tail2) :-
	tran_cl_acts(A, Bs, TemporalConstraints, ProcVars, Strings, Tail1, Tail2).
	
% Predicates for generating the CSP whose soluitions represents possible disposition of activities on the timeline
generate_I_F_vars([], IFs, IFs).
generate_I_F_vars([A-IA-Proc-IP|ActivityInstances], [A-IA-Proc-IP-_-_|Tail1], Tail2) :-
	generate_I_F_vars(ActivityInstances, Tail1, Tail2).


domains_I_F([], _, Domains, Domains).
domains_I_F([_-_-_-_-I-F|IFs], FD, [I in FD, F in FD|Tail1], Tail2) :- domains_I_F(IFs, FD, Tail1, Tail2).

basic_constraints_I_F([], Basic, Basic).
basic_constraints_I_F([_-_-_-_-I-F|IFs], [I #< F|Tail1], Tail2) :- basic_constraints_I_F(IFs, Tail1, Tail2).

if_symmetries(IFs, Symmetries) :-
	extract_IFs(IFs, Vars, []),
	length(Vars, MaxInstant),
	Max is MaxInstant + 1,
	symmetries(Vars, 1, Max, Symmetries).
	
extract_IFs([], Vars, Vars).
extract_IFs([_-_-_-_-IV-FV|IFs], [IV,FV|Tail1], Tail2) :- extract_IFs(IFs, Tail1, Tail2).


temporal_constraints_I_F(ActivityInstances, TCsIF, ActivityInstsAssoc, IFs, 
								 ProcHierarchy, TemporalConstraints, ProcVars, B, TemporalConstraintsIFs) :-
	% Instantiate all temporal constraints that con be instantiated on activities in ActivityInstances
	inst_constraints_if(ActivityInstances, TCsIF, ActivityInstsAssoc, ProcVars, B, IFs, TCs),
	% Determine and instantiate constraints on activities in different sub-processes
	compute_sub_proc_constraints(ActivityInstances, ProcHierarchy, TemporalConstraints, ProcVars, B, IFs, SubTCs),
	append(TCs, SubTCs, TemporalConstraintsIFs).
	
inst_constraints_if(ActivityInstances, TCs, ActivityInstsAssoc, ProcVars, B, IFs, TCsInsts) :-
	inst_constraints_if(TCs, ActivityInstances, ActivityInstsAssoc, ProcVars, B, IFs, TCsInsts, []).
	
inst_constraints_if([], _, _, _, _, _, TCsInsts, TCsInsts).
inst_constraints_if([before(A,A)|TCs], ActivityInstances, ActivityInstsAssoc, ProcVars, B, IFs, List, Tail2) :-
	activity_tuples_aa_ai(A, ActivityInstances, ActivityInstsAssoc, Tuples),
	instantiate_tc_t(before(A,A), Tuples, IFs, InstTCs, []),
	append(InstTCs, Tail1, List),
	inst_constraints_if(TCs, ActivityInstances, ActivityInstsAssoc, ProcVars, B, IFs, Tail1, Tail2).
inst_constraints_if([TC|TCs], ActivityInstances, ActivityInstsAssoc, ProcVars, B, IFs, List, Tail2) :-
	once(involved_activities(TC, Activities)),
	activity_tuples_ai(Activities, ActivityInstances, ActivityInstsAssoc, Tuples),
	instantiate_tc_t(TC, Tuples, IFs, InstTCs, []),
	append(InstTCs, Tail1, List),
	inst_constraints_if(TCs, ActivityInstances, ActivityInstsAssoc, ProcVars, B, IFs, Tail1, Tail2).
	
is_inst_tc_ai(Acts, ActsInst) :- forall( member(A, Acts), member(A-_-_-_, ActsInst)).

find_constraints(ActivityInstances, ProcVars, B, TCs) :-
	findall(TC, 
			 (
			 	temporal_constraint(TC),
			 	TC \= must_be_executed(_),
			 	TC \= is_absent(_),
			 	TC \= cond(_,must_be_executed(_)),
			 	TC \= cond(_,is_absent(_))
			 ), 
			 Ts),
	find_constraints(Ts, ActivityInstances, ProcVars, B, TCs, []).

find_constraints([], _, _, _, TCs, TCs).
find_constraints([T|Ts], ActivityInstances, ProcVars, B, [T|Tail1], Tail2) :-
	once(involved_activities(T, Activities)),
	is_inst_tc_ai(Activities, ActivityInstances),
	is_true_cond_tc(T, ProcVars, B),
	find_constraints(Ts, ActivityInstances, ProcVars, B, Tail1, Tail2).
find_constraints([T|Ts], ActivityInstances, ProcVars, B, List, Tail) :-
	once(involved_activities(T, Activities)),
	(\+ is_inst_tc_ai(Activities, ActivityInstances) ; \+ is_true_cond_tc(T, ProcVars, B)),
	find_constraints(Ts, ActivityInstances, ProcVars, B, List, Tail).

is_true_cond_tc(TC, ProcVars, B) :-
	( TC = cond(Cond, _)
	-> check_condition_tf(Cond, ProcVars, B, 1)
	;	true
	).
	
activity_tuples_ai(Acts, ActivityInsts, ActivityInstsAssoc, Tuples) :-
	Acts = [A|_],
	activity(A,_,Proc,_,_),
	( Proc = 'main'
	-> SubProcs = ['main'-0]
	;	findall( Proc-IP, gen_assoc(Proc-IP-_-_, ActivityInstsAssoc, _), SubProcs)
	),
	act_tuple_ai(SubProcs, Acts, ActivityInsts, Tuples, []).

act_tuple_ai([], _, _, T, T).
act_tuple_ai([Proc-IP|Procs], Acts, ActivityInsts, List, Tail2) :-
	findall(T, a_tuple_ai(Acts, Proc, IP, ActivityInsts, T, []), Tuples),
	append(Tuples, Tail1, List),
	act_tuple_ai(Procs, Acts, ActivityInsts, Tail1, Tail2).

a_tuple_ai([], _, _, _, T, T).
a_tuple_ai([A|As], Proc, IP, ActivityInsts, [A-I-Proc-IP|Tail1], Tail2) :-
	member(A-I-Proc-IP, ActivityInsts),
	a_tuple_ai(As, Proc, IP, ActivityInsts, Tail1, Tail2).
	
activity_tuples_aa_ai(A, ActivityInsts, ActivityAssoc, Tuples) :-
	activity(A,_,Proc,_,_),
	( Proc = 'main'
	-> SubProcs = ['main'-0]
	;	findall( Proc-IP, gen_assoc(Proc-IP-_-_, ActivityAssoc, _), SubProcs)
	),
	act_tuple_aa_ai(SubProcs, A, ActivityInsts, Tuples, []).

act_tuple_aa_ai([], _, _, Tuples, Tuples).
act_tuple_aa_ai([Proc-IP|Procs], A, ActivityInsts, List, Tail2) :-
	findall(A-IA-Proc-IP, member(A-IA-Proc-IP, ActivityInsts), InstsA),
	a_tuple_aa(InstsA, T, []),
	append(T, Tail1, List),
	act_tuple_aa_ai(Procs, A, ActivityInsts, Tail1, Tail2).


instantiate_tc_t(_, [], _, InstTCs, InstTCs).
instantiate_tc_t(TC, [T|Tuples], IFs, [ITC|Tail1], Tail2) :-
	instantiate_tc(TC, T, IFs, ITC),
	instantiate_tc_t(TC, Tuples, IFs, Tail1, Tail2).
	
instantiate_tc(TC, Tuple, IFs, InstTC) :-
	( TC = before(A,B) ; TC = after(B,A)),
	member(A-IdA-Proc-IP, Tuple),
	member(B-IdB-Proc-IP, Tuple),
	(A = B -> IdA \= IdB ; true),
	member(A-IdA-Proc-IP-IA-FA, IFs), 
	member(B-IdB-Proc-IP-IB-_, IFs),
	InstTC = (IA #< IB #/\ FA #< IB).
instantiate_tc(TC, Tuple, IFs, InstTC) :-
	( TC = meets(A,B) ; TC = met_by(B,A) ),
	member(A-IdA-Proc-IP, Tuple),
	member(B-IdB-Proc-IP, Tuple),
	member(A-IdA-Proc-IP-IA-FA, IFs),
	member(B-IdB-Proc-IP-IB-_, IFs),
	InstTC = (IA #< IB #/\ FA #= IB).
instantiate_tc(TC, Tuple, IFs, InstTC) :-
	( TC = overlaps(A,B) ; TC = overlapped_by(B,A) ),
	member(A-IdA-Proc-IP, Tuple),
	member(B-IdB-Proc-IP, Tuple),
	member(A-IdA-Proc-IP-IA-FA, IFs),
	member(B-IdB-Proc-IP-IB-FB, IFs),
	InstTC = (IA #< IB #/\ FA #> IB #/\ FA #< FB).
instantiate_tc(TC, Tuple, IFs, InstTC) :-
	( TC = during(A,B) ; TC = includes(B,A) ),
	member(A-IdA-Proc-IP, Tuple),
	member(B-IdB-Proc-IP, Tuple),
	member(A-IdA-Proc-IP-IA-FA, IFs),
	member(B-IdB-Proc-IP-IB-FB, IFs),
	InstTC = (IA #> IB #/\ IA #< FB #/\ FA #< FB).
instantiate_tc(TC, Tuple, IFs, InstTC) :-
	( TC = starts(A,B) ; TC = started_by(B,A) ),
	member(A-IdA-Proc-IP, Tuple),
	member(B-IdB-Proc-IP, Tuple),
	member(A-IdA-Proc-IP-IA-FA, IFs),
	member(B-IdB-Proc-IP-IB-FB, IFs),
	InstTC = (IA #= IB #/\ FA #< FB).
instantiate_tc(TC, Tuple, IFs, InstTC) :-
	( TC = finishes(A,B) ; TC = finished_by(B,A) ),
	member(A-IdA-Proc-IP, Tuple),
	member(B-IdB-Proc-IP, Tuple),
	member(A-IdA-Proc-IP-IA-FA, IFs),
	member(B-IdB-Proc-IP-IB-FB, IFs),
	InstTC = (IA #> IB #/\ IA #< FB #/\ FA #= FB).
instantiate_tc(equals(A,B), Tuple, IFs, InstTC) :-
	member(A-IdA-Proc-IP, Tuple),
	member(B-IdB-Proc-IP, Tuple),
	member(A-IdA-Proc-IP-IA-FA, IFs),
	member(B-IdB-Proc-IP-IB-FB, IFs),
	InstTC = (IA #= IB #/\ FA #= FB).
instantiate_tc(succeeded_by(A,B), Tuple, IFs, InstTC) :-
	member(A-IdA-Proc-IP, Tuple),
	member(B-IdB-Proc-IP, Tuple),
	member(A-IdA-Proc-IP-_-FA, IFs),
	member(B-IdB-Proc-IP-IB-_, IFs),
	InstTC = (IB #> FA).

instantiate_tc(and(TC1,TC2), Tuple, IFs, InstTC) :-
	instantiate_tc(TC1, Tuple, IFs, InstTC1),
	instantiate_tc(TC2, Tuple, IFs, InstTC2),
	InstTC = (InstTC1 #/\ InstTC2).
instantiate_tc(or(TC1,TC2), Tuple, IFs, InstTC) :-
	instantiate_tc(TC1, Tuple, IFs, InstTC1),
	instantiate_tc(TC2, Tuple, IFs, InstTC2),
	InstTC = (InstTC1 #\/ InstTC2).
instantiate_tc(cond(if(_),TC), Tuple, IFs, InstTC) :-
	instantiate_tc(TC, Tuple, IFs, InstTC).
instantiate_tc(cond(iff(_),TC), Tuple, IFs, InstTC) :-
	instantiate_tc(TC, Tuple, IFs, InstTC).
	
% Compute the process hierarchy (i.e., an association list Activity - (Level-Process))
process_hierarchy(ProcHierarchy) :- 
	findall(A-T-Proc, activity(A, T, Proc, _, _), Activities),
	process_hierarchy(Activities, 1, main, ProcHierarchy, t).

process_hierarchy(Activities, Level, Process, ProcHierarchy, Aux) :-
	findall(A-T, member(A-T-Process, Activities), ActProc),
	add_to_hierarchy(ActProc, Activities, Level, Process, ProcHierarchy, Aux).
	
add_to_hierarchy([], _, _, _, H, H).
add_to_hierarchy([A-T|ActProc], Activities, Level, Process, H, Aux) :-
	( T = atomic ; T = multi),
	put_assoc(A, Aux, Level-Process, NewAux),
	add_to_hierarchy(ActProc, Activities, Level, Process, H, NewAux).
add_to_hierarchy([A-T|ActProc], Activities, Level, Process, H, Aux) :-
	( T = comp ; T = multicomp),
	put_assoc(A, Aux, Level-Process, NewAux_1),
	NextLevel is Level + 1,
	process_hierarchy(Activities, NextLevel, A, NewAux, NewAux_1),
	add_to_hierarchy(ActProc, Activities, Level, Process, H, NewAux).

% Compute constraints deriving from contraints between composite activities
compute_sub_proc_constraints(ActivityInstances, ProcHierarchy, TemporalConstraints, ProcVars, B, IFs, SubTCs) :-
	% Partition the set of activities into sets of activities belonging to the same sub-process
	activity_proc_sets(ActivityInstances, ProcSets),
	% For each couple of sets, determine temporal constraints between the sets
	temporal_constraint_sets(ProcSets, ProcHierarchy, TemporalConstraints, ProcVars, B, TCSets, []),
	% Instantiate the constraints on minimum/maximum of I, F variables of activities in the sets
	( TCSets = []
	->	SubTCs = []
	; 	instantiate_tc_sets(TCSets, ProcSets, IFs, SubTCs, [])
	).

% Partition the set of activities into sets of activities belonging to the same sub-process
activity_proc_sets(ActivityInstances, ProcSets) :- 
	list_to_ord_set(ActivityInstances, OrdActivityInstances),
	activity_proc_sets(OrdActivityInstances, ProcSets, []).
activity_proc_sets([], ProcSets, ProcSets).
activity_proc_sets([A-IA-Proc-IP|ActivityInstances], [Proc-IP-SetProc|Tail1], Tail2) :-
	findall(B-IB-Proc-IP, member(B-IB-Proc-IP, ActivityInstances), ActProc),
	SetProc = [A-IA-Proc-IP|ActProc],
	list_to_ord_set(ActProc, OrdActProc),
	ord_subtract(ActivityInstances, OrdActProc, NewActivityInstances),
	activity_proc_sets(NewActivityInstances, Tail1, Tail2).

% Determine temporal constraints between sets of activities
temporal_constraint_sets([_], _, _, _, _, TCSets, TCSets).
temporal_constraint_sets([Set|ProcSets], ProcHierarchy, TemporalConstraints, ProcVars, B, [TCSet|Tail1], Tail2) :-
	temporal_constraint_sets(Set, ProcSets, ProcHierarchy, TemporalConstraints, ProcVars, B, TCSet, []),
	temporal_constraint_sets(ProcSets, ProcHierarchy, TemporalConstraints, ProcVars, B, Tail1, Tail2).

temporal_constraint_sets(_, [], _, _, _, _, TCSet, TCSet).
temporal_constraint_sets(Set1, [Set2|ProcSets], ProcHierarchy, TemporalConstraints, ProcVars, B, [ProcTC|Tail1], Tail2) :-
	Set1 = Proc-IP1-_, Set2 = Proc-IP2-_, IP1 \= IP2,
	proc_ancestors(Proc, Proc, ProcHierarchy, Anc1, Anc2),
	relationship_a_b(Anc1, Anc2, TemporalConstraints, ProcVars, B, TC),
	ProcTC = Proc-IP1-Proc-IP2-TC,
	temporal_constraint_sets(Set1, ProcSets, ProcHierarchy, TemporalConstraints, ProcVars, B, Tail1, Tail2).
temporal_constraint_sets(Set1, [Set2|ProcSets], ProcHierarchy, TemporalConstraints, ProcVars, B, List, Tail) :-
	Set1 = Proc-_-_, Set2 = Proc-_-_,
	temporal_constraint_sets(Set1, ProcSets, ProcHierarchy, TemporalConstraints, ProcVars, B, List, Tail).
temporal_constraint_sets(Set1, [Set2|ProcSets], ProcHierarchy, TemporalConstraints, ProcVars, B, [ProcTC|Tail1], Tail2) :-
	Set1 = Proc1-IP1-_, Set2 = Proc2-IP2-_, Proc1 \= Proc2,
	proc_ancestors(Proc1, Proc2, ProcHierarchy, Anc1, Anc2),
	relationship_a_b(Anc1, Anc2, TemporalConstraints, ProcVars, B, TC),
	ProcTC = Proc1-IP1-Proc2-IP2-TC,
	temporal_constraint_sets(Set1, ProcSets, ProcHierarchy, TemporalConstraints, ProcVars, B, Tail1, Tail2).
	
% Given two activities, find two ancestors of these activities at the same level and with the same father in the hierarchy
proc_ancestors(Proc1, Proc2, ProcHierarchy, Anc1, Anc2) :-
	get_assoc(Proc1, ProcHierarchy, LevelP1-FatherP1),
	get_assoc(Proc2, ProcHierarchy, LevelP2-FatherP2),
	( (LevelP1 = LevelP2, FatherP1 = FatherP2)
	->	Anc1 = Proc1, Anc2 = Proc2
	;	(	LevelP1 > LevelP2
		->	proc_ancestors(FatherP1, Proc2, ProcHierarchy, Anc1, Anc2)
		;	proc_ancestors(Proc1, FatherP2, ProcHierarchy, Anc1, Anc2)
		)
	).

% Instantiate constraints between sets of activities	
instantiate_tc_sets([], _, _, SubTCs, SubTCs).
instantiate_tc_sets([Set|TCSets], ProcSets, IFs, List, Tail2) :-
	instantiate_tc_sets(Set, ProcSets, IFs, TCs, []),
	append(TCs, Tail1, List),
	instantiate_tc_sets(TCSets, ProcSets, IFs, Tail1, Tail2).
	
instantiate_tc_sets([], _, _, FDTCs, FDTCs).
instantiate_tc_sets([Proc1-IP1-Proc2-IP2-TC|Set], ProcSets, IFs, List, Tail2) :-
	member(Proc1-IP1-Set1, ProcSets),
	member(Proc2-IP2-Set2, ProcSets),
	min_max_act_set(Set1, IFs, MinVSet1, MaxVSet1, MinConstrSet1, MaxConstrSet1),
	min_max_act_set(Set2, IFs, MinVSet2, MaxVSet2, MinConstrSet2, MaxConstrSet2),
	inst_tc_sets(TC, Proc1-MinVSet1-MaxVSet1, Proc2-MinVSet2-MaxVSet2, FDTC),
	List = [MinConstrSet1, MaxConstrSet1, MinConstrSet2, MaxConstrSet2, FDTC|Tail1],
	instantiate_tc_sets(Set, ProcSets, IFs, Tail1, Tail2).
	
min_max_act_set(Acts, IFs, MinV, MaxV, MinConstr, MaxConstr) :-
	acts_i_vars(Acts, IFs, IVars, []),
	MinConstr = minimum(MinV, IVars),
	acts_f_vars(Acts, IFs, FVars, []),
	MaxConstr = maximum(MaxV, FVars).
	
acts_i_vars([], _, IVars, IVars).
acts_i_vars([A-IA-Proc-IP|Acts], IFs, [I|Tail1], Tail2) :-
	member(A-IA-Proc-IP-I-_, IFs),
	acts_i_vars(Acts, IFs, Tail1, Tail2).

acts_f_vars([], _, FVars, FVars).
acts_f_vars([A-IA-Proc-IP|Acts], IFs, [F|Tail1], Tail2) :-
	member(A-IA-Proc-IP-_-F, IFs), var(F),
	acts_f_vars(Acts, IFs, Tail1, Tail2).	
	
inst_tc_sets(before(A,B), A-IA-FA, B-IB-_, InstTC) :-
	InstTC = (IA #< IB #/\ FA #< IB).
inst_tc_sets(after(A,B), A-IA-_, B-IB-FB, InstTC) :-
	InstTC = (IB #< IA #/\ FB #< IA).
inst_tc_sets(meets(A,B), A-IA-FA, B-IB-_, InstTC) :-
	InstTC = (IA #< IB #/\ FA #= IB).
inst_tc_sets(met_by(A,B), A-IA-_, B-IB-FB, InstTC) :-
	InstTC = (IB #< IA #/\ FB #= IA).
inst_tc_sets(overlaps(A,B), A-IA-FA, B-IB-FB, InstTC) :-
	InstTC = (IA #< IB #/\ FA #> IB #/\ FA #< FB).
inst_tc_sets(overlapped_by(A,B), A-IA-FA, B-IB-FB, InstTC) :-
	InstTC = (IB #< IA #/\ FB #> IA #/\ FB #< FA).
inst_tc_sets(during(A,B), A-IA-FA, B-IB-FB, InstTC) :-
	InstTC = (IA #> IB #/\ IA #< FB #/\ FA #< FB).
inst_tc_sets(includes(A,B), A-IA-FA, B-IB-FB, InstTC) :-
	InstTC = (IB #> IA #/\ IB #< FA #/\ FB #< FA).
inst_tc_sets(starts(A,B), A-IA-FA, B-IB-FB, InstTC) :-
	InstTC = (IA #= IB #/\ FA #< FB).
inst_tc_sets(started_by(A,B), A-IA-FA, B-IB-FB, InstTC) :-
	InstTC = (IB #= IA #/\ FB #< FA).
inst_tc_sets(finishes(A,B), A-IA-FA, B-IB-FB, InstTC) :-
	InstTC = (IA #> IB #/\ IA #< FB #/\ FA #= FB).
inst_tc_sets(finished_by(A,B), A-IA-FA, B-IB-FB, InstTC) :-
	InstTC = (IB #> IA #/\ IB #< FA #/\ FB #= FA).	
inst_tc_sets(equals(A,B), A-IA-FA, B-IB-FB, InstTC) :-
	InstTC = (IA #= IB #/\ FA #= FB).

inst_tc_sets(or(TC1,TC2), A-IA-FA, B-IB-FB, InstTC) :-
	inst_tc_sets(TC1, A-IA-FA, B-IB-FB, InstTC1),
	inst_tc_sets(TC2, A-IA-FA, B-IB-FB, InstTC2),
	InstTC = (InstTC1 #\/ InstTC2).
	
	
csp_variables([], Vars, Vars).
csp_variables([_-_-_-_-I-F|IFs], [I,F|Tail1], Tail2) :- csp_variables(IFs, Tail1, Tail2).

	
post([]).
post([C|Cs]) :-
	C,
	post(Cs).
	
% Compute a constraint of the form COND #==> CONS form each solution of the CSP on I,F variables
implied_res_constr(_, [], _, _, _, _, _, _, _, _, ResConstrR, ResConstrR).
implied_res_constr(IFs, [Sol|CSPSolutions], TwoTimesK,
						 R, FDR, IV, ConstraintsR, StartEndVars, ActivityInstancesAssoc, QuantityAssoc,
						 [IRC|Tail1], Tail2) :-
	activity_order_empty(ActivityOrderEmpty, t, 1, TwoTimesK),
	% Construct an association list that associate instant i (in 1..2*k) with the list of activities ending/starting at i
	activity_order(IFs, Sol, ActivityOrder, ActivityOrderEmpty),
	% Construct the condition on start and end times of activities
	irc_condition(ActivityOrder, ActivityInstancesAssoc, TwoTimesK, IRCcond),
	% Construct the consequence on resource quantity variables and start/end quantity variables
	irc_consequence(ActivityOrder, R, FDR, IV, ConstraintsR, StartEndVars, QuantityAssoc, TwoTimesK, IRCcons),
	IRC = (IRCcond #==> IRCcons),
	%NotIRC = ((#\ IRCcons) #==> (#\ IRCcond)),
	implied_res_constr(IFs, CSPSolutions, TwoTimesK,
						 	 R, FDR, IV, ConstraintsR, StartEndVars, ActivityInstancesAssoc, QuantityAssoc,
						 	 Tail1, Tail2).
						 	 
activity_order_empty(ActEmpty, ActEmpty, N, M) :- N > M.
activity_order_empty(ActEmpty, Aux, N, M) :-
	N =< M,
	put_assoc(N, Aux, [], NewAux),
	NewN is N + 1,
	activity_order_empty(ActEmpty, NewAux, NewN, M).

activity_order([], _, ActOrder, ActOrder).
activity_order([A-IdA-Proc-IP-_-_|IFs], [IA,FA|Sol], ActOrder, AuxOrder) :-
	get_assoc(IA, AuxOrder, ListIA),
	put_assoc(IA, AuxOrder, [A-IdA-Proc-IP-start|ListIA], NewAuxOrder1),
	get_assoc(FA, NewAuxOrder1, ListFA),
	put_assoc(FA, NewAuxOrder1, [A-IdA-Proc-IP-end|ListFA], NewAuxOrder),
	activity_order(IFs, Sol, ActOrder, NewAuxOrder).

% Compute COND
irc_condition(ActivityOrder, ActivityInstancesAssoc, TwoTimesK, Cond) :-
	first_not_empty(1, ActivityOrder, N, List_1),
	irc_cond_L(List_1, ActivityInstancesAssoc, Last, 0, Cond_L),
	M is N + 1,
	irc_cond(ActivityOrder, ActivityInstancesAssoc, Last, M, TwoTimesK, Cond, Cond_L).
	
first_not_empty(N, ActivityOrder, M, List) :-
	get_assoc(N, ActivityOrder, ListN),
	(ListN = []
	->	NewN is N + 1, first_not_empty(NewN, ActivityOrder, M, List)
	;	M = N, List = ListN
	).	
	
irc_cond(_, _, _, N, TwoTimesK, Cond, Cond) :- N > TwoTimesK.
irc_cond(ActivityOrder, ActivityInstancesAssoc, Last, N, TwoTimesK, Cond, CondAux) :-
	N =< TwoTimesK,
	get_assoc(N, ActivityOrder, List),
	( List \= []
	->	irc_cond_L(List, ActivityInstancesAssoc, NewLast, Last, Cond_L),
		NewCondAux = (CondAux #/\ Cond_L)
	;	NewCondAux = CondAux
	),
	NewN is N + 1,
	irc_cond(ActivityOrder, ActivityInstancesAssoc, NewLast, NewN, TwoTimesK, Cond, NewCondAux).

irc_cond_L([A-IA-Proc-IP-start|Ls], ActivityInstancesAssoc, Last, Zero, CondL) :-
	get_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, TS-_-_-_-_-_-_-_),
	( (integer(Zero), Zero = 0)
	->	irc_cond_Ls(Ls, ActivityInstancesAssoc, Last, TS, CondL, Zero #=< TS)
	;	irc_cond_Ls(Ls, ActivityInstancesAssoc, Last, TS, CondL, Zero #< TS) 
	).
irc_cond_L([A-IA-Proc-IP-end|Ls], ActivityInstancesAssoc, Last, Zero, CondL) :-
	get_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, _-_-TE-_-_-_-_-_),
	( (integer(Zero), Zero = 0)
	->	irc_cond_Ls(Ls, ActivityInstancesAssoc, Last, TE, CondL, Zero #=< TE)
	;	irc_cond_Ls(Ls, ActivityInstancesAssoc, Last, TE, CondL, Zero #< TE)
	).

irc_cond_Ls([], _, Last, Last, CondL, CondL).
irc_cond_Ls([A-IA-Proc-IP-start|Ls], ActivityInstancesAssoc, Last, CurrentLast, CondL, CondAux) :-
	get_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, TS-_-_-_-_-_-_-_),
	NewCondAux = (CondAux #/\ CurrentLast #= TS),
	irc_cond_Ls(Ls, ActivityInstancesAssoc, Last, TS, CondL, NewCondAux).
irc_cond_Ls([A-IA-Proc-IP-end|Ls], ActivityInstancesAssoc, Last, CurrentLast, CondL, CondAux) :-
	get_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, _-_-TE-_-_-_-_-_),
	NewCondAux = (CondAux #/\ CurrentLast #= TE),
	irc_cond_Ls(Ls, ActivityInstancesAssoc, Last, TE, CondL, NewCondAux).

% Compute CONS
irc_consequence(ActivityOrder, R, FDR, IV, ConstraintsR, StartEndVars, QuantityAssoc, TwoTimesK, IRCcons) :-
	put_assoc(0, t, VIV-FDR-[IV]-VIV, ConsAssocInit),
	irc_cons_elements(ActivityOrder, R, ConstraintsR, StartEndVars, QuantityAssoc, 1, TwoTimesK, ConsAssoc, ConsAssocInit),
	construct_irc_cons(ConsAssoc, IRCcons).
	
irc_cons_elements(_, _, _, _, _, N, TwoTimesK, ConsAssoc, ConsAssoc) :- N > TwoTimesK.
irc_cons_elements(ActivityOrder, R, ConstraintsR, StartEndVars, QuantityAssoc, N, TwoTimesK, ConsAssoc, ConsAssocAux) :-
	N =< TwoTimesK,
	get_assoc(N, ActivityOrder, List),
	( List \= []
	->	irc_cons_Ls(List, R, ConstraintsR, StartEndVars, QuantityAssoc, N, NewConsAssocAux, ConsAssocAux)
	;	NewConsAssocAux = ConsAssocAux
	),
	NewN is N + 1,
	irc_cons_elements(ActivityOrder, R, ConstraintsR, StartEndVars, QuantityAssoc, NewN, TwoTimesK, ConsAssoc, NewConsAssocAux).
	
irc_cons_Ls(List, R, ConstraintsR, StartEndVars, QuantityAssoc, N, ConsAssoc, ConsAssocAux) :-
	( List \= [L]
	->	separate_start_end(List, ListStart, [], ListEnd, []),
		irc_cons_Ls_SE(ListStart, ListEnd, R, ConstraintsR, StartEndVars, QuantityAssoc, N, ConsAssoc, ConsAssocAux)
	;	List = [L], irc_cons_L(L, R, ConstraintsR, StartEndVars, QuantityAssoc, N, ConsAssoc, ConsAssocAux)
	).
	
irc_cons_L(A-IA-Proc-IP-start, R, ConstraintsR, StartEndVars, QuantityAssoc, N, ConsAssoc, ConsAssocAux) :-
	get_assoc(A-IA-Proc-IP-R, StartEndVars, SA-_-_),
	Last is N - 1,
	get_assoc(Last, ConsAssocAux, _-XLast),
	member(A-R-Q-TE, ConstraintsR),
	( integer(Q)
	->	QA = Q
	; 	Q = (V,_), get_assoc(A-IA-Proc-IP-R-V, QuantityAssoc, QA-_)
	),
	( member(TE,['FromStartToEnd', 'AfterStart'])
	->	put_assoc(N, ConsAssocAux, SA-[XLast, QA]-[]-SA, ConsAssoc)
	;	( TE = 'AfterEnd'
		->	put_assoc(N, ConsAssocAux, SA-[]-[SA #= XLast]-SA, ConsAssoc)
		;	( TE = 'BeforeStart'
			->	put_assoc(N, ConsAssocAux, SA-[XLast,-1 * QA]-[]-SA, ConsAssoc_1),
				get_assoc(0, ConsAssoc_1, VIV-FDR-ListIV),
				put_assoc(0, ConsAssoc_1, VIV-FDR-[QA|ListIV], ConsAssoc)
			; ( member(TE,['BeforeEnd', 'Always'])
			  ->	put_assoc(N, ConsAssocAux, SA-[]-[SA #= XLast]-SA, ConsAssoc_1),
			  		get_assoc(0, ConsAssoc_1, VIV-FDR-ListIV-VIV),
					put_assoc(0, ConsAssoc_1, VIV-FDR-[QA|ListIV]-VIV, ConsAssoc)
			  ;	false
			  )
			)
		)
	).
irc_cons_L(A-IA-Proc-IP-end, R, ConstraintsR, StartEndVars, QuantityAssoc, N, ConsAssoc, ConsAssocAux) :-
	get_assoc(A-IA-Proc-IP-R, StartEndVars, _-EA-_),
	Last is N - 1,
	get_assoc(Last, ConsAssocAux, _-XLast),
	member(A-R-Q-TE, ConstraintsR),
	( integer(Q)
	->	QA = Q
	; 	Q = (V,_), get_assoc(A-IA-Proc-IP-R-V, QuantityAssoc, QA-_)
	),
	( member(TE,['FromStartToEnd', 'BeforeEnd'])
	->	put_assoc(N, ConsAssocAux, EA-[XLast, -1 * QA]-[]-EA, ConsAssoc)
	;	( member(TE, ['AfterStart', 'BeforeStart', 'Always'])
		->	put_assoc(N, ConsAssocAux, EA-[]-[EA #= XLast]-EA, ConsAssoc)
		;	( TE = 'AfterEnd'
			->	put_assoc(N, ConsAssocAux, EA-[XLast, QA]-[]-EA, ConsAssoc)
			;	false
			)
		)
	).
	
separate_start_end([], ListStart, ListStart, ListEnd, ListEnd).
separate_start_end([A-IA-Prod-IP-start|Ls], [A-IA-Prod-IP-start|TailS1], TailS2, ListEnd, TailE) :-
	separate_start_end(Ls, TailS1, TailS2, ListEnd, TailE).
separate_start_end([A-IA-Prod-IP-end|Ls], ListStart, TailS, [A-IA-Prod-IP-end|TailE1], TailE2) :-
	separate_start_end(Ls, ListStart, TailS, TailE1, TailE2).
	
irc_cons_Ls_SE(ListStart, ListEnd, R, ConstraintsR, StartEndVars, QuantityAssoc, N, ConsAssoc, ConsAssocAux) :-
	irc_cons_Ls_S(ListStart, R, ConstraintsR, StartEndVars, QuantityAssoc, N, NewConsAssocAux, ConsAssocAux),
	( NewConsAssocAux \= ConsAssocAux
	-> irc_cons_Ls_E(ListEnd, R, ConstraintsR, StartEndVars, QuantityAssoc, N, 1, ConsAssoc, NewConsAssocAux)
	;	irc_cons_Ls_only_E(ListEnd, R, ConstraintsR, StartEndVars, QuantityAssoc, N, 1, ConsAssoc, NewConsAssocAux)
	).
	
irc_cons_Ls_S([], _, _, _, _, _, ConsAssoc, ConsAssoc).
irc_cons_Ls_S([A-IA-Prod-IP-start|Ls], R, ConstraintsR, StartEndVars, QuantityAssoc, N, ConsAssoc, ConsAssocAux) :-
	( get_assoc(N, ConsAssocAux, _)
	-> irc_cons_L_S(A-IA-Prod-IP-start, R, ConstraintsR, StartEndVars, QuantityAssoc, N, NewConsAssocAux, ConsAssocAux)
	; 	irc_cons_L(A-IA-Prod-IP-start, R, ConstraintsR, StartEndVars, QuantityAssoc, N, NewConsAssocAux, ConsAssocAux)
	),
	irc_cons_Ls_S(Ls, R, ConstraintsR, StartEndVars, QuantityAssoc, N, ConsAssoc, NewConsAssocAux).
	
irc_cons_L_S(A-IA-Proc-IP-start, R, ConstraintsR, StartEndVars, QuantityAssoc, N, ConsAssoc, ConsAssocAux) :-
	get_assoc(N, ConsAssocAux, SA1-SumList-EqList-SA1),
	get_assoc(A-IA-Proc-IP-R, StartEndVars, SA-_-_),
	member(A-R-Q-TE, ConstraintsR),
	( integer(Q)
	->	QA = Q
	; 	Q = (V,_), get_assoc(A-IA-Proc-IP-R-V, QuantityAssoc, QA-_)
	),
	( member(TE,['FromStartToEnd', 'AfterStart'])
	->	put_assoc(N, ConsAssocAux, SA1-[QA|SumList]-[SA #= SA1|EqList]-SA1, ConsAssoc)
	;	( TE = 'AfterEnd'
		->	put_assoc(N, ConsAssocAux, SA1-SumList-[SA #= SA1|EqList]-SA1, ConsAssoc)
		;	( TE = 'BeforeStart'
			->	put_assoc(N, ConsAssocAux, SA1-[-1 * QA|SumList]-[SA #= SA1|EqList]-SA1, ConsAssoc_1),
				get_assoc(0, ConsAssoc_1, VIV-FDR-ListIV),
				put_assoc(0, ConsAssoc_1, VIV-FDR-[QA|ListIV], ConsAssoc)
			; ( member(TE,['BeforeEnd', 'Always'])
			  ->	put_assoc(N, ConsAssocAux, SA-SumList-[SA #= SA1|EqList]-SA1, ConsAssoc_1),
			  		get_assoc(0, ConsAssoc_1, VIV-FDR-ListIV-VIV),
					put_assoc(0, ConsAssoc_1, VIV-FDR-[QA|ListIV]-VIV, ConsAssoc)
			  ;	false
			  )
			)
		)
	).
	
irc_cons_Ls_E([], _, _, _, _, _, _, ConsAssoc, ConsAssoc).
irc_cons_Ls_E([L|Ls], R, ConstraintsR, StartEndVars, QuantityAssoc, N, Flag, ConsAssoc, ConsAssocAux) :-
	irc_cons_L_E(L, R, ConstraintsR, StartEndVars, QuantityAssoc, N, Flag, NewConsAssocAux, ConsAssocAux),
	NewFlag is Flag + 1,
	irc_cons_Ls_E(Ls, R, ConstraintsR, StartEndVars, QuantityAssoc, N, NewFlag, ConsAssoc, NewConsAssocAux).
	
irc_cons_L_E(A-IA-Proc-IP-end, R, ConstraintsR, StartEndVars, QuantityAssoc, N, Flag, ConsAssoc, ConsAssocAux) :-
	get_assoc(A-IA-Proc-IP-R, StartEndVars, _-EA-_),
	( Flag = 1
	->	get_assoc(N, ConsAssocAux, SA1-SumListS-EqListS-SA1), EA1 = EA, SumListE = [SA1], EqListE = []
	;	get_assoc(N, ConsAssocAux, SA1-SumListS-EqListS-SA1-EA1-SumListE-EqListE-EA1)
	),
	member(A-R-Q-TE, ConstraintsR),
	( integer(Q)
	->	QA = Q
	; 	Q = (V,_), get_assoc(A-IA-Proc-IP-R-V, QuantityAssoc, QA-_)
	),
	( member(TE,['FromStartToEnd', 'BeforeEnd'])
	->	put_assoc(N, ConsAssocAux, SA1-SumListS-EqListS-SA1-EA1-[-1 * QA|SumListE]-[EA #= EA1|EqListE]-EA1, ConsAssoc)
	;	( member(TE, ['AfterStart', 'BeforeStart', 'Always'])
		->	put_assoc(N, ConsAssocAux, SA1-SumListS-EqListS-SA1-EA1-SumListE-[EA #= EA1|EqListE]-EA1, ConsAssoc)
		;	( TE = 'AfterEnd'
			->	put_assoc(N, ConsAssocAux, SA1-SumListS-EqListS-SA1-EA1-[QA|SumListE]-[EA #= EA1|EqListE]-EA1, ConsAssoc)
			;	false
			)
		)
	).
	
irc_cons_Ls_only_E([], _, _, _, _, _, _, ConsAssoc, ConsAssoc).
irc_cons_Ls_only_E([L|ListEnd], R, ConstraintsR, StartEndVars, QuantityAssoc, N, 1, ConsAssoc, ConsAssocAux) :-
	( get_assoc(N, ConsAssocAux, _)
	-> irc_cons_L_only_E(L, R, ConstraintsR, StartEndVars, QuantityAssoc, N, NewConsAssocAux, ConsAssocAux)
	; 	irc_cons_L(L, R, ConstraintsR, StartEndVars, QuantityAssoc, N, NewConsAssocAux, ConsAssocAux)
	),
	irc_cons_Ls_only_E(ListEnd, R, ConstraintsR, StartEndVars, QuantityAssoc, N, 1, ConsAssoc, NewConsAssocAux).
	
irc_cons_L_only_E(A-IA-Proc-IP-end, R, ConstraintsR, StartEndVars, QuantityAssoc, N, ConsAssoc, ConsAssocAux) :-
	get_assoc(A-IA-Proc-IP-R, StartEndVars, _-EA-_),
	get_assoc(N, ConsAssocAux, EA1-SumList-EqList-EA1),
	member(A-R-Q-TE, ConstraintsR),
	( integer(Q)
	->	QA = Q
	; 	Q = (V,_), get_assoc(A-IA-Proc-IP-R-V, QuantityAssoc, QA-_)
	),
	( member(TE,['FromStartToEnd', 'BeforeEnd'])
	->	put_assoc(N, ConsAssocAux, EA1-[-1 * QA|SumList]-[EA#=EA1|EqList]-EA1, ConsAssoc)
	;	( member(TE, ['AfterStart', 'BeforeStart', 'Always'])
		->	put_assoc(N, ConsAssocAux, EA1-SumList-[EA #= EA1|EqList]-EA1, ConsAssoc)
		;	( TE = 'AfterEnd'
			->	put_assoc(N, ConsAssocAux, EA1-[QA|SumList]-[EA1#=EA|EqList]-EA1, ConsAssoc)
			;	false
			)
		)
	).
	
construct_irc_cons(ConsAssoc, IRCcons) :-
	assoc_to_list(ConsAssoc, [IVel|ConsList]),
	construct_cons_iv(IVel, IVcons),
	construct_irc_cons(ConsList, IRCcons, IVcons).
	
construct_cons_iv(0-(VIV-FD-[S|SumList]-VIV), Cons) :-
	sum_list(SumList,S,Sum),
	Cons = (VIV in FD #/\ VIV #= Sum).
	
sum_list([], Sum, Sum).
sum_list([S|Ls], Aux, Sum) :-
	NewAux = (Aux + S), 
	sum_list(Ls, NewAux, Sum).
	
eq_list([], Eqs, Eqs).
eq_list([E|Es], Aux, Eqs) :-
	NewAux = (E #/\ Aux), 
	eq_list(Es, NewAux, Eqs).
	
construct_irc_cons([], IRCcons, IRCcons).
construct_irc_cons([L|ConsList], IRCcons, Aux) :-
	( L \= _-([]-_)
	->	construct_cons_el(L, Cons),
		NewAux = (Aux #/\ Cons)
	;  NewAux = Aux
	),
	construct_irc_cons(ConsList, IRCcons, NewAux).
	
construct_cons_el(N-(V-SumList-EqList-V), Cons) :-
	integer(N), var(V), is_list(SumList), is_list(EqList),
	(SumList \= []
	->	SumList = [S|Ls],
		sum_list(Ls, S, Sum)
	;	Sum = no
	),
	(EqList \= []
	->	EqList = [E|Es], eq_list(Es, E, Eqs)
	;	Eqs = no
	),
	( (\+ var(Sum), Sum = no)
	->	Cons = Eqs
	;	( Eqs = no
		->	Cons = (V #= Sum)
		;	Cons = (V #= Sum #/\ Eqs)
		)
	).
construct_cons_el(N-(VS-SumListS-EqListS-VS-VE-SumListE-EqListE-VE), Cons) :-
	integer(N), var(VS), var(VE), is_list(SumListS), is_list(SumListE), is_list(EqListS), is_list(EqListE),
	(SumListS \= []
	->	SumListS = [S|Ss],
		sum_list(Ss, S, SumS)
	;	SumS = no
	),
	(EqListS \= []
	->	EqListS = [E|Es], eq_list(Es, E, EqsS)
	;	EqsS = no
	),
	( (\+ var(SumS), SumS = no)
	->	ConsS = EqsS
	;	( EqsS = no
		->	ConsS = (VS #= SumS)
		;	ConsS = (VS #= SumS #/\ EqsS)
		)
	),
	(SumListE \= []
	->	SumListE = [SE|SEs],
		sum_list(SEs, SE, SumE)
	;	SumE = no
	),
	(EqListE \= []
	->	EqListE = [EE|EEs], eq_list(EEs, EE, EqsE)
	;	EqsE = no
	),
	( (\+ var(SumE), SumE = no)
	->	ConsE = EqsE
	;	( EqsE = no
		->	ConsE = (VE #= SumE)
		;	ConsE = (VE #= SumE #/\ EqsE)
		)
	),
	Cons = (ConsS #/\ ConsE).

% Encode product related constraints in resource constraints
encode_product_related_constraints(FinalTree) :-
	findall(Node, produces_for(_,_,Node,_), Nodes),
	encode_node_resource(Nodes, FinalTree),
	findall(A-N-Node-B, produces_for(A,N,Node,B), PRConstraints),
	encode_prc_rc(PRConstraints).
	
encode_node_resource([], _).
encode_node_resource([Node|Ns], Tree) :-
	inst_node(Tree, Node, InstNode),
	assert(type(Node, int, (0,InstNode))),
	assert(res(Node, Node, 0)),
	encode_node_resource(Ns, Tree).
	
inst_node(Tree, Node, InstN) :-
	iTree_iNodes(Tree,Nodes),
	findall(Node-ID, member(Node-ID, Nodes), ListNode),
	length(ListNode, InstN).

encode_prc_rc([]).
encode_prc_rc([A-N-Node-B|PRConstraints]) :-
	atomic_list_concat(['q_',Node], QA),
	assert(resource_constraint(A, Node, (QA, Node), 'AfterEnd',true)),
	NN is -1 * N,
	assert(resource_constraint(B, Node, NN, 'AfterStart',true)),
	encode_prc_rc(PRConstraints).
	
prc_q_constraints(QuantityVarAssoc, PRCqConstraints) :-
	findall(Node, produces_for(_,_,Node,_), Nodes),
	prc_q_constraints(Nodes, QuantityVarAssoc, PRCqConstraints, []).
	
prc_q_constraints([], _, PRCqConstraints, PRCqConstraints).
prc_q_constraints([Node|Nodes], QuantityVarAssoc, [C|Tail1], Tail2) :-
	type(Node, int, (_, InstNode)),
	atomic_list_concat(['q_',Node], QA),
	findall(QVar, gen_assoc(_-_-_-_-Node-QA, QuantityVarAssoc, QVar-_), QVars),
	C = sum(QVars, #=<, InstNode),
	prc_q_constraints(Nodes, QuantityVarAssoc, Tail1, Tail2).
	
	
remove_product_related_constraints(FinalTree) :-
	findall(Node, produces_for(_,_,Node,_), Nodes),
	remove_node_resource(Nodes, FinalTree),
	findall(A-N-Node-B, produces_for(A,N,Node,B), PRConstraints),
	remove_prc_rc(PRConstraints).

remove_node_resource([], _).
remove_node_resource([Node|Ns], Tree) :-
	inst_node(Tree, Node, InstNode),
	retract(type(Node, int, (0,InstNode))),
	retract(res(Node, Node, 0)),
	remove_node_resource(Ns, Tree).
	
remove_prc_rc([]).
remove_prc_rc([A-N-Node-B|PRConstraints]) :-
	atomic_list_concat(['q_',Node], QA),
	retract(resource_constraint(A, Node, (QA, Node), 'AfterEnd',true)),
	NN is -1 * N,
	retract(resource_constraint(B, Node, NN, 'AfterStart',true)),
	remove_prc_rc(PRConstraints).
	


% Process CSP
prop_label_process(TimeLimit, ActivityInstancesAssoc, QuantityVarsAssoc, StartEndVars,
						 TemporalConstraints, ImpTemporalConstraints, ResourceConstraints, DurationConstraints, PRCqConstraints,
						 SimplConstraints, Browser) :-
	activity_variables(ActivityInstancesAssoc, TVars, DVars),
	start_end_variables(StartEndVars, SEVars),
	assoc_to_values(QuantityVarsAssoc, QVars),
	append([TVars, DVars, QVars, SEVars], VarFDs),
	domainConstraints(VarFDs, [], DomainConstraints),
	append([DomainConstraints, TemporalConstraints, ImpTemporalConstraints, ResourceConstraints, DurationConstraints, PRCqConstraints, SimplConstraints], Constraints),
	send(Browser, append, '\nCSP\n'),
	length(Constraints, NConstraints), 
	atomic_list_concat([NConstraints, ' constraint/s\n'], NConstrs),
	send(Browser, append, NConstrs),
	propagate_constraints(Constraints),
	extract_vars(VarFDs, Vars, []),
	catch(
		call_with_time_limit( TimeLimit, labeling([leftmost, up, enum],Vars) ),
		time_limit_exceeded,
		(send(Browser, append, '\nTime limit exceeded!\n'), false)).

symmetries(Vars, Min, Max, Symmetries) :-
	create_pairs(Min, Max, Pairs, []),
	GC = global_cardinality(Vars, Pairs),
	zero_impl(Pairs, ZeroImpl, []),
	Symmetries = [GC|ZeroImpl].

create_pairs(MaxInstant, MaxInstant, Pairs, Pairs).
create_pairs(N, MaxInstant, [N-_|Tail1], Tail2) :-
	N < MaxInstant,
	NextN is N + 1,
	create_pairs(NextN, MaxInstant, Tail1, Tail2).

zero_impl([_], Impl, Impl).
zero_impl([_-VN,_-VNN|VNs], [VN #= 0 #==> VNN #= 0|Tail1], Tail2) :-
	zero_impl([_-VNN|VNs], Tail1, Tail2).

extract_vars([],Vars, Vars).
extract_vars([V-_|Vs], [V|Tail1], Tail2) :-
	extract_vars(Vs, Tail1, Tail2).

activity_variables(Assoc, TVars, DVars) :-
	assoc_to_values(Assoc, List),
	activity_variables(List, TVars, [], DVars, []).

activity_variables([], TVars, TVars, DVars, DVars).
activity_variables([TS-FD1-TE-FD2-D-FD3-_-_|Ls], [TS-FD1,TE-FD2|TTail1], TTail2, [D-FD3|DTail1], DTail2) :- 
	activity_variables(Ls, TTail1, TTail2, DTail1, DTail2).

start_end_variables(Assoc, Vars) :-
	assoc_to_values(Assoc, List),
	start_end_variables(List, Vars, []).

start_end_variables([], Vars, Vars).
start_end_variables([S-E-FD|Ls], [S-FD,E-FD|Tail1], Tail2) :- start_end_variables(Ls, Tail1, Tail2).

% Print assignments to process variables and parameters
print_process_assignments(FDProcVarsAssocList, ActivityInstancesAssoc, QuantityVarsAssoc, Browser) :-
	send(Browser, append, '\n\nProcess variables\n'),
	print_process_variables(FDProcVarsAssocList, Browser),
	send(Browser, append, '\nActivities\n'),
	print_activities(ActivityInstancesAssoc, QuantityVarsAssoc, Browser).

print_process_variables(ProcVars, Browser) :-
	forall( gen_assoc(ProcVar, ProcVars, FDV-_),
			  (
			  	atomic_list_concat([ProcVar,' = ',FDV,'\n'], Msg),
			  	send(Browser, append, Msg)
			  )
	).
	
print_activities(ActivityInstancesAssoc, QuantityVarsAssoc, Browser) :-
	forall( gen_assoc(A-IA-Proc-IP, ActivityInstancesAssoc, TS-_-TE-_-D-_-_-_),
			  (
			  	atomic_list_concat([A,'-',IA,'-',Proc,'-',IP,'\n'], Msg1),
			  	send(Browser, append, Msg1),
			  	atomic_list_concat(['Start = ',TS,'; End = ',TE,'; Duration = ',D,'\n'], Msg2),
			  	send(Browser, append, Msg2),
			  	forall( gen_assoc(A-IA-Proc-IP-R-V, QuantityVarsAssoc, FDV-_),
			  			  (
			  			  	atomic_list_concat([FDV,' unit/s of resource ', R, '(', V, ' = ', FDV, ')\n'], Msg3),
			  			  	send(Browser, append, Msg3)
			  			  )
			   ),
			   send(Browser, append, '\n')
			  )
	).
