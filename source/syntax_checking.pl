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

%%%% Modeling language specification
:- module(syntax_checking, [product_process_model/1,pathsInvolved/2,nodeInvolved/2]).

%:- use_module(library(pce)).
:- use_module(messages).

% Product/process model	
product_process_model(Model) :-
	use_module(Model),

	% Product predicates
	% Types
	findall(TypeName,type(TypeName,_,_),TypeNames),
	types(TypeNames),
	% Nodes
	findall((NodeName,NodeVariables,NodeConstraints),node(NodeName,NodeVariables,NodeConstraints),Nodes),
	findall((RName,PNode,CNode,Card,CardConst),hasPartRelationModel(RName,PNode,CNode,Card,CardConst),HasPartRelations),
	nodes(Nodes,HasPartRelations),
	% Has-part relations
	findall(NodeName,member((NodeName,_,_),Nodes),NodeNames),
	hasPartRelations(HasPartRelations,NodeNames,Nodes),
	% Global constraints
	%findall(GC,globalConstraint(GC),GlobalConstraints),
	findall(GC,nodeModelConstraint(GC),GlobalConstraints),
	globalConstraints(GlobalConstraints,Nodes,HasPartRelations),
	% Aggregate constraints
	findall((AggFun,Vars,ROp,Num),aggConstraint(AggFun,Vars,ROp,Num),AggConstraints),
	aggregateConstraints(AggConstraints,Nodes,HasPartRelations),
	% alldifferent constraints
	findall(AllDiffVars,allDifferentValues(AllDiffVars),AllDifferentConstraints),
	allDifferentConstraints(AllDifferentConstraints,Nodes,HasPartRelations),
	% Cardinality model constraints
	findall(CC,cardModelConstraint(CC),CardConstrs),
	cardinalityModelConstraints(CardConstrs,HasPartRelations),
	
	% Process predicates
	% Activities
	% activity(+Name, +Kind, +Proc, +Insts, +DurationConstraints)
	% Name is a string representing the name of the activity
	% Kind is one of the following atoms: atomic, comp, multi, multicomp
	% Proc is the name of the process the activity belongs too, it is main or the name of a composite activity
	% Insts is an integer variable (VarName, Type) or nil
	% DurationConstraints is a list of duration constraints
	% Duration constraints use the same syntax as of node constraints
	% Variables are indicated in constraints with their name only
	findall(Kind-Proc-Insts, activity(_,Kind,Proc,Insts,_), KPI),
	( check_activities(KPI) ; error_message('Error/s in activitie/s!') ),
	findall(ActivityName-Insts-DurationConstraints, activity(ActivityName,_,_,Insts,DurationConstraints), Activities),
	findall(ActivityName-ActivityName, activity(ActivityName,_,_,_,_), ActivityNames),
	%check_activities(Activities),
	list_to_assoc(ActivityNames, ActivityAssocList),
	% Process variables
	% process_variable(+Name, +Type)
	% Name is a string representing the name of the process variable
	% Type is the type of the process variable
	findall(VarName-VarType, process_variable(VarName, VarType), ProcessVariables),
	( check_pv(ProcessVariables) ; error_message('Error/s in process variable/s!') ),
	list_to_assoc(ProcessVariables, ProcVarAssocList),
	% Temporal constraints
	findall(TC, temporal_constraint(TC), TemporalConstraints),
	temporalConstraints(TemporalConstraints, ActivityAssocList, ProcVarAssocList),
	% Resources
	% res(+ResourceName, +Type, +InitialValue)
	findall(Type-InitialValue, res(_, Type, InitialValue), Resources),
	( check_resources(Resources) ; error_message('Error/s in resource/s!') ),
	% ResourceName is a string representing the name of the resource
	% Type is an integer type representing the allowed values for resource quantity
	% InitialValue is an integer representing the initial quantiy available
	%
	% Resource constraints
	% resource_constraint(+ActivityName, +ResourceName, +Quantity, +TimeExtent, +Condition)
	% Quantity is an integer or an integer variable
	% TimeExtent is one of the following atoms: FromStartToEnd, ...
	findall(A-R-Q-TE, resource_constraint(A,R,Q,TE,_), ResourceConstraints),
	(check_resource_constraints(ResourceConstraints) ; error_message('Error/s in resource constraints!')),
	findall((QVar-A)-A, resource_constraint(A,_,(QVar,_),_,_), QuantityResourceVariables),
	findall(Cond, resource_constraint(_,_,_,_,Cond), ResourceConstraintConditions),
	list_to_assoc(QuantityResourceVariables, QRVAssocList),
	check_rc_conditions(ResourceConstraintConditions, ProcVarAssocList),
	% Duration constraints
	check_duration_constraints(Activities, ProcVarAssocList, QRVAssocList),
	% Product related constraints
	% produces_for(+Producer, +N, +Node, +Consumer)
	% Producer and Consumer are activity names
	% N is an integer denoting the unity produced
	% Node is a node name
	findall(A-B-Node, produces_for(A, _, Node, B), PRCNodes),
	check_prc(PRCNodes, NodeNames),
	% Coupling constraints
	findall(CouplingConstraint, coupling_constraint(CouplingConstraint), CouplingConstraints),
	check_coupling_constraints(CouplingConstraints, ProcVarAssocList, Activities, Nodes, HasPartRelations),
		
	generic_message('Message','Syntax checking completed.'),
	unload_file(Model).


% Product predicates

% Types
% type(+TypeName,+Sort,+Domain)
types(Types) :-
	forall(	member(TypeName,Types),
			((type(TypeName,Sort,Domain),
			 domain(Sort,Domain)
			 );
			 atomic_concat('Error in definition of type: ',TypeName,Msg),
			 error_message(Msg)
			)
	).
	
% Nodes
% node(+NodeName,+Variables,+Constraints)	
nodes(Nodes,HasPartRelations) :-
	forall(	member((NName,Variables,Constraints),Nodes),
			(variables(Variables),
			 (nodeConstraints(Constraints,NName,Variables,Nodes,HasPartRelations);
			  ( atomic_concat('Error in definition of constraints of node: ',NName,Msg), error_message(Msg) )
			 )
			)
	).
	
% Node variables: (VariableName,VariableType)
variables(Variables) :-
	forall(	member((_,VType),Variables),
			(type(VType,_,_);
			 ( atomic_list_concat(['Error: type ',VType,' not defined'], Msg), error_message(Msg) )
			)
	).

% Domains	
domain(int,Set) :-
	is_list(Set),
	forall(	member(Num,Set),
			integer(Num)
	).
domain(int,(Min,Max)) :-
	integer(Min), integer(Max).
domain(string,Set) :-
	forall( member(StrList,Set),
			atom(StrList)
	).

% Node constraints: constraint on variable of a node N and on varaible of its ancestors.
nodeConstraints(Constraints,NName,NVariables,Nodes,HasPartRelations) :-
	forall( member(C,Constraints),
			(constraint(C,node,NVariables,Nodes,[]),
			 pathsInvolved(C,NodesPaths,NName,NVariables),
			 checkPaths(NodesPaths,NName,HasPartRelations)
			)
	).
		
ancestor(Ni,N,HPR) :- path(Ni,N,HPR).

path(N,M,HPR) :- member((_,N,M,_,_),HPR).
path(N,M,HPR) :- member((_,N,O,_,_),HPR), path(O,M,HPR).

checkPaths(Paths,NName,HPR) :-
	forall(	(member((N,P),Paths)),
			(
			 (P = [], (N = NName ; ancestor(N,NName,HPR)))
			 ;
			 (P \= [], checkPath(N,P,NName,HPR))
			)
	).
	
checkPath(N,Path,M,HPR) :- 
	forall( member(Edge,Path),
			( member((Edge,_,_,_,_),HPR)
			  ;
			  member(Edge,['',has_part,'*'])
			)
	),
	pathExists(N,Path,M,HPR).
	
pathExists(NName,[],NName,_).
pathExists(N,[has_part|Path],NName,HPR) :- member((_,N,M,_,_),HPR),
									  	   pathExists(M,Path,NName,HPR).
pathExists(N,[RName|Path],NName,HPR) :- member((RName,N,M,_,_),HPR),
								   		pathExists(M,Path,NName,HPR).
% '*' = undefined number of edges
pathExists(N,['*'|Path],NName,HPR) :- path(N,M,HPR),
									  pathExists(M,Path,NName,HPR).									


% Edges (Has-part relations), they define a graph of nodes
% edge(+Name,+ParentNode,+ChildNode,+Cardinality,+CardinalityConstraints)
% edge(+Name,+ParentNode,+ChildNode,+(Cardinality,Type),+CardinalityConstraints)
% edge(+ParentNode,+ChildNode,+Cardinality,+CardinalityConstraints)
% edge(+ParentNode,+ChildNode,+(Cardinality,Type),+CardinalityConstraints)

hasPartRelationModel(A,B,C,D,E) :- edge(A,B,C,D,E).
hasPartRelationModel('',B,C,D,E) :- edge(B,C,D,E).
	
hasPartRelations(HasPartRelations,NodeNames,Nodes) :-
	% Check if has-part relations define a weakly connected directed graph in which exactly n-1 nodes are reached by multi-edges
	(
		(	
			% weakly connected
	  		nodeCouples(NodeNames,NodeNames,[],Couples),
	  		forall( member((N,M),Couples),
	  		  undPath(N,M,HasPartRelations,[])
	  		),
	  		% n-1 nodes reached by edges
			length(NodeNames,NNumb),
	  		findall(C,member((_,_,C,_,_),HasPartRelations),NC),
	  		%removeDuplicates(HPR,HPRND),
	  		%findall(C,member((_,C),HPRND),NC),
	  		removeDuplicates(NC,NCR),
	  		length(NCR,CNumb),
	  		CNumb =:= NNumb - 1
	 	);
	 	HasPartRelations \= [],
	 	error_message('Has-part relations do not define a product model graph!')
	),
	% Check nodes, cardinality and cardinality constraints
	forall(	member((RName,Pa,Ch,Cardinality,CardConsts),HasPartRelations),
			(
			 (
			 % Check if a realtion is defined on existing nodes
			  ( member((Pa,_,_),Nodes)
			  ; ( atomic_list_concat(['Error on relation ',RName,', ',Pa,' is not a node!'], Msg),
				  	error_message(Msg))
			  ),
	  		  ( member((Ch,_,_),Nodes) 
	  		  ; ( atomic_list_concat(['Error on relation ',RName,', ',Ch,' is not a node!'], Msg),
				  	error_message(Msg))
	        )
	       ),
			 (
			  % Has-part relations with a cardinality variable
			  (Cardinality = (Card,CType),
			   ((type(CType,int,D), domain(int,D) ; ( atomic_concat('Error on type ',CType,Msg), error_message(Msg) )),
			    (cardinalityConstraints(CardConsts,Card,Pa,Nodes,HasPartRelations);
			     ( atomic_list_concat(['Error on constraints of relation ',RName,' between node ',Pa,
			     								' and node ',Ch],Msg),
			     	 error_message(Msg) )
			    )
			   )
			  );
			  (
			   % Has-part relations with an integer as cardinality
			   (integer(Cardinality),length(CardConsts,0);
			    ( atomic_list_concat(['Error on relation ',RName,' between node ',Pa,
			     								' and node ',Ch],Msg),
			     	 error_message(Msg) )
			   );
			    ( atomic_list_concat(['Error on relation between node ',Pa,
			     								' and node ',Ch],Msg),
			     	 error_message(Msg) )
		      )
		     )
		    )
	).

removeDuplicates([],[]).
removeDuplicates([X|Xs],[X|Zs]) :- delete(Xs,X,Ys), removeDuplicates(Ys,Zs).
	
nodeCouples([],[],Couples,Couples).
nodeCouples([_|Ns],[],Couples,C) :- nodeCouples(Ns,Ns,Couples,C).
nodeCouples([N|Ns],[M|Ms],Couples,C) :- nodeCouples([N|Ns],Ms,[(N,M)|Couples],C).

undPath(N,N,_,_).	
undPath(N,M,HPR,_) :- member((_,N,M,_,_),HPR) ; member((_,M,N,_,_),HPR).
undPath(N,M,HPR,Visited) :- (member((_,N,C,_,_),HPR) ; member((_,C,N,_,_),HPR)),
						\+ member(C,Visited),
						undPath(C,M,HPR,[N|Visited]).

% Cardinality constraints: constraint on variable of the parent node on the cardinality of % the relation and on varaible of parent node ancestors.
cardinalityConstraints(CardConsts,Card,ParentNode,Nodes,HPR) :-
	forall( member(C,CardConsts),
			(member((ParentNode,NV,_),Nodes),
			 constraint(C,card,NV,Nodes,HPR),
			 cardInvolved(C,Card),
			 pathsInvolved(C,NodesPaths),			
			 checkPaths(NodesPaths,ParentNode,HPR)
			)
	).

% Global Constraints: constraints on node variables	
globalConstraints(GlobalConstraints,Nodes,HPR) :-
	forall(	member(GC,GlobalConstraints),
			((constraint(GC,glob,_,Nodes,[]),
			  pathsInvolved(GC,Paths),
			  checkPathsGlobal(Paths,HPR)
			 );
			 ( error_message('Error/s in model constraint/s!') )
			)
	).
	
checkPathsGlobal(Paths,HPR) :-
	forall(	member((N,P),Paths),
			checkPath(_,P,N,HPR)
	).
	
% Aggregate constraints
aggregateConstraints(AggConstraints,Nodes,HPR) :-
	forall(	member((AggFun,Vars,ROp,Num),AggConstraints),
			((member(AggFun,[sum,avg]),
			  Vars \= [],
			  findall((N,P),member((_,N,P),Vars),Paths),
			  checkPathsGlobal(Paths,HPR),
			  forall( member((V,N,P),Vars),
			  		  (member((N,Variables,_),Nodes),member((V,Type),Variables),type(Type,int,_))
			  		),
			  member(ROp,[lt,leq,eq,geq,gt,neq]),
			  integer(Num)
			 );
			 error_message('Error/s in aggregate constraint/s!')
			)
	).
	
% alldifferent constraints
allDifferentConstraints(AllDiffConstraints,Nodes,HasPartRelations) :-
	forall( member(AllDiffVars,AllDiffConstraints),
			((
			 AllDiffVars \= [],
			 findall((N,P),member((_,N,P),AllDiffVars),Paths),
			 checkPathsGlobal(Paths,HasPartRelations),
			 (forall( member((V,N,P),AllDiffVars),
			 		  (member((N,Variables,_),Nodes),member((V,Type),Variables),type(Type,int,_))
			 		);
			  forall( member((V,N,P),AllDiffVars),
			 		  (member((N,Variables,_),Nodes),member((V,Type),Variables),type(Type,string,_))
			 		)
			 ));
			 error_message('Error/s in allDifferentValues constraint/s!')
			)
	).

% Cardinality model constraints
cardinalityModelConstraints(CardConstrs,HasPartRelations) :-
	forall( member(C,CardConstrs),
				(
					(nodeInvolved(C,[_]), constraint(C,cardConstr,_,_,HasPartRelations))
					;
					error_message('Error/s in cardinality model constraint/s!')
				)
	).


% Sorted terms
termInt(T,_,_,_,_) :- integer(T).
termInt(Var,card,_,_,HPR) :- member((_,_,_,(Var,_),_),HPR).
termInt(Var,T,NV,_,_) :- (T = node ; T = card), member((Var,VType),NV), type(VType,int,_).

termInt((Var,Node,_),T,_,Nodes,_) :- 
	T \= cardConstr,
	member((Node,Vars,_),Nodes),
	member((Var,VType),Vars), type(VType,int,_).
								   
termInt((Label,P,C,Card),cardConstr,_,_,HasPartRelations) :- member((Label,P,C,(Card,_),_),HasPartRelations).								   
							 
termInt(TI,CC,NV,Nodes,HPR) :-
	TI =.. [Op,T1,T2],
	member(Op,[plus,minus,times,frac,mod]),
	termInt(T1,CC,NV,Nodes,HPR), termInt(T2,CC,NV,Nodes,HPR).
termInt(minus_sign(T),CC,NV,Nodes,HPR) :- termInt(T,CC,NV,Nodes,HPR).

termString(Var,T,NV,_,_) :- (T = node ; T = card), member((Var,VType),NV),
						     		 type(VType,string,_).

termString((Var,Node,_),_,_,Nodes,_) :- member((Node,Vars,_),Nodes),
									  member((Var,VType),Vars), type(VType,string,_).
termString(T,_,_,_,_) :- atom(T).

% Constraints
primitiveConstraint(PC,CC,NV,Nodes,HPR) :-
	PC =.. [Op,T1,T2],
	member(Op,[lt,leq,eq,geq,gt,neq]),
	termInt(T1,CC,NV,Nodes,HPR), termInt(T2,CC,NV,Nodes,HPR).

primitiveConstraint(eq(T1,T2),CC,NV,Nodes,HPR) :- termString(T1,CC,NV,Nodes,HPR), termString(T2,CC,NV,Nodes,HPR).
primitiveConstraint(neq(T1,T2),CC,NV,Nodes,HPR) :- termString(T1,CC,NV,Nodes,HPR), termString(T2,CC,NV,Nodes,HPR).
% For CLab models boolean variables
primitiveConstraint(Var,node,NV,_,_) :- member((Var,VType),NV),
									  			 type(VType,int,(0,1)).
primitiveConstraint((Var,Node,_),_,_,Nodes,_) :- member((Node,Vars,_),Nodes),
											   member((Var,VType),Vars),
							 			 	   type(VType,int,(0,1)).
primitiveConstraint(T,_,_,_,_) :- integer(T), T = 1 ; T = 0.

constraint(C,CC,NV,Nodes,HPR) :- primitiveConstraint(C,CC,NV,Nodes,HPR).
constraint(not(C),CC,NV,Nodes,HPR) :- constraint(C,CC,NV,Nodes,HPR).

constraint(C,CC,NV,Nodes,HPR) :-
	C =.. [Op,C1,C2],
	member(Op,[and,or,impl]),
	constraint(C1,CC,NV,Nodes,HPR), constraint(C2,CC,NV,Nodes,HPR).

constraint(valid_tuples(Vars,Tuples),CC,NV,Nodes,HPR) :-
	forall(	member(Var,Vars),
			(
			 (Var = (V,Node,_), member((Node,NVars,_),Nodes), member((V,_),NVars));
			 (atom(Var), CC = node, member((Var,_),NV));
			 (atom(Var), CC = card, member((_,_,_,(Var,_),_),HPR))
			)
	),
	varSort(Vars,CC,NV,Nodes,HPR,[],Sorts),
	forall( member(Tuple,Tuples),
			checkTuple(Tuple,Sorts)
	).

varSort([],_,_,_,_,Sorts,Sorts).
varSort([V|Vars],node,NV,Nodes,HPR,Acc,Sorts) :-
	atom(V), member((V,T),NV), type(T,S,_),
	append(Acc,[S],NAcc), varSort(Vars,node,NV,Nodes,HPR,NAcc,Sorts).
varSort([V|Vars],card,NV,Nodes,HPR,Acc,Sorts) :-
	atom(V), member((_,_,_,(V,_),_),HPR),
	append(Acc,[int],NAcc), varSort(Vars,card,NV,Nodes,HPR,NAcc,Sorts).
varSort([(V,N,_)|Vars],CC,NV,Nodes,HPR,Acc,Sorts) :-
	member((N,NVars,_),Nodes), member((V,T),NVars), type(T,S,_),
	append(Acc,[S],NAcc), varSort(Vars,CC,NV,Nodes,HPR,NAcc,Sorts).
	
checkTuple([],[]).
checkTuple([T|Ts],[S|Ss]) :-
	((S = int, integer(T));
	 (S = string, atom(T))
	),
	checkTuple(Ts,Ss).
	

% Cardinality involved in a cardinality constraint
cardInvolved(Card,Card).

cardInvolved(E,Card) :-
	E =.. [Op,E1,E2], Op \= valid_tuples,
	(cardInvolved(E1,Card) ; cardInvolved(E2,Card)).

cardInvolved(minus_sign(T),Card) :-
	cardInvolved(T,Card).
	
cardInvolved(abs(T),Card) :-
	cardInvolved(T,Card).	

cardInvolved(not(C1),Card) :-
	cardInvolved(C1,Card).

cardInvolved(valid_tuples(Vars,_),Card) :-
	member(Card,Vars).
	
	
% Couples (Node,Path) involved in a node constraint
pathsInvolved((_,Node,P),[(Node,P)],_,_).
pathsInvolved(Var,[(Node,[])],Node,NV) :- member((Var,_),NV).
pathsInvolved(T,[],_,NV) :- integer(T) ; (atom(T), \+ member((T,_),NV)).

pathsInvolved(E,Paths,N,NV) :-
	E =.. [Op,E1,E2], Op \= valid_tuples, Op \= ',',
	pathsInvolved(E1,P1,N,NV), pathsInvolved(E2,P2,N,NV),
	union(P1,P2,Paths).

pathsInvolved(minus_sign(T),Paths,N,NV) :-
	pathsInvolved(T,Paths,N,NV).
	
pathsInvolved(abs(T),Paths,N,NV) :-
	pathsInvolved(T,Paths,N,NV).
	
pathsInvolved(not(C1),Paths,N,NV) :-
	pathsInvolved(C1,Paths,N,NV).

pathsInvolved(valid_tuples(Vars,_),Paths,N,NV) :-
	pathsInvolvedV(Vars,Paths,N,NV).
	
pathsInvolvedV([],[],_,_).
pathsInvolvedV([V|Vars],Paths,N,NV) :-
	pathsInvolved(V,PV,N,NV),
	pathsInvolvedV(Vars,PVs,N,NV),
	union(PV,PVs,Paths).

% Couples (Node,Path) involved in a global or cardinality constraint
pathsInvolved((_,Node,P),[(Node,P)]).
pathsInvolved(T,[]) :- integer(T) ; atom(T).

pathsInvolved(E,Paths) :-
	E =.. [Op,E1,E2], Op \= valid_tuples, Op \= ',',
	pathsInvolved(E1,P1), pathsInvolved(E2,P2),
	union(P1,P2,Paths).

pathsInvolved(minus_sign(T),Paths) :-
	pathsInvolved(T,Paths).

pathsInvolved(abs(T),Paths) :-
	pathsInvolved(T,Paths).

pathsInvolved(not(C1),Paths) :-
	pathsInvolved(C1,Paths).

pathsInvolved(valid_tuples(Vars,_),Paths) :-
	pathsInvolvedV(Vars,Paths).
	
pathsInvolvedV([],[]).
pathsInvolvedV([V|Vars],Paths) :-
	pathsInvolved(V,PV),
	pathsInvolvedV(Vars,PVs),
	union(PV,PVs,Paths).
	
% List of nodes involved in a cardinality model constraint (it must contain only one node)
nodeInvolved((_,Node,_,_),[Node]).

nodeInvolved(T,[]) :- integer(T) ; atom(T).

nodeInvolved(E,Nodes) :-
	E =.. [Op,E1,E2], Op \= ',',
	nodeInvolved(E1,Ns1), nodeInvolved(E2,Ns2),
	union(Ns1,Ns2,Nodes).

nodeInvolved(minus_sign(T),Nodes) :- nodeInvolved(T,Nodes).

nodeInvolved(abs(T),Nodes) :- nodeInvolved(T,Nodes).

nodeInvolved(not(E),Nodes) :- nodeInvolved(E,Nodes).




% Process elements checking

% Activities
check_activities(KPI) :-
	forall(member(K-P-I, KPI),
		(
			member(K, ['atomic','comp','multi','multicomp']),
			(P = main ; activity(P,_,_,_,_)),
			(I = nil ; (I = (_,T), type(T, int, _)))
		)
	).

% Process variables
check_pv(ProcessVariables) :-
	forall(member(_-T, ProcessVariables), type(T, _, _)).

% Resources
check_resources(Resources) :-
	forall(member(T-IV, Resources),
		(
			type(T, int, _),
			integer(IV)
		)
	).

% Temporal constraints
% temporal_constraint(+Constraint)
% Constraint is a term obtained from terms before/2, meets/2, ..., or/2, and/2, if/1, iff/1, 
% and activity names
temporalConstraints(TemporalConstraints, ActivityAssocList, ProcVarAssocList) :-
	forall(member(TC, TemporalConstraints), 
		(	check_tc(TC, ActivityAssocList, ProcVarAssocList) 
			; 
			( term_to_atom(TC,TCAtom), atomic_list_concat(['Error in temporal constraint ',TCAtom,'!'],Msg),
			  error_message(Msg) )
		)).
	
% check_tc(TC, Activities)
% Atomic constraints
check_tc(before(A,B), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A),
   get_assoc(B, Activities, B).
check_tc(after(A,B), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A),
   get_assoc(B, Activities, B).
check_tc(meets(A,B), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A),
   get_assoc(B, Activities, B).
check_tc(met_by(A,B), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A),
   get_assoc(B, Activities, B).
check_tc(overlaps(A,B), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A),
   get_assoc(B, Activities, B).
check_tc(overlapped_by(A,B), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A),
   get_assoc(B, Activities, B).
check_tc(during(A,B), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A),
   get_assoc(B, Activities, B).
check_tc(includes(A,B), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A),
   get_assoc(B, Activities, B).
check_tc(starts(A,B), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A),
   get_assoc(B, Activities, B).
check_tc(started_by(A,B), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A),
   get_assoc(B, Activities, B).
check_tc(finishes(A,B), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A),
   get_assoc(B, Activities, B).
check_tc(finished_by(A,B), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A),
   get_assoc(B, Activities, B).
check_tc(equals(A,B), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A),
   get_assoc(B, Activities, B).
check_tc(is_absent(A), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A).
check_tc(must_be_executed(A), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A).
check_tc(not_co_existent-with(A,B), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A),
   get_assoc(B, Activities, B).
check_tc(succeeded_by(A,B), Activities, _ProcVarAssocList) :-
   get_assoc(A, Activities, A),
   get_assoc(B, Activities, B).

% Conditional constraints
check_tc(cond(Cond, TC), Activities, ProcVarAssocList) :-
   check_cond(Cond, ProcVarAssocList),
   check_tc(TC, Activities, ProcVarAssocList).

% and/or
check_tc(and(TC1, TC2), Activities, ProcVarAssocList) :-
   check_tc(TC1, Activities, ProcVarAssocList),
   check_tc(TC2, Activities, ProcVarAssocList).
check_tc(or(TC1, TC2), Activities, ProcVarAssocList) :-
   check_tc(TC1, Activities, ProcVarAssocList),
   check_tc(TC2, Activities, ProcVarAssocList).
   
% Resource constraints
check_resource_constraints(ResourceConstraints) :-
	forall(member(A-R-Q-TE, ResourceConstraints),
		(
			activity(A,_,_,_,_),
			res(R,_,_),
			member(TE, ['FromStartToEnd', 'BeforeStart', 'BeforeEnd', 'AfterStart', 'AfterEnd', 'Always']),
			( integer(Q)
			->	true
			;	Q = (_,T), type(T, int, _)
			)
		)
	).

% Resource constraints conditions
check_rc_conditions(RCConditions, ProcVarAssocList) :-
	forall(member(Cond, RCConditions), 
		(	check_cond(Cond, ProcVarAssocList)
		;	( term_to_atom(Cond, CondAtom),
			  atomic_list_concat(['Error in resource constraint condition ',CondAtom,'!'],Msg), 
		     error_message(Msg) )
		)
	).
	
% Product related constraints
check_prc(PRCNodes, NodeNames) :-
	forall(member(A-B-N, PRCNodes), 
		(	( activity(A,_,_,_,_), activity(B,_,_,_,_), member(N, NodeNames) )
		;	( atomic_list_concat(['Error in product related constraint, node ',N,' does not exist!'], Msg), 
			  error_message(Msg) )
		)
	).
	
% Conditions
check_cond(if(C), ProcVarAssocList) :- 
   constraint(C, ProcVarAssocList, t, nil).
check_cond(iff(C), ProcVarAssocList) :- 
   constraint(C, ProcVarAssocList, t, nil).
check_cond(true, _).

% Duration constraints
check_duration_constraints(Activities, ProcVarAssocList, QRVAssocList) :-	
	forall(member(A-_-DCs,Activities), 
			forall(member(DC, DCs), 
				( constraint(DC, ProcVarAssocList, QRVAssocList, A)
				; ( term_to_atom(DC, DCAtom),
				 	 atomic_list_concat(['Error in duration constraint ',DCAtom,' of activity ',A,'!'], Msg), 
				    error_message(Msg) )
				)
			)
	).

% Condition and duration constraints
termInt(T, _, _, _) :- integer(T).
termInt(Var, ProcVarAssocList, _, _) :- 
	get_assoc(Var, ProcVarAssocList, Type),
	type(Type, int, _).
termInt(Var, _, QRVAssocList, A) :-	get_assoc(Var-A, QRVAssocList, A).
termInt(D, _, QRVAssocList, A) :- QRVAssocList \== t, atomic_list_concat(['d','_',A],D).
					 
termInt(TI, ProcVarAssocList, QRVAssocList, A) :-
	TI =.. [Op,T1,T2],
	member(Op,[plus,minus,times,frac,mod]),
	termInt(T1, ProcVarAssocList, QRVAssocList, A), 
	termInt(T2, ProcVarAssocList, QRVAssocList, A).
termInt(minus_sign(T), ProcVarAssocList, QRVAssocList, A) :- 
	termInt(T, ProcVarAssocList, QRVAssocList, A).
termInt(abs(T), ProcVarAssocList, QRVAssocList, A) :- 
	termInt(T, ProcVarAssocList, QRVAssocList, A).

termString(Var, ProcVarAssocList, _, _) :- 
	get_assoc(Var, ProcVarAssocList, Type), 
	type(Type, string, _).
termString(T,_,_,_) :- atom(T).

primitiveConstraint(PC, ProcVarAssocList, QRVAssocList, A) :-
	PC =.. [Op,T1,T2],
	member(Op,[lt,leq,eq,geq,gt,neq]),
	termInt(T1, ProcVarAssocList, QRVAssocList, A), 
	termInt(T2, ProcVarAssocList, QRVAssocList, A).

primitiveConstraint(eq(T1,T2), ProcVarAssocList, _, _) :- 
	termString(T1, ProcVarAssocList, _, _), termString(T2, ProcVarAssocList, _, _).
primitiveConstraint(neq(T1,T2), ProcVarAssocList, _, _) :- 
	termString(T1, ProcVarAssocList, _, _), termString(T2, ProcVarAssocList, _, _).

constraint(C, ProcVarAssocList, QRVAssocList, A) :- primitiveConstraint(C, ProcVarAssocList, QRVAssocList, A).
constraint(not(C), ProcVarAssocList, QRVAssocList, A) :- constraint(C, ProcVarAssocList, QRVAssocList, A).

constraint(C, ProcVarAssocList, QRVAssocList, A) :-
	C =.. [Op,C1,C2],
	member(Op,[and,or,impl]),
	constraint(C1, ProcVarAssocList, QRVAssocList, A), constraint(C2, ProcVarAssocList, QRVAssocList, A).
	
% Coupling constraints
% coupling_constraint(+Constraint)
% Constraint is a constraint on product and process variables, the syntax is the
% same as of node (model) constraints
check_coupling_constraints(CouplingConstraints, ProcVarAssocList, Activities, Nodes, HasPartRelations) :-
	forall(member(CC, CouplingConstraints),
		( check_cc(CC, ProcVarAssocList, Activities, Nodes, HasPartRelations)
		;	( term_to_atom(CC, CCAtom),
			  atomic_list_concat(['Error in coupling constraint ', CCAtom,'!'], Msg),
			  error_message(Msg)
			)
		)
	).
	
int_cc(T, _, _, _, _) :- integer(T).
int_cc((Label,N,M,Card), _, _, _, HasPartRelations) :-
	member((Label,N,M,(Card,_),_), HasPartRelations).
int_cc((Var,Node,_), _, _, Nodes, _) :-
	member((Node,Vars,_), Nodes),
	member((Var,Type), Vars), type(Type, int, _).
int_cc(Var, ProcVarAssocList, _, _, _) :- 
	get_assoc(Var, ProcVarAssocList, Type),
	type(Type, int, _).
int_cc(InstVar, _, Activities, _, _) :-
	member((_-(InstVar,_)-_), Activities).
					 
int_cc(TI, ProcVarAssocList, Activities, Nodes, HasPartRelations) :-
	TI =.. [Op,T1,T2],
	member(Op,[plus,minus,times,frac,mod]),
	int_cc(T1, ProcVarAssocList, Activities, Nodes, HasPartRelations), 
	int_cc(T2, ProcVarAssocList, Activities, Nodes, HasPartRelations).
int_cc(minus_sign(T), ProcVarAssocList, Activities, Nodes, HasPartRelations) :- 
	int_cc(T, ProcVarAssocList, Activities, Nodes, HasPartRelations).

string_cc((Var,Node,_), _, _, Nodes, _) :- 
	member((Node,Vars,_), Nodes),
	member((Var,Type), Vars), type(Type, string, _).
string_cc(Var, ProcVarAssocList, _, _, _) :- 
	get_assoc(Var, ProcVarAssocList, Type), 
	type(Type, string, _).
string_cc(T, _, _, _, _) :- atom(T).

primitive_cc(eq(T1,T2), ProcVarAssocList, Activities, Nodes, HasPartRelations) :-
	int_cc(T1, ProcVarAssocList, Activities, Nodes, HasPartRelations), 
	int_cc(T2, ProcVarAssocList, Activities, Nodes, HasPartRelations).

primitive_cc(eq(T1,T2), ProcVarAssocList, Activities, Nodes, HasPartRelations) :- 
	string_cc(T1, ProcVarAssocList, Activities, Nodes, HasPartRelations), 
	string_cc(T2, ProcVarAssocList, Activities, Nodes, HasPartRelations).

check_cc(C, ProcVarAssocList, Activities, Nodes, HasPartRelations) :- 
	primitive_cc(C, ProcVarAssocList, Activities, Nodes, HasPartRelations).
check_cc(not(C), ProcVarAssocList, Activities, Nodes, HasPartRelations) :- 
	check_cc(C, ProcVarAssocList, Activities, Nodes, HasPartRelations).

check_cc(C, ProcVarAssocList, Activities, Nodes, HasPartRelations) :-
	C =.. [Op,C1,C2],
	member(Op,[and,or,impl]),
	check_cc(C1, ProcVarAssocList, Activities, Nodes, HasPartRelations), 
	check_cc(C2, ProcVarAssocList, Activities, Nodes, HasPartRelations).