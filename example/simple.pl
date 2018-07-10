:- module(simple,
	[type/3,
	 node/3,
	 edge/4,
	 edge/5,
	 nodeModelConstraint/1,
	 aggConstraint/4,
	 allDifferentValues/1,
	 cardModelConstraint/1,
	 process_variable/2,
	 activity/5,
	 res/3,
	 resource_constraint/5,
	 temporal_constraint/1,
	 coupling_constraint/1,
	 produces_for/4]).

:- dynamic simple:edge/4.
:- dynamic simple:edge/5.
:- dynamic simple:nodeModelConstraint/1.
:- dynamic simple:aggConstraint/4.
:- dynamic simple:allDifferentValues/1.
:- dynamic simple:cardModelConstraint/1.
:- dynamic simple:process_variable/2.
:- dynamic simple:res/3.
:- dynamic simple:resource_constraint/5.
:- dynamic simple:temporal_constraint/1.
:- dynamic simple:coupling_constraint/1.
:- dynamic simple:produces_for/4.
:- dynamic simple:type/3.


type(type_3,int,(0,3)).
type(type_1,int,(1,10)).
type(type_2,int,(2,4)).

node('A',[('n',type_1)],[]).
node('B',[('m',type_2)],[]).

edge('has','A','B',('Card',type_3),[]).




activity('New activity',atomic,'main',nil,[]).





