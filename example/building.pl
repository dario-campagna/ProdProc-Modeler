:- module(building_test,
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

:- discontiguous building_test:edge/4.
:- discontiguous building_test:edge/5.
:- discontiguous building_test:nodeModelConstraint/1.
:- discontiguous building_test:aggConstraint/4.
:- discontiguous building_test:allDifferentValues/1.
:- discontiguous building_test:cardModelConstraint/1.
:- discontiguous building_test:process_variable/2.
:- discontiguous building_test:res/3.
:- discontiguous building_test:resource_constraint/5.
:- discontiguous building_test:temporal_constraint/1.
:- discontiguous building_test:coupling_constraint/1.
:- discontiguous building_test:produces_for/4.
:- discontiguous building_test:type/3.


type(type_10,int,[3,6]).
type(type_11,string,[ct1,ct2]).
type(type_15,string,[flat,sloping]).
type(type_7,string,[floor,ceiling]).
type(type_12,string,[heavy,light_1,light_2]).
type(type_8,string,[hydronic_convectors,floor_heating]).
type(type_6,string,[medium,normal]).
type(type_13,string,[pc1,pc2,pc3]).
type(type_14,string,[screed_covered,hollow_floor]).
type(type_4,string,[vav,vavInd,cav,multizone]).
type(type_1,string,[warehouse,office_building]).
type(type_24,int,(-10,-6)).
type(type_26,int,(-10,-5)).
type(type_18,int,(-10,-4)).
type(type_27,int,(-10,-3)).
type(type_25,int,(-10,-2)).
type(type_29,int,(-4,-2)).
type(type_20,int,(-3,-1)).
type(type_21,int,(-2,-1)).
type(type_22,int,(-1,0)).
type(type_5,int,(0,1)).
type(type_23,int,(0,2)).
type(type_19,int,(0,3)).
type(type_28,int,(0,4)).
type(type_17,int,(0,10)).
type(type_16,int,(0,45)).
type(type_2,int,(1,3)).
type(type_9,int,(3,15)).
type(type_3,int,(7,90)).
type(type_30,int,(49,8100)).

node('Building',[('BuildingType',type_1),('StoryNum',type_2),('Width',type_3),('Length',type_3)],[impl(eq('BuildingType','warehouse'),eq('StoryNum',1)),impl(gt('StoryNum',1),eq('BuildingType','office_building'))]).
node('Ventilation service',[('VentType',type_4)],[impl(eq(('BuildingType','Building',['ventilation']),'warehouse'),neq('VentType','vavInd')),impl(eq(('BuildingType','Building',['ventilation']),'office_building'),neq('VentType','cav'))]).
node('Sanitary service',[('Gas',type_5),('Water',type_5),('SanInstall',type_5)],[impl(eq(('BuildingType','Building',['sanitary']),'office_building'),eq('SanInstall',1)),impl(eq('SanInstall',1),eq('Water',1))]).
node('Electr./Light. service',[('Voltage',type_6),('ElectrInstall',type_7)],[impl(eq(('BuildingType','Building',['electr./light.']),'office_building'),eq('Voltage','normal'))]).
node('Heating service',[('HeatType',type_8)],[impl(eq(('BuildingType','Building',['heating']),'warehouse'),neq('HeatType','floor_heating'))]).
node('Story',[('FloorNum',type_2),('Height',type_9)],[eq('FloorNum',plus(('FloorNum','Story',['upper story']),1)),leq('FloorNum',('StoryNum','Building',['first story',*])),impl(eq(('BuildingType','Building',['first story',*]),'office_building'),and(geq('Height',4),leq('Height',5)))]).
node('Basement',[('Height',type_10)],[]).
node('Suspended ceiling',[('CeilingType',type_11)],[]).
node('Partition wall system',[('PartType',type_12),('Conf',type_13)],[]).
node('Floor',[('FloorType',type_14),('SoundAbsorption',type_5)],[impl(eq('FloorType','hollow_floor'),eq('SoundAbsorption',0))]).
node('Roof',[('RoofType',type_15),('Angle',type_16)],[impl(eq('RoofType','flat'),eq('Angle',0)),impl(eq('RoofType','sloping'),gt('Angle',0))]).

edge('ventilation','Building','Ventilation service',('Card',type_5),[]).
edge('sanitary','Building','Sanitary service',('Card',type_5),[impl(eq('BuildingType','office_building'),eq('Card',1))]).
edge('electr./light.','Building','Electr./Light. service',1,[]).
edge('heating','Building','Heating service',('Card',type_5),[impl(eq('BuildingType','office_building'),eq('Card',1))]).
edge('first story','Building','Story',1,[]).
edge('upper story','Story','Story',('Card',type_5),[impl(eq('FloorNum',('StoryNum','Building',['first story',*])),eq('Card',0)),impl(lt('FloorNum',('StoryNum','Building',['first story',*])),eq('Card',1))]).
edge('basement','Building','Basement',('Card',type_5),[]).
edge('ceiling','Story','Suspended ceiling',('Card',type_5),[impl(eq(('BuildingType','Building',['first story']),'warehouse'),eq('Card',0)),impl(eq(('BuildingType','Building',['first story',*]),'office_building'),eq('Card',1))]).
edge('walls','Story','Partition wall system',('Card',type_5),[impl(eq(('BuildingType','Building',['first story']),'warehouse'),eq('Card',0)),impl(neq(('BuildingType','Building',['first story',*]),'warehouse'),eq('Card',1))]).
edge('floor','Story','Floor',('Card',type_5),[impl(eq(('BuildingType','Building',['first story',*]),'office_building'),eq('Card',1))]).
edge('roof','Story','Roof',('Card',type_5),[]).

nodeModelConstraint(eq(('FloorNum','Story',['first story']),1)).
cardModelConstraint(neq(('upper story','Story','Story','Card'),('roof','Story','Roof','Card'))).
nodeModelConstraint(impl(eq(('ElectrInstall','Electr./Light. service',['electr./light.']),'floor'),eq(('FloorType','Floor',['floor']),'hollow_floor'))).


process_variable('SubImp',type_5).
process_variable('PitLin',type_5).
process_variable('Ceiling',type_5).
process_variable('Part',type_5).
process_variable('Floor',type_5).
process_variable('Vent',type_5).
process_variable('San',type_5).
process_variable('Heat',type_5).
process_variable('StoryNum',type_2).
process_variable('BuildingArea',type_30).
process_variable('Basement',type_5).

activity('Prep. dev. build. site',comp,'main',nil,[]).
activity('Sec. terrain',atomic,'Prep. dev. build. site',nil,[eq('d_Sec. terrain',frac('BuildingArea',times(2,abs('q_GW'))))]).
activity('Prep. terrain',atomic,'Prep. dev. build. site',nil,[eq('d_Prep. terrain',frac('BuildingArea',plus(abs('q_GW'),plus(times(2,abs('q_E')),times(2,abs('q_T'))))))]).
activity('Sup. disp. conn.',atomic,'Prep. dev. build. site',nil,[eq('d_Sup. disp. conn.',frac('BuildingArea',plus(times(2,abs('q_GW')),times(4,abs('q_E')))))]).
activity('Lines',atomic,'Prep. dev. build. site',nil,[eq('d_Lines',frac('BuildingArea',plus(times(2,abs('q_GW')),plus(times(2,abs('q_E')),times(2,abs('q_T'))))))]).
activity('Roads',atomic,'Prep. dev. build. site',nil,[eq('d_Roads',frac('BuildingArea',plus(plus(abs('q_GW'),times(2,abs('q_E'))),plus(times(2,abs('q_T')),times(3,abs('q_S'))))))]).
activity('Site facilities',atomic,'Prep. dev. build. site',nil,[eq('d_Site facilities',frac('BuildingArea',plus(times(2,abs('q_GW')),plus(times(3,abs('q_E')),times(3,abs('q_T'))))))]).
activity('Subsoil imp.',atomic,'Prep. dev. build. site',nil,[eq('d_Subsoil imp.',frac('BuildingArea',plus(abs('q_GW'),plus(times(3,abs('q_E')),times(3,abs('q_T'))))))]).
activity('Build. shell build. env.',comp,'main',nil,[]).
activity('Excavation',atomic,'Build. shell build. env.',nil,[eq('d_Excavation',plus(times('Basement',frac('BuildingArea',plus(abs('q_GW'),plus(times(2,abs('q_E')),times(2,abs('q_T')))))),times(minus(1,'Basement'),frac('BuildingArea',plus(times(2,abs('q_GW')),plus(times(3,abs('q_E')),times(2,abs('q_T'))))))))]).
activity('Pit lining',atomic,'Build. shell build. env.',nil,[]).
activity('Foundation',atomic,'Build. shell build. env.',nil,[eq('d_Foundation',frac('BuildingArea',plus(times(4,abs('q_GW')),times(2,abs('q_M')))))]).
activity('Rising struct. design',atomic,'Build. shell build. env.',nil,[eq('d_Rising struct. design',frac(times('BuildingArea','StoryNum'),plus(plus(abs('q_GW'),times(3,abs('q_M'))),plus(times(2,abs('q_T')),times(3,abs('q_C'))))))]).
activity('Carpentry',atomic,'Build. shell build. env.',nil,[eq('d_Carpentry',frac(times('BuildingArea','StoryNum'),abs('q_GW')))]).
activity('Facade',atomic,'Build. shell build. env.',nil,[eq('d_Facade',frac(times('BuildingArea','StoryNum'),plus(abs('q_GW'),plus(times(2,abs('q_T')),abs('q_C')))))]).
activity('Roof seal. plumb.',atomic,'Build. shell build. env.',nil,[eq('d_Roof seal. plumb.',frac(times('BuildingArea','StoryNum'),plus(abs('q_GW'),plus(abs('q_T'),abs('q_C')))))]).
activity('Finishing works',multicomp,'main',('insts_fin_works',type_2),[]).
activity('Assembly ceil. sus.',atomic,'Finishing works',nil,[eq('d_Assembly ceil. sus.',frac('BuildingArea',times(4,abs('q_GW'))))]).
activity('Inst. substr.',atomic,'Finishing works',nil,[eq('d_Inst. substr.',frac('BuildingArea',times(4,abs('q_GW'))))]).
activity('Align. struct.',atomic,'Finishing works',nil,[eq('d_Align. struct.',frac('BuildingArea',times(4,abs('q_GW'))))]).
activity('Wall. conn.',atomic,'Finishing works',nil,[eq('d_Wall. conn.',frac('BuildingArea',abs('q_GW')))]).
activity('Inst. light.',atomic,'Finishing works',nil,[eq('d_Inst. light.',frac('BuildingArea',times(4,abs('q_E'))))]).
activity('Vent. diff. rails',atomic,'Finishing works',nil,[eq('d_Vent. diff. rails',frac('BuildingArea',times(4,abs('q_E'))))]).
activity('Conn. rails',atomic,'Finishing works',nil,[eq('d_Conn. rails',frac('BuildingArea',times(4,abs('q_E'))))]).
activity('Inst. walls',atomic,'Finishing works',nil,[eq('d_Inst. walls',frac('BuildingArea',abs('q_GW')))]).
activity('Vent. electr. test',atomic,'Finishing works',nil,[eq('d_Vent. electr. test',frac('BuildingArea',times(4,abs('q_E'))))]).
activity('Ins. ceiling panels',atomic,'Finishing works',nil,[eq('d_Ins. ceiling panels',frac('BuildingArea',times(4,abs('q_GW'))))]).
activity('Ins. acustic mats',atomic,'Finishing works',nil,[eq('d_Ins. acustic mats',frac('BuildingArea',times(4,abs('q_GW'))))]).
activity('Mirror lamps blinds',atomic,'Finishing works',nil,[eq('d_Mirror lamps blinds',frac('BuildingArea',times(4,abs('q_E'))))]).
activity('Close slits',atomic,'Finishing works',nil,[eq('d_Close slits',frac('BuildingArea',times(4,abs('q_GW'))))]).
activity('Floor installation',atomic,'Finishing works',nil,[eq('d_Floor installation',frac('BuildingArea',times(2,abs('q_GW'))))]).
activity('Build. service equip.',comp,'main',nil,[]).
activity('Roof insulation',atomic,'Build. service equip.',nil,[eq('d_Roof insulation',frac('BuildingArea',plus(times(2,abs('q_GW')),plus(times(2,abs('q_T')),times(3,abs('q_C'))))))]).
activity('Vent. assembly',atomic,'Build. service equip.',nil,[eq('d_Vent. assembly',times('StoryNum',18))]).
activity('Heat. assembly',atomic,'Build. service equip.',nil,[eq('d_Heat. assembly',frac(times('StoryNum',27),abs('q_P')))]).
activity('San. assembly',atomic,'Build. service equip.',nil,[eq('d_San. assembly',frac(times('StoryNum',27),abs('q_P')))]).
activity('Pressure test',atomic,'Build. service equip.',nil,[eq('d_Pressure test',frac(times('StoryNum',27),abs('q_P')))]).
activity('Service insulation',atomic,'Build. service equip.',nil,[eq('d_Service insulation',frac(times('StoryNum',12),abs('q_P')))]).

res('General Workers',type_17,10).
res('Trucks',type_19,3).
res('Excavators',type_19,3).
res('Steamrollers',type_23,2).
res('Cranes',type_23,2).
res('Mixers',type_23,2).
res('Electricians',type_28,4).
res('Vent. inst.',type_23,2).
res('Plumbers',type_28,4).

resource_constraint('Sec. terrain','General Workers',('q_GW',type_18),'FromStartToEnd',true).
resource_constraint('Prep. terrain','General Workers',('q_GW',type_18),'FromStartToEnd',true).
resource_constraint('Sup. disp. conn.','General Workers',('q_GW',type_18),'FromStartToEnd',true).
resource_constraint('Lines','General Workers',('q_GW',type_18),'FromStartToEnd',true).
resource_constraint('Subsoil imp.','General Workers',('q_GW',type_18),'FromStartToEnd',true).
resource_constraint('Roads','General Workers',('q_GW',type_18),'FromStartToEnd',true).
resource_constraint('Site facilities','General Workers',('q_GW',type_18),'FromStartToEnd',true).
resource_constraint('Prep. terrain','Trucks',('q_T',type_20),'FromStartToEnd',true).
resource_constraint('Lines','Trucks',('q_T',type_20),'FromStartToEnd',true).
resource_constraint('Roads','Trucks',('q_T',type_21),'FromStartToEnd',true).
resource_constraint('Subsoil imp.','Trucks',('q_T',type_21),'FromStartToEnd',true).
resource_constraint('Site facilities','Trucks',('q_T',type_22),'FromStartToEnd',true).
resource_constraint('Prep. terrain','Excavators',('q_E',type_20),'FromStartToEnd',true).
resource_constraint('Sup. disp. conn.','Excavators',('q_E',type_20),'FromStartToEnd',true).
resource_constraint('Lines','Excavators',('q_E',type_20),'FromStartToEnd',true).
resource_constraint('Roads','Excavators',('q_E',type_21),'FromStartToEnd',true).
resource_constraint('Subsoil imp.','Excavators',('q_E',type_21),'FromStartToEnd',true).
resource_constraint('Site facilities','Excavators',('q_E',type_22),'FromStartToEnd',true).
resource_constraint('Roads','Steamrollers',('q_S',type_21),'FromStartToEnd',true).
resource_constraint('Excavation','General Workers',('q_GW',type_18),'FromStartToEnd',true).
resource_constraint('Pit lining','General Workers',('q_GW',type_18),'FromStartToEnd',true).
resource_constraint('Foundation','General Workers',('q_GW',type_18),'FromStartToEnd',true).
resource_constraint('Rising struct. design','General Workers',('q_GW',type_24),'FromStartToEnd',true).
resource_constraint('Carpentry','General Workers',('q_GW',type_25),'FromStartToEnd',true).
resource_constraint('Facade','General Workers',('q_GW',type_26),'FromStartToEnd',true).
resource_constraint('Roof seal. plumb.','General Workers',('q_GW',type_26),'FromStartToEnd',true).
resource_constraint('Roof seal. plumb.','Cranes',('q_C',type_21),'FromStartToEnd',true).
resource_constraint('Facade','Cranes',('q_C',type_21),'FromStartToEnd',true).
resource_constraint('Rising struct. design','Cranes',('q_C',type_21),'FromStartToEnd',true).
resource_constraint('Excavation','Excavators',('q_E',type_20),'FromStartToEnd',true).
resource_constraint('Pit lining','Excavators',('q_E',type_21),'FromStartToEnd',true).
resource_constraint('Excavation','Trucks',('q_T',type_20),'FromStartToEnd',true).
resource_constraint('Pit lining','Trucks',('q_T',type_21),'FromStartToEnd',true).
resource_constraint('Rising struct. design','Trucks',('q_T',type_20),'FromStartToEnd',true).
resource_constraint('Facade','Trucks',('q_T',type_21),'FromStartToEnd',true).
resource_constraint('Roof seal. plumb.','Trucks',('q_T',type_21),'FromStartToEnd',true).
resource_constraint('Foundation','Mixers',('q_M',type_21),'FromStartToEnd',true).
resource_constraint('Rising struct. design','Mixers',('q_M',type_21),'FromStartToEnd',true).
resource_constraint('Facade','Mixers',('q_M',type_21),'FromStartToEnd',true).
resource_constraint('Assembly ceil. sus.','General Workers',('q_GW',type_27),'FromStartToEnd',true).
resource_constraint('Inst. substr.','General Workers',('q_GW',type_27),'FromStartToEnd',true).
resource_constraint('Align. struct.','General Workers',('q_GW',type_27),'FromStartToEnd',true).
resource_constraint('Wall. conn.','General Workers',('q_GW',type_25),'FromStartToEnd',true).
resource_constraint('Inst. walls','General Workers',('q_GW',type_25),'FromStartToEnd',true).
resource_constraint('Ins. acustic mats','General Workers',('q_GW',type_25),'FromStartToEnd',true).
resource_constraint('Ins. ceiling panels','General Workers',('q_GW',type_25),'FromStartToEnd',true).
resource_constraint('Close slits','General Workers',('q_GW',type_25),'FromStartToEnd',true).
resource_constraint('Floor installation','General Workers',('q_GW',type_25),'FromStartToEnd',true).
resource_constraint('Inst. light.','Electricians',('q_E',type_29),'FromStartToEnd',true).
resource_constraint('Vent. diff. rails','Electricians',('q_E',type_29),'FromStartToEnd',true).
resource_constraint('Conn. rails','Electricians',('q_E',type_29),'FromStartToEnd',true).
resource_constraint('Vent. electr. test','Electricians',('q_E',type_29),'FromStartToEnd',true).
resource_constraint('Mirror lamps blinds','Electricians',('q_E',type_29),'FromStartToEnd',true).
resource_constraint('Vent. diff. rails','Vent. inst.',-2,'FromStartToEnd',true).
resource_constraint('Conn. rails','Vent. inst.',-2,'FromStartToEnd',true).
resource_constraint('Vent. electr. test','Vent. inst.',('q_V',type_21),'FromStartToEnd',true).
resource_constraint('San. assembly','Plumbers',('q_P',type_29),'FromStartToEnd',true).
resource_constraint('Pressure test','Plumbers',('q_P',type_29),'FromStartToEnd',true).
resource_constraint('Service insulation','Plumbers',('q_P',type_29),'FromStartToEnd',true).
resource_constraint('Heat. assembly','Plumbers',('q_P',type_29),'FromStartToEnd',true).
resource_constraint('Vent. assembly','Vent. inst.',-2,'FromStartToEnd',true).
resource_constraint('Service insulation','Vent. inst.',-2,'FromStartToEnd',true).
resource_constraint('Roof insulation','General Workers',('q_GW',type_18),'FromStartToEnd',true).
resource_constraint('Roof insulation','Cranes',('q_C',type_21),'FromStartToEnd',true).
resource_constraint('Roof insulation','Trucks',('q_T',type_21),'FromStartToEnd',true).

temporal_constraint(must_be_executed('Prep. dev. build. site')).
temporal_constraint(must_be_executed('Sec. terrain')).
temporal_constraint(must_be_executed('Prep. terrain')).
temporal_constraint(before('Sec. terrain','Prep. terrain')).
temporal_constraint(must_be_executed('Sup. disp. conn.')).
temporal_constraint(before('Prep. terrain','Sup. disp. conn.')).
temporal_constraint(must_be_executed('Lines')).
temporal_constraint(before('Prep. terrain','Lines')).
temporal_constraint(must_be_executed('Roads')).
temporal_constraint(before('Sup. disp. conn.','Roads')).
temporal_constraint(before('Lines','Roads')).
temporal_constraint(must_be_executed('Site facilities')).
temporal_constraint(before('Roads','Site facilities')).
temporal_constraint(cond(iff(eq('SubImp',0)),is_absent('Subsoil imp.'))).
temporal_constraint(or(equals('Roads','Subsoil imp.'),before('Roads','Subsoil imp.'))).
temporal_constraint(must_be_executed('Build. shell build. env.')).
temporal_constraint(must_be_executed('Excavation')).
temporal_constraint(cond(iff(eq('PitLin',0)),is_absent('Pit lining'))).
temporal_constraint(equals('Excavation','Pit lining')).
temporal_constraint(must_be_executed('Foundation')).
temporal_constraint(before('Excavation','Foundation')).
temporal_constraint(must_be_executed('Rising struct. design')).
temporal_constraint(before('Foundation','Rising struct. design')).
temporal_constraint(must_be_executed('Carpentry')).
temporal_constraint(before('Rising struct. design','Carpentry')).
temporal_constraint(must_be_executed('Facade')).
temporal_constraint(before('Carpentry','Facade')).
temporal_constraint(must_be_executed('Roof seal. plumb.')).
temporal_constraint(before('Carpentry','Roof seal. plumb.')).
temporal_constraint(before('Prep. dev. build. site','Build. shell build. env.')).
temporal_constraint(must_be_executed('Finishing works')).
temporal_constraint(cond(iff(eq('Ceiling',0)),is_absent('Assembly ceil. sus.'))).
temporal_constraint(succeeded_by('Assembly ceil. sus.','Inst. substr.')).
temporal_constraint(succeeded_by('Inst. substr.','Align. struct.')).
temporal_constraint(cond(iff(eq('Part',0)),is_absent('Wall. conn.'))).
temporal_constraint(before('Align. struct.','Wall. conn.')).
temporal_constraint(must_be_executed('Inst. light.')).
temporal_constraint(before('Align. struct.','Inst. light.')).
temporal_constraint(must_be_executed('Vent. diff. rails')).
temporal_constraint(before('Align. struct.','Vent. diff. rails')).
temporal_constraint(must_be_executed('Conn. rails')).
temporal_constraint(before('Vent. diff. rails','Conn. rails')).
temporal_constraint(before('Inst. light.','Conn. rails')).
temporal_constraint(before('Wall. conn.','Conn. rails')).
temporal_constraint(succeeded_by('Wall. conn.','Inst. walls')).
temporal_constraint(must_be_executed('Vent. electr. test')).
temporal_constraint(before('Conn. rails','Vent. electr. test')).
temporal_constraint(before('Inst. walls','Ins. ceiling panels')).
temporal_constraint(cond(iff(eq('Part',0)),is_absent('Ins. acustic mats'))).
temporal_constraint(before('Vent. electr. test','Ins. acustic mats')).
temporal_constraint(succeeded_by('Ins. acustic mats','Ins. ceiling panels')).
temporal_constraint(must_be_executed('Mirror lamps blinds')).
temporal_constraint(before('Ins. ceiling panels','Mirror lamps blinds')).
temporal_constraint(before('Vent. electr. test','Mirror lamps blinds')).
temporal_constraint(must_be_executed('Close slits')).
temporal_constraint(before('Mirror lamps blinds','Close slits')).
temporal_constraint(cond(iff(eq('Floor',0)),is_absent('Floor installation'))).
temporal_constraint(before('Close slits','Floor installation')).
temporal_constraint(must_be_executed('Build. service equip.')).
temporal_constraint(must_be_executed('Roof insulation')).
temporal_constraint(cond(iff(eq('Vent',0)),is_absent('Vent. assembly'))).
temporal_constraint(before('Roof insulation','Vent. assembly')).
temporal_constraint(cond(iff(eq('Heat',0)),is_absent('Heat. assembly'))).
temporal_constraint(before('Roof insulation','Heat. assembly')).
temporal_constraint(cond(iff(eq('San',0)),is_absent('San. assembly'))).
temporal_constraint(before('Roof insulation','San. assembly')).
temporal_constraint(succeeded_by('Heat. assembly','Pressure test')).
temporal_constraint(succeeded_by('San. assembly','Pressure test')).
temporal_constraint(succeeded_by('Vent. assembly','Service insulation')).
temporal_constraint(succeeded_by('Pressure test','Service insulation')).
temporal_constraint(before('Build. shell build. env.','Build. service equip.')).
temporal_constraint(before('Build. service equip.','Finishing works')).


coupling_constraint(eq(('sanitary','Building','Sanitary service','Card'),'San')).
coupling_constraint(eq(('heating','Building','Heating service','Card'),'Heat')).
coupling_constraint(eq(('ventilation','Building','Ventilation service','Card'),'Vent')).
coupling_constraint(eq(('StoryNum','Building',[]),'insts_fin_works')).
coupling_constraint(eq(('StoryNum','Building',[]),'StoryNum')).
coupling_constraint(eq(times(('Width','Building',[]),('Length','Building',[])),'BuildingArea')).
coupling_constraint(eq(('floor','Story','Floor','Card'),'Floor')).
coupling_constraint(eq(('ceiling','Story','Suspended ceiling','Card'),'Ceiling')).
coupling_constraint(eq(('walls','Story','Partition wall system','Card'),'Part')).
coupling_constraint(eq(('basement','Building','Basement','Card'),'Basement')).
