(* ::Package:: *)

(* ::Text:: *)
(*This file contains all the routines necessary for the extraction of the counterterm Lagrangian.*)


(* ::Section::Closed:: *)
(*Field renormalization*)


(* ::Subsection:: *)
(*Rewriting the arguments of the functions*)


FieldRenormalization[field_[inds__]]:=RenField[field,{inds}];

FieldRenormalization[field_]:=RenField[field,{}];


(* ::Subsection:: *)
(*Core function: returns a renormalized field from a bare field*)


(* ::Text:: *)
(*The list of indices must be given as a second argument*)


RenField[field_,inds__List]:=Block[{rfield=field,NewIndices,FreeIndices, ColorIndicesRules, fsym,MixList, spi, result,myRule},
  (* Restore the types of the indices *)
  FreeIndices=Inner[Index,$IndList[field]/.Index[aa_]->aa,inds/.Index[_,bb_]->bb,List];
  ColorIndicesRules=(myRule[#/.Index[typ_,bla_]->Index[typ,Blank[]],#]&/@Cases[FreeIndices,Index[Colour,_]|Index[Gluon,_]])/.myRule->Rule;

  (* Create new indices and remove Spin and Lorentz indices from the list (the renormalization constants do not carry them) *)
  NewIndices=DeleteCases[FreeIndices/.Inner[Rule,inds,Table[Unique["idx"],{Length[inds]}],List],Index[Lorentz|Spin,__]];

  (* Compute the list of fields which may mix with 'field' at the one-loop level *)
  If[AntiFieldQ[field]===True,rfield=anti[field]];
  fsym=Plus@@Extract[MR$ClassesList,Position[ClassName/.MR$ClassesRules[#]&/@ MR$ClassesList,rfield]];
  MixList=ClassName/.MR$ClassesRules[#]&/@  (DeleteCases[MR$ClassesList,_?( (Unphysical/.MR$ClassesRules[#])===True || 
    $IndList[(ClassName/.MR$ClassesRules[#])]=!=$IndList[rfield]|| (QuantumNumbers/.MR$ClassesRules[#])=!=(QuantumNumbers/.MR$ClassesRules[fsym])&)]); 

  (* Replace the bare 'field' by the corresponding renormalized quantity *)
  result=Which[
    (* Fermion fields *)
    FermionQ[rfield]===True && GhostFieldQ[rfield]=!=True, 
      spi=Index[Spin,Unique["sp"]];
      Plus@@ List[ rfield[Sequence@@FreeIndices], 
        Sequence@@ (1/2 FR$deltaZ[{rfield,#},
          Inner[List,DeleteCases[FreeIndices,Index[Spin,_]|Index[Colour,_]],DeleteCases[NewIndices,Index[Colour,_]],List],"L"] *
          ProjM[FreeIndices[[1]],spi] #[spi,Sequence@@NewIndices]/.ColorIndicesRules&/@MixList), 
        Sequence@@ (1/2 FR$deltaZ[{rfield,#},
          Inner[List,DeleteCases[FreeIndices,Index[Spin,_]|Index[Colour,_]],DeleteCases[NewIndices,Index[Colour,_]],List],"R"] *
          ProjP[FreeIndices[[1]],spi] #[spi,Sequence@@NewIndices]/.ColorIndicesRules&/@MixList)],
    (* Vector fields *)
    VectorFieldQ[rfield]===True, 
      Plus@@ List[ rfield[Sequence@@FreeIndices],Sequence@@ (1/2 FR$deltaZ[{rfield,#},
        Inner[List,DeleteCases[FreeIndices,Index[Lorentz,_]|Index[Gluon,_]],DeleteCases[NewIndices,Index[Gluon,_]],List]] *
        (#[FreeIndices[[1]],Sequence@@NewIndices]/.ColorIndicesRules)&/@MixList)],
    (* Scalar fields and ghosts*)
    ScalarFieldQ[rfield]===True,
    Plus@@ List[ rfield[Sequence@@FreeIndices]/.rfield[]->rfield,Sequence@@ (1/2 FR$deltaZ[{rfield,#},
        Inner[List,DeleteCases[FreeIndices,Index[Colour,_] | Index[Gluon,_]],DeleteCases[NewIndices,Index[Colour,_]| Index[Gluon,_]],List]] *
        (#[Sequence@@NewIndices]/.#[]->#/.ColorIndicesRules)&/@MixList)]
  ]; 

  If[AntiFieldQ[field]===True,result=result/.{rfield->field,FR$deltaZ[args__]->Conjugate[FR$deltaZ[args]],ProjP[a_,b_]->ProjM[b,a],ProjM[a_,b_]->ProjP[b,a]}];

  (* Return the result *)
result];


(* ::Subsection:: *)
(*Derive all the renormalized fields and antifields*)


(* ::Text:: *)
(*This produces a list of rules allowing to replace bare fields by renormalized ones for the while model field content.*)


FieldRenormalization[]:=Block[{MyModule,MyRuleDelayed},
  Flatten[Block[{Inds,bare,ren,antibare,antiren,SummedInds,IDX, MyPattern},
  (* Bare and renormalized fields *)
  Inds=Table[Unique["idx"],{Length[$IndList[#]]}];
  bare=#[Sequence@@Inds]/.#[]->#;
  ren=FieldRenormalization[bare]; 

  (* Gets the summed indices appearing in the renormalized field *)
  SummedInds=DeleteCases[List@@ren/.Index[_,aaa_]->aaa,bare]/.{
    FR$deltaZ[_List,args_,opt___]:>IDX[Sequence@@Flatten[args]], 
    ProjP[args__]->IDX[args],ProjM[args__]->IDX[args],
    fi_?(FieldQ[#]===True&)[args__]->IDX[args]}; 
  SummedInds=SummedInds//.{IDX[]->1,IDX[aaa__] IDX[bbb__]->IDX[aaa,bbb],fi_?(FieldQ[#]===True&)->1};
  SummedInds=DeleteCases[SummedInds,_?(NumericQ[#]&)];
  SummedInds=Union@@((SummedInds//.muf_ IDX[aaa__] :>Cases[Tally[{aaa}],{_,2}])/.List[a_,2]->a);

  (* Prepare the replacement rule *)
  Inds=(MyPattern[#,Blank[]]&/@Inds);
  bare=#[Sequence@@Inner[Index,$IndList[#]/.Index[aa_]->aa,Inds,List]]/.#[]->#;
  If[SummedInds=!={},ren=MyModule[SummedInds,ren]];

  (* Antifields *)
  If[SelfConjugateQ[#]=!=True,
    antibare=If[FermionQ[#]===True && GhostFieldQ[#]=!=True,bare/.#->HC[#].Ga[0],bare/.#->anti[#]];
    antiren=ren/.{fi_?(FieldQ[#]===True&)[args__]->(anti[fi])[args],fi_?(FieldQ[#]===True&)->anti[fi]}/.{
      FR$deltaZ[args__]->Conjugate[FR$deltaZ[args]],ProjP[a_,b_]->ProjM[b,a],ProjM[a_,b_]->ProjP[b,a]}/.
      Conjugate[FR$deltaZ[{fi_?(AntiFieldQ[#]===True&),gi_?(AntiFieldQ[#]===True&)},rest_List,opt___]]->Conjugate[FR$deltaZ[{anti[fi],anti[gi]},rest,opt]];
  ];

  (* output *)
  If[SelfConjugateQ[#]===True,
    {MyRuleDelayed[bare,ren]}, 
    {MyRuleDelayed[bare,ren],MyRuleDelayed[antibare,antiren]}]/.MyPattern->Pattern]&/@(DeleteCases[MR$ClassNameList,_?(UnphysicalQ[#]===True&)])/.
    MyRuleDelayed:>RuleDelayed/.MyModule->Module]
];


(* ::Section::Closed:: *)
(*Parameter renormalization*)


(* ::Subsection:: *)
(*Formatting the functions*)


ParameterRenormalization[param_[inds__]]:=RenPrm[param,{inds}];

ParameterRenormalization[param_]:=RenPrm[param,{}];


(* ::Subsection:: *)
(*Main function: returns a renormalized parameter*)


RenPrm[param_,inds__List]:=Block[{PrmIndices={}, result},
  (* Restore the types of the indices *)
  If[inds=!={},PrmIndices=Inner[Index,$IndList[param]/.Index[aa_]->aa,inds/.Index[_,bb_]->bb,List]];

  (* Results *)
  (param[Sequence@@PrmIndices]/.param[]->param)+FR$delta[{param},PrmIndices]
];


(* ::Subsection:: *)
(*Derive all the renormalized parameters*)


(* ::Text:: *)
(*This produces a list of rules allowing to replace bare external parameters by renormalized ones*)


ParameterRenormalization[]:=Flatten[Block[{bare, ren, Inds,MyPattern,bare2,ren2,results,testpname,dum=#},
  (* Initialization *)
  testpname=If[MemberQ[MR$ParameterList,#], bare2=(ParameterName/.MR$ParameterRules[#]); bare2=!=ParameterName,False];
  If[testpname, If[ListQ[bare2], bare2=bare2/.Rule[a_,b_]->b, bare2={bare2}],False];

  (* Bare and renormalized parameter *)
  Inds=If[ListQ[$IndList[#]],Table[Unique["idx"],{Length[$IndList[#]]}],{}];
  bare=#[Sequence@@Inds]/.#[]->#;
  ren=ParameterRenormalization[bare]; 
  If[testpname, ren2=RenPrm[#,{}]&/@bare2];

  (* Prepare the replacement rule *)
  If[Inds=!={}, Inds=(MyPattern[#,Blank[]]&/@Inds); bare=#[Sequence@@Inner[Index,$IndList[#]/.Index[aa_]->aa,Inds,List]]/.#[]->#];

  (* output *)
  results=If[testpname, Join[{Rule[bare,ren]},Inner[Rule,bare2,ren2,List]], List[Rule[bare,ren]]];
  results/.MyPattern->Pattern]&/@
    (Flatten[Union[MassList[[2,All,2]],MR$ParameterList]]/.Rule[a_,b_]:>b)
];


(* ::Section::Closed:: *)
(*Perturbative expansion of the renormalization constants*)


(* ::Text:: *)
(*This function perturbatively expand renormalization constants, factorizing out the couplings as well as canonical 2 Pi factors. All the new constants are added to the parameter list of the model.*)


(* ::Subsection:: *)
(*This function prepares the addition of a renormalization constants to the parameter lists*)


(* ::Text:: *)
(*Note the treatment of the internal parameters.*)


AddRenConstToLists[deltaprm_List,param_,order_,orderlist_List]:=Block[{PrmFlag,rules,Pindx,IntPrmFlag,Pvalue,alows},
  (* Initialization *)
  PrmFlag=MemberQ[Union[MR$ParameterList,(ParameterName/.MR$ParameterRules[#])&/@MR$ParameterList],param] ;
  rules=If[ListQ[MR$ParameterRules[param]],MR$ParameterRules[param],List[]];
  Pindx=Indices/.rules/.Indices->{};
  alows=AllowSummation/.rules/.AllowSummation->False;
  IntPrmFlag=If[rules=!=List[],(ParameterType/.rules/.ParameterType->Internal)===Internal,False];

  (* Wave function renormalization constant *)
  If[param===MR$Null,CreateDeltaParams/@deltaprm];

  (* External parameters without indices, including external masses *)
  If[param=!=MR$Null && Not[IntPrmFlag] && Pindx==={},CreateDeltaParams/@deltaprm];

  (* External parameters with indices *)
  If[param=!=MR$Null && Not[IntPrmFlag] && Pindx=!={},
  CreateDeltaParams[Replace[#,fg_[iii__]:>fg[Sequence@@(Pindx/.Index[type_]->Index[type,Unique["bla"]])]],AllowSummation->alows]&/@deltaprm];

  (* Internal parameter without indices: the value function must be calculated *)
  If[param=!=MR$Null && IntPrmFlag && Pindx==={},
    (* Calculating the value *)
    Pvalue=Value/.rules;
    Pvalue=Block[{dependencies,valtmp,PrmToRenormList,PVr,PVl,muf,MyRule},
      PrmToRenormList=Flatten[Union[MassList[[2,All,2]],MR$ParameterList,(ParameterName/.MR$ParameterRules[#])&/@MR$ParameterList]]/.Rule[a_,b_]:>b;
      valtmp=Pvalue/.{Cos[x_]->x,Sin[x_]->x,Exp[x_]->x, Conjugate[x_]->x};
      dependencies=Flatten[ReplaceRepeated[valtmp,fg_?(AtomQ[#]&&Not[NumericQ[#]]&&Not[MemberQ[PrmToRenormList,#]]&)->MR$Null]/.MR$Null->List];
      dependencies=Union[DeleteCases[dependencies,_?(NumericQ[#]&)]];
      PVr=Rule[#,# (1+muf[#])]&/@dependencies;
      PVl={muf[#],0,1}&/@dependencies;
      valtmp=Normal[Series[Pvalue/.PVr,Sequence@@PVl]];
      PVr=MyRule[muf[#],(ParameterRenormalization[#]-#)/#/.FR$delta[rg__]:>PerturbativelyExpand[FR$delta[rg],order]]&/@dependencies;
      PVr=PVr/.MyRule->Rule;
      valtmp/.PVr]; 
    (* Matching the coefficients to the right terms of the expansion *)
    Block[{ valrule, term,mufmuf,keeporder},
      term=orderlist[[Position[deltaprm,#][[1,1]]]];
      Pvalue=Expand[Pvalue];
      valrule=If[Head[Pvalue]===Plus,List@@Pvalue,{Pvalue}];
      keeporder[gr_List]:=Select[gr, MatchQ[#//.{
        FR$delta[ar__][ord__]:>mufmuf[{ord}],mufmuf[aaa_List] mufmuf[bbb_List]->mufmuf[aaa+bbb], Power[mufmuf[aaa_List],n_]->mufmuf[n*aaa]},_* mufmuf[term]]&];
      valrule=Plus@@keeporder[valrule];
      CreateDeltaParams[#,Rule[Value,valrule]]]&/@deltaprm
    ];

  (* Internal parameter with indices: all the value functions must be calculated *)
  If[param=!=MR$Null && IntPrmFlag && Pindx=!={},
    (* Calculating the value *)
    Pvalue=Value/.rules;
    Pvalue=Block[{dependencies,valtmp,PrmToRenormList,PVr,PVl,muf,MyRule},
      PrmToRenormList=Flatten[Union[MassList[[2,All,2]],MR$ParameterList,(ParameterName/.MR$ParameterRules[#])&/@MR$ParameterList]]/.Rule[a_,b_]:>b;
      valtmp=Pvalue/.{Cos[x_]->x,Sin[x_]->x,Exp[x_]->x};
      dependencies=(ReplaceRepeated[#,fg_?(AtomQ[#]&&Not[NumericQ[#]]&&Not[MemberQ[PrmToRenormList,#]]&)->MR$Null]/.MR$Null->List)&/@valtmp[[All,2]];
      dependencies=Union[DeleteCases[Flatten[dependencies],_?(NumericQ[#]&)]];
      PVr=Rule[#,# (1+muf[#])]&/@dependencies;
      PVl={muf[#],0,1}&/@dependencies;
      valtmp=Rule[#[[1]],Normal[Series[#[[2]]/.PVr,Sequence@@PVl]]]&/@Pvalue;
      PVr=MyRule[muf[#],(ParameterRenormalization[#]-#)/#/.FR$delta[rg__]:>PerturbativelyExpand[FR$delta[rg],order]]&/@dependencies;
      PVr=PVr/.MyRule->Rule;
      valtmp/.PVr]; 
    (* Matching the coefficients to the right terms of the expansion *)
    Block[{ valrule, term,LeftRuleMember,mufmuf=#/.fg_[ii__]->fg,mufmuf2,keeporder},
      term=orderlist[[Position[deltaprm,#][[1,1]]]];
      Pvalue={#[[1]],Expand[#[[2]]]}&/@Pvalue;
      valrule=If[Head[#[[2]]]===Plus,List@@#[[2]],{#[[2]]}]&/@Pvalue;
      keeporder[gr_List]:=Select[gr,MatchQ[#//.{
        FR$delta[ar__][ord__]:>mufmuf[{ord}],mufmuf[aaa_List] mufmuf[bbb_List]->mufmuf[aaa+bbb],Power[mufmuf[aaa_List],n_]->mufmuf[n*aaa]},_* mufmuf[term]]&];
      valrule=(Plus@@keeporder[#])&/@valrule;
      LeftRuleMember=#/.param->mufmuf&/@Pvalue[[All,1]];
      valrule=Inner[Rule,LeftRuleMember,valrule,List]; 
      CreateDeltaParams[
        Replace[#,fg_[ii__]->fg[Sequence@@(Pindx/.Index[type_]->Index[type,Unique["bla"]])]],Rule[Value,valrule],Rule[AllowSummation,alows]]]&/@deltaprm
  ];
];


(* ::Subsection:: *)
(*Adding a renormalization constant to the parameter lists, once everything has been prepared *)


Options[CreateDeltaParams]={Value-> MR$Null, AllowSummation->False};


CreateDeltaParams[delta_,OptionsPattern[]]:= Block[{Iprm,Iinds={},Pindices, Pdescr, Pcomp, Pinter,Pall=OptionValue[AllowSummation],MyPattern,mySet,
   PValue= OptionValue[Value],description},
  (* Initialization *)
  Iprm = delta/.pr_[__]->pr;
  If[MatchQ[delta,_[__]],Iinds=delta/._[inds__]->{inds}/.Index[type_,_]->Index[type]];

  (* Add the renormalization constant to the renormalization constant list *)
  M$RenormalizationConstants=Union[{Iprm},M$RenormalizationConstants];

  (* Attributes *)
  Pindices=Rule[Indices,Iinds];
  Pdescr=Rule[Description,"Renormalization constant"];
  Pcomp=Rule[ComplexParameter,True];
  Pinter=Rule[ParameterType,Internal];

  (* Get the function for internal parameters *)
  description=List[Pinter,Pindices,Pcomp,Pdescr];  
  If[PValue=!=MR$Null,description= Append[description, Rule[Value,PValue]]];
  If[Pall=!=False,description= Prepend[description, Rule[AllowSummation,Pall]]];

  (* Declare the new parameter *)
  DeclareTensor[Iprm, Iinds,Pcomp];
  If[Iinds==={},numQ[Iprm] = True];

  (* Appending to the various list of parameters *)
  If[Not[MemberQ[M$Parameters[[All,1]],Iprm]],
    M$Parameters=Append[M$Parameters,Equal[Iprm,description]];
    MR$ParameterRules[Iprm]=description;
    If[Iinds=!={},M$IndParam=Append[M$IndParam,Iprm]]
  ];
];


(* ::Subsection::Closed:: *)
(*Main routine for the perturbative expansion of any renormalization constant*)


PerturbativelyExpand[FR$del_[args__],ExpOrder_List]:=Block[{NewOrder,param,Pindx,CTType,MyTable,MyPattern,dummy,dummy2,resu,Prules,newparam,
  PrmToRenomList,deltaFuncList,deltaParamList,deltaOrderList},

  (* Initialization *)
  NewOrder=If[MatrixQ[ExpOrder],ExpOrder,{ExpOrder}];
  PrmToRenomList=Flatten[Union[MassList[[2,All,2]],MR$ParameterList,(ParameterName/.MR$ParameterRules[#])&/@MR$ParameterList]]/.Rule[a_,b_]:>b;
  param=FR$del[args]/.FR$del[{prm_},__]->prm/.FR$del[args]->MR$Null;
  newparam=param;
  Pindx=FR$del[args]/.FR$del[_List,Indx_List]->Indx/.FR$del[args]->MR$Null;
  Prules=If[ListQ[MR$ParameterRules[param]],MR$ParameterRules[param],List[]];
  Prules=ParameterName/.Prules/.ParameterName->{};
  If[Not[ListQ[Prules]],Prules={param->Prules}];
  CTType=If[param===MR$Null,"deltaZ","delta"];

  (* Creation of three lists *)
  (* 1. deltaFuncList: list of the coefficient of the series expansion of the renormalization constant under a functional form *)
  (* 2. deltaParamList: list of the parameters associated to each coefficient *)
  (* 3. deltaOrderList: place of each coefficient in the series *)
  deltaFuncList=Drop[Flatten[
    MyTable[FR$del[dummy][NewOrder[[All,1]]],Sequence@@(List[#[[1]],0,#[[2]]] &/@NewOrder)]/.MyTable->Table],1]/.FR$del[dummy]->FR$del[args];
  deltaParamList=(#/.FR$del[fld_List,inds_List,opt___][ord_List]:> 
    Symbol[StringJoin[CTType,opt, Sequence@@(ToString[#]&/@fld),Sequence@@(ToString[#]&/@ord)]][Sequence@@Flatten[inds]])&/@deltaFuncList;
  deltaParamList=If[deltaFuncList=!={},deltaParamList/.fff_[]->fff,{}];
  deltaOrderList=deltaFuncList/.FR$del[args][orde_List]->orde;
  deltaFuncList=#/.FR$del[fi_List,inds_List,opt___][orde_List]:>FR$del[fi,{MyPattern[ids,BlankNullSequence[]]},opt][Sequence@@orde]&/@deltaFuncList;

  (* We now add the series coefficient to the various parameter lists, if they are not in it already *)
  dummy=#/.fg_[__]->fg&/@deltaParamList;
  If[Complement[dummy,Intersection[dummy,M$RenormalizationConstants]]=!={},
    If[Prules=!={} && (param/.Prules)=!=param,
     (* Case 1, the ParameterName is different from the parameter label *)
       newparam=param/.Prules;
       (* Mapping function to parameter *)
       M$DeltaToParameters=Union[Inner[MyRule,(deltaFuncList/.param->newparam),
         If[MatchQ[#,_[__]], Print["test::",#, " :: ", param, " :: ",newparam];Print[#/.pr_[inds__]:>StringReplace[ToString[pr],ToString[param]->ToString[newparam]]]
           #/.pr_[inds__]:>Symbol[StringReplace[ToString[pr],ToString[param]->ToString[newparam]]][ids],
           #/.#->Symbol[StringReplace[ToString[#],ToString[param]->ToString[newparam]]]]&/@deltaParamList,
         List],M$DeltaToParameters];
       M$DeltaToParameters=Union[M$DeltaToParameters/.MyPattern->Pattern/.MyRule->RuleDelayed];
       (* Creating constants associated to the ParameterName *)
       dummy2=Symbol[StringReplace[ToString[#/.fg_[__]->fg],ToString[param]->ToString[newparam]]]&/@deltaParamList;
       If[Complement[dummy2,Intersection[dummy2,M$RenormalizationConstants]]=!={},
         AddRenConstToLists[Symbol[StringReplace[ToString[#],ToString[param]->ToString[newparam]]]&/@deltaParamList, newparam,NewOrder,deltaOrderList]],
   (* Case 2: the normal case *)
      (* Mapping function to parameter *)
      M$DeltaToParameters=Union[Inner[MyRule,deltaFuncList,#/.pr_[inds__]->pr[ids]&/@deltaParamList,List],M$DeltaToParameters];
      M$DeltaToParameters=Union[M$DeltaToParameters/.MyPattern->Pattern/.MyRule->RuleDelayed];
      (*Creating the new parameters *)
      AddRenConstToLists[deltaParamList, param,NewOrder,deltaOrderList]]];
  Clear[dummy];

  (* And now finally, the expansion of the renormalization constants *)
  resu=Expand[Normal[Series[FR$del[dummy][Sequence@@NewOrder[[All,1]]],Sequence@@(List[#[[1]],0,#[[2]]]&/@NewOrder)]]];
  resu=resu/.{FR$del[dummy][__]->0,Derivative[aa__][FR$del[dummy]][__]->FR$del[dummy][aa]};
  resu=resu/.(Rule[#[[1]],#[[1]]/(2 Pi)]&/@NewOrder)/.(Rule[Power[#[[1]],n_],Power[#[[1]],n]*Factorial[n]]&/@NewOrder);
  resu=resu/.FR$del[dummy]->FR$del[args];
  If[param=!=newparam,resu=resu/.param->newparam];
resu];


(* ::Section:: *)
(*Extraction of the counterterm Lagrangian*)


ExtractCounterterms[lagr_,ExpOrder_List]:=Block[{NewOrder,ExpLag,WaveFunctions,PrmToRenormList,GrpMat,MyRuleDelayed},
  (* Initialization *)
  Print["Extraction of the counterterm Lagrangian."];
  NewOrder=If[MatrixQ[ExpOrder],ExpOrder,{ExpOrder}];
  ExpLag=Expand[ExpandIndices[lagr]];
  WaveFunctions=FieldRenormalization[];
  ParameterRenormalization[];
  PrmToRenormList=Flatten[Union[MassList[[2,All,2]],MR$ParameterList,(ParameterName/.MR$ParameterRules[#])&/@MR$ParameterList]]/.Rule[a_,b_]:>b;
  ExpLag=If[Head[ExpLag]=!=Plus,{ExpLag},List@@ExpLag];

  GrpMat=Flatten[DeleteCases[(Representations/.MR$GaugeGroupRules[#])&/@MR$GaugeGroupList,Representations],1][[All,1]];
  GrpMat=Join[GrpMat,DeleteCases[(StructureConstant/.MR$GaugeGroupRules[#])&/@MR$GaugeGroupList,StructureConstant]];

  (* Field renormalization and simplifications *)
  Print["Field renormalization... "];
  ExpLag=Block[{tmpresu},
    tmpresu=Expand[#/.WaveFunctions];
    tmpresu=Expand[tmpresu/.Dot->FR$Dot]/.FR$Dot->Dot;
    tmpresu=OptimizeIndex[Expand[tmpresu]];
  tmpresu]&/@ExpLag;
  ExpLag=Plus@@ExpLag;
  ExpLag = If[Head[ExpLag]=!=Plus,{ExpLag},List@@ExpLag];

  (* Parameter renormalization *)
  (* Care must be taken when we have a term with a function of several parameters *)
  Print["Parameter renormalization..."];
  ExpLag = Block[{PValue,valtmp,dependencies,PVr, PVl,muf,tmpdot},
    PValue=#/.TensDot->tmpdot//.{
      del[_,_]->1,aa_?(FieldQ[#]===True || AntiFieldQ[#]===True&)[inds__]->1, aa_?(FieldQ[#]===True || AntiFieldQ[#]===True&)->1,
      Ga[__]->1,ProjP[__]->1,ProjM[__]->1,
      tmpdot[__][_,_]->1,
      FR$deltaZ[__]->1,
      aa_?(MemberQ[GrpMat,#]&)[inds__]->1};
    valtmp=PValue/.{Cos[x_]->x,Sin[x_]->x,Exp[x_]->x, Conjugate[x_]->x, IndexDelta[_,_]->1}; 
    dependencies = DeleteCases[Flatten[ReplaceRepeated[{valtmp}, {_?(NumericQ[#]&)->1,Times->List}]],1];
    If[dependencies=!={}, 
      PVr=Rule[#,# (1+muf[#])]&/@dependencies;
      PVl={muf[#],0,1}&/@dependencies;
      valtmp=Normal[Series[PValue/.PVr,Sequence@@PVl]];
      PVr=MyRule[muf[#],(ParameterRenormalization[#]-#)/#]&/@dependencies;
      PVr=PVr/.MyRule->Rule;
      OptimizeIndex[Expand[#/.PValue->valtmp/.PVr]],
      OptimizeIndex[#]]]&/@ExpLag;

  (* Expansion of the renormalization constants *)
  Print["Renormalization constant perturbative expansion..."];
  ExpLag = Expand[# /. FR$deltaZ[args__] :> PerturbativelyExpand[FR$deltaZ[args], NewOrder]] &/@ ExpLag;
  ExpLag = Expand[# /. FR$delta[args__] :> PerturbativelyExpand[FR$delta[args], NewOrder]] &/@ ExpLag; 

  (* Rejecting all terms which have an higher order *)
  ExpLag=Plus@@ExpLag;
  ExpLag = If[Head[ExpLag]=!=Plus,{ExpLag},List@@ExpLag];
  ExpLag = Block[{ttemp, mufmuf, result}, 
    ttemp = # //. {FR$delta[ar__][ord__] :> mufmuf[{ord}], FR$deltaZ[ar__][ord__] :> mufmuf[{ord}], 
      mufmuf[aaa_List]*mufmuf[bbb_List] -> mufmuf[aaa+bbb], Power[mufmuf[aaa_List],n_] -> mufmuf[n*aaa], Conjugate[mufmuf[arg__]] -> mufmuf[arg]};
    result = If[MatchQ[ttemp, _*mufmuf[__]], 
      ttemp = ttemp /.(_*mufmuf[argus__] :> Select[NewOrder[[All,2]] - argus, #1 < 0 & ]); If[ttemp =!= {}, 0, #],
      #];
    result] &/@ ExpLag;
  ExpLag=DeleteCases[ExpLag,0];

  (* Formatting *)
  ExpLag=ExpLag/.M$DeltaToParameters/.aa_?(MemberQ[M$RenormalizationConstants,#]&)[args1_?(Head[#]===List&),args___]:>aa[Sequence@@Join[args1,args]];

  (* Outputting *)
  Print["Done."];
Plus@@ExpLag];


(* ::Subsection:: *)
(*On-shell renormalisation*)


InternalRule[param_]:=If[FreeQ[param,Indices],
	If[FreeQ[param,Value],Definitions/.param[[2]],param[[1]]->Value/.param[[2]]],
	(Head[#[[1]]]@@Index@@@Transpose[{(Indices/.param[[2]])[[All,1]],List@@#[[1]]}]->#[[2]]&)/@(If[FreeQ[param,Value],Definitions,Value]/.param[[2]])];


PlusI[x__]:=Plus@@(I*List[x])


(*Dot for the renormalization*)
RenDot[a_+b_,c_]:=RenDot[a,c]+RenDot[b,c];
RenDot[c_,a_+b_]:=RenDot[c,a]+RenDot[c,b];
RenDot[c_,FR$CT*b_]:=FR$CT*RenDot[c,b];
RenDot[FR$CT*b_,c_]:=FR$CT*RenDot[b,c];
RenDot[c_,a_FR$deltaZ*b_]:=a*RenDot[c,b];
RenDot[a_FR$deltaZ*b_,c_]:=a*RenDot[b,c];


Options[OnShellRenormalization] = {QCDOnly->False,FlavorMixing->True,Only2Point->False};


OnShellRenormalization[Lag_,options___]:=Module[{FieldRenoList,ExternalParamList,InternalParamList,internalMasses,massRules, deltaLagp,deltaLag,classname, classmembers,
flavor,fi,paramreno,FreeM,Patbis,qcd,flm,only2,qcdind,qcdclasses,kk1,extNotMass},

qcd=QCDOnly/.{options}/.Options[OnShellRenormalization];
flm=FlavorMixing/.{options}/.Options[OnShellRenormalization];
only2=Only2Point/.{options}/.Options[OnShellRenormalization];

qcdind=(Representations/.Flatten[Cases[M$GaugeGroups,_?(Not[FreeQ[#,gs]]&),{2}]])[[All,2]];
qcdclasses=DeleteCases[DeleteCases[M$ClassesDescription,_?(Not[FreeQ[#,Unphysical->True]]||Not[FreeQ[#,Ghost]]&)],
           _?(And@@Table[FreeQ[#,qcdind[[kk]]],{kk,Length[qcdind]}]&)];

(*Keep two point in the vertices list*)
FR$Loop=True;

(*Check if the on-shell scheme is feasible, i.e. all the masses are external parameters*)
internalMasses=Cases[Union[(DeleteCases[MassList,{_,m_?(Not[FreeQ[MassList,#]]&),Internal},2])[[2,All,2;;3]]],{xx_,Internal}->xx];
If[Length[internalMasses]>0&&
  If[Length[FR$LoopSwitches]>0,
     Length[Complement[internalMasses,FR$LoopSwitches[[All,2]]]]>0,True],
     Message[NLO::ExtMass];Return[]];

Print["renormalizing the fields"];
(*No renormalization of the ghost field is needed*)
FieldRenoList=DeleteCases[FieldRenormalization[], _?(GhostFieldQ[#[[1]]] &)]/.FR$deltaZ[xx__]->FR$CT*FR$deltaZ[xx];
(*Expand the flavors*)
For[fi=1,fi<=Length[FieldRenoList],fi++,
  If[FlavoredQ[FieldRenoList[[fi,1]]],
    classname=Head[FieldRenoList[[fi,1]]];
    {classmembers,flavor}={ClassMembers,FlavorIndex}/.Join[Cases[M$ClassesDescription,_?(Not[FreeQ[#,ClassName->classname]]&),{2}],
                                                          Cases[M$ClassesDescription,_?(Not[FreeQ[#,ClassName->anti[classname]]]&),{2}]][[1]];

    If[flm,
      FieldRenoList[[fi]]=DeleteCases[FieldRenoList[[fi]],Index[flavor,_],\[Infinity]]Table[1,{jj,Length[classmembers]}];
      For[kk1=1,kk1<=Length[classmembers],kk1++,
        FieldRenoList[[fi,kk1,1]]=FieldRenoList[[fi,kk1,1]]/.{classname->If[Not[AntiFieldQ[classname]],classmembers[[kk1]],anti[classmembers[[kk1]]]]};
        FieldRenoList[[fi,kk1,2,2]]=(Coefficient[Refine[FieldRenoList[[fi,kk1,2,2]],Assumptions->{FR$CT\[Element]Reals}],FR$CT,0]/.
            If[Not[AntiFieldQ[classname]],{classname->classmembers[[kk1]]}, {classname->anti[classmembers[[kk1]]]}])+ 
          FR$CT*Total[Table[(Coefficient[Refine[FieldRenoList[[fi,kk1,2,2]],Assumptions->{FR$CT\[Element]Reals}],FR$CT,1]/.
            If[Not[AntiFieldQ[classname]],{classname,classname},anti/@{classname,classname}]->{classmembers[[kk1]],classmembers[[kk2]]})/.
            If[Not[AntiFieldQ[classname]],classname->classmembers[[kk2]],classname->anti[classmembers[[kk2]]]],{kk2,1,Length[classmembers]}]];
      ];
      ,
      FieldRenoList[[fi]]=Table[DeleteCases[FieldRenoList[[fi]],Index[flavor,_],\[Infinity]]/.If[Not[AntiFieldQ[classname]],classname->classmembers[[kk]],
        {classname->anti[classmembers[[kk]]],anti[classname]->classmembers[[kk]]}],{kk,Length[classmembers]}];
    ];
  ];
];
FieldRenoList=Flatten[FieldRenoList];
If[qcd,FieldRenoList=DeleteCases[FieldRenoList,_?(FreeQ[qcdclasses,Head[#[[1]]]]&&FreeQ[qcdclasses,anti[Head[#[[1]]]]]&)];];
(*Print[FieldRenoList];*)

Print["renormalizing the parameters"];

InternalParamList=Cases[M$Parameters,xx_?(Not[FreeQ[#,Internal]]&)];


(*Replacement rules for the internal parameters*)
InternalParamList=Flatten[InternalRule/@InternalParamList];

(*Invert the replacement for the element in FR$LoopSwitches[[All,2]]*)
If[Length[FR$LoopSwitches]>0,
  InternalParamList=Flatten[(If[FreeQ[FR$LoopSwitches[[All,2]],#1],#1->#2,
    Solve[#1==#2,FR$LoopSwitches[[Position[FR$LoopSwitches[[All,2]],#1][[1,1]],1]] ][[1]]]&)@@@InternalParamList/.ConditionalExpression[a_,b_]->a];
];

(*List of external parameters that should be renormalized*)
ExternalParamList=DeleteCases[M$Parameters,_?(FreeQ[#,External]&)][[All,1]];
If[Length[FR$LoopSwitches]>0,ExternalParamList=ExternalParamList/.Rule@@@FR$LoopSwitches;];
extNotMass=Cases[ExternalParamList/.FR$RmDblExt,_?(FreeQ[(Mass/.#&)/@M$ClassesDescription[[All,2]],#]&)];
If[qcd,extNotMass=DeleteCases[extNotMass,_?(FreeQ[Cases[M$Parameters,#=={x__}],QCD]&)]];

(*renormalization of the masses only*)
paramreno=((#->#+FR$CT*FR$delta[{#},{}])&)/@If[qcd,DeleteCases[Union[MassList[[2,All,2]]],_?(FreeQ[qcdclasses,#]&)],Union[MassList[[2,All,2]]]];

If[Not[only2],Print["not only 2 point"];paramreno=Join[paramreno,((#->#+FR$CT*FR$delta[{#},{}])&)/@extNotMass];];

(*Print[paramreno];*)
massRules=If[Length[FR$RmDblExt]>0,Join[FR$RmDblExt,Cases[InternalParamList,_?(Not[(#[[2]]/.FR$RmDblExt)===#[[2]]]&)]/.FR$RmDblExt],{}];
InternalParamList[[All,2]] = InternalParamList[[All,2]]//.InternalParamList;
InternalParamList[[All,2]] = InternalParamList[[All,2]]/.massRules/.paramreno;
InternalParamList=(#1->If[Not[#2===0],(#1/.Pattern->Patbis/.{Patbis[a_,b__]->a})(Simplify[Normal[Series[#2,{FR$CT,0,1}]]/Normal[Series[#2,{FR$CT,0,0}]]]),#2]&)@@@(InternalParamList);
Print["Internal parameter renormalization"];
(*Print[InternalParamList];*)

Print["renormalizing the Lagrangian"];
deltaLag=ExpandIndices[Lag, FlavorExpand->True];
Print["with the masses"];
deltaLagp=Refine[ComplexExpand[(deltaLag//.massRules/.paramreno/.InternalParamList),FR$CT]/.Im[FR$CT]->0, Assumptions->{FR$CT>0}];
(*Print[Length[deltaLagp]];*)
deltaLagp=Replace[deltaLagp,Times[I*a_Plus]:>PlusI@@a,1];
deltaLagp=(Coefficient[Normal[Series[#,{FR$CT,0,1}]],FR$CT]&)/@deltaLagp/.Dot->RenDot/.RenDot->Dot;
(*Print[deltaLagp];*)
Print["with the fields"];
deltaLag=Refine[ComplexExpand[(deltaLag//.massRules)/.FieldRenoList], Assumptions->{FR$CT>0}]/.Dot->RenDot/.RenDot->Dot/.Conjugate[FR$deltaZ[{x_,x_},y___]]->FR$deltaZ[{x,x},y];
deltaLag=deltaLag/.{FR$CT^2->0,FR$CT^3->0,FR$CT^4->0};
deltaLag=Replace[deltaLag,Times[I*a_Plus]:>PlusI@@a,1];
test=deltaLag;(*Print[Length[deltaLag]];*)
deltaLag = (Normal[Series[#,{FR$CT,0,1}]]&)/@deltaLag;

Return[deltaLag+deltaLagp*FR$CT]

];
