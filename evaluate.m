(* ::Package:: *)

(* Wolfram Language Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: evaluate *)
(* :Context: evaluate` *)
(* :Author: marcus/phil *)
(* :Date: 2025-09-26 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 14.0 *)
(* :Copyright: (c) 2025 Lambda Feedback *)
(* :Keywords: *)
(* :Discussion: *)

(* For new style packages see: https://mathematica.stackexchange.com/a/176489) *)
(* Declare package context *)
BeginPackage["evaluate`"];

EvaluationFunction[answer_, response_, params_] := Module[{result, feedback, type},
  type = params["type"];
  Print["Running Evaluation Function"];
  result = evalQ[type, answer, response, params];
  feedback = If[result["is_correct"],
      Lookup[params, "correct_response_feedback", "Correct!"],
      Lookup[params, "incorrect_response_feedback", "Incorrect!"]
      ];

  <|
    "is_correct" -> result["is_correct"],
    "feedback" -> feedback,
    "error" -> result["error"]
  |>
];

Begin["`Private`"];

equalQNumeric[answer_, response_, params_] := Module[{tolerance, error},
  Print["Evaluating Equal Numeric"];
  tolerance = If[Lookup[params, "tolerance_is_absolute", False],
    Lookup[params, "tolerance", 0],
    Lookup[params, "tolerance", 0] * answer
  ];
  error = Abs[answer - response];
  <|
    "error" -> Null,
    "is_correct" -> TrueQ[error <= tolerance]
  |>
]

equalQOther[answer_, response_, params_] := Module[{correctQ},
  Print["Evaluating Equal Other"];
  <|
    "error" -> Null,
    "is_correct" -> TrueQ[answer == response]
  |>
];

(* Patternize: a function that takes an expression and a list of \
named variables, and converts all unnamed symbols in the expression \
into Optional[..] patterns *)

Options[PatternizeSymbol] = {Atomic -> False};

PatternizeSymbol[a_Symbol, namedVariables_, OptionsPattern[]] /; 
  Not[MemberQ[namedVariables, a]] := \!\(\*
TagBox[
StyleBox[
RowBox[{"\n", "  ", 
RowBox[{"If", "[", 
RowBox[{
RowBox[{"OptionValue", "[", "Atomic", "]"}], ",", 
RowBox[{"(", 
RowBox[{"Optional", "[", 
RowBox[{"PatternTest", "[", 
RowBox[{
RowBox[{"pattern", "[", 
RowBox[{"a", ",", 
RowBox[{"Blank", "[", "]"}]}], "]"}], ",", "AtomQ"}], "]"}], "]"}], ")"}], ",", "\n", "  ", 
RowBox[{"(", 
RowBox[{"Optional", "[", 
RowBox[{"pattern", "[", 
RowBox[{"a", ",", 
RowBox[{"Blank", "[", "]"}]}], "]"}], "]"}], ")"}]}], "]"}]}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\) /. pattern -> Pattern

PatternizeSymbol[a_, namedVariables_, OptionsPattern[]] := a

ComplexResolve[Optional[a_Symbol] + I Optional[b_Symbol]] := 
 Complex[a, b]

ComplexResolve[I Optional[b_Symbol]*Pi] := Complex[0, b]*Pi

ComplexResolve[Complex[0, Optional[b_Symbol]] + Optional[a_Symbol]] :=
  Complex[a, b]

ComplexResolve[a_] := a

DepatternizePattern[pattern_Optional] := pattern[[1, 1, 1]]

DepatternizePattern[pattern_] := pattern

Options[Patternize] = {Atomic -> False};

Patternize[expression_, namedVariables_, OptionsPattern[]] := 
 Map[PatternizeSymbol[#, Union[namedVariables,{E,Pi}], 
    Atomic -> OptionValue[Atomic]] &, 
  MapAll[ComplexResolve, expression], {-1}]

Depatternize[pattern_] := MapAll[DepatternizePattern, pattern]

(*StandardizeString: a function that automatically converts all instances
of the equals sign in a string to the repeated equals sign, so that anything WL 
would parse as an assignment gets parsed instead as an equation, and also carries out
other standard string replacements*)

Options[StandardizeString] = {PlusMinusSplit->True};

StandardizeString[str_String,OptionsPattern[]]:=Module[{output},output=StringReplace[
    FixedPoint[StringReplace["==="->"=="],StringReplace[str,"="->"=="]],
    {"**"->"^","plus_minus"->"\[PlusMinus]","minus_plus"->"\[MinusPlus]"}];
If[OptionValue[PlusMinusSplit]&&StringContainsQ[output,{"\[PlusMinus]","\[MinusPlus]"}],output="{"<>StringReplace[output,{"\[PlusMinus]"->"+","\[MinusPlus]"->"-"}]<>", "<>StringReplace[output,{"\[PlusMinus]"->"-","\[MinusPlus]"->"+"}]<>"}"];output]

(*StandardizeExpression: a function that performs a number of standard replacements
at the Expression stage, namely:
- replacing s_[arg_Plus] by s*(arg) unless s is a symbol representing a known function
- replacing expressions of the form dy_^n_/dx_^n_ with y'[x]^n
- if the option SuppressIndependentVariable is set to True, replacing each y'[x] with y'*)

Options[StandardizeExpression] = {SuppressIndependentVariable -> True};

StandardizeExpression[expr_, OptionsPattern[]]:=Module[{output,suppress},
	suppress = OptionValue[SuppressIndependentVariable];
    output = expr/.activeFunctionRules;
	output = output/.s_Symbol[arg_Plus]/;Not[MemberQ[Attributes[s], NumericFunction]] :> s*arg;
	output = output/.{
		dx_^a_. dy_^b_.:>
			(ToExpression[StringTake[ToString[dx],{2}]]'[StringTake[ToString[dy],{2}]])^a/;
				StringTake[ToString[dx],{1}]=="d"&&StringTake[ToString[dy],{1}]=="d"&&a>0&&a+b==0,
		dx_^a_. dy_^b_.:>
			(ToExpression[StringTake[ToString[dy],{2}]]'[StringTake[ToString[dx],{2}]])^a/;
				StringTake[ToString[dx],{1}]=="d"&&StringTake[ToString[dy],{1}]=="d"&&b>0&&a+b==0};
	If[suppress,output=output/.Derivative[n_][y_][x_]:>Derivative[n][y]];
	output
]

(* FullStandardizeString *)

Options[FullStandardizeString] = {SuppressIndependentVariable -> True};

FullStandardizeString[str_,OptionsPattern[]] := Module[{output,suppress},
	output=StandardizeString[str,PlusMinusSplit->True];
	output = ToExpression[output,TraditionalForm];
	output=StandardizeExpression[output,SuppressIndependentVariable->OptionValue[SuppressIndependentVariable]];
	ToString[output,InputForm]]

(*StructureMatchQ: a function that checks whether a user's response \
has the same structure as a given answer template, given a set of \
named variables.*)

inertFunctionRules = {
   Sin -> fSin, sin -> fSin, Cos -> fCos,cos->fCos, Tan -> fTan, tan -> fTan,
   Sec -> fSec, sec -> fSec, Csc -> fCsc, Cosec -> fCsc, csc -> fCsc, cosec -> fCsc, Cot -> fCot, cot -> fCot, 
   ArcSin -> fArcSin, arcsin -> fArcSin, asin -> fArcSin, ArcCos -> fArcCos, arccos -> fArcCos, acos -> fArcCos, 
   ArcTan -> fArcTan, arctan -> fArcTan, atan -> fArcTan, 
   ArcSec -> fArcSec, arcsec -> fArcSec, asec -> fArcSec, 
   ArcCsc -> fArcCsc, ArcCosec -> fArcCsc, arccsc -> fArcCsc, acsc -> fArcCsc, acosec -> fArcCsc, 
   ArcCot -> fArcCot, arccot -> fArcCot, acot -> fArcCot, 
   Sinh -> fSinh, sinh -> fSinh, Cosh -> fCosh, cosh -> fCosh, tanh -> fTanh, tanh->fTanh, 
   Sech -> fSech, sech -> fSech, Csch -> fCsch, Cosech -> fCsch, csch -> fCsch, cosech -> fCsch, Coth -> fCoth, coth->fCoth,
   ArcSinh -> fArcSinh, arcsinh -> fArcSinh, asinh -> fArcSinh, ArcCosh -> fArcCosh, arccosh -> fArcCosh, acosh -> fArcCosh, 
   ArcTanh -> fArcTanh, arctanh -> fArcTanh, atanh -> fArcTanh, 
   ArcSech -> fArcSech, arcsech -> fArcSech, asech -> fArcSech, 
   ArcCsch -> fArcCsch, ArcCosech -> fArcCsch, arccsch -> fArcCsch, acsch -> fArcCsch, acosech -> fArcCsch, 
   ArcCoth -> fArcCoth, arccoth -> fArcCoth, acoth->fArcCoth, 
   Exp -> fExp, exp -> fExp, Log -> fLog, log -> fLog, ln -> fLog, 
   Sqrt -> fSqrt, sqrt -> fSqrt,
   pi -> Pi, e -> E, i -> I};

ComplexSymbolize[a_Integer?Positive]:=Symbol["$sym"<>ToString[a]]

ComplexSymbolize[a_Integer?Negative]:=Symbol["$symmin"<>ToString[-a]]

ComplexSymbolize[a_Rational] :=Symbol["$num"<>ToString[Numerator[a]]<>"den"<>ToString[Denominator[a]]]

ComplexSymbolize[0]:=0

CanonicComplex[Complex[a_,b_]]:=ComplexSymbolize[a]+ComplexSymbolize[b] I

CanonicComplex[I]:=I

CanonicComplex[arg_]:=arg
   
Options[StructureMatchQ] = {Atomic -> False};

StructureMatchQ[answerTemplate_String,response_String,namedVariables_List,multipleAnswersInterpretation_String] := 
	Module[{response2,answerTemplate2},
	    response2=MapAll[CanonicComplex,ReplaceAll[ToExpression[response],inertFunctionRules]];
		answerTemplate2=ReplaceAll[ToExpression[answerTemplate],inertFunctionRules];
		StructureMatchQ[answerTemplate2,response2,namedVariables,multipleAnswersInterpretation]]

StructureMatchQ[answerTemplate_,response_,namedVariables_List,multipleAnswersInterpretation_String] := 
	MatchQ[response,Patternize[answerTemplate,namedVariables]]

StructureMatchQ[answerTemplate_List,response_List,namedVariables_List,"match_all"]:=Apply[And,Apply[Or,Apply[StructureMatchQ[#1,#2,namedVariables,"match_all"]&,Outer[List,answerTemplate,response],{2}],{1}]]

StructureMatchQ[answerTemplate_List,response_,namedVariables_List,"match_all"]:=False

StructureMatchQ[answerTemplate_List,response_List,namedVariables_List,"match_any"]:=Apply[And,Apply[Or,Apply[StructureMatchQ[#1,#2,namedVariables,"match_all"]&,Outer[List,answerTemplate,response],{2}],{1}]]

StructureMatchQ[answerTemplate_List,response_,namedVariables_List,"match_any"]:=Apply[Or,Map[StructureMatchQ[#1,response,namedVariables,"match_any"]&,answerTemplate]]

StructureMatchQ[answerTemplate_List,response_List,namedVariables_List,"match_order"]:=Apply[And,Apply[Or,Apply[StructureMatchQ[#1,#2,namedVariables,"match_all"]&,Outer[List,answerTemplate,response],{2}],{1}]]

StructureMatchQ[answerTemplate_List,response_,namedVariables_List,"match_order"]:=Apply[And,Apply[StructureMatchQ[#1,#2,namedVariables,"match_order"]&,Transpose[{answerTemplate,response}],{1}]]

equalQStructure[answer_String, response_String, params_Association] := Module[{namedVariables,correctQ,standardizedAnswer,standardizedResponse,suppress,multipleAnswersInterpretation},
  Print["Evaluating Structure"];
    suppress = Lookup[params,"suppress_independent_variable",True];
	namedVariables = ToExpression[Lookup[params,"named_variables",{}],TraditionalForm];
	standardizedAnswer = FullStandardizeString[answer,SuppressIndependentVariable->suppress];
	standardizedResponse= FullStandardizeString[response,SuppressIndependentVariable->suppress];
    multipleAnswersInterpretation=Lookup[params,"multiple_answers_interpretation","match_all"];
	correctQ = StructureMatchQ[standardizedAnswer,standardizedResponse,namedVariables,multipleAnswersInterpretation];

	<|
		"error" -> Null,
		"is_correct" -> correctQ
    |>
]

(* SemanticAndStructureMatchQ: a function that checks whether a user's response both 
	(a) is the same mathematical object as a given answer,and (b) has the same structure as a given answer template,
	given a set of named variables. *)

activeFunctionRules = {
	sin -> Sin, cos -> Cos, tan -> Tan, sec -> Sec, Cosec -> Csc, csc -> Csc, cosec -> Csc, cot -> Cot, 
	arcsin -> ArcSin, asin -> ArcSin, arccos -> ArcCos, acos -> ArcCos, arctan -> ArcTan, atan -> ArcTan, 
	arcsec -> ArcSec, asec -> ArcSec, ArcCosec -> ArcCsc, arccsc -> ArcCsc, acsc -> ArcCsc, acosec -> ArcCsc,
	arccot -> ArcCot,acot -> ArcCot, 
	sinh -> Sinh, cosh -> Cosh, tanh -> Tanh, sech -> Sech, Cosech -> Csch, csch -> Csch, cosech -> Csch, coth -> Coth, 
	arcsinh -> ArcSinh, asinh -> ArcSinh, arccosh -> ArcCosh, acosh -> ArcCosh, arctanh -> ArcTanh, atanh -> ArcTanh, 
	arcsech -> ArcSech, asech -> ArcSech, 
	ArcCsch -> ArcCsch, ArcCosech -> ArcCsch, arccsch->ArcCsch, acsch -> ArcCsch, acosech -> ArcCsch,
	arccoth -> ArcCoth, acoth -> ArcCoth,
	exp -> Exp, log -> Log, ln -> Log, sqrt -> Sqrt,
	pi -> Pi, e -> E, i -> I};

SemanticMatchQ[answer_,response_,multipleAnswersInterpretation_String] := TrueQ[Simplify[(response-answer)/.activeFunctionRules] == 0] || 
    TrueQ[FullSimplify[(response-answer)/.activeFunctionRules] == 0]

SemanticMatchQ[answer_List,response_List,"match_all"]:=Apply[And,Apply[Or,Apply[SemanticMatchQ[#1,#2,"match_all"]&,Outer[List,answer,response],{2}],{1}]];

SemanticMatchQ[answer_List,response_,"match_all"]:=False;

SemanticMatchQ[answer_List,response_List,"match_any"]:=Apply[And,Apply[Or,Apply[SemanticMatchQ[#1,#2,"match_any"]&,Outer[List,answer,response],{2}],{1}]];

SemanticMatchQ[answer_List,response_,"match_any"]:=Apply[Or,Map[SemanticMatchQ[#1,response,"match_any"]&,answer]];

SemanticMatchQ[answer_List,response_List,"match_order"]:=Apply[And,Apply[SemanticMatchQ[#1,#2,"match_order"]&,Transpose[{answer,response}],{1}]];

SemanticMatchQ[answer_List,response_,"match_order"]:=False;

SemanticMatchQ[answer_Equal, response_Equal,multipleAnswersInterpretation_String] := 
	SemanticMatchQ[answer[[1]]-answer[[2]], response[[1]]-response[[2]],multipleAnswersInterpretation]||
	SemanticMatchQ[answer[[1]]-answer[[2]], response[[2]]-response[[1]],multipleAnswersInterpretation]||
	SemanticMatchQ[Denominator[Cancel[(answer[[1]]-answer[[2]])/(response[[1]]-response[[2]])]],1,multipleAnswersInterpretation]||
	SemanticMatchQ[Denominator[Cancel[(response[[1]]-response[[2]])/(answer[[1]]-answer[[2]])]],1,multipleAnswersInterpretation]

SemanticMatchQ[answer_Equal, response_,multipleAnswersInterpretation_String] := False

SemanticMatchQ[answer_, response_Equal,multipleAnswersInterpretation_String] := False

SemanticMatchQ[answer_String,response_String,multipleAnswersInterpretation_String] := 
  SemanticMatchQ[
    ToExpression[answer],
    ToExpression[response],
	multipleAnswersInterpretation
  ]

SemanticAndStructureMatchQ[answer_String,response_String,answerTemplate_String,namedVariables_List,multipleAnswersInterpretation_String] :=
	TrueQ[SemanticMatchQ[answer,response,multipleAnswersInterpretation]&&StructureMatchQ[answerTemplate,response,namedVariables,multipleAnswersInterpretation]]

equalQSemantic[answer_String, response_String, params_Association] := Module[{correctQ, standardizedAnswer,standardizedResponse,suppress,multipleAnswersInterpretation},
  Print["Evaluating Semantic"];    
    suppress = Lookup[params,"suppress_independent_variable",True];
	standardizedAnswer = FullStandardizeString[answer,SuppressIndependentVariable->suppress];
	standardizedResponse = FullStandardizeString[response,SuppressIndependentVariable->suppress];
	multipleAnswersInterpretation=Lookup[params,"multiple_answers_interpretation","match_all"];
	correctQ = SemanticMatchQ[standardizedAnswer,standardizedResponse,multipleAnswersInterpretation];
		
	<|
		"error" -> Null,
		"is_correct" -> correctQ
    |>
]

equalQSemanticAndStructure[answer_String, response_String, params_Association] := Module[{
   namedVariables,standardizedAnswer,standardizedResponse,answerTemplate,standardizedAnswerTemplate,correctQ,suppress,multipleAnswersInterpretation},
  Print["Evaluating SemanticAndStructure"];
    namedVariables = ToExpression[Lookup[params,"named_variables",{}],TraditionalForm];    
    answerTemplate = Lookup[params,"answer_template",Automatic]; 
    suppress = Lookup[params,"suppress_independent_variable",True];
	standardizedAnswer= FullStandardizeString[answer,SuppressIndependentVariable->suppress];
	standardizedResponse= FullStandardizeString[response,SuppressIndependentVariable->suppress];
	multipleAnswersInterpretation=Lookup[params,"multiple_answers_interpretation","match_all"];
	standardizedAnswerTemplate=If[TrueQ[answerTemplate==Automatic],
		standardizedAnswer,
		FullStandardizeString[answerTemplate,SuppressIndependentVariable->suppress]];
	correctQ = SemanticAndStructureMatchQ[standardizedAnswer,standardizedResponse,standardizedAnswerTemplate,namedVariables,multipleAnswersInterpretation];

	<|
		"error" -> Null,
		"is_correct" -> correctQ
    |>
]

(* UnnamedSymbols: a function that takes an expression and a list of named variables,
   and returns all other symbolic quantities in the expression. *)

UnnamedSymbols[expression_,namedVariables_] := 
	Cases[Reap[Scan[Sow,expression,{-1}]][[2,1]],a_Symbol/;Not[MemberQ[namedVariables,a]]]

(* StrictStructureMatchQ: a function that matches structures more strictly,taking account of the unnamed symbols 
	in each.*)

(* THIS FUNCTION IS FLAWED, AND REPRESENTS A CRUDE FIRST GO *)

(* IN PARTICULAR, THE COMPARISON OF THE LENGTHS OF THE SYMBOL LISTS IS VERY HAMFISTED *)

StrictStructureMatchQ[answerTemplate_String,response_String,namedVariables_List,multipleAnswersInterpretation_String] := 
	StructureMatchQ[answerTemplate,response,namedVariables,multipleAnswersInterpretation]&&
	TrueQ[
		(Length[Union[UnnamedSymbols[ToExpression[response],namedVariables]]]==
		 Length[Union[UnnamedSymbols[ToExpression[StandardizeString[answerTemplate],TraditionalForm],namedVariables]]])]

(* SemanticAndStrictStructureMatchQ: a function that combines a strict structure comparison with a test of
	mathematical equivalence  *)

SemanticAndStrictStructureMatchQ[answer_String,response_String,answerTemplate_String,namedVariables_List,multipleAnswersInterpretation_String] := 
	TrueQ[SemanticMatchQ[answer,response,multipleAnswersInterpretation]&&StrictStructureMatchQ[answerTemplate,response,namedVariables,multipleAnswersInterpretation]]
	
equalQStrictStructure[answer_String, response_String, params_Association] := Module[{namedVariables,correctQ,suppress,standardizedAnswer,standardizedResponse,multipleAnswersInterpretation},
  Print["Evaluating Strict Structure"];
	namedVariables = ToExpression[Lookup[params,"named_variables",{}],TraditionalForm];
    suppress = Lookup[params,"suppress_independent_variable",True];
	standardizedAnswer = FullStandardizeString[answer,SuppressIndependentVariable->suppress];
	standardizedResponse= FullStandardizeString[response,SuppressIndependentVariable->suppress];
	multipleAnswersInterpretation=Lookup[params,"multiple_answers_interpretation","match_all"];
	correctQ = StrictStructureMatchQ[standardizedAnswer,standardizedResponse,namedVariables,multipleAnswersInterpretation];

	<|
		"error" -> Null,
		"is_correct" -> correctQ
    |>
]

equalQSemanticAndStrictStructure[answer_String, response_String, params_Association] := Module[{
    namedVariables,answerTemplate,correctQ,suppress,standardizedAnswer,standardizedResponse,standardizedAnswerTemplate,multipleAnswersInterpretation},
  Print["Evaluating SemanticAndStrictStructure"];
    namedVariables = ToExpression[Lookup[params,"named_variables",{}],TraditionalForm];    
    answerTemplate = Lookup[params,"answer_template",Automatic]; 
    suppress = Lookup[params,"suppress_independent_variable",True];
	standardizedAnswer= FullStandardizeString[answer,SuppressIndependentVariable->suppress];
	standardizedResponse= FullStandardizeString[response,SuppressIndependentVariable->suppress];
standardizedAnswerTemplate=If[TrueQ[answerTemplate==Automatic],
standardizedAnswer,
FullStandardizeString[answerTemplate,SuppressIndependentVariable->suppress]];
multipleAnswersInterpretation=Lookup[params,"multiple_answers_interpretation","match_all"];
	correctQ = SemanticAndStrictStructureMatchQ[standardizedAnswer,standardizedAnswer,standardizedAnswerTemplate,namedVariables,multipleAnswersInterpretation];

	<|
		"error" -> Null,
		"is_correct" -> correctQ
    |>
]

(* The evaluation function itself *)

evalQ[type_, answer_, response_, params_] := Module[{},
  Which[
	type == "structure",
	equalQStructure[answer, response, params],
	type == "semantic",
	equalQSemantic[answer, response, params],
	type == "semantic_and_structure",	
	equalQSemanticAndStructure[answer, response, params],
	type == "strict_structure",	
	equalQStrictStructure[answer, response, params],
	type == "semantic_and_strict_structure",	
	equalQSemanticAndStrictStructure[answer, response, params],
	NumericQ[answer],
    equalQNumeric[answer, response, params],
    True,
    equalQOther[answer, response, params]
  ]
];
End[];
EndPackage[]
