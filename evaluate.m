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

EvaluationFunction[type_, answer_, response_, params_] := Module[{result, feedback},
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

equalQNumeric[answer_, response_, params_] := Module[{tolerance},
  Print["Evaluating Equal Numeric"];
  tolerance = If[Lookup[params, "tolerance_is_absolute", False],
    Lookup[params, "tolerance", 0],
    Lookup[params, "tolerance", 0] * answer
  ];
  error = Abs[answer - response];
  <|
    "error" -> error,
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
 Map[PatternizeSymbol[#, namedVariables, 
    Atomic -> OptionValue[Atomic]] &, 
  MapAll[ComplexResolve, expression], {-1}]

Depatternize[pattern_] := MapAll[DepatternizePattern, pattern]

(*StandardizeEquation: a function that automatically converts all instances
of the equals sign in a string to the repeated equals sign, so that anything WL 
would parse as an assignment gets parsed instead as an equation*)

StandardizeEquation[str_String]:=FixedPoint[StringReplace["==="->"=="],StringReplace[str,"="->"=="]]

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
   Exp -> fExp, exp -> fExp, Log -> fLog, log -> fLog, ln -> fLog};

ComplexSymbolize[a_Integer?Positive]:=Symbol["$sym"<>ToString[a]]

ComplexSymbolize[a_Integer?Negative]:=Symbol["$symmin"<>ToString[-a]]

ComplexSymbolize[a_Rational] :=Symbol["$num"<>ToString[Numerator[a]]<>"den"<>ToString[Denominator[a]]]

ComplexSymbolize[0]:=0

CanonicComplex[Complex[a_,b_]]:=ComplexSymbolize[a]+ComplexSymbolize[b] I

CanonicComplex[I]:=I

CanonicComplex[arg_]:=arg
   
Options[StructureMatchQ] = {Atomic -> False};

StructureMatchQ[answerTemplate_String,response_String,namedVariables_List] := 
	Module[{response2,answerTemplate2},response2=MapAll[CanonicComplex,ReplaceAll[response,inertFunctionRules]];
		answerTemplate2=ReplaceAll[answerTemplate,inertFunctionRules];
		MatchQ[response2,Patternize[answerTemplate2,namedVariables]]]

equalQStructure[answer_String, response_String, params_Association] := Module[{namedVariables,correctQ},
  Print["Evaluating Structure"];
	namedVariables = ToExpression[Lookup[params,"named_variables",{}],TraditionalForm];
	correctQ = StructureMatchQ[
		ToString[ToExpression[StandardizeEquation[answer],TraditionalForm],InputForm],
		response,
		namedVariables];

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
	exp -> Exp, log -> Log, ln -> Log};

SemanticMatchQ[answer_,response_] := Simplify[(response-answer)/.activeFunctionRules] == 0

SemanticMatchQ[answer_Equal, response_Equal] := 
	SemanticMatchQ[answer[[1]]-answer[[2]], response[[1]]-response[[2]]]||
	SemanticMatchQ[answer[[1]]-answer[[2]], response[[2]]-response[[1]]]

SemanticMatchQ[answer_String,response_String] := 
  SemanticMatchQ[
    ToExpression[answer],
    ToExpression[response]
  ]

SemanticAndStructureMatchQ[answer_String,response_String,answerTemplate_String,namedVariables_List] :=
	TrueQ[SemanticMatchQ[answer,response]&&StructureMatchQ[answerTemplate,response,namedVariables]]

equalQSemantic[answer_String, response_String, params_Association] := Module[{correctQ},
  Print["Evaluating Semantic"];
	correctQ = SemanticMatchQ[
		ToString[ToExpression[StandardizeEquation[answer],TraditionalForm],InputForm],		
		response];
		
	<|
		"error" -> Null,
		"is_correct" -> correctQ
    |>
]

equalQSemanticAndStructure[answer_String, response_String, params_Association] := Module[{namedVariables,answerTemplate,correctQ},
  Print["Evaluating SemanticAndStructure"];
    namedVariables = ToExpression[Lookup[params,"named_variables",{}],TraditionalForm];    
    answerTemplate = Lookup[params,"answer_template",{}];
	correctQ = SemanticAndStructureMatchQ[
		ToString[ToExpression[StandardizeEquation[answer],TraditionalForm],InputForm],
		response,
		ToString[ToExpression[StandardizeEquation[answerTemplate],TraditionalForm],InputForm],
		namedVariables
		];

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

StrictStructureMatchQ[answerTemplate_String,response_String,namedVariables_List] := 
	StructureMatchQ[answerTemplate,response,namedVariables]&&
	TrueQ[
		(Length[Union[UnnamedSymbols[ToExpression[response],namedVariables]]]==
		 Length[Union[UnnamedSymbols[ToExpression[StandardizeEquation[answerTemplate],TraditionalForm],namedVariables]]])]

(* SemanticAndStrictStructureMatchQ: a function that combines a strict structure comparison with a test of
	mathematical equivalence  *)

SemanticAndStrictStructureMatchQ[answer_String,response_String,answerTemplate_String,namedVariables_List] := 
	TrueQ[SemanticMatchQ[answer,response]&&StrictStructureMatchQ[answerTemplate,response,namedVariables]]
	
equalQStrictStructure[answer_String, response_String, params_Association] := Module[{namedVariables,correctQ},
  Print["Evaluating Structure"];
	namedVariables = ToExpression[Lookup[params,"named_variables",{}],TraditionalForm];
	correctQ = StrictStructureMatchQ[
		ToString[ToExpression[StandardizeEquation[answer],TraditionalForm],InputForm],
		response,
		namedVariables];

	<|
		"error" -> Null,
		"is_correct" -> correctQ
    |>
]

equalQSemanticAndStrictStructure[answer_String, response_String, params_Association] := Module[{namedVariables,answerTemplate,correctQ},
  Print["Evaluating SemanticAndStructure"];
    namedVariables = ToExpression[Lookup[params,"named_variables",{}],TraditionalForm];    
    answerTemplate = Lookup[params,"answer_template",{}];
	correctQ = SemanticAndStrictStructureMatchQ[
		ToString[ToExpression[StandardizeEquation[answer],TraditionalForm],InputForm],
		response,
		answerTemplate,
		namedVariables
		];

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
