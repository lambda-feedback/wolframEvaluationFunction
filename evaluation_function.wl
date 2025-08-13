(* ::Package:: *)

(* The basic evaluation function code*)

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
RowBox[{"If", "[", 
RowBox[{
RowBox[{"OptionValue", "[", "Atomic", "]"}], ",", 
RowBox[{"(", 
RowBox[{"Optional", "[", 
RowBox[{"PatternTest", "[", 
RowBox[{
RowBox[{"pattern", "[", 
RowBox[{"a", ",", 
RowBox[{"Blank", "[", "]"}]}], "]"}], ",", "AtomQ"}], "]"}], "]"}], ")"}], ",", 
RowBox[{"(", 
RowBox[{"Optional", "[", 
RowBox[{"pattern", "[", 
RowBox[{"a", ",", 
RowBox[{"Blank", "[", "]"}]}], "]"}], "]"}], ")"}]}], "]"}],
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

ComplexResolve[Optional[a_Symbol] + I Optional[b_Symbol]] := 
 Complex[a, b]

ComplexResolve[I Optional[b_Symbol]*Pi] := Complex[0, b]*Pi

ComplexResolve[Complex[0, Optional[b_Symbol]] + Optional[a_Symbol]] :=
  Complex[a, b]

ComplexResolve[a_] := a

Options[Patternize] = {Atomic -> False};

Patternize[expression_, namedVariables_, OptionsPattern[]] := 
 Map[PatternizeSymbol[#, namedVariables, 
    Atomic -> OptionValue[Atomic]] &, 
  MapAll[ComplexResolve, expression], {-1}]

Depatternize[pattern_] := MapAll[DepatternizePattern, pattern]

(*StructureMatchQ: a function that checks whether a user's response \
has the same structure as a given answer template, given a set of \
named variables.*)

inertFunctionRules = {Sin -> fSin, Cos -> fCos, Tan -> fTan, 
   Sec -> fSec, Csc -> fCsc, Cot -> fCot, ArcSin -> fArcSin, 
   ArcCos -> fArcCos, ArcTan -> fArcTan, ArcSec -> fArcSec, 
   ArcCsc -> fArcCsc, ArcCot -> fArcCot, Sinh -> fSinh, Cosh -> fCosh,
    Tanh -> fTanh, Sech -> fSech, Csch -> fCsch, Coth -> fCoth, 
   ArcSinh -> fArcSinh, ArcCosh -> fArcCosh, ArcTanh -> fArcTanh, 
   ArcSech -> fArcSech, ArcCsch -> fArcCsch, ArcCoth -> fArcCoth, 
   Exp -> fExp, Log -> fLog};
   
Options[StructureMatchQ] = {Atomic -> False};

StructureMatchQ[response_, answerTemplate_, namedVariables_, 
  OptionsPattern[]] := 
 Module[{response2, answerTemplate2}, 
  response2 = ReplaceAll[response, inertFunctionRules]; 
  answerTemplate2 = ReplaceAll[answerTemplate, inertFunctionRules]; 
  MatchQ[response2, 
   Patternize[answerTemplate2, namedVariables, 
    Atomic -> OptionValue[Atomic]]]]

equalQStructure[answer_, response_, params_] := Module[{namedVariables,correctQ},
  Print["Evaluating Structure"];
	namedVariables = ToExpression[Lookup[params,"named_variables",{}],TraditionalForm];
	correctQ = StructureMatchQ[
		ToExpression[ToString[response],TraditionalForm],
		ToExpression[ToString[answer],TraditionalForm],
		namedVariables];

	<|
		"error" -> Null,
		"is_correct" -> correctQ
    |>
]

(* The evaluation function itself *)

evalQ[type_, answer_, response_, params_] := Module[{},
  Which[
	type == "structure",
	equalQStructure[answer,response,params],
	NumericQ[answer],
    equalQNumeric[answer, response, params],
    True,
    equalQOther[answer, response, params]
  ]
];

EvaluationFunction[type_, answer_, response_, params_] := Module[{tolerance, correctQ, error},
  Print["Running Evaluation Function"];
  result = evalQ[type, answer, response, params];
  Print["Results"];
  Print[result];
  feedback = If[result["is_correct"],
      Lookup[params, "correct_response_feedback", "Correct!"],
      Lookup[params, "incorrect_response_feedback", "Incorrect!"]
      ];
  <|
        "command" -> "eval",
        "result" -> <|
          "is_correct" -> result["is_correct"],
          "feedback" -> feedback,
          "error" -> result["error"]
        |>
  |>
];

evalQuestionIO = Function[
  Module[{jsonData, result},
    jsonData = Import[#1, "JSON"] //. List :> Association;
    requestData = jsonData["params"];
    answer = requestData["answer"];
    response = requestData["response"];
    params = requestData["params"];
    type = params["comparisonType"];
    Print["Evaluating Response Against Answer"];
    result = EvaluationFunction[type, answer, response, params];
    Print["Response"];
    Print[result];
    Export[#2, result, "JSON", "Compact" -> True]
  ]
];

argv = Rest[$ScriptCommandLine];
evalQuestionIO[argv[[1]], argv[[2]]]

