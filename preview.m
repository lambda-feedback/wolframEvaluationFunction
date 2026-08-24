(* ::Package:: *)

(* Wolfram Language Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: preview *)
(* :Context: preview` *)
(* :Author: marcus *)
(* :Date: 2025-09-26 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 14.0 *)
(* :Copyright: (c) 2025 Lambda Feedback *)
(* :Keywords: *)
(* :Discussion: *)

(* For new style packages see: https://mathematica.stackexchange.com/a/176489) *)
(* Declare package context *)
BeginPackage["preview`"];

PreviewFunction[response_, params_] := Module[{
	latexString, wolframString, parsedResponse, isLatex,suppress,plusMinusSplit},
	Print["Running Preview Function"];
	Print["Preview Input:", response];
	
	isLatex = Lookup[params,"is_latex",False];
	suppress=Lookup[params,"suppress_independent_variable",True];
	plusMinusSplit = Lookup[params,"plus_minus_split_preview",True];
	
	parsedResponse = SafeToExpression[response, isLatex,suppress,plusMinusSplit];

   If[StringQ[parsedResponse] && StringStartsQ[parsedResponse, "Error:"],
    Return[
      <|
        "error" -> <|
          "message" -> parsedResponse
        |>
      |>
    ]
  ];

    latexString = ToString[parsedResponse/.activeFunctionRulesPublic, TeXForm];
    wolframString = ToString[parsedResponse/.activeFunctionRulesPublic, InputForm];

  <|
        "latex" -> latexString,
        "sympy" -> wolframString
    |>
];

activeFunctionRulesPublic = {
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

Begin["`Private`"];

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
	exp -> Exp, log -> Log, ln -> Log, 
	sqrt ->Sqrt,
	pi -> Pi, e -> E, i -> I};

SafeToExpression[str_String, isLatex_,suppress_,plusMinusSplit_] :=
  Module[{expr, result},
    (* First check for obviously dangerous patterns in the raw string *)
    If[StringContainsQ[str,
        RegularExpression["\\b(Set|SetDelayed|Module|Block|Function|With|Do|For|While|RunProcess|Import|Export|DeleteFile|CreateFile|Get|Put|Install|Uninstall)\\b"]],
      Return["Error: Expression contains unsafe constructs"]
    ];

    (* Try to parse the expression safely *)

    If[isLatex,
      result = Quiet @ Check[
      ToExpression[StandardizeString[str,PlusMinusSplit->plusMinusSplit], TeXForm, Hold],
      Return["Error: Failed to parse expression"]
      ],
      result = Quiet @ Check[
      ToExpression[StandardizeString[str,PlusMinusSplit->plusMinusSplit], TraditionalForm, Hold],
      Return["Error: Failed to parse expression"]
      ]
    ];

    (* ToExpression can return $Failed without raising a message (e.g. malformed LaTeX),
       which Check would not catch, so check explicitly here *)
    If[!FreeQ[result, $Failed],
      Return["Error: Failed to parse expression"]
    ];

    (* If parsing succeeded, check the parsed structure *)
    If[MatchQ[result, Hold[_]],
      expr = First[result];

      (* Check for unsafe constructs in the parsed expression *)
      If[!FreeQ[expr,
          Alternatives[
            Set, SetDelayed, Module, Block, Function, With,
            Do, For, While,
            RunProcess, Import, Export, DeleteFile, CreateFile,
            Get, Put, Install, Uninstall
          ]],
        "Error: Expression contains unsafe constructs",
       StandardizeExpression[expr,SuppressIndependentVariable->suppress](* safe expression *)
      ],
      "Error: Unexpected parsing result"
    ]
  ]
  
(*StandardizeString: a function that automatically converts all instances
of the equals sign in a string to the repeated equals sign, so that anything WL 
would parse as an assignment gets parsed instead as an equation*)

Options[StandardizeString] = {PlusMinusSplit->True};

StandardizeString[str_String,OptionsPattern[]]:=Module[{output},
	output=StringReplace[
		FixedPoint[StringReplace["==="->"=="],StringReplace[str,"="->"=="]],
		{"**"->"^","plus_minus"->"\[PlusMinus]","minus_plus"->"\[MinusPlus]"}];
	If[OptionValue[PlusMinusSplit]&&StringContainsQ[output,{"\[PlusMinus]","\[MinusPlus]"}],
		output="{"<>StringReplace[output,{"\[PlusMinus]"->"+","\[MinusPlus]"->"-"}]<>", "<>StringReplace[output,{"\[PlusMinus]"->"-","\[MinusPlus]"->"+"}]<>"}"];
	output]

(*StandardizeExpression: a function that performs a number of standard replacements
at the Expression stage, namely:
- replacing s_[arg_Plus] by s*(arg) unless s is a symbol representing a known function
- replacing expressions of the form dy_^n_/dx_^n_ with y'[x]^n
- if the option SuppressIndependentVariable is set to True, replacing each y'[x] with y'*)

Options[StandardizeExpression] = {SuppressIndependentVariable -> True};

StandardizeExpression[expr_, OptionsPattern[]]:=Module[{output,suppress},
	suppress = OptionValue[SuppressIndependentVariable];
         output=expr/.activeFunctionRules;
	output = output/.s:_Symbol[arg_Plus]/;Not[MemberQ[Attributes[s], NumericFunction]] :> s*arg;
	output = output/.I[arg_]:>I*arg;
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



End[];
EndPackage[];
