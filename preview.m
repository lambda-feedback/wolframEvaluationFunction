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

PreviewFunction[response_] := Module[{latexString, wolframString, parsedResponse},
  Print["Running Preview Function"];
  Print["Preview Input:", response];

  parsedResponse = SafeToExpression[response];

   If[StringQ[parsedResponse] && StringStartsQ[parsedResponse, "Error:"],
    Return[
      <|
        "error" -> <|
          "message" -> parsedResponse
        |>
      |>
    ]
  ];

  latexString = ToString[parsedResponse, TeXForm];
  wolframString = ToString[ToExpression[ToString[parsedResponse],TraditionalForm]/.activeFunctionRules];

  <|
        "latex" -> latexString,
        "sympy" -> wolframString
    |>
];

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

Begin["`Private`"];

SafeToExpression[str_String] :=
  Module[{expr, result},
    (* First check for obviously dangerous patterns in the raw string *)
    If[StringContainsQ[str,
        RegularExpression["\\b(Set|SetDelayed|Module|Block|Function|With|Do|For|While|RunProcess|Import|Export|DeleteFile|CreateFile|Get|Put|Install|Uninstall)\\b"]],
      Return["Error: Expression contains unsafe constructs"]
    ];

    (* Try to parse the expression safely *)
    result = Quiet @ Check[
      ToExpression[str, InputForm, Hold],
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
        expr  (* safe expression *)
      ],
      "Error: Unexpected parsing result"
    ]
  ]



End[];
EndPackage[];
