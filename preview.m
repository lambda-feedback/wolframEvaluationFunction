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
Get["utils.m"];
BeginPackage["preview`", {"utils`"}];

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

    latexString = ToString[parsedResponse/.activeFunctionRules, TeXForm];
    wolframString = ToString[parsedResponse/.activeFunctionRules, InputForm];

  <|
        "latex" -> latexString,
        "sympy" -> wolframString
    |>
];

Begin["`Private`"];

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

(* StandardizeString and StandardizeExpression live in utils.m, shared with evaluate` *)

End[];
EndPackage[];
