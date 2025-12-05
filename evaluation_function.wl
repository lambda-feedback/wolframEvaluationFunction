(* ::Package:: *)

(* The code that handles incoming messages and passes them to evaluate or preview accordingly*)

<< "evaluate.m";
<< "preview.m";


processEvaluate[jsonData_] := Module[{result, requestData, answer, response, params, type},
    requestData = jsonData["params"];
    answer = requestData["answer"];
    response = requestData["response"];
    params = requestData["params"];
    type = params["comparisonType"];

    Print["Evaluating Response Against Answer"];
    result = EvaluationFunction[type, answer, response, params];
    Print["Output: ", result];
    <| "command" -> "eval", "result" -> result |>
]

processPreview[jsonData_] := Module[{result, requestData, response},
    requestData = jsonData["params"];
    response = requestData["response"];

    Print["Previewing Response"];

    result = PreviewFunction[response];
    Print["Result: ", result];
    <| "command" -> "preview", "result" -> result |>
]

evalQuestionIO = Function[
  Module[{jsonData, command, resultAssoc, response},
    jsonData = Import[#1, "JSON"] //. List :> Association;
    command = jsonData["method"];

    resultAssoc = Which[
      command == "eval", processEvaluate[jsonData],
      command == "preview", processPreview[jsonData],
      True, "Incorrect command"
    ];

    Print["Outputted JSON"];
    Print[resultAssoc];
    Export[#2, resultAssoc, "JSON", "Compact" -> True]
  ]
];

argv = Rest[$ScriptCommandLine];
evalQuestionIO[argv[[1]], argv[[2]]]

