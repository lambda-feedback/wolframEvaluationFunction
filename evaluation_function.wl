(* ::Package:: *)

(* The code that handles incoming messages and passes them to evaluate or preview accordingly*)

<< "evaluate.m";
<< "preview.m";


processEvaluate[jsonData_] := Module[{result, requestData, answer, response, params, type},
    requestData = jsonData["params"];
    answer = requestData["answer"];
    response = requestData["response"];
    params = requestData["params"];
    type = params["type"];

    Print["Evaluating Response Against Answer"];
    result = EvaluationFunction[type, answer, response, params];
    Print["Output: ", result];

    If[result["error"] != Null,
      Return[
      <| "command" -> "eval",
        "error" -> <|
          "message" -> result["error"]
        |>
      |>
      ]
    ];

    <| "command" -> "eval",
      "result" -> <|
        "is_correct" -> result["is_correct"],
        "feedback" -> result["feedback"]
      |>
    |>
]

processPreview[jsonData_] := Module[{result, requestData, response},
    requestData = jsonData["params"];
    response = requestData["response"];

    Print["Previewing Response"];

    result = PreviewFunction[response];
    Print["Result: ", result];

    If[result["error"] != Null,
      Return[
      <| "command" -> "eval",
        "error" -> <|
          "message" -> result["error"]
        |>
      |>
      ]
    ];

    <| "command" -> "preview",
      "result" ->
          <|"preview" -> result|>
    |>
]

evalQuestionIO = Function[
  Module[{jsonData, command, resultAssoc, response},
    jsonData = Import[#1, "JSON"] //. List :> Association;

    Print["Input"];
    Print[jsonData];

    command = Lookup[jsonData, "command", "unknown"];

    resultAssoc = Which[
      command == "eval", processEvaluate[jsonData],
      command == "preview", processPreview[jsonData],
      True, <| "status" -> "error", "message" -> "Incorrect command" |>
    ];

    Print["Outputted JSON"];
    Print[resultAssoc];
    Export[#2, resultAssoc, "JSON", "Compact" -> True]
  ]
];

argv = Rest[$ScriptCommandLine];
evalQuestionIO[argv[[1]], argv[[2]]]

