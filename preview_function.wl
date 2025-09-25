
PreviewFunction[response_] := Module[{tolerance, correctQ, error},
  Print["Running Preview Function"];
  result = response;
  Print["Results"];
  Print[result];
  <|
        "command" -> "preview",
        "result" -> <|
          "preview" -> <|
            "message" -> result
          |>
        |>
  |>
];


evalQuestionIO = Function[
  Module[{jsonData, result},
    jsonData = Import[#1, "JSON"] //. List :> Association;
    requestData = jsonData["params"];
    response = requestData["response"];
    Print["Evaluating Response Against Answer"];
    result = PreviewFunction[response];
    Print["Response"];
    Print[result];
    Export[#2, result, "JSON", "Compact" -> True]
  ]
];

argv = Rest[$ScriptCommandLine];
evalQuestionIO[argv[[1]], argv[[2]]]