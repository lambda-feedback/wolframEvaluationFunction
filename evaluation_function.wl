(* ::Package:: *)

(* The code that handles incoming messanges and passes them to evaluate or preview accordingly*)

<< "evaluate.m";

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
    resultAssoc = result[[4]];
    Print["Response"];
    Print[resultAssoc];
    Export[#2, resultAssoc, "JSON", "Compact" -> True]
  ]
];

argv = Rest[$ScriptCommandLine];
evalQuestionIO[argv[[1]], argv[[2]]]

