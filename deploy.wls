#!/usr/bin/env wolframscript

previewPackagePath = FileNameJoin[{Directory[], "Preview.m"}];

If[!FileExistsQ[previewPackagePath],
    Print["Error: file not found at " <> previewPackagePath];
    Abort[];
];

evaluatePackagePath = FileNameJoin[{Directory[], "Evaluate.m"}];

If[!FileExistsQ[evaluatePackagePath],
    Print["Error: file not found at " <> evaluatePackagePath];
    Abort[];
];

(* Load the package locally first *)
Get[previewPackagePath];
Get[evaluatePackagePath];

(* Test locally *)
Print["Testing preview locally:"];
Print[preview`PreviewFunction["x^2"]];

Print["Testing evaluate locally:"];
Print[evaluate`EvaluationFunction["structure", "x^2", "y^2",
    <|"correct_response_feedback" -> "Correct", "incorrect_response_feedback" -> "Incorrect"|>]];



(* Deploy the API with explicit JSON export *)
previewAPI = APIFunction[
    {"response" -> "String"},
    ExportForm[preview`PreviewFunction[#response], "JSON"] &
];

previewObj = CloudDeploy[previewAPI, "preview-api", Permissions -> "Public"];

Print["Preview API deployed to: ", previewObj];

evaluateAPI = APIFunction[
    {"type" -> "String",
     "response" -> "String",
     "answer" -> "String",
     "params" -> "JSON"},
    ExportForm[evaluate`EvaluationFunction[#type, #answer, #response, #params], "JSON"] &
];

evaluateObj = CloudDeploy[evaluateAPI, "evaluate-api", Permissions -> "Public"];

Print["Evaluate API deployed to: ", evaluateObj];