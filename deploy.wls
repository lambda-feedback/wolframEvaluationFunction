#!/usr/bin/env wolframscript

packagePath = FileNameJoin[{Directory[], "Preview.m"}];

If[!FileExistsQ[packagePath],
    Print["Error: file not found at " <> packagePath];
    Abort[];
];

(* Load the package locally first *)
Get[packagePath];

(* Test locally *)
Print["Testing locally:"];
Print[preview`PreviewFunction["x^2"]];



(* Deploy the API with explicit JSON export *)
api = APIFunction[
    {"response" -> "String"},
    ExportForm[preview`PreviewFunction[#response], "JSON"] &
];

obj = CloudDeploy[api, "preview-api", Permissions -> "Public"];

Print["API deployed to: ", obj];