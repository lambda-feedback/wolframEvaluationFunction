(* ::Package:: *)

(* Loads the shared JSON comms layer (LambdaFeedback/EvaluationFunctionToolkit)
   and wires evaluate.m / preview.m into it. *)

toolkitPath = Environment["LF_TOOLKIT_PATH"];
If[toolkitPath === $Failed || toolkitPath === "",
  toolkitPath = "/opt/lambda-feedback/toolkit-wolfram"
];
PacletDirectoryLoad[toolkitPath];
Needs["LambdaFeedback`EvaluationFunctionToolkit`"];

<< "evaluate.m";
<< "preview.m";

ServeEvaluationFunction[evaluate`EvaluationFunction, preview`PreviewFunction]
