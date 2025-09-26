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
BeginPackage["preview`"]

PreviewFunction[response_] := Module[{result},
  Print["Running Preview Function"];
  Print["Preview Input:", response];
  <|
        "command" -> "preview",
        "result" -> <|
          "preview" -> <|
            "message" -> response
          |>
        |>
  |>
]

EndPackage[]