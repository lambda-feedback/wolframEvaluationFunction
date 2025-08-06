(* A function to test whether a response is equal to an answer, \
including to within a given tolerance; input and output as
Associations *)

equalQAssociation =
    Function[
        Module[{tolerance, correctQ, error},
            If[NumericQ[#answer],
                tolerance =
                    If[#params["tolerance_is_absolute"],
                        #params["tolerance"]
                        ,
                        #params["tolerance"] * #params["answer"]
                    ];
                error = Abs[#answer - #response];
                correctQ = TrueQ[error <= tolerance]
                ,
                error = "not applicable";
                correctQ = TrueQ[#answer == #response]
            ];
            <|
                "command" -> "eval"
                ,
                "result" ->
                    {
                        "is_correct" -> correctQ
                        ,
                        "feedback" ->
                            If[correctQ,
                                #params["correct_response_feedback"]
                                ,
                                #params["incorrect_response_feedback"
                                    ]
                            ]
                        ,
                        "error" -> error
                    }
            |>
        ]
    ];

(* A function to test whether a response is equal to an answer, \
including to within a given tolerance; input and output as
JSON strings

Calls equalQAssociation  *)

equalQJSON = Function[ExportString[equalQAssociation[ImportString[#, 
    "JSON"] //. List :> Association], "JSON", "Compact" -> True]];

(* A function to test whether a response is equal to an answer, \
including to within a given tolerance; input and output read in from \
external JSON files

Calls equalQAssociation  *)

equalQIO = Function[Export[#2, equalQAssociation[Import[#1, "JSON"] //.
     List :> Association], "JSON", "Compact" -> True]];

argv = Rest[$ScriptCommandLine]

equalQIO[argv[[1]], argv[[2]]]
