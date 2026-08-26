(* ::Package:: *)

(* Wolfram Language Package *)

(* :Title: utils *)
(* :Context: utils` *)
(* :Author: marcus *)
(* :Date: 2026-08-26 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 14.0 *)
(* :Copyright: (c) 2025 Lambda Feedback *)
(* :Keywords: *)

(* Shared symbol table and string/expression standardization helpers used by
both evaluate` and preview`. Previously evaluate.m and preview.m each declared
their own copy of activeFunctionRules/StandardizeString/StandardizeExpression;
since BeginPackage narrows $ContextPath to just the new package context while
its body evaluates, each copy created its own distinct symbols (e.g.
evaluate`pi vs preview`pi) rather than sharing one. Whichever package loaded
last "won" for any bare name typed by a user (pi, sin, e, ...), so rules like
pi -> Pi could silently fail to fire depending on load order. Both packages
now list this context as a BeginPackage dependency
(BeginPackage["evaluate`", {"utils`"}] / BeginPackage["preview`", {"utils`"}])
so there is exactly one copy of each symbol. *)

BeginPackage["utils`"];

activeFunctionRules = {
	sin -> Sin, cos -> Cos, tan -> Tan, sec -> Sec, Cosec -> Csc, csc -> Csc, cosec -> Csc, cot -> Cot,
	arcsin -> ArcSin, asin -> ArcSin, arccos -> ArcCos, acos -> ArcCos, arctan -> ArcTan, atan -> ArcTan,
	arcsec -> ArcSec, asec -> ArcSec, ArcCosec -> ArcCsc, arccsc -> ArcCsc, acsc -> ArcCsc, acosec -> ArcCsc,
	arccot -> ArcCot,acot -> ArcCot,
	sinh -> Sinh, cosh -> Cosh, tanh -> Tanh, sech -> Sech, Cosech -> Csch, csch -> Csch, cosech -> Csch, coth -> Coth,
	arcsinh -> ArcSinh, asinh -> ArcSinh, arccosh -> ArcCosh, acosh -> ArcCosh, arctanh -> ArcTanh, atanh -> ArcTanh,
	arcsech -> ArcSech, asech -> ArcSech,
	ArcCsch -> ArcCsch, ArcCosech -> ArcCsch, arccsch->ArcCsch, acsch -> ArcCsch, acosech -> ArcCsch,
	arccoth -> ArcCoth, acoth -> ArcCoth,
	exp -> Exp, log -> Log, ln -> Log, sqrt -> Sqrt,
	pi -> Pi, e -> E, i -> I};

Options[StandardizeString] = {PlusMinusSplit->True};

Options[StandardizeExpression] = {SuppressIndependentVariable -> True};

Begin["`Private`"];

(*StandardizeString: a function that automatically converts all instances
of the equals sign in a string to the repeated equals sign, so that anything WL
would parse as an assignment gets parsed instead as an equation, and also carries out
other standard string replacements*)

StandardizeString[str_String,OptionsPattern[]]:=Module[{output},
output=StringReplace[
    FixedPoint[StringReplace["==="->"=="],StringReplace[str,"="->"=="]],
    {"**"->"^","plus_minus"->"\[PlusMinus]","minus_plus"->"\[MinusPlus]"}];
If[OptionValue[PlusMinusSplit]&&StringContainsQ[output,{"\[PlusMinus]","\[MinusPlus]"}],output="{"<>StringReplace[output,{"\[PlusMinus]"->"+","\[MinusPlus]"->"-"}]<>", "<>StringReplace[output,{"\[PlusMinus]"->"-","\[MinusPlus]"->"+"}]<>"}"];
output]

(*StandardizeExpression: a function that performs a number of standard replacements
at the Expression stage, namely:
- replacing s_[arg_Plus] by s*(arg) unless s is a symbol representing a known function
- replacing expressions of the form dy_^n_/dx_^n_ with y'[x]^n
- if the option SuppressIndependentVariable is set to True, replacing each y'[x] with y'*)

StandardizeExpression[expr_, OptionsPattern[]]:=Module[{output,suppress},
	suppress = OptionValue[SuppressIndependentVariable];
    output = expr/.activeFunctionRules;
	output = output/.(s:_Symbol)[arg_Plus]/;Not[MemberQ[Attributes[s], NumericFunction]] :> s*arg;
	output = output/.I[arg_]:>I*arg;
	output = output/.{
		dx_^a_. dy_^b_.:>
			(ToExpression[StringTake[ToString[dx],{2}]]'[StringTake[ToString[dy],{2}]])^a/;
				StringTake[ToString[dx],{1}]=="d"&&StringTake[ToString[dy],{1}]=="d"&&a>0&&a+b==0,
		dx_^a_. dy_^b_.:>
			(ToExpression[StringTake[ToString[dy],{2}]]'[StringTake[ToString[dx],{2}]])^a/;
				StringTake[ToString[dx],{1}]=="d"&&StringTake[ToString[dy],{1}]=="d"&&b>0&&a+b==0};
	If[suppress,output=output/.Derivative[n_][y_][x_]:>Derivative[n][y]];
	output
]

End[];
EndPackage[];
