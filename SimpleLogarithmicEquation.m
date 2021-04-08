(* ::Package:: *)

(*Mathematica Package*)

(*Mathematica Package*)BeginPackage["SimpleLogarithmicEquation`"]
(*Exported symbols added here with SymbolName::usage*)

generateSimpleLogarithmicEquation::usage="
generateSimpleLogarithmicEquation[]
 - returns List with 2 elemets, equation and X value
";

solveSimpleLogarithmicEquation::usage="
solveSimpleLogarithmicEquation[equation_]
 - returns List of step by step solution
";

Begin["`Private`"] (*Begin Private Context*)

maskValue[number_]:=
Module[{expression,unknownVariable,coefficient,remainder,transformChance,transformedValues,badNumbers},
	While[True,
	unknownVariable = RandomInteger[{Max[-10,-number],Min[10,number]}];
	coefficient = RandomInteger[{-10,10}];
	badNumbers = Range[-1,1];
    If[MemberQ[badNumbers,coefficient],Continue[]];
    If[MemberQ[badNumbers,unknownVariable],Continue[]];
	remainder = number - coefficient*unknownVariable;
	If[remainder == 0,Continue[]];
	expression = coefficient*"x"+remainder;
	Return[List[expression,unknownVariable]]
]
]

makeString[base_,body_, result_]:= Return[HoldForm[Log[base,body] == result]]

generateSimpleLogarithmicEquation[]:=
Module[{result,body,base,returnValues,maskedBody,results,equation,xValue},
	results = Range[6];
	While[True,
	base = RandomInteger[{2,10}];
	body = RandomInteger[{base+1,1000}];
	result = Log[base,body];
	If[MemberQ[results,result],
		returnValues = maskValue[body];
		maskedBody = Part[returnValues,1];
		xValue = Part[returnValues,2];
		equation = makeString[base,maskedBody,result];
		Return[List[equation,xValue]]
]
]
]

solveSimpleLogarithmicEquation[equation_]:=
Module[{steps,fullForm,step1,linearPart,constantPart,step2,base,body,result,coefficient,step3,rightSide,xValue,string},
	steps = List[];
	AppendTo[steps,equation];
	fullForm = equation // FullForm;
	base = fullForm[[1,1,1,1]];
	body = fullForm[[1,1,1,2]];
	result = fullForm[[1,1,2]];
	rightSide=base^result;
	step1 = body == rightSide;
	AppendTo[steps,step1];
	constantPart = body[[1]];
	linearPart = body[[2]];
	rightSide -= constantPart;
	step2 = linearPart == rightSide;
	AppendTo[steps,step2];
	coefficient = linearPart[[1]];
	xValue =rightSide / coefficient;
	string = xValue//InputForm;
	step3 = "x" ==string;
	AppendTo[steps,step3];
	Return[steps];
]

End[] (*End Private Context*)

EndPackage[]









