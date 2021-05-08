(* ::Package:: *)

(*Package with functions to generate and solve Simple Logarithmic Equations*)

BeginPackage["SimpleLogarithmicEquation`"]

generateSimpleLogarithmicEquation::usage="
generateSimpleLogarithmicEquation[]
 - returns List with 2 elemets, equation and X value
";

solveSimpleLogarithmicEquation::usage="
solveSimpleLogarithmicEquation[equation_]
 - returns List of step by step solution
";

Begin["`Private`"]

(*this function masks the body of logarithm *)
maskValue[number_]:=
Module[{expression,unknownVariable,coefficient,remainder,transformChance,transformedValues,badNumbers},
	
	While[True,
		(*initialize variables randomly*)
		unknownVariable = RandomInteger[{Max[-10,-number],Min[10,number]}];
		coefficient = RandomInteger[{-10,10}];
		badNumbers = Range[-1,1];

		(*try again if they are outside desired interval*)
        If[MemberQ[badNumbers,coefficient],Continue[]];
        If[MemberQ[badNumbers,unknownVariable],Continue[]];
	    remainder = number - coefficient*unknownVariable;
	    If[remainder == 0,Continue[]];

	    expression = coefficient*"x"+remainder;
	    Return[List[expression,unknownVariable]]
]
]

(*this function returns the equation as string *)
makeString[base_,body_, result_]:= Return[HoldForm[Log[base,body] == result]]

(*this function generates simple logarithmic equation *)
generateSimpleLogarithmicEquation[]:=
Module[{result,body,base,returnValues,maskedBody,results,equation,xValue},

	(*initialize variables randomly*)
	results = Range[6];
	While[True,
		base = RandomInteger[{2,10}];
		body = RandomInteger[{base+1,1000}];
		result = Log[base,body];
		(*if result of logarithm is whole number, mask value of logarithm body and return equation*) 
		If[MemberQ[results,result],
			returnValues = maskValue[body];
			maskedBody = Part[returnValues,1];
			xValue = Part[returnValues,2];
			equation = makeString[base,maskedBody,result];
			Return[List[equation,xValue]]
]
]
]

(*this function creates solution to given simple logarithmic equation *)
solveSimpleLogarithmicEquation[equation_]:=
Module[{steps,fullForm,step,linearPart,constantPart,base,body,result,coefficient,rightSide,xValue,string,explanations,explanation},
	
	steps = List[];
    explanations = List[];
	AppendTo[steps,equation];

	(* get rid of the logarithm*)
	fullForm = equation // FullForm;
	base = fullForm[[1,1,1,1]];
	body = fullForm[[1,1,1,2]];
	result = fullForm[[1,1,2]];
	rightSide=base^result;
	step = body == rightSide;
	AppendTo[steps,step];
    explanation = DisplayForm[RowBox[{base,"^",result,"=",rightSide}]];
    AppendTo[explanations,explanation];

	(* isolate variable x *)
	constantPart = body[[1]];
	linearPart = body[[2]];
	rightSide -= constantPart;
	step = linearPart == rightSide;
	AppendTo[steps,step];
    explanation = DisplayForm[RowBox[{"-(",body[[1]],")"}]];
    AppendTo[explanations,explanation];

	(* divide both sides *)
	coefficient = linearPart[[1]];
	xValue =rightSide / coefficient;
	string = xValue//InputForm;
	step = "x" ==string;
	AppendTo[steps,step]; 
    explanation = DisplayForm[RowBox[{"/(",coefficient,")"}]];
    AppendTo[explanations,explanation];

    AppendTo[explanations,""];
	Return[List[steps,explanations]];
]

End[] 

EndPackage[]









