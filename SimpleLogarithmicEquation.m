(* ::Package:: *)

(*Mathematica Package*)

(*Mathematica Package*)BeginPackage["SimpleLogarithmicEquation`"]
(*Exported symbols added here with SymbolName::usage*)

generateSimpleLogarithmicEquation::usage="
generateSimpleLogarithmicEquation[]
 - returns List with 2 elemets, equation and X value
";

Begin["`Private`"] (*Begin Private Context*)

transformToSubtraction[coefficient_,remainder_,unknownVariable_] :=
Module[{sum,newRemainder,newCoefficient,temp},
sum = coefficient * unknownVariable + remainder;
While[coefficient * unknownVariable < sum,
coefficient++;
];
remainder = sum - coefficient * unknownVariable;
Return[List[coefficient,remainder]]
]
SetAttributes[transformToSubtraction,HoldAll]

maskValue[number_]:=
Module[{expression,unknownVariable,coefficient,remainder,transformChance,transformedValues},
	While[True,
	unknownVariable = RandomInteger[{2,Min[5,number]}];
	coefficient = Quotient[number,unknownVariable];
	remainder = Mod[number,unknownVariable];

	transformChance = RandomInteger[{1,2}];
	If[transformChance == 2, 
	transformedValues = transformToSubtraction[coefficient,remainder,unknownVariable];
	coefficient = Part[transformedValues,1];
	remainder = Part[transformedValues,2]
	];
	
	If[remainder == 0,Continue[]];

	expression = coefficient*"x"+remainder;
	Return[List[expression,unknownVariable]]
]
]

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
		equation = Log[base,maskedBody] == result;
		Return[List[equation,xValue]]
]
]
]

End[] (*End Private Context*)

EndPackage[]






