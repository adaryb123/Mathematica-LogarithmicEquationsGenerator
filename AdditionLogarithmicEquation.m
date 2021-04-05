(* ::Package:: *)

(*Mathematica Package*)

(*Mathematica Package*)BeginPackage["AdditionLogarithmicEquation`"]
(*Exported symbols added here with SymbolName::usage*)

generateAdditionLogarithmicEquation::usage="
generateAdditionLogarithmicEquation[]
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

maskNumbers[number1_,number2_] := Module[{smallerNumber,unknownVariable, coefficient1, remainder1, coefficient2, remainder2, expression1,expression2,transformChance,transformedValues},

	smallerNumber = Min[number1,number2];
While[True,
	unknownVariable = RandomInteger[{2,Min[20,smallerNumber]}];
	coefficient1 = Quotient[number1,unknownVariable];
	remainder1 = Mod[number1,unknownVariable];
	coefficient2 = Quotient[number2,unknownVariable];
	remainder2 = Mod[number2,unknownVariable];


	transformChance = RandomInteger[{1,2}];
	If[transformChance == 2,transformedValues = transformToSubtraction[coefficient1,remainder1,unknownVariable];
	coefficient1 = Part[transformedValues,1];
	remainder1 = Part[transformedValues,2];
	];
	transformChance = RandomInteger[{1,2}];
	If[transformChance == 2,transformedValues = transformToSubtraction[coefficient2,remainder2,unknownVariable];
		coefficient2 = Part[transformedValues,1];
		remainder2 = Part[transformedValues,2]
	];

	If[remainder1 ==0 ||remainder2 == 0,Continue[]];

	expression1 = coefficient1*"x"+remainder1;
	expression2 = coefficient2*"x"+remainder2;
	Return[List[expression1,expression2,unknownVariable,remainder1,remainder2]]
	]
]

makeString[base_,body1_, body2_, result_]:= Return[HoldForm[Log[base,body1] + Log[base,body2] == result]]

generateAdditionLogarithmicEquation[]:=
Module[{base,body1,body2,result,returnValues,maskedBody1,maskedBody2,unknownVariable, fitness,remainder1,remainder2,results,xValue,equation,mode},

results = Range[30];

mode =0;
While[True,
	If[mode == 3,mode=0];
  If [mode == 2,base = RandomInteger[{2,10}];mode = 3];
  If[mode == 1, base = RandomInteger[{3,10}];mode = 2];
  If[mode== 0, base = RandomInteger[{6,10}];mode = 1];
	body1 = RandomInteger[{base+1,1000}];
	body2 = RandomInteger[{base+1,body1-1}];
	result = Log[base,body1] + Log[base,body2];
	If[MemberQ[results,result],
		returnValues =maskNumbers[body1,body2];
		maskedBody1 = Part[returnValues,1];
		maskedBody2 = Part[returnValues,2];
		unknownVariable = Part[returnValues,3];
		remainder1 = Part[returnValues,4];
		remainder2 = Part[returnValues,5];
		fitness = calculateFitness[remainder1,remainder2,base,result];
	equation = makeString[base,maskedBody1,maskedBody2,result];
	xValue = unknownVariable;
	Return[List[equation,xValue]]
	]
];
]

End[] (*End Private Context*)

EndPackage[]

