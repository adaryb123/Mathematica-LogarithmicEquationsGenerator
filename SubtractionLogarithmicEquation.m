(* ::Package:: *)

(*Mathematica Package*)

(*Mathematica Package*)BeginPackage["SubtractionLogarithmicEquation`"]
(*Exported symbols added here with SymbolName::usage*)

generateSubtractionLogarithmicEquation::usage="
generateSubtractionLogarithmicEquation[]
 - returns List with 2 elemets, equation and X value
";

solveSubtractionLogarithmicEquation[]::usage="
solveSubtractionLogarithmicEquation[equation_]
 - returns List of step by step solution
";

Begin["`Private`"] (*Begin Private Context*)

maskNumbers[number1_,number2_] := Module[{smallerNumber,unknownVariable, coefficient1, remainder1, coefficient2, remainder2, expression1,expression2,biggerNumber},
	
	smallerNumber = Min[number1,number2];
     biggerNumber = Max[number1,number2];
While[True,
	unknownVariable = RandomInteger[{Max[smallerNumber,3],Min[20,biggerNumber]}];
	coefficient1 = RandomInteger[{2,10}];
  coefficient2 = RandomInteger[{2,10}];
	remainder1 =number1 - coefficient1*unknownVariable;
  remainder2 =number2 - coefficient2 * unknownVariable; 
	If[remainder1 ==0 ||remainder2 == 0,Continue[]];
	expression1 = coefficient1*"x"+remainder1;
	expression2 = coefficient2*"x"+remainder2;
	Return[List[expression1,expression2,unknownVariable,remainder1,remainder2]]
	]
]

makeString[base_,body1_, body2_, result_]:= Return[HoldForm[Log[base,body1] - Log[base,body2] == result]]

generateSubtractionLogarithmicEquation[]:=
Module[{base,body1,body2,result,returnValues,maskedBody1,maskedBody2,unknownVariable,remainder1,remainder2,equation,part1,part2,futureRightSide},

While[True,
	result = RandomInteger[{1,3}];
base = RandomInteger[{2,10}];
part2 = RandomInteger[{0,result}];
part1 = result + part2;
	body1 = base^part1;
	body2 = base^part2;
If[body1 > 1000, Continue[]];
	returnValues =maskNumbers[body1,body2];
	maskedBody1 = Part[returnValues,1];
	maskedBody2 = Part[returnValues,2];
If[maskedBody1 == maskedBody2, Continue[]];
futureRightSide = maskedBody2[[1]] * base^result + maskedBody2[[2]] * base^result ;
If[futureRightSide == maskedBody1,Continue[]];
If[futureRightSide[[1]] > 1000 || futureRightSide[[1]] < -1000, Continue[]];
unknownVariable = Part[returnValues,3];
	remainder1 = Part[returnValues,4];
	remainder2 = Part[returnValues,5];
	equation = makeString[base,maskedBody1,maskedBody2,result];
Return[List[equation,unknownVariable]]
	];
]

solveSubtractionLogarithmicEquation[equation_]:=
Module[{steps,fullForm,base,body1,body2,rightSide,combinedLog,step1,leftSide,step2,step3,step4,string,constantPart,coefficient,step5,xValue},
	steps = List[];
	fullForm = equation //FullForm;
	AppendTo[steps,equation];
	rightSide = fullForm[[1,1,2]];
	base = fullForm[[1,1,1,1,1]];
	body1= fullForm[[1,1,1,1,2]];
	body2= fullForm[[1,1,1,2,2,2]];
	combinedLog = Log[base,(body1)/(body2)];
	step1 = combinedLog == rightSide;
	AppendTo[steps,step1];
	rightSide = base^rightSide;
	leftSide = (body1)/(body2);
	step2 = leftSide == rightSide;
	AppendTo[steps,step2];
	rightSide =  body2[[1]] * rightSide+ body2[[2]]*rightSide;
	step3 = body1 ==rightSide;
	AppendTo[steps,step3];
	leftSide = body1 - rightSide;
	step4 = leftSide ==0;
	AppendTo[steps,step4];
	constantPart = leftSide[[1]];
	Quiet[Check[coefficient = leftSide[[2,1]];xValue = -constantPart/coefficient,
			xValue = -constantPart]];
	string = xValue //InputForm;
	step5 = "x" == string;
	AppendTo[steps,step5];
	Return[steps]
]


End[] (*End Private Context*)

EndPackage[]
