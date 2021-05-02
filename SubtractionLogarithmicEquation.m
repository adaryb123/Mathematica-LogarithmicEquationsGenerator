(* ::Package:: *)

(*Package with functions to generate and solve Subtraction Logarithmic Equations*)

BeginPackage["SubtractionLogarithmicEquation`"]

generateSubtractionLogarithmicEquation::usage="
generateSubtractionLogarithmicEquation[]
 - returns List with 2 elemets, equation and X value
";

solveSubtractionLogarithmicEquation::usage="
solveSubtractionLogarithmicEquation[equation_]
 - returns List of step by step solution
";

Begin["`Private`"] 

maskNumbers[number1_,number2_] := Module[{smallerNumber,unknownVariable, coefficient1, remainder1, coefficient2, remainder2, expression1,expression2,biggerNumber,badNumbers},
	
	smallerNumber = Min[number1,number2];
     biggerNumber = Max[number1,number2];
While[True,
	unknownVariable = RandomInteger[{Max[-10,-biggerNumber],Min[10,biggerNumber]}];
	coefficient1 = RandomInteger[{-10,10}];
    coefficient2 = RandomInteger[{-10,10}];
	badNumbers = Range[-1,1];
    If[MemberQ[badNumbers,coefficient1],Continue[]];
    If[MemberQ[badNumbers,coefficient2],Continue[]];
    If[MemberQ[badNumbers,unknownVariable],Continue[]];
	remainder1 =number1 - coefficient1*unknownVariable;
  remainder2 =number2 - coefficient2 * unknownVariable; 
	If[remainder1 ==0 ||remainder2 == 0,Continue[]];
	expression1 = coefficient1*"x"+remainder1;
	expression2 = coefficient2*"x"+remainder2;
	Return[List[expression1,expression2,unknownVariable,remainder1,remainder2]]
	]
]

makeString[base_,body1_, body2_, result_]:= Return[HoldForm[Log[base,body1] - Log[base,body2] == result]]

makeString1[base_,body_, result_]:= Return[HoldForm[Log[base,body] == result]]

makeString2[base_,body1_,body2_,combinedBody_]:= Return[HoldForm[Log[base,body1] + Log[base,body2] == Log[base,combinedBody]]]


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
Module[{steps,fullForm,base,body1,body2,rightSide,combinedBody,step,leftSide,,string,constantPart,coefficient,xValue,explanations,explanation,result},
	steps = List[];
    explanations = List[];
	fullForm = equation //FullForm;
	AppendTo[steps,equation];
	result = fullForm[[1,1,2]];
	base = fullForm[[1,1,1,1,1]];
	body1= fullForm[[1,1,1,1,2]];
	body2= fullForm[[1,1,1,2,2,2]];
	combinedBody = (body1)/(body2);
	step = makeString1[base,combinedBody,result];
	AppendTo[steps,step];
     explanation = makeString2["x","a","b","a / b"];
    AppendTo[explanations,explanation];
	rightSide = base^result;
	leftSide = (body1)/(body2);
	step = leftSide == rightSide;
	AppendTo[steps,step];
    explanation = DisplayForm[RowBox[{base,"^",result,"=",rightSide}]];
    AppendTo[explanations,explanation];
	rightSide =  body2[[1]] * rightSide+ body2[[2]]*rightSide;
	step = body1 ==rightSide;
	AppendTo[steps,step];
    explanation = DisplayForm[RowBox[{"*(",body2,")"}]];
    AppendTo[explanations,explanation];
	leftSide = body1 - rightSide;
	step = leftSide ==0;
	AppendTo[steps,step];
    explanation =DisplayForm[RowBox[{"-(",rightSide,")"}]];
    AppendTo[explanations,explanation];
	constantPart = leftSide[[1]];
	Quiet[Check[coefficient = leftSide[[2,1]];xValue = -constantPart/coefficient;
		explanation = DisplayForm[RowBox[{"/(",coefficient,")"}]];
		AppendTo[explanations,explanation],
			xValue = -constantPart]];
	string = xValue //InputForm;
	step = "x" == string;
	AppendTo[steps,step];
AppendTo[explanations," "];
	Return[List[steps,explanations]]
]



End[] 

EndPackage[]
