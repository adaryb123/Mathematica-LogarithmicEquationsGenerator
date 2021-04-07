(* ::Package:: *)

(*Mathematica Package*)

(*Mathematica Package*)BeginPackage["AdditionLogarithmicEquation`"]
(*Exported symbols added here with SymbolName::usage*)

generateAdditionLogarithmicEquation::usage="
generateAdditionLogarithmicEquation[]
 - returns List with 2 elemets, equation and X value
";

solveAdditionLogarithmicEquation::usage="
solveAdditionLogarithmicEquation[equation_]
 - returns List of step by step solution
";

Begin["`Private`"] (*Begin Private Context*)

maskNumbers[number1_,number2_] := Module[{smallerNumber,unknownVariable, coefficient1, remainder1, coefficient2, remainder2, expression1,expression2,biggerNumber},

	smallerNumber = Min[number1,number2];
     biggerNumber = Max[number1,number2];
While[True,
	unknownVariable = RandomInteger[{Max[smallerNumber,5],Min[20,biggerNumber]}];
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

makeString[base_,body1_, body2_, result_]:= Return[HoldForm[Log[base,body1] + Log[base,body2] == result]]

generateAdditionLogarithmicEquation[]:=
Module[{base,body1,body2,result,returnValues,maskedBody1,maskedBody2,unknownVariable,remainder1,remainder2,equation,part1,part2,constantPart1,linearPart1,constantPart2,linearPart2,leftSide,string,xOccurences,a,b,c,discriminant},

While[True,
	result = RandomInteger[{1,5}];
base = RandomInteger[{2,10}];
part1 = RandomInteger[{1,result}];
part2 = result -part1;
	body1 = base^part1;
	body2 = base^part2;
	returnValues =maskNumbers[body1,body2];
	maskedBody1 = Part[returnValues,1];
	maskedBody2 = Part[returnValues,2];
	If[maskedBody1 == maskedBody2, Continue[]];
leftSide = (maskedBody1)*(maskedBody2);
constantPart1 = leftSide[[1,1]];
constantPart2 = leftSide[[2,1]];
linearPart1 = leftSide[[1,2]];
linearPart2 = leftSide[[2,2]];

leftSide = constantPart1*constantPart2 + linearPart2 * constantPart1 +   linearPart1 * constantPart2 + linearPart1*linearPart2 -  base^result ==0;
string  = ToString[leftSide];
xOccurences = StringCount[string,"x"];
Quiet[Check[If[xOccurences ==2, 
	a=0;
	b = 0;
	c = 0;
	c = leftSide[[1,1]];
	b = leftSide[[1,2,1]];
	a = leftSide[[1,3,1]];
	discriminant = b^2 - 4*a*c;
	If[discriminant > 500, Continue[]];
],Continue[]]];


unknownVariable = Part[returnValues,3];
	remainder1 = Part[returnValues,4];
	remainder2 = Part[returnValues,5];
	equation = makeString[base,maskedBody1,maskedBody2,result];
	Return[List[equation,unknownVariable]]
	];
]

solveQuadraticEquation[equation_] :=
Module[{a,b,c,gcd,newEquation,newA,newB,newC,gcdStep,discriminant,steps,x1,x2,step1,string,step2,step3,step4,result,step5},
	steps = List[];
	c = equation[[1,1]];
	b = equation[[1,2,1]];
	a = equation[[1,3,1]];
	gcd = GCD[a,b,c];
	gcdStep = equation;
	If[gcd != 1,
	a = a/gcd;
	b= b/gcd;
	c= c/gcd;
	gcdStep = a* "x"^2 +b *"x" + c ==0;
	AppendTo[steps,gcdStep];
	];
	discriminant = b^2 - 4*a*c;
	string = discriminant //InputForm;
	step1 = "D" == string;
	AppendTo[steps,step1];
	string = Sqrt[discriminant] //InputForm;
	step2 = "Sqrt[D]" == string;
	AppendTo[steps,step2];
	     string = PlusMinus[Minus[b],Sqrt[discriminant]]/(2*a) //InputForm;
	step3 = "x" == string;
	AppendTo[steps,step3];
	result = Plus[Minus[b],Sqrt[discriminant]]/(2*a)//InputForm;
	step4 = "x1" == result;
	AppendTo[steps,step4];
	result = Subtract[Minus[b],Sqrt[discriminant]]/(2*a)//InputForm;
	step5 = "x2" == result;
	AppendTo[steps,step5];
	Return[steps];
]


solveAdditionLogarithmicEquation[equation_]:=
Module[{steps,fullForm,base,body1,body2,rightSide,combinedLog,step1,leftSide,step2,constantPart1,constantPart2,linearPart1,linearPart2,step3,step4,string,xOccurences,additionalSteps,gcd,coefficient,number,step5,step6,xValue,i},
	steps = List[];
	fullForm = equation //FullForm;
	AppendTo[steps,equation];
	rightSide = fullForm[[1,1,2]];
	base = fullForm[[1,1,1,1,1]];
	body1= fullForm[[1,1,1,1,2]];
	body2= fullForm[[1,1,1,2,2]];
	combinedLog = Log[base,(body1)*(body2)];
	rightSide = Log[base,base^rightSide];
	step1 = combinedLog == rightSide;
	AppendTo[steps,step1];
	rightSide = base^rightSide;
	leftSide = (body1)*(body2);

	constantPart1 = leftSide[[1,1]];
	constantPart2 = leftSide[[2,1]];
	linearPart1 = leftSide[[1,2]];
	linearPart2 = leftSide[[2,2]];
	step2 = leftSide == rightSide;

	AppendTo[steps,step2];
	leftSide = constantPart1*constantPart2 + linearPart2 * constantPart1 +   linearPart1 * constantPart2 + linearPart1*linearPart2;
	step3 = leftSide == rightSide;
	AppendTo[steps,step3];
	step4 = leftSide - rightSide == 0;
	AppendTo[steps,step4];
	string  = ToString[step4];
	xOccurences = StringCount[string,"x"];
	If[xOccurences ==2, additionalSteps = solveQuadraticEquation[step4];
	For[i = 1, i <= Length[additionalSteps], i++,
		AppendTo[steps,Part[additionalSteps,i]]
	];
	Return[steps]
	]; 
	number = step4[[1,1]];
	coefficient= step4[[1,2,1]];
	step5="x"^2 ==-number/coefficient;
         AppendTo[steps,step5];
	xValue =-number/coefficient;
	         string = Sqrt[xValue]//InputForm;
	         step6 = "x" ==string;
	         AppendTo[steps,step6];
	Return[steps]
]



End[] (*End Private Context*)

EndPackage[]

