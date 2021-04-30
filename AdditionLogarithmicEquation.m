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

makeString[base_,body1_, body2_, result_]:= Return[HoldForm[Log[base,body1] + Log[base,body2] == result]]

makeString1[base_,body_, result_]:= Return[HoldForm[Log[base,body] == result]]

makeString2[base_,body1_,body2_,combinedBody_]:= Return[HoldForm[Log[base,body1] + Log[base,body2] == Log[base,combinedBody]]]

generateAdditionLogarithmicEquation[]:=
Module[{base,body1,body2,result,returnValues,maskedBody1,maskedBody2,unknownVariable,remainder1,remainder2,equation,part1,part2,constantPart1,linearPart1,constantPart2,linearPart2,leftSide,string,xOccurences,a,b,c,discriminant,x1,x2,acceptedValues},

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
x1 = Plus[Minus[b],Sqrt[discriminant]]/(2*a);
x2 = Subtract[Minus[b],Sqrt[discriminant]]/(2*a);
acceptedValues = Range[-10,10];
If[!MemberQ[acceptedValues,x1] || !MemberQ[acceptedValues,x2], Continue[]];
],Continue[]]];
unknownVariable = Part[returnValues,3];
remainder1 = Part[returnValues,4];
remainder2 = Part[returnValues,5];
equation = makeString[base,maskedBody1,maskedBody2,result];
Return[List[equation,unknownVariable]]
]
]

testXValues[x1_,x2_,body1_,body2_]:= Module[{steps,result,constantCoef1,constantCoef2,linearCoef1,linearCoef2,number1,number2,string,step},
	steps = List[];
	constantCoef1 = body1[[1]];
	constantCoef2 = body2[[1]];
	linearCoef1 = body1[[2,1]];
	linearCoef2 = body2[[2,1]];
	number1 =constantCoef1 + Times[x1,linearCoef1];
	number2 = constantCoef2 + Times[x2,linearCoef2];
	If[number1 > 0 && number2 > 0 && x1 != x2,
	string = x1// InputForm;
	step = "x1" == string;
	AppendTo[steps,step];
	string = x2 // InputForm;
	step = "x2" == string;
	AppendTo[steps,step];
	Return[steps]
	];
	If[number1 > 0 && number2 > 0 && x1 == x2,
	string = x1 // InputForm;
	step = "x" == string;
	AppendTo[steps,step];
	Return[steps]
	];
	 If[number1 > 0 && number2 <= 0,
	string =x1 // InputForm;
	step = "x" == string;
	AppendTo[steps,step];
	Return[steps]
	];
	 If[number1 <= 0 && number2 > 0,
	string = x2 // InputForm;
	step = "x" == string;
	AppendTo[steps,step];
	Return[steps]
	];
]

solveQuadraticEquation[equation_,body1_,body2_] :=
Module[{a,b,c,gcd,newEquation,newA,newB,newC,gcdStep,discriminant,steps,x1,x2,step,string,result,additionalSteps,i,explanations,explanation},
	steps = List[];
    explanations = List[];
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
  explanation = DisplayForm[RowBox[{"/",gcd}]];
  AppendTo[explanations,explanation];

	];
	discriminant = b^2 - 4*a*c;
	string = discriminant //InputForm;
	step = "D" == string;
	AppendTo[steps,step];
     explanation = DisplayForm[RowBox[{"D = b^2 - 4ac"}]];
     AppendTo[explanations,explanation];

    Which[discriminant != 0,
		      string = Sqrt[discriminant] //InputForm;
                string = PlusMinus[Minus[b],Sqrt[discriminant]]/(2*a) //InputForm;
		     step = "x" == string;
		    AppendTo[steps,step];
                 explanation =  DisplayForm[RowBox[{"x = ",PlusMinus[Minus["b"],Sqrt["D"]],"/ 2a"}]];
                   AppendTo[explanations,explanation];

		    x1= Plus[Minus[b],Sqrt[discriminant]]/(2*a);
		    x2 = Subtract[Minus[b],Sqrt[discriminant]]/(2*a);
	            additionalSteps = testXValues[x1,x2,body1,body2];
              For [i = 1, i <= Length[additionalSteps],i++,
		AppendTo[steps,additionalSteps[[i]]];AppendTo[explanations," "]];
		AppendTo[explanations," "],

                     discriminant == 0,
             string =DisplayForm[RowBox[{-b," / ",2*a}]];
	           step = "x" == string;
	           AppendTo[steps,step];
	    explanation = DisplayForm[RowBox[{"x = -b / 2a"}]];
               AppendTo[explanations,explanation];
            string = Minus[b]/(2*a) // InputForm;
	   step = "x" == string;
	           AppendTo[steps,step];
          AppendTo[explanations," "];
	 AppendTo[explanations," "];

  ];
	Return[List[steps,explanations]];
]

solveAdditionLogarithmicEquation[equation_]:=
Module[{steps,fullForm,base,body1,body2,rightSide,combinedBody,step,leftSide,constantPart1,constantPart2,linearPart1,linearPart2,string,xOccurences,additionalSteps,gcd,coefficient,number,xValue,i,explanations,explanation,result},
	steps = List[];
     explanations = List[];
	fullForm = equation //FullForm;
	AppendTo[steps,equation];
	result = fullForm[[1,1,2]];
	base = fullForm[[1,1,1,1,1]];
	body1= fullForm[[1,1,1,1,2]];
	body2= fullForm[[1,1,1,2,2]];
	combinedBody = DisplayForm[RowBox[{"(",body1,")*(",body2,")"}]];
	rightSide = Log[base,base^result];
	step = makeString1[base,combinedBody,rightSide];
    AppendTo[steps,step];

	rightSide = base^result;
	leftSide = (body1)*(body2);
	constantPart1 = leftSide[[1,1]];
	constantPart2 = leftSide[[2,1]];
	linearPart1 = leftSide[[1,2]];
	linearPart2 = leftSide[[2,2]];
    leftSide =  DisplayForm[RowBox[{"(",body1,")*(",body2,")"}]];
	step = leftSide == rightSide;
	AppendTo[steps,step];
    explanation = makeString2["x","a","b","a * b"];
      AppendTo[explanations,explanation];

	leftSide = constantPart1*constantPart2 + linearPart2 * constantPart1 +   linearPart1 * constantPart2 + linearPart1*linearPart2;
	step = leftSide == rightSide;
	AppendTo[steps,step];
    explanation = DisplayForm[RowBox[{base,"^",result,"=",rightSide}]];
      AppendTo[explanations,explanation];
     AppendTo[explanations,""];
	step = leftSide - rightSide == 0;
	AppendTo[steps,step];
   explanation = DisplayForm[RowBox[{"-(",rightSide,")"}]];
     AppendTo[explanations,explanation];

	string  = ToString[step];
	xOccurences = StringCount[string,"x"];
	If[xOccurences ==2, additionalSteps = solveQuadraticEquation[step,body1,body2];
	For[i = 1, i <= Length[additionalSteps[[1]]], i++,
		AppendTo[steps,Part[additionalSteps[[1]],i]];
	AppendTo[explanations,Part[additionalSteps[[2]],i]];
		];
	 AppendTo[explanations," "];
		Return[List[steps,explanations]];
	]; 
	number = step[[1,1]];
	coefficient= step[[1,2,1]];
	step="x"^2 ==-number/coefficient;
         AppendTo[steps,step];
    explanation = DisplayForm[RowBox[{"/",coefficient}]];
      AppendTo[explanations,explanation];

	xValue =-number/coefficient;
	         string = Sqrt[xValue]//InputForm;
	         step = "x1" ==string;
	         AppendTo[steps,step];
	      AppendTo[explanations," "];
	string = -Sqrt[xValue]//InputForm;
	         step = "x2" ==string;
	         AppendTo[steps,step];
              AppendTo[explanations," "];
 AppendTo[explanations," "];
	Return[List[steps,explanations]];
]


End[] (*End Private Context*)

EndPackage[]

