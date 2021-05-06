(* ::Package:: *)

(*Package with functions to generate and solve Base Logarithmic Equations*)

BeginPackage["BaseLogarithmicEquation`"]

generateBaseLogarithmicEquation::usage="
generateBaseLogarithmicEquation[]
 - returns List with 2 elemets, equation and X value
";

solveBaseLogarithmicEquation::usage="
solveBaseLogarithmicEquation[equation_]
 - returns List of step by step solution
";

Begin["`Private`"] (*Begin Private Context*)


maskBase[number_]:=
Module[{expression,badNumbers,unknownVariable, linearCoefficient},
badNumbers = Range[-1,1];
While[True,
	unknownVariable = RandomInteger[{-10,10}];
 
	linearCoefficient  = number - unknownVariable;
expression = "x" + linearCoefficient;
	badNumbers = Range[-1,1];
    If[MemberQ[badNumbers,linearCoefficient],Continue[]];
    If[MemberQ[badNumbers,unknownVariable],Continue[]];
Return[List[expression,unknownVariable]]
]
]

maskBody[number_,unknownVariable_]:=
Module[{expression,quadraticPart,linearPart,constantPart,linearAndConstantPart,linearCoefficient,badNumbers},
	
badNumbers = Range[-1,1];
While[True,
    quadraticPart = unknownVariable^2;
   linearAndConstantPart = number - quadraticPart;
   linearCoefficient = RandomInteger[{-10,10}];
If[MemberQ[badNumbers,linearCoefficient],Continue[]];
	linearPart =linearCoefficient * unknownVariable;
constantPart = linearAndConstantPart  - linearPart;
If[MemberQ[badNumbers,linearCoefficient],Continue[]];
If[constantPart > 100 || constantPart < -100 || constantPart == 0, Continue[]];
expression = "x"^2 + linearCoefficient * "x" + constantPart;
	Return[expression]
]
    ]

makeString[base_,body_, result_]:=
Return[HoldForm[Log[base,body] == result]]

comparePolynomial[poly1_,poly2_] :=Module[{linearCoef1,linearCoef2,constant1,constant2},
linearCoef1 = poly1[[2]];
constant1 = poly1[[1]];
linearCoef2 = poly2[[2]];
constant2 = poly2[[1]];
If[linearCoef1 == linearCoef2 && constant1 == constant2,Return[True]];
Return[False]
]

generateBaseLogarithmicEquation[]:=
Module[{base,body,result,returnValues,equation,unknownVariable,maskedBase,maskedBody,sameFormulaCheck,expandedBase},
While[True,
base = RandomInteger[{2,10}];
result = 2;
body = base^2;
	returnValues =maskBase[base];
	maskedBase = Part[returnValues,1];
	unknownVariable = Part[returnValues,2];
          maskedBody= maskBody[body,unknownVariable];
expandedBase = maskedBase^2;
If[comparePolynomial[maskedBody,Expand[expandedBase]],Continue[]];
equation = makeString[maskedBase,maskedBody,result];
Return[List[equation,unknownVariable]]
]
]

solveBaseLogarithmicEquation[equation_]:=
Module[{steps,fullForm,step,linearPart,base,body,result,coefficient,rightSide,xValue,string,explanations,explanation,xPart,leftSide,constantPart,canBeDevided},
	steps = List[];
    explanations = List[];
	AppendTo[steps,equation];
	fullForm = equation // FullForm;
	base = fullForm[[1,1,1,1]];
	body = fullForm[[1,1,1,2]];
	result = fullForm[[1,1,2]];
   rightSide= base ^ result;
    step = body == rightSide;
	AppendTo[steps,step];
 AppendTo[explanations,""];
linearPart = base[[1]];
rightSide= linearPart^2 + 2*linearPart*"x" + "x"^2 ;
step = body == rightSide;
	AppendTo[steps,step];
explanation = DisplayForm[RowBox[{Power[PlusMinus["a","b"],2],"==",PlusMinus["a"^2,2*"a"*"b"],"+","b"^2}]];
 AppendTo[explanations,explanation];
body = body - "x"^2;
rightSide = rightSide - "x"^2;
step = body==rightSide;
AppendTo[steps,step];
explanation = DisplayForm[RowBox[{"-(","x"^2,")"}]];
AppendTo[explanations,explanation];
leftSide = body - rightSide;
step = leftSide ==0;
AppendTo[steps,step];
explanation = DisplayForm[RowBox[{"-(",rightSide,")"}]];
AppendTo[explanations,explanation];
constantPart = leftSide[[1]];
canBeDevided = True;
Quiet[Check[coefficient = leftSide[[2,1]],canBeDevided = False]];
    If[canBeDevided == True,
xValue = -constantPart/coefficient;
		explanation = DisplayForm[RowBox[{"/(",coefficient,")"}]];
		AppendTo[explanations,explanation],
xValue = -constantPart;
AppendTo[explanations,""]];
	string = xValue //InputForm;
	step = "x" == string;
	AppendTo[steps,step];
AppendTo[explanations,""];
	Return[List[steps,explanations]]
]



End[] 

EndPackage[]








