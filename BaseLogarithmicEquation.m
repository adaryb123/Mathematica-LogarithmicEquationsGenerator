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


(*this function masks the base of logarithm*)
maskBase[number_]:=
Module[{expression,badNumbers,unknownVariable, linearCoefficient},

	badNumbers = Range[-1,1];
	While[True,
		
		(*initialize variable randomly *)
		unknownVariable = RandomInteger[{-10,10}];

		linearCoefficient  = number - unknownVariable;
		expression = "x" + linearCoefficient;
		
		(*try again if they are outside desired interval*)
		badNumbers = Range[-1,1];
        If[MemberQ[badNumbers,linearCoefficient],Continue[]];
        If[MemberQ[badNumbers,unknownVariable],Continue[]];

     Return[List[expression,unknownVariable]]
]
]

(*this function masks the body of logarithm*)
maskBody[number_,unknownVariable_]:=
Module[{expression,quadraticPart,linearPart,constantPart,linearAndConstantPart,linearCoefficient,badNumbers},
	
	badNumbers = Range[-1,1];
	While[True,
		(*initialize variables*)
        quadraticPart = unknownVariable^2;
        linearAndConstantPart = number - quadraticPart;
        linearCoefficient = RandomInteger[{-10,10}];
		
		(*try again if they are outside desired interval *)
        If[MemberQ[badNumbers,linearCoefficient],Continue[]];
	    linearPart =linearCoefficient * unknownVariable;
        constantPart = linearAndConstantPart  - linearPart;
        If[MemberQ[badNumbers,linearCoefficient],Continue[]];
        If[constantPart > 100 || constantPart < -100 || constantPart == 0, Continue[]];

        expression = "x"^2 + linearCoefficient * "x" + constantPart;
	    Return[expression]
]
]

(*this function returns the equation as string *)
makeString[base_,body_, result_]:=Return[HoldForm[Log[base,body] == result]]

(*this function checks if base^2 and body are the same expression *)
comparePolynomial[poly1_,poly2_] :=Module[{linearCoef1,linearCoef2,constant1,constant2},
	linearCoef1 = poly1[[2]];
	constant1 = poly1[[1]];
	linearCoef2 = poly2[[2]];
	constant2 = poly2[[1]];
	If[linearCoef1 == linearCoef2 && constant1 == constant2,Return[True]];
	Return[False]
]

(*this function generates base logarithmic equation *)
generateBaseLogarithmicEquation[]:=
Module[{base,body,result,returnValues,equation,unknownVariable,maskedBase,maskedBody,sameFormulaCheck,expandedBase},

	While[True,

	(*initialize variable at random*)
	base = RandomInteger[{2,10}];
	result = 2;
	body = base^2;

	(*mask base and body of the logarithm *)
	returnValues =maskBase[base];
	maskedBase = Part[returnValues,1];
	unknownVariable = Part[returnValues,2];
    maskedBody= maskBody[body,unknownVariable];
	expandedBase = maskedBase^2;

	(* try again if both parts got masked as the same expression *)
	If[comparePolynomial[maskedBody,Expand[expandedBase]],Continue[]];

	equation = makeString[maskedBase,maskedBody,result];
	Return[List[equation,unknownVariable]]
]
]

(*this function solves the given base logarithmic equation*)
solveBaseLogarithmicEquation[equation_]:=
Module[{steps,fullForm,step,linearPart,base,body,result,coefficient,rightSide,xValue,string,explanations,explanation,xPart,leftSide,constantPart,canBeDevided},
	
	steps = List[];
    explanations = List[];
	AppendTo[steps,equation];

	(* get rid of the logarithm *)
	fullForm = equation // FullForm;
	base = fullForm[[1,1,1,1]];
	body = fullForm[[1,1,1,2]];
	result = fullForm[[1,1,2]];
    rightSide= base ^ result;
    step = body == rightSide;
	AppendTo[steps,step];
    AppendTo[explanations,""];

	(*expand the right side *)
    linearPart = base[[1]];
    rightSide= linearPart^2 + 2*linearPart*"x" + "x"^2 ;
    step = body == rightSide;
	AppendTo[steps,step];
    explanation = DisplayForm[RowBox[{Power[PlusMinus["a","b"],2],"==",PlusMinus["a"^2,2*"a"*"b"],"+","b"^2}]];
    AppendTo[explanations,explanation];
	
	(* get rid of the x^2 part *)
    body = body - "x"^2;
    rightSide = rightSide - "x"^2;
    step = body==rightSide;
    AppendTo[steps,step];
    explanation = DisplayForm[RowBox[{"-(","x"^2,")"}]];
    AppendTo[explanations,explanation];

	(* isolate variable x *)
    leftSide = body - rightSide;
    step = leftSide ==0;
    AppendTo[steps,step];
    explanation = DisplayForm[RowBox[{"-(",rightSide,")"}]];
    AppendTo[explanations,explanation];

	(* divide both sides if possible *)
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








