(* ::Package:: *)

(*Package with functions to generate and solve Exponential Logarithmic Equations*)

BeginPackage["ExponentialLogarithmicEquation`"]

generateExponentialLogarithmicEquation::usage="
ggenerateExponentialLogarithmicEquation[]
 - returns List with 2 elemets, equation and X value
";

solveExponentialLogarithmicEquation::usage="
solveExponentialLogarithmicEquation[equation_]
 - returns List of step by step solution
";

Begin["`Private`"] 


(*this function masks the body of logarithm *)
maskBody[body_,base_]:=
Module[{exponentialPart,linearPart,exponent,sum,expression,plusChance},

	While[True,
		(*initialize variable randomly*)
		exponentialPart =RandomInteger[{2,Min[5,Round[body/2]]}];
		(*increment exponent until the expression is bigger then body of the logarithm*)
		If[exponentialPart == base, exponentialPart++];
		exponent =2;
		While[exponentialPart ^ exponent < body,exponent++];
		linearPart = body - exponentialPart^exponent;

		(* chance to convert the expression to subtraction*) 
		plusChance = RandomInteger[{1,2}];
		If[plusChance == 2,
			exponent--;
			linearPart = body - exponentialPart^exponent
		];

		(*try again if they are outside desired interval*)
		If [linearPart == 0,Continue[]];
		If [linearPart > 500 || linearPart < -500, Continue[]];

		expression = Power[exponentialPart,"x"] + linearPart;
		Return[List[expression,exponent]]
]
]

(*these functions return the equation as string *)
makeString[base_,body_, result_]:= Return[HoldForm[Log[base,body] == result]]
makeString1[body_,rightSide_] :=Return[HoldForm[body== rightSide]]

(*this function generates exponential logarithmic equation *)
generateExponentialLogarithmicEquation[]:=
Module[{result,base,body,returnValues,expression,results,xValue,equation},

	(*initialize variables randomly*)
	results = Range[5];
	While[True,
		base = RandomInteger[{2,10}];
		body = RandomInteger[{base+1,500}];
		result = Log[base,body];
		(*if result of logarithm is whole number, mask value of logarithm body and return equation*) 
		If[MemberQ[results,result],
			returnValues = maskBody[body,base];
			expression = Part[returnValues,1];
			xValue = Part[returnValues,2];
			equation = makeString[base,expression,result];
			Return[List[equation,xValue]]
		]
]
]

(*this function creates solution to given exponential logarithmic equation *)
solveExponentialLogarithmicEquation[equation_]:=
Module[{steps,fullForm,base,body,result,step,rightSide,constantPart,exponentialPart,numberUnderExponent,xValue,string, explanations, explanation,i},
         
	steps = List[];
    explanations = List[];
	AppendTo[steps,equation];
	fullForm = equation // FullForm;

	(* get rid of the logarithm*)
	base = fullForm[[1,1,1,1]];
	body = fullForm[[1,1,1,2]];
	result = fullForm[[1,1,2]];
	rightSide =base^result;
	step = makeString1[body,rightSide];
	AppendTo[steps,step];
    explanation = DisplayForm[RowBox[{base,"^",result,"=",rightSide}]];
    AppendTo[explanations,explanation];

	(* isolate variable x *)
	constantPart = body[[1]];
	exponentialPart = body[[2]];
	rightSide =  rightSide - constantPart;
	step = exponentialPart == rightSide;
	AppendTo[steps,step];
    explanation = DisplayForm[RowBox[{"-(",constantPart,")"}]];
    AppendTo[explanations,explanation];

	(*find the matching exponent*)
	numberUnderExponent = exponentialPart[[1]];
	xValue = Log[numberUnderExponent, rightSide];
	step = exponentialPart == ToString[numberUnderExponent]^ToString[xValue];
	AppendTo[steps,step];
    explanation = DisplayForm[RowBox[{rightSide, " = ",numberUnderExponent,"^",xValue}]];
    AppendTo[explanations,explanation];
	
	string = xValue //InputForm;
	step = "x" == string;
	AppendTo[steps,step];
    AppendTo[explanations, " "];
    AppendTo[explanations, " "];
	Return[List[steps,explanations]]
]


End[] 

EndPackage[]




