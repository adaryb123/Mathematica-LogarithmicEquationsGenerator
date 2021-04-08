(* ::Package:: *)

(*Mathematica Package*)

(*Mathematica Package*)BeginPackage["ExponentialLogarithmicEquation`"]
(*Exported symbols added here with SymbolName::usage*)

generateExponentialLogarithmicEquation::usage="
ggenerateExponentialLogarithmicEquation[]
 - returns List with 2 elemets, equation and X value
";

solveExponentialLogarithmicEquation::usage="
solveExponentialLogarithmicEquation[equation_]
 - returns List of step by step solution
";

Begin["`Private`"] (*Begin Private Context*)


maskBody[body_,base_]:=
Module[{exponentialPart,linearPart,exponent,sum,expression,plusChance},
While[True,
exponentialPart =RandomInteger[{2,Min[5,Round[body/2]]}];
If[exponentialPart == base, exponentialPart++];
exponent =2;
While[exponentialPart ^ exponent < body,exponent++];
linearPart = body - exponentialPart^exponent;

plusChance = RandomInteger[{1,2}];
If[plusChance == 2,
exponent--;
linearPart = body - exponentialPart^exponent
];

If [linearPart == 0,Continue[]];
If [linearPart > 500 || linearPart < -500, Continue[]];

expression = Power[exponentialPart,"x"] + linearPart;
Return[List[expression,exponent]]
]
]

makeString[base_,body_, result_]:= Return[HoldForm[Log[base,body] == result]]

makeString1[body_,rightSide_] :=Return[HoldForm[body== rightSide]]

generateExponentialLogarithmicEquation[]:=
Module[{result,base,body,returnValues,expression,results,xValue,equation},

results = Range[5];
While[True,
	base = RandomInteger[{2,10}];
	body = RandomInteger[{base+1,500}];
	result = Log[base,body];
	If[MemberQ[results,result],
		returnValues = maskBody[body,base];
		expression = Part[returnValues,1];
		xValue = Part[returnValues,2];
	equation = makeString[base,expression,result];
		Return[List[equation,xValue]]
     ]
]
]

solveExponentialLogarithmicEquation[equation_]:=
Module[{steps,fullForm,base,body,result,step1,rightSide,constantPart,exponentialPart,step2,numberUnderExponent,step3,xValue,step4,string},
          steps = List[];
	     AppendTo[steps,equation];
	     fullForm = equation // FullForm;
	     base = fullForm[[1,1,1,1]];
	     body = fullForm[[1,1,1,2]];
	     result = fullForm[[1,1,2]];
	rightSide =base^result;
	step1 = makeString1[body,rightSide];
	AppendTo[steps,step1];
	constantPart = body[[1]];
	exponentialPart = body[[2]];
	rightSide =  rightSide - constantPart;
	step2 = exponentialPart == rightSide;
	AppendTo[steps,step2];
	numberUnderExponent = exponentialPart[[1]];
	xValue = Log[numberUnderExponent, rightSide];
	step3 = exponentialPart == ToString[numberUnderExponent]^ToString[xValue];
	AppendTo[steps,step3];
	string = xValue //InputForm;
	step4 = "x" == string;
	AppendTo[steps,step4];
	Return[steps];
]


End[] (*End Private Context*)

EndPackage[]




