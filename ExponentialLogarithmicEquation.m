(* ::Package:: *)

(*Mathematica Package*)

(*Mathematica Package*)BeginPackage["ExponentialLogarithmicEquation`"]
(*Exported symbols added here with SymbolName::usage*)

generateExponentialLogarithmicEquation::usage="
ggenerateExponentialLogarithmicEquation[]
 - returns List with 2 elemets, equation and X value
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

generateExponentialLogarithmicEquation[]:=
Module[{result,base,body,returnValues,expression,results,xValue,equation},

results = Range[5];
While[True,
	expression = Null;
	xValue = Null;
	equation = Null;
	base = RandomInteger[{2,10}];
	body = RandomInteger[{base+1,1000}];
	result = Log[base,body];
	If[MemberQ[results,result],
		returnValues = maskBody[body,base];
		expression = Part[returnValues,1];
		xValue = Part[returnValues,2];
		equation = Log[base,expression] == result;
		Return[List[equation,xValue]]
     ]
]
]

End[] (*End Private Context*)

EndPackage[]




