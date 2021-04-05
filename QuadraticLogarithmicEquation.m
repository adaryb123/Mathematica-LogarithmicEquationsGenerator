(* ::Package:: *)

(*Mathematica Package*)

(*Mathematica Package*)BeginPackage["QuadraticLogarithmicEquation`"]
(*Exported symbols added here with SymbolName::usage*)

generateQuadraticLogarithmicEquation::usage="
generateQuadraticLogarithmicEquation[]
 - returns List with 2 elemets, equation and X value
";

Begin["`Private`"] (*Begin Private Context*)

generateQuadraticEquation[results_]:=
Module[{a,b,c,i,tValue1,tValue2,part1,part2,equation},
i = 1;
While[i > 0,
	a = RandomInteger[{-10,10}];
	If[a == 0,a++];
	b = RandomInteger[{-20,20}];
	c= RandomInteger[{-30,30}];
	tValue1 = Divide[Plus[Minus[b],Sqrt[Subtract[Power[b,2],Times[4,a,c]]]],Times[2,a]] ;
	tValue2 = Divide[Subtract[Minus[b],Sqrt[Subtract[Power[b,2],Times[4,a,c]]]],Times[2,a]];
	If[MemberQ[results,tValue1],
		equation = Plus[Times[a, Power["t",2]] , Times[b,"t"] ,c] == 0;
		Return[List[equation,tValue1]]
	,];
	If[MemberQ[results,tValue2],
		equation = Plus[Times[a, Power["t",2]] , Times[b,"t"] ,c] == 0;
		Return[List[equation,tValue2]]
	,];
 i++]
]

generateQuadraticLogarithmicEquation[] :=
Module[{returnValues,quadraticEquation,tValue, base,results,equation,xValue},
results = Range[30];
While[True,
returnValues = generateQuadraticEquation[results];
quadraticEquation = Part[returnValues,1];
tValue = Part[returnValues,2];
base = RandomInteger[{2,10}];
equation = quadraticEquation /. "t" -> Log[base,"x"];
xValue = base^tValue;
If[xValue  > 1000 || xValue < -500,Continue[]];
Return[List[equation,xValue]]
]
]
End[] (*End Private Context*)

EndPackage[]





