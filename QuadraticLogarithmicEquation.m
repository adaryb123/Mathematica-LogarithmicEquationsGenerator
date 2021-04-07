(* ::Package:: *)

(*Mathematica Package*)

(*Mathematica Package*)BeginPackage["QuadraticLogarithmicEquation`"]
(*Exported symbols added here with SymbolName::usage*)

generateQuadraticLogarithmicEquation::usage="
generateQuadraticLogarithmicEquation[]
 - returns List with 2 elemets, equation and X value
";

solveQuadraticLogarithmicEquation::usage="
solveQuadraticLogarithmicEquation[equation_]
 - returns List of step by step solution
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
  If[c ==0 || a == 0 || b == 0 || b == 1 || a == 1, Continue[]];
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

solveQuadraticEquation[equation_] :=
Module[{a,b,c,gcd,newEquation,newA,newB,newC,gcdStep,discriminant,steps,x1,x2,step1,string,step2,step3,step4,result1,result2,step5},
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
	gcdStep = a* "t"^2 +b *"t" + c ==0;
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
	step3 = "t" == string;
	AppendTo[steps,step3];
	result1 = Plus[Minus[b],Sqrt[discriminant]]/(2*a)//InputForm;
	step4 = "t1" == result1;
	AppendTo[steps,step4];
	result2 = Subtract[Minus[b],Sqrt[discriminant]]/(2*a)//InputForm;
	step5 = "t2" == result2;
	AppendTo[steps,step5];
	Return[List[steps,result1,result2]];
]

solveQuadraticLogarithmicEquation[equation_] :=
Module[{steps,fullForm,base,string,step1,step2,constantCoeff,linearPart,quadraticPart, linearCoeff, quadraticCoeff,substitutionStep,additionalSteps,t1,t2,returnValues,xValue1,xValue2,naturalNumbers,i},
	steps = List[];
	AppendTo[steps,equation];
	fullForm = equation // FullForm;
	constantCoeff = fullForm[[1,1,1]];
	linearPart = fullForm[[1,1,2]];
	quadraticPart = fullForm[[1,1,3]];
	linearCoeff = linearPart[[1]];
	quadraticCoeff = quadraticPart[[1]];
	base = linearPart[[2,1,1]];
         substitutionStep= Log[base,"x"] == "t";
         AppendTo[steps,substitutionStep];
	step1 = constantCoeff + linearCoeff * "t" + quadraticCoeff * "t"^2 == 0;
	AppendTo[steps,step1];
	returnValues = solveQuadraticEquation[equation];
	For[i = 1, i <= Length[returnValues[[1]]], i++,
		AppendTo[steps,Part[returnValues[[1]],i]]
		];
	t1 = returnValues[[2]];
	t2 = returnValues[[3]];
        AppendTo[steps,substitutionStep];
	xValue1 =base^t1;
	xValue2 = base^t2;
	naturalNumbers = Range[0,20];
	step2 = "x1" == xValue1;
	AppendTo[steps,step2];
	string = xValue2;
	step2 = "x2" == xValue2;
	AppendTo[steps,step2];
	Return[steps];
]

End[] (*End Private Context*)

EndPackage[]





