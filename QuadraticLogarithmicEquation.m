(* ::Package:: *)

(*Package with functions to generate and solve Quadratic Logarithmic Equations*)

BeginPackage["QuadraticLogarithmicEquation`"]

generateQuadraticLogarithmicEquation::usage="
generateQuadraticLogarithmicEquation[]
 - returns List with 2 elemets, equation and X value
";

solveQuadraticLogarithmicEquation::usage="
solveQuadraticLogarithmicEquation[equation_]
 - returns List of step by step solution
";

Begin["`Private`"] 

(*these function return the equation as string  *)
makeString[base_,body_,a_,b_,c_, result_]:= Return[HoldForm[a*Log[base,body]^2 + b*Log[base,body] + c == result]]
makeString1[base_,body_,result_] :=Return[HoldForm[Log[base,body] == result]]

(*this function generates quadratic equation *)
generateQuadraticEquation[]:=
Module[{a,b,c,i,tValue1,tValue2,part1,part2,equation,acceptedValues,betterResult},

	While[True,
		(*initialize variables randomly*)
		a = RandomInteger[{-10,10}];
		If[a == 0,a++];
		b = RandomInteger[{-20,20}];
		c= RandomInteger[{-30,30}];
		(*try again if they are outside desired interval*)
        If[c ==0 || a == 0 || b == 0 || b == 1 || a == 1, Continue[]];
		
		(*same goes for equation results*)
		tValue1 = Divide[Plus[Minus[b],Sqrt[Subtract[Power[b,2],Times[4,a,c]]]],Times[2,a]] ;
		tValue2 = Divide[Subtract[Minus[b],Sqrt[Subtract[Power[b,2],Times[4,a,c]]]],Times[2,a]];
		acceptedValues = Range[-1,3];
		If[!MemberQ[acceptedValues,tValue1] || !MemberQ[acceptedValues,tValue2], Continue[]];
		
		equation = Plus[Times[a, Power["t",2]] , Times[b,"t"] ,c] == 0;
		If[tValue2 > tValue1  && MemberQ[List[1,2,3],tValue2] && !MemberQ[List[1,2,3],tValue1], 
			betterResult = tValue2,
			betterResult = tValue1];
		Return[List[equation,betterResult,a,b,c]];
]
]

(*this function generates quadratic logarithmic equation *)
generateQuadraticLogarithmicEquation[] :=
Module[{returnValues,quadraticEquation,tValue, base,results,equation,xValue,a,b,c},

	While[True,
		
		(* generate quadratic equation without logarithms*)
		returnValues = generateQuadraticEquation[];
		quadraticEquation = Part[returnValues,1];
		tValue = Part[returnValues,2];
		a = Part[returnValues,3];
		b = Part[returnValues,4];
		c = Part[returnValues,5];
	
		(*initialize variable randomly*)
		base = RandomInteger[{2,10}];

		equation = makeString[base,"x",a,b,c,0];
		xValue = base^tValue;
		(*try again if the result is outside desired interval*)
		If[xValue  > 1000 || xValue < -500,Continue[]];
		
		Return[List[equation,xValue]]
]
]

(* this function checks if the 2 values are equal*)
testTValues[t1_,t2_]:= Module[{steps,string,step},
	
	steps = List[];
	If[t1 != t2,
		string = t1// InputForm;
		step = "t1" == string;
		AppendTo[steps,step];
		string = t2 // InputForm;
		step = "t2" == string;
		AppendTo[steps,step];
		Return[steps]
	];
	If[ t1 == t2,
		string = t1 // InputForm;
		step = "t" == string;
		AppendTo[steps,step];
		Return[steps];
	];
]

(*this function solves the given quadratic equation *)
solveQuadraticEquation[equation_] :=
Module[{a,b,c,gcd,newEquation,newA,newB,newC,gcdStep,discriminant,steps,x1,x2,step,string,result1,result2,,t1,t2,additionalSteps,i,explanations,explanation},
	
	steps = List[];
    explanations = List[];

	(*check if we can divide the equation with greatest common divisor*)
	c = equation[[1,1,3]];
	b = equation[[1,1,2,1]];
	a = equation[[1,1,1,1]];
	gcd = GCD[a,b,c];
	gcdStep = equation;
	If[gcd != 1,
		a = a/gcd;
		b= b/gcd;
		c= c/gcd;
		gcdStep = a* "t"^2 +b *"t" + c ==0;
		AppendTo[steps,gcdStep];
        explanation = DisplayForm[RowBox[{"/(",gcd,")"}]];
        AppendTo[explanations,explanation];
	];

	(*calculate discriminant*)
	discriminant = b^2 - 4*a*c;
	string = discriminant //InputForm;
	step = "D" == string;
	AppendTo[steps,step];
    explanation = DisplayForm[RowBox[{"D = b^2 - 4ac"}]];
    AppendTo[explanations,explanation];
    If[discriminant != 0,
		string = Sqrt[discriminant] //InputForm;
        explanation =  DisplayForm[RowBox[{"t = ",PlusMinus[Minus["b"],Sqrt["D"]],"/ 2a"}]];
        AppendTo[explanations,explanation];
    ];
	If [discriminant == 0,
		explanation =  DisplayForm[RowBox[{"t = -b / 2a"}]];
        AppendTo[explanations,explanation];
        AppendTo[explanations, " "]
    ];
	If[discriminant != 0,
		string = PlusMinus[Minus[b],Sqrt[discriminant]]/(2*a) //InputForm;
		step = "t" == string;
		AppendTo[steps,step]
	];

	(* call testTValues function to check if both tValues are valid*)
	t1= Plus[Minus[b],Sqrt[discriminant]]/(2*a);
	t2 = Subtract[Minus[b],Sqrt[discriminant]]/(2*a);
	additionalSteps = testTValues[t1,t2];
    For [i = 1, i <= Length[additionalSteps],i++,AppendTo[steps,additionalSteps[[i]]]];
    AppendTo[explanations," "];
    AppendTo[explanations," "];
	Return[List[steps,explanations,t1,t2]];
]

(*this function solves the given quadratic logarithmic equation *)
solveQuadraticLogarithmicEquation[equation_] :=
Module[{steps,fullForm,base,string,step,constantCoeff,linearPart,quadraticPart, linearCoeff, quadraticCoeff,substitutionStep,additionalSteps,t1,t2,returnValues,xValue1,xValue2,xValue1String,xValue2String,i,explanations,explanation},
	
	steps = List[];
    explanations = List[];
	AppendTo[steps,equation];

	(*make substitution*)
	fullForm = equation // FullForm;
	constantCoeff = fullForm[[1,1,1,3]];
	linearPart = fullForm[[1,1,1,2]];
	quadraticPart = fullForm[[1,1,1,1]];
	linearCoeff = linearPart[[1]];
	quadraticCoeff = quadraticPart[[1]];
	base = linearPart[[2,1,1]];
    substitutionStep= makeString1[base,"x","t"];
    AppendTo[explanations,substitutionStep];
	
	(*call the solveQuadraticEquation function*)
	step = constantCoeff + linearCoeff * "t" + quadraticCoeff * "t"^2 == 0;
	AppendTo[steps,step];
	returnValues = solveQuadraticEquation[equation];
	For[i = 1, i <= Length[returnValues[[1]]], i++,
		AppendTo[steps,Part[returnValues[[1]],i]];
		AppendTo[explanations,Part[returnValues[[2]],i]]
	];

	(* reverse the substitution*)
	t1 = returnValues[[3]];
	t2 = returnValues[[4]];
    AppendTo[steps,substitutionStep];
    explanation = DisplayForm[RowBox[{"x = ",base,"^t"}]];
    AppendTo[explanations," "];
    AppendTo[explanations,explanation];
	xValue1String =base^ToString[t1] ;
	xValue2String = base^ToString[t2] ;
    xValue1=base^t1 //InputForm ;
	xValue2 = base^t2  // InputForm;

	(*print the results*)
    If[xValue1 == xValue2,
		step = "x" == xValue1String == xValue1;
		AppendTo[steps,step];
        AppendTo[explanations," "];
	Return[List[steps,explanations]];
	];
	step = "x1" == xValue1String == xValue1;
	AppendTo[steps,step];
	string = xValue2;
	step = "x2" == xValue2String  == xValue2;
	AppendTo[steps,step];
	AppendTo[explanations," "];

	AppendTo[explanations," "];
	Return[List[steps,explanations]];
]


End[] 

EndPackage[]





