(* ::Package:: *)

BeginPackage["HornH1H5`"]


Print["HornH1H5.wl v1.0\n","Authors : Souvik Bera & Tanay Pathak\n","There are 22 analytic continuations of Horn H1 and 20 analytic continuations of Horn H5"];


(* ::Section:: *)
(*Informations*)


H1expose::usage="Given an integer (<= 22), this command shows the analytic continuation (AC) of Horn H1 labelled by
that integer and its corresponding region of convergence (ROC)
For example : H1expose[p_Integer]
yields {ROC, AC}.
";
H1ROC::usage="The command gives the region of convergence (ROC) of the analytic continuation of Horn H1 along with the given point
H1ROC[{x,y},series_number,{plot range},Option]
The only available option is \!\(\*
StyleBox[\"resolution\",\nFontWeight->\"Bold\"]\). It is a list of two entries. The first one is the number for PlotPoints
and the second one is the number for MaxRecursion.
The default value of \!\(\*
StyleBox[\"resolution\",\nFontWeight->\"Bold\"]\) is : {100,3}.
";

H5expose::usage="Given an integer (<= 20), this command shows the analytic continuation (AC) of Horn H5 labelled by
that integer and its corresponding region of convergence (ROC)
For example : H5expose[p_Integer]
yields {ROC, AC}.
";
H5ROC::usage="The command gives the region of convergence (ROC) of the analytic continuation of Horn H5 along with the given point
H5ROC[{x,y},series_number,{plot range},Option]
The only available option is \!\(\*
StyleBox[\"resolution\",\nFontWeight->\"Bold\"]\). It is a list of two entries. The first one is the number for PlotPoints
and the second one is the number for MaxRecursion.
The default value of \!\(\*
StyleBox[\"resolution\",\nFontWeight->\"Bold\"]\) is : {100,3}.
";


Begin["`Private`"]


ClearAll[Global`x,Global`y,Global`a,Global`b,Global`c,Global`d,Global`m,Global`n];


Off[General::nord];


(* ::Section:: *)
(*H1*)


H1expose[p_]:=Module[{},

Return[If[p>22,Print["Total number of ACs is 22"];Abort[];,
H1series[Global`a,Global`b,Global`c,Global`d,Global`x,Global`y,Global`m,Global`n][[p]]]]];



Options[H1ROC]={resolution->{100,3}};
H1ROC[args___] := h1core[H1ROC,args];

h1core[H1ROC,point___List,list_/;(IntegerQ[list]&&list>0),range_List,OptionsPattern[H1ROC]]:=Module[{i},
roc=H1series[a,b,c,d,x,y,m,n][[list,1]];
Show[ListPlot[{point},PlotStyle->Directive[PointSize[Medium],Red],PlotRange->{{range[[1]],range[[2]]},{range[[1]],range[[2]]}}],RegionPlot[roc,{x,range[[1]]-1,range[[2]]+1},{y,range[[1]]-1,range[[2]]+1},PlotPoints->OptionValue[resolution][[1]],MaxRecursion->OptionValue[resolution][[2]]],PlotRange->{{range[[1]],range[[2]]},{range[[1]],range[[2]]}},AspectRatio-> 1]
]


H1series[a_,b_,c_,d_,x_,y_,m_,n_]={{Abs[x]<1&&Abs[y]<1&&2 Sqrt[Abs[x]] Sqrt[Abs[y]]+Abs[y]<1,(x^m y^n Pochhammer[a,m-n] Pochhammer[b,m+n] Pochhammer[c,n])/(m! n! Pochhammer[d,m])},{Abs[1-x]<1&&2+4 Abs[1-x]<1/Abs[y]+Abs[y]&&Abs[y]<1,((-1)^(1+n) (1-x)^m y^n Gamma[a+b-d] Gamma[d] Gamma[1-a-b+d] Pochhammer[a,m-n] Pochhammer[b,m+n] Pochhammer[c,n] Pochhammer[1+b-d,n])/(m! n! Gamma[1+a+b-d] Gamma[-a+d] Gamma[-b+d] Pochhammer[1+a+b-d,m] Pochhammer[-a+d,n])+((-1)^n (1-x)^(-a-b+d+m) y^n Gamma[a+b-d] Gamma[d] Pochhammer[c,n] Pochhammer[1+b-d,n] Pochhammer[-a+d,m+n] Pochhammer[-b+d,m-n])/(m! n! Gamma[a] Gamma[b] Pochhammer[-a+d,n] Pochhammer[1-a-b+d,m])},{Abs[1+y]<1&&1/Abs[x/(1+y)^2]>4+4 Abs[1+y],-((4^m x^m (1+y)^(1-a-b-c-2 m+n) Gamma[1-a] Gamma[1-a-b-c] Gamma[a+b+c] Pochhammer[1-a-b,-2 m+n] Pochhammer[(a+b)/2,m] Pochhammer[1/2 (1+a+b),m] Pochhammer[1-a-c,-m+n] Pochhammer[a+c,m])/(m! n! Gamma[b] Gamma[2-a-b-c] Gamma[c] Pochhammer[2-a-b-c,-2 m+n] Pochhammer[d,m]))+(4^m x^m (1+y)^n Gamma[1-a] Gamma[1-a-b-c] Pochhammer[b,m+n] Pochhammer[(a+b)/2,m] Pochhammer[1/2 (1+a+b),m] Pochhammer[c,n] Pochhammer[a+c,m])/(m! n! Gamma[1-a-b] Gamma[1-a-c] Pochhammer[a+b+c,2 m+n] Pochhammer[d,m])},{1/4 Abs[(1+y)^2/x]<1&&2 Abs[Sqrt[x/(1+y)^2] (1+y)]<2&&4 Abs[Sqrt[x/(1+y)^2] (1+y)]^2<-2+4/Abs[(1+y)^2/x]+1/4 Abs[(1+y)^2/x],(4^m x^m (1+y)^n Gamma[1-a] Gamma[1-a-b-c] Pochhammer[b,m+n] Pochhammer[(a+b)/2,m] Pochhammer[1/2 (1+a+b),m] Pochhammer[c,n] Pochhammer[a+c,m])/(m! n! Gamma[1-a-b] Gamma[1-a-c] Pochhammer[a+b+c,2 m+n] Pochhammer[d,m])+((-1)^n 2^(1-a-b-c-2 m+n) Sqrt[\[Pi]] (x/(1+y)^2)^(-(a/2)-b/2-c/2+n/2) (1+y)^(-a-b-c+n) (-((1+y)^2/x))^m Gamma[1-a] Gamma[1-a-b-c] Gamma[a+b+c] Gamma[d] Pochhammer[1-a-b,n] Pochhammer[(a+b)/2,-(n/2)] Pochhammer[1/2 (1+a+b),-(n/2)] Pochhammer[(1-c)/2,n/2] Pochhammer[1/2 (2-a+b-c),m+n/2] Pochhammer[1/2 (2-a+b-c),-(n/2)] Pochhammer[-(c/2),n/2] Pochhammer[(1+c)/2,m] Pochhammer[(1+c)/2,-(n/2)] Pochhammer[(2+c)/2,m] Pochhammer[(2+c)/2,-(n/2)] Pochhammer[1/2 (a-b+c),n/2] Pochhammer[1/2 (a+b+c),m-n/2] Pochhammer[1/2 (2+a+b+c-2 d),m-n/2])/(m! n! Gamma[b] Gamma[2-a-b-c] Gamma[c] Gamma[1/2 (-1+a+b+c)] Gamma[1/2 (-a-b-c+2 d)] Pochhammer[3/2,m] Pochhammer[2-a-b-c,n] Pochhammer[1/2 (2-a+b-c),m-n/2] Pochhammer[1/2 (2-a+b-c),n/2] Pochhammer[(1+c)/2,m-n/2] Pochhammer[(2+c)/2,m-n/2] Pochhammer[1/2 (a-b+c),-(n/2)] Pochhammer[1/2 (-1+a+b+c),-(n/2)] Pochhammer[1/2 (a+b+c),-(n/2)] Pochhammer[1/2 (2+a+b+c-2 d),-(n/2)] Pochhammer[1/2 (-a-b-c+2 d),n/2])+((-1)^n 2^(1-a-b-c-2 m+n) Sqrt[\[Pi]] y (x/(1+y)^2)^(-(a/2)-b/2-c/2+n/2) (1+y)^(-a-b-c+n) (-((1+y)^2/x))^m Gamma[1-a] Gamma[1-a-b-c] Gamma[a+b+c] Gamma[d] Pochhammer[1-a-b,n] Pochhammer[(a+b)/2,-(n/2)] Pochhammer[1/2 (1+a+b),-(n/2)] Pochhammer[(1-c)/2,n/2] Pochhammer[1/2 (2-a+b-c),m+n/2] Pochhammer[1/2 (2-a+b-c),-(n/2)] Pochhammer[-(c/2),n/2] Pochhammer[(1+c)/2,m] Pochhammer[(1+c)/2,-(n/2)] Pochhammer[(2+c)/2,m] Pochhammer[(2+c)/2,-(n/2)] Pochhammer[1/2 (a-b+c),n/2] Pochhammer[1/2 (a+b+c),m-n/2] Pochhammer[1/2 (2+a+b+c-2 d),m-n/2])/(m! n! Gamma[b] Gamma[2-a-b-c] Gamma[c] Gamma[1/2 (-1+a+b+c)] Gamma[1/2 (-a-b-c+2 d)] Pochhammer[3/2,m] Pochhammer[2-a-b-c,n] Pochhammer[1/2 (2-a+b-c),m-n/2] Pochhammer[1/2 (2-a+b-c),n/2] Pochhammer[(1+c)/2,m-n/2] Pochhammer[(2+c)/2,m-n/2] Pochhammer[1/2 (a-b+c),-(n/2)] Pochhammer[1/2 (-1+a+b+c),-(n/2)] Pochhammer[1/2 (a+b+c),-(n/2)] Pochhammer[1/2 (2+a+b+c-2 d),-(n/2)] Pochhammer[1/2 (-a-b-c+2 d),n/2])+((-1)^(1+n) 2^(1-a-b-c-2 m+n) Sqrt[\[Pi]] (x/(1+y)^2)^(1/2-a/2-b/2-c/2+n/2) (1+y)^(-a-b-c+n) (-((1+y)^2/x))^m Gamma[1-a] Gamma[1-a-b-c] Gamma[a+b+c] Gamma[d] Pochhammer[1-a-b,n] Pochhammer[(a+b)/2,-(n/2)] Pochhammer[1/2 (1+a+b),-(n/2)] Pochhammer[(1-c)/2,n/2] Pochhammer[1/2 (1-a+b-c),m+n/2] Pochhammer[1/2 (1-a+b-c),-(n/2)] Pochhammer[1-c/2,n/2] Pochhammer[c/2,m] Pochhammer[c/2,-(n/2)] Pochhammer[(1+c)/2,m] Pochhammer[(1+c)/2,-(n/2)] Pochhammer[1/2 (1+a-b+c),n/2] Pochhammer[1/2 (-1+a+b+c),m-n/2] Pochhammer[1/2 (1+a+b+c-2 d),m-n/2])/(m! n! Gamma[b] Gamma[2-a-b-c] Gamma[c] Gamma[1/2 (a+b+c)] Gamma[1/2 (1-a-b-c+2 d)] Pochhammer[1/2,m] Pochhammer[2-a-b-c,n] Pochhammer[1/2 (1-a+b-c),m-n/2] Pochhammer[1/2 (1-a+b-c),n/2] Pochhammer[c/2,m-n/2] Pochhammer[(1+c)/2,m-n/2] Pochhammer[1/2 (1+a-b+c),-(n/2)] Pochhammer[1/2 (-1+a+b+c),-(n/2)] Pochhammer[1/2 (a+b+c),-(n/2)] Pochhammer[1/2 (1+a+b+c-2 d),-(n/2)] Pochhammer[1/2 (1-a-b-c+2 d),n/2])+((-1)^(1+n) 2^(1-a-b-c-2 m+n) Sqrt[\[Pi]] y (x/(1+y)^2)^(1/2-a/2-b/2-c/2+n/2) (1+y)^(-a-b-c+n) (-((1+y)^2/x))^m Gamma[1-a] Gamma[1-a-b-c] Gamma[a+b+c] Gamma[d] Pochhammer[1-a-b,n] Pochhammer[(a+b)/2,-(n/2)] Pochhammer[1/2 (1+a+b),-(n/2)] Pochhammer[(1-c)/2,n/2] Pochhammer[1/2 (1-a+b-c),m+n/2] Pochhammer[1/2 (1-a+b-c),-(n/2)] Pochhammer[1-c/2,n/2] Pochhammer[c/2,m] Pochhammer[c/2,-(n/2)] Pochhammer[(1+c)/2,m] Pochhammer[(1+c)/2,-(n/2)] Pochhammer[1/2 (1+a-b+c),n/2] Pochhammer[1/2 (-1+a+b+c),m-n/2] Pochhammer[1/2 (1+a+b+c-2 d),m-n/2])/(m! n! Gamma[b] Gamma[2-a-b-c] Gamma[c] Gamma[1/2 (a+b+c)] Gamma[1/2 (1-a-b-c+2 d)] Pochhammer[1/2,m] Pochhammer[2-a-b-c,n] Pochhammer[1/2 (1-a+b-c),m-n/2] Pochhammer[1/2 (1-a+b-c),n/2] Pochhammer[c/2,m-n/2] Pochhammer[(1+c)/2,m-n/2] Pochhammer[1/2 (1+a-b+c),-(n/2)] Pochhammer[1/2 (-1+a+b+c),-(n/2)] Pochhammer[1/2 (a+b+c),-(n/2)] Pochhammer[1/2 (1+a+b+c-2 d),-(n/2)] Pochhammer[1/2 (1-a-b-c+2 d),n/2])},{Abs[x]<x Conjugate[x]&&Sqrt[Abs[x y]]<-Abs[x]+Sqrt[1+1/Abs[x]] Abs[x]&&4 Abs[x y]<1&&Sqrt[Abs[x]]<Abs[x]&&Sqrt[Abs[y/x]]<2&&(1+Sqrt[1-1/Abs[x]]>Sqrt[Abs[y/x]]||Sqrt[-1+Abs[x]]/Sqrt[Abs[x]]+Sqrt[Abs[y]]/Sqrt[Abs[x]]<1||(Sqrt[-1+Abs[x]]/Sqrt[Abs[x]]+Sqrt[Abs[y]]/Sqrt[Abs[x]]>1&&Sqrt[Abs[y/x]]<=1)),((-1)^n (-x)^(-a+n) x^-m y^n Gamma[a-b] Gamma[1-a+b] Gamma[1+a-d] Gamma[d] Pochhammer[a,m-n] Pochhammer[c,n] Pochhammer[1+a-d,m-n])/(m! n! Gamma[1+a-b] Gamma[b] Gamma[a-d] Gamma[1-a+d] Pochhammer[1+a-b,m-2 n])+((-1)^(1+n) (-x)^(-b-n) x^-m y^n Gamma[a-b] Gamma[1+b-d] Gamma[d] Pochhammer[b,m+n] Pochhammer[c,n] Pochhammer[1+b-d,m+n])/(m! n! Gamma[a] Gamma[b-d] Gamma[1-b+d] Pochhammer[1-a+b,m+2 n])},{1/Sqrt[Abs[1-x]]<1&&Sqrt[Abs[y/(-1+x)]]<2&&Sqrt[Abs[(-1+x) y]]<(-1+Sqrt[1+1/Abs[1-x]]) Abs[1-x]&&4 Abs[(-1+x) y]<1&&((Sqrt[-1+Abs[1-x]]+Sqrt[Abs[y]])/Sqrt[Abs[-1+x]]<1||1+Sqrt[1-1/Abs[1-x]]>Sqrt[Abs[y/(-1+x)]]||((Sqrt[-1+Abs[1-x]]+Sqrt[Abs[y]])/Sqrt[Abs[-1+x]]>1&&Sqrt[Abs[y/(-1+x)]]<=1)),-(((1-x)^(-a-b-m) (-1+x)^(-b-d-n) y^n Gamma[a-b] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] ((\[Pi] (1-x)^d (-1+x)^(a+b))/(Gamma[1-b] Gamma[b])+(\[Pi] (1-x)^(a+b) (-1+x)^d)/(Gamma[a-d] Gamma[1-a+d])) Gamma[1-a-b+d] Pochhammer[b,m+n] Pochhammer[c,n] Pochhammer[1+b-d,n] Pochhammer[-a+d,m+n])/(\[Pi] m! n! Gamma[a] Gamma[b-d] Gamma[1-b+d] Pochhammer[1-a+b,m+2 n] Pochhammer[-a+d,n]))+((1-x)^(-a-b-m) (-1+x)^(-a-d+n) y^n Gamma[a-b] Gamma[1-a+b] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] ((1-x)^(a+b) (-1+x)^d Gamma[1-a] Gamma[a]+(1-x)^d (-1+x)^(a+b) Gamma[b-d] Gamma[1-b+d]) Gamma[1-a-b+d] Pochhammer[a,m-n] Pochhammer[c,n] Pochhammer[1+b-d,n] Pochhammer[-b+d,m-n])/(m! n! Gamma[1-a] Gamma[a] Gamma[1+a-b] Gamma[b] Gamma[b-d]^2 Gamma[-a+d] Gamma[1-b+d]^2 Pochhammer[1+a-b,m-2 n] Pochhammer[-a+d,n])},{Abs[x]<1&&1/Abs[y]+(2 Sqrt[Abs[x y]])/Abs[y]<1&&2+4 Abs[x]<1/Abs[y]+Abs[y]&&Abs[y]<y Conjugate[y],-((x^m (-y)^-n y^(-b-m) Gamma[1-a] Gamma[b-c] Gamma[1-b+c] Pochhammer[b,m+n] Pochhammer[a+b,2 m+n])/(m! n! Gamma[1-a-b] Gamma[1+b-c] Gamma[c] Pochhammer[1+b-c,m+n] Pochhammer[d,m]))+((-1)^m x^m (-y)^-n y^-c Gamma[1-a] Gamma[b-c] Pochhammer[c,n] Pochhammer[a+c,m+n])/(m! n! Gamma[b] Gamma[1-a-c] Pochhammer[1-b+c,-m+n] Pochhammer[d,m])},{Abs[x]<1&&Abs[1+y]>1&&Abs[(1+y)/x]>4+4/Abs[1+y],-((x^m (-1-y)^(a+c+m) (1+y)^(-a-b-c-2 m-n) Gamma[1-a] Gamma[1-a-b-c] Gamma[b-c] Gamma[1-b+c] Gamma[a+b+c] Pochhammer[b,m+n] Pochhammer[a+b,2 m] Pochhammer[1-a-c,-m+n] Pochhammer[a+c,m])/(m! n! Gamma[1-b] Gamma[1-a-b] Gamma[b] Gamma[1+b-c] Gamma[c] Pochhammer[1+b-c,m+n] Pochhammer[d,m]))-(x^m (-1-y)^(-b-m) (1+y)^-n Gamma[1-a] Gamma[1-a-b-c] Gamma[b-c] Gamma[1-b+c] Gamma[a+b+c] Pochhammer[b,m+n] Pochhammer[a+b,2 m] Pochhammer[1-a-c,-m+n] Pochhammer[a+c,m])/(m! n! Gamma[1-a-b] Gamma[1-a-c] Gamma[1+b-c] Gamma[c] Gamma[a+c] Pochhammer[1+b-c,m+n] Pochhammer[d,m])+((-1)^m x^m (-1-y)^-c (1+y)^-n Gamma[1-a] Gamma[1-a-b-c] Gamma[b-c] Gamma[a+b+c] Pochhammer[1-a-b,-2 m+n] Pochhammer[a+b,2 m] Pochhammer[c,n] Pochhammer[a+c,m])/(m! n! Gamma[1-a-b] Gamma[b] Gamma[a+b] Gamma[1-a-c] Pochhammer[1-b+c,-m+n] Pochhammer[d,m])+((-1)^m x^m (-1-y)^(a+b+2 m) (1+y)^(-a-b-c-2 m-n) Gamma[1-a] Gamma[1-a-b-c] Gamma[b-c] Gamma[a+b+c] Pochhammer[1-a-b,-2 m+n] Pochhammer[a+b,2 m] Pochhammer[c,n] Pochhammer[a+c,m])/(m! n! Gamma[b] Gamma[1-c] Gamma[1-a-c] Gamma[c] Pochhammer[1-b+c,-m+n] Pochhammer[d,m])},{Abs[-1+x]<1&&(1+2 Sqrt[Abs[(-1+x) y]])/Abs[y]<1&&2+4 Abs[-1+x]<1/Abs[y]+Abs[y]&&Abs[y]<y Conjugate[y],((-1)^m (1-x)^m (-y)^(-b+d) y^(-1-n) Gamma[1-a] Gamma[a+b-d] Gamma[-1+d] Gamma[d] Gamma[1-a-b+d] Gamma[-1-b+c+d] Pochhammer[2+a+b-2 d,n] Pochhammer[1+b-d,n] Pochhammer[1+a+b-d,m+n])/(m! n! Gamma[b] Gamma[c] Gamma[1+a+b-d] Gamma[-b+d] Gamma[-a-b+d] Gamma[-1-a-b+2 d] Pochhammer[2-d,-m+n] Pochhammer[1+a+b-d,m] Pochhammer[2+b-c-d,n])-((1-x)^m (-y)^(-b-m) y^-n Gamma[1-a] Gamma[-b+c] Gamma[1-d] Gamma[a+b-d] Gamma[d] Gamma[1-a-b+d] Pochhammer[b,m+n] Pochhammer[a+b,2 m+n] Pochhammer[1+a+b-d,m+n])/(m! n! Gamma[1-a-b] Gamma[c] Gamma[1+b-d] Gamma[1+a+b-d] Gamma[-b+d] Gamma[-a-b+d] Pochhammer[1+b-c,m+n] Pochhammer[1+a+b-d,m] Pochhammer[d,m+n])+((-1)^(1+m) (1-x)^m (-y)^-c y^-n Gamma[1-a] Gamma[b-c] Gamma[a+b-d] Gamma[1+b-c-d] Gamma[d] Gamma[1-a-b+d] Pochhammer[c,n] Pochhammer[a+c,m+n] Pochhammer[1+a+c-d,n])/(m! n! Gamma[b] Gamma[1-a-c] Gamma[1+b-d] Gamma[1+a+b-d] Gamma[-b+d] Gamma[-a-c+d] Pochhammer[1-b+c,-m+n] Pochhammer[1+a+b-d,m] Pochhammer[-b+c+d,n])+((-1)^m (1-x)^(-a-b+d+m) (-y)^-c y^-n Gamma[a+b-d] Gamma[d] Pochhammer[c,n] Pochhammer[1+a+c-d,n] Pochhammer[-b+c+d,m+n])/(m! n! Gamma[a] Gamma[b] Pochhammer[1+a+c-d,-m+n] Pochhammer[1-a-b+d,m] Pochhammer[-b+c+d,n])},{Sqrt[Abs[x]]<Abs[x]&&Sqrt[Abs[1/(x y)]]<2&&(1+Sqrt[1-1/Abs[x]]>Sqrt[Abs[1/(x y)]]||Sqrt[1-1/Abs[x]]+Sqrt[Abs[1/(x y)]]<1||(Sqrt[1-1/Abs[x]]+Sqrt[Abs[1/(x y)]]>1&&Sqrt[Abs[1/(x y)]]<=1))&&Abs[y/Sqrt[-x y]]<2&&Sqrt[Abs[1/(x y)]]+Abs[y/Sqrt[-x y]]<2&&Abs[1/(x y)]<4,((-1)^(1+n) (-x)^(-b-n) x^-m y^n Gamma[a-b] Gamma[1+b-d] Gamma[d] Pochhammer[b,m+n] Pochhammer[c,n] Pochhammer[1+b-d,m+n])/(m! n! Gamma[a] Gamma[b-d] Gamma[1-b+d] Pochhammer[1-a+b,m+2 n])+(2^(-2 c-2 n) (-x)^-a x^-m (1/(x y))^n (-x y)^-c Gamma[1-a] Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (-a+b-2 c)] Gamma[1/2 (1-a+b-2 c)] Gamma[1+a-d] Gamma[d] Gamma[-a+d] Pochhammer[1/2 (-a+b-2 c),-(m/2)] Pochhammer[1/2 (1-a+b-2 c),-(m/2)] Pochhammer[c,n] Pochhammer[a+c,m+n] Pochhammer[1/2 (1+a-b+2 c),m/2] Pochhammer[1/2 (2+a-b+2 c),m/2] Pochhammer[1+a+c-d,m+n])/(m! n! Gamma[1+a-b] Gamma[b] Gamma[1/2 (-a+b)] Gamma[1/2 (1-a+b)] Gamma[1-a-c] Gamma[a-d] Gamma[1-a+d] Gamma[-a-c+d] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[1/2 (1+a-b+2 c),m/2+n] Pochhammer[1/2 (2+a-b+2 c),m/2+n])+(2^(a-b+m-2 n) Sqrt[\[Pi]] (-x)^-a x^-m (1/(x y))^n (-x y)^(a/2-b/2+m/2) Gamma[1-a] Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (a-b+2 c)] Gamma[1+a-d] Gamma[d] Gamma[-a+d] Pochhammer[1/2 (-a+b),-(m/2)+n] Pochhammer[(a+b)/2,m/2+n] Pochhammer[1/2 (2-a+b-2 c),-(m/2)] Pochhammer[1/2 (a-b+2 c),m/2] Pochhammer[1/2 (2+a+b-2 d),m/2+n])/(m! n! Gamma[1/2 (2-a-b)] Gamma[1+a-b] Gamma[b] Gamma[1/2 (1-a+b)] Gamma[c] Gamma[a-d] Gamma[1-a+d] Gamma[-(a/2)-b/2+d] Pochhammer[1/2,n] Pochhammer[1/2 (2-a-b),-(m/2)] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[(a+b)/2,m/2] Pochhammer[1/2 (2-a+b-2 c),-(m/2)+n] Pochhammer[1/2 (2+a+b-2 d),m/2] Pochhammer[-(a/2)-b/2+d,-(m/2)])+(2^(a-b+m-2 n) Sqrt[\[Pi]] (-x)^-a x^(-1-m) (1/(x y))^n (-x y)^(1/2+a/2-b/2+m/2) Gamma[1-a] Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (-1+a-b+2 c)] Gamma[1+a-d] Gamma[d] Gamma[-a+d] Pochhammer[1/2 (1-a+b),-(m/2)+n] Pochhammer[1/2 (1+a+b),m/2+n] Pochhammer[1/2 (3-a+b-2 c),-(m/2)] Pochhammer[1/2 (-1+a-b+2 c),m/2] Pochhammer[1/2 (3+a+b-2 d),m/2+n])/(y m! n! Gamma[1/2 (1-a-b)] Gamma[1+a-b] Gamma[b] Gamma[1/2 (-a+b)] Gamma[c] Gamma[a-d] Gamma[1-a+d] Gamma[1/2 (-1-a-b+2 d)] Pochhammer[3/2,n] Pochhammer[1/2 (1-a-b),-(m/2)] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[1/2 (1+a+b),m/2] Pochhammer[1/2 (3-a+b-2 c),-(m/2)+n] Pochhammer[1/2 (3+a+b-2 d),m/2] Pochhammer[1/2 (-1-a-b+2 d),-(m/2)])},{1/Abs[x]<1&&Sqrt[Abs[x/y]]<-Abs[x]+Sqrt[1+1/Abs[x]] Abs[x]&&Abs[x/y]<1/4&&1/Sqrt[Abs[x]]<1&&Sqrt[Abs[1/(x y)]]<2&&(1+Sqrt[1-1/Abs[x]]>Sqrt[Abs[1/(x y)]]||Sqrt[1-1/Abs[x]]+Sqrt[Abs[1/(x y)]]<1||(Sqrt[1-1/Abs[x]]+Sqrt[Abs[1/(x y)]]>1&&Sqrt[Abs[1/(x y)]]<=1)),((-x)^(-b-c-n+2 (c+n)) x^-m (-y)^-n y^-c Gamma[1-a] Gamma[b-c] Gamma[a-b+2 c] Gamma[d] Pochhammer[b-c,m-n] Pochhammer[c,n] Pochhammer[1+b-c-d,m-n])/(m! n! Gamma[b] Gamma[1-a-c] Gamma[a+c] Gamma[-b+c+d] Pochhammer[1-a+b-2 c,m-2 n])+((-x)^(-a-c-n) x^-m (-y)^-n y^-c Gamma[1-a] Gamma[-a+b-2 c] Gamma[d] Pochhammer[c,n] Pochhammer[a+c,m+n] Pochhammer[1+a+c-d,m+n])/(m! n! Gamma[b] Gamma[1-a-c] Gamma[-a-c+d] Pochhammer[1+a-b+2 c,m+2 n])-(x^m (-y)^-n y^(-b-m) Gamma[1-a] Gamma[b-c] Gamma[1-b+c] Pochhammer[b,m+n] Pochhammer[a+b,2 m+n])/(m! n! Gamma[1-a-b] Gamma[1+b-c] Gamma[c] Pochhammer[1+b-c,m+n] Pochhammer[d,m])},{2 Abs[Sqrt[y-x y]/(-1+x)]<4&&1/2 Sqrt[Abs[1/((-1+x) y)]]<1-1/2 Abs[Sqrt[y-x y]/(-1+x)]&&1/4 Abs[1/((-1+x) y)]<1&&1/Sqrt[Abs[1-x]]<1&&1/2 Sqrt[Abs[1/((-1+x) y)]]<1&&(1+Sqrt[1-1/Abs[1-x]]>Sqrt[Abs[1/((-1+x) y)]]||Sqrt[1-1/Abs[1-x]]+Sqrt[Abs[1/((-1+x) y)]]<1||(Sqrt[1-1/Abs[1-x]]+Sqrt[Abs[1/((-1+x) y)]]>1&&Sqrt[Abs[1/((-1+x) y)]]<=1)),-(((1-x)^(-a-b-m) (-1+x)^(-b-d-n) y^n Gamma[a-b] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] ((\[Pi] (1-x)^d (-1+x)^(a+b))/(Gamma[1-b] Gamma[b])+(\[Pi] (1-x)^(a+b) (-1+x)^d)/(Gamma[a-d] Gamma[1-a+d])) Gamma[1-a-b+d] Pochhammer[b,m+n] Pochhammer[c,n] Pochhammer[1+b-d,n] Pochhammer[-a+d,m+n])/(\[Pi] m! n! Gamma[a] Gamma[b-d] Gamma[1-b+d] Pochhammer[1-a+b,m+2 n] Pochhammer[-a+d,n]))-(2^(a-b+m-2 n) Sqrt[\[Pi]] (1-x)^-m (-1+x)^(-1-a) (1/((-1+x) y))^n (y-x y)^(a/2-b/2+m/2) Gamma[1-a] Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (a-b+2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b),-(m/2)+n] Pochhammer[(a+b)/2,m/2+n] Pochhammer[1/2 (2-a+b-2 c),-(m/2)] Pochhammer[1/2 (a-b+2 c),m/2] Pochhammer[1/2 (2+a+b-2 d),m/2] Pochhammer[1/2 (2+a+b-2 d),-(m/2)+n] Pochhammer[-(a/2)-b/2+d,-(m/2)] Pochhammer[-(a/2)-b/2+d,m/2+n])/(m! n! Gamma[1/2 (2-a-b)] Gamma[1+a-b] Gamma[b] Gamma[1/2 (1-a+b)] Gamma[c] Gamma[b-d]^2 Gamma[1-b+d]^2 Gamma[-(a/2)-b/2+d] Pochhammer[1/2,n] Pochhammer[1/2 (2-a-b),-(m/2)] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[(a+b)/2,m/2] Pochhammer[1/2 (2-a+b-2 c),-(m/2)+n] Pochhammer[1/2 (2+a+b-2 d),-(m/2)]^2 Pochhammer[-(a/2)-b/2+d,m/2]^2 Pochhammer[-(a/2)-b/2+d,-(m/2)+n])+(2^(a-b+m-2 n) Sqrt[\[Pi]] (1-x)^-m (-1+x)^(-1-a) x (1/((-1+x) y))^n (y-x y)^(a/2-b/2+m/2) Gamma[1-a] Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (a-b+2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b),-(m/2)+n] Pochhammer[(a+b)/2,m/2+n] Pochhammer[1/2 (2-a+b-2 c),-(m/2)] Pochhammer[1/2 (a-b+2 c),m/2] Pochhammer[1/2 (2+a+b-2 d),m/2] Pochhammer[1/2 (2+a+b-2 d),-(m/2)+n] Pochhammer[-(a/2)-b/2+d,-(m/2)] Pochhammer[-(a/2)-b/2+d,m/2+n])/(m! n! Gamma[1/2 (2-a-b)] Gamma[1+a-b] Gamma[b] Gamma[1/2 (1-a+b)] Gamma[c] Gamma[b-d]^2 Gamma[1-b+d]^2 Gamma[-(a/2)-b/2+d] Pochhammer[1/2,n] Pochhammer[1/2 (2-a-b),-(m/2)] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[(a+b)/2,m/2] Pochhammer[1/2 (2-a+b-2 c),-(m/2)+n] Pochhammer[1/2 (2+a+b-2 d),-(m/2)]^2 Pochhammer[-(a/2)-b/2+d,m/2]^2 Pochhammer[-(a/2)-b/2+d,-(m/2)+n])-(2^(a-b+m-2 n) Sqrt[\[Pi]] (1-x)^(-a-b+d-m) (-1+x)^(-1+b-d) (1/((-1+x) y))^n (y-x y)^(a/2-b/2+m/2) Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (a-b+2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b),-(m/2)+n] Pochhammer[(a+b)/2,m/2+n] Pochhammer[1/2 (2-a+b-2 c),-(m/2)] Pochhammer[1/2 (a-b+2 c),m/2] Pochhammer[1/2 (2+a+b-2 d),m/2] Pochhammer[1/2 (2+a+b-2 d),-(m/2)+n] Pochhammer[-(a/2)-b/2+d,-(m/2)] Pochhammer[-(a/2)-b/2+d,m/2+n])/(m! n! Gamma[a] Gamma[1/2 (2-a-b)] Gamma[1+a-b] Gamma[b] Gamma[1/2 (1-a+b)] Gamma[c] Gamma[b-d] Gamma[1-b+d] Gamma[-(a/2)-b/2+d] Pochhammer[1/2,n] Pochhammer[1/2 (2-a-b),-(m/2)] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[(a+b)/2,m/2] Pochhammer[1/2 (2-a+b-2 c),-(m/2)+n] Pochhammer[1/2 (2+a+b-2 d),-(m/2)]^2 Pochhammer[-(a/2)-b/2+d,m/2]^2 Pochhammer[-(a/2)-b/2+d,-(m/2)+n])+(2^(a-b+m-2 n) Sqrt[\[Pi]] (1-x)^(-a-b+d-m) (-1+x)^(-1+b-d) x (1/((-1+x) y))^n (y-x y)^(a/2-b/2+m/2) Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (a-b+2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b),-(m/2)+n] Pochhammer[(a+b)/2,m/2+n] Pochhammer[1/2 (2-a+b-2 c),-(m/2)] Pochhammer[1/2 (a-b+2 c),m/2] Pochhammer[1/2 (2+a+b-2 d),m/2] Pochhammer[1/2 (2+a+b-2 d),-(m/2)+n] Pochhammer[-(a/2)-b/2+d,-(m/2)] Pochhammer[-(a/2)-b/2+d,m/2+n])/(m! n! Gamma[a] Gamma[1/2 (2-a-b)] Gamma[1+a-b] Gamma[b] Gamma[1/2 (1-a+b)] Gamma[c] Gamma[b-d] Gamma[1-b+d] Gamma[-(a/2)-b/2+d] Pochhammer[1/2,n] Pochhammer[1/2 (2-a-b),-(m/2)] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[(a+b)/2,m/2] Pochhammer[1/2 (2-a+b-2 c),-(m/2)+n] Pochhammer[1/2 (2+a+b-2 d),-(m/2)]^2 Pochhammer[-(a/2)-b/2+d,m/2]^2 Pochhammer[-(a/2)-b/2+d,-(m/2)+n])-(2^(-2 c-2 n) (1-x)^-m (-1+x)^(-1-a) (1/((-1+x) y))^n (y-x y)^-c Gamma[1-a] Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (-a+b-2 c)] Gamma[1/2 (1-a+b-2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b-2 c),-(m/2)] Pochhammer[1/2 (1-a+b-2 c),-(m/2)] Pochhammer[c,n] Pochhammer[a+c,m+n] Pochhammer[1/2 (1+a-b+2 c),m/2] Pochhammer[1/2 (2+a-b+2 c),m/2] Pochhammer[1+a+c-d,n] Pochhammer[-b+c+d,m+n])/(m! n! Gamma[1+a-b] Gamma[b] Gamma[1/2 (-a+b)] Gamma[1/2 (1-a+b)] Gamma[1-a-c] Gamma[b-d]^2 Gamma[1-b+d]^2 Gamma[-a-c+d] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[1/2 (1+a-b+2 c),m/2+n] Pochhammer[1/2 (2+a-b+2 c),m/2+n] Pochhammer[-b+c+d,n])+(2^(-2 c-2 n) (1-x)^-m (-1+x)^(-1-a) x (1/((-1+x) y))^n (y-x y)^-c Gamma[1-a] Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (-a+b-2 c)] Gamma[1/2 (1-a+b-2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b-2 c),-(m/2)] Pochhammer[1/2 (1-a+b-2 c),-(m/2)] Pochhammer[c,n] Pochhammer[a+c,m+n] Pochhammer[1/2 (1+a-b+2 c),m/2] Pochhammer[1/2 (2+a-b+2 c),m/2] Pochhammer[1+a+c-d,n] Pochhammer[-b+c+d,m+n])/(m! n! Gamma[1+a-b] Gamma[b] Gamma[1/2 (-a+b)] Gamma[1/2 (1-a+b)] Gamma[1-a-c] Gamma[b-d]^2 Gamma[1-b+d]^2 Gamma[-a-c+d] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[1/2 (1+a-b+2 c),m/2+n] Pochhammer[1/2 (2+a-b+2 c),m/2+n] Pochhammer[-b+c+d,n])-(2^(-2 c-2 n) (1-x)^(-a-b+d-m) (-1+x)^(-1+b-d) (1/((-1+x) y))^n (y-x y)^-c Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (-a+b-2 c)] Gamma[1/2 (1-a+b-2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b-2 c),-(m/2)] Pochhammer[1/2 (1-a+b-2 c),-(m/2)] Pochhammer[c,n] Pochhammer[a+c,m+n] Pochhammer[1/2 (1+a-b+2 c),m/2] Pochhammer[1/2 (2+a-b+2 c),m/2] Pochhammer[1+a+c-d,n] Pochhammer[-b+c+d,m+n])/(m! n! Gamma[a] Gamma[1+a-b] Gamma[b] Gamma[1/2 (-a+b)] Gamma[1/2 (1-a+b)] Gamma[1-a-c] Gamma[b-d] Gamma[1-b+d] Gamma[-a-c+d] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[1/2 (1+a-b+2 c),m/2+n] Pochhammer[1/2 (2+a-b+2 c),m/2+n] Pochhammer[-b+c+d,n])+(2^(-2 c-2 n) (1-x)^(-a-b+d-m) (-1+x)^(-1+b-d) x (1/((-1+x) y))^n (y-x y)^-c Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (-a+b-2 c)] Gamma[1/2 (1-a+b-2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b-2 c),-(m/2)] Pochhammer[1/2 (1-a+b-2 c),-(m/2)] Pochhammer[c,n] Pochhammer[a+c,m+n] Pochhammer[1/2 (1+a-b+2 c),m/2] Pochhammer[1/2 (2+a-b+2 c),m/2] Pochhammer[1+a+c-d,n] Pochhammer[-b+c+d,m+n])/(m! n! Gamma[a] Gamma[1+a-b] Gamma[b] Gamma[1/2 (-a+b)] Gamma[1/2 (1-a+b)] Gamma[1-a-c] Gamma[b-d] Gamma[1-b+d] Gamma[-a-c+d] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[1/2 (1+a-b+2 c),m/2+n] Pochhammer[1/2 (2+a-b+2 c),m/2+n] Pochhammer[-b+c+d,n])+(2^(a-b+m-2 n) Sqrt[\[Pi]] (1-x)^-m (-1+x)^(-1-a) (1/((-1+x) y))^n (y-x y)^(1/2+a/2-b/2+m/2) Gamma[1-a] Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (-1+a-b+2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (1-a+b),-(m/2)+n] Pochhammer[1/2 (1+a+b),m/2+n] Pochhammer[1/2 (3-a+b-2 c),-(m/2)] Pochhammer[1/2 (-1+a-b+2 c),m/2] Pochhammer[1/2 (1+a+b-2 d),m/2] Pochhammer[1/2 (3+a+b-2 d),-(m/2)+n] Pochhammer[1/2 (1-a-b+2 d),-(m/2)] Pochhammer[1/2 (1-a-b+2 d),m/2+n])/(y m! n! Gamma[1/2 (1-a-b)] Gamma[1+a-b] Gamma[b] Gamma[1/2 (-a+b)] Gamma[c] Gamma[b-d]^2 Gamma[1-b+d]^2 Gamma[1/2 (-1-a-b+2 d)] Pochhammer[3/2,n] Pochhammer[1/2 (1-a-b),-(m/2)] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[1/2 (1+a+b),m/2] Pochhammer[1/2 (3-a+b-2 c),-(m/2)+n] Pochhammer[1/2 (1+a+b-2 d),-(m/2)] Pochhammer[1/2 (3+a+b-2 d),-(m/2)] Pochhammer[1/2 (-1-a-b+2 d),m/2] Pochhammer[1/2 (1-a-b+2 d),m/2] Pochhammer[1/2 (1-a-b+2 d),-(m/2)+n])+(2^(a-b+m-2 n) Sqrt[\[Pi]] (1-x)^(-a-b+d-m) (-1+x)^(-1+b-d) (1/((-1+x) y))^n (y-x y)^(1/2+a/2-b/2+m/2) Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (-1+a-b+2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (1-a+b),-(m/2)+n] Pochhammer[1/2 (1+a+b),m/2+n] Pochhammer[1/2 (3-a+b-2 c),-(m/2)] Pochhammer[1/2 (-1+a-b+2 c),m/2] Pochhammer[1/2 (1+a+b-2 d),m/2] Pochhammer[1/2 (3+a+b-2 d),-(m/2)+n] Pochhammer[1/2 (1-a-b+2 d),-(m/2)] Pochhammer[1/2 (1-a-b+2 d),m/2+n])/(y m! n! Gamma[a] Gamma[1/2 (1-a-b)] Gamma[1+a-b] Gamma[b] Gamma[1/2 (-a+b)] Gamma[c] Gamma[b-d] Gamma[1-b+d] Gamma[1/2 (-1-a-b+2 d)] Pochhammer[3/2,n] Pochhammer[1/2 (1-a-b),-(m/2)] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[1/2 (1+a+b),m/2] Pochhammer[1/2 (3-a+b-2 c),-(m/2)+n] Pochhammer[1/2 (1+a+b-2 d),-(m/2)] Pochhammer[1/2 (3+a+b-2 d),-(m/2)] Pochhammer[1/2 (-1-a-b+2 d),m/2] Pochhammer[1/2 (1-a-b+2 d),m/2] Pochhammer[1/2 (1-a-b+2 d),-(m/2)+n])},{1/Abs[x]<1&&Abs[x/(1+y)]<1/4&&Abs[x/(1+y)]<-(Abs[x]/2)+1/2 Sqrt[1+1/Abs[x]] Abs[x]&&1/Sqrt[Abs[x]]<1&&1/Abs[1+y]<1&&(1+Sqrt[1-1/Abs[x]]>2/Abs[1+y]||Sqrt[1-1/Abs[x]]+2/Abs[1+y]<1||(2/Abs[1+y]<=1&&Sqrt[1-1/Abs[x]]+2/Abs[1+y]>1)),((-1)^n (-x)^(-b+c+n) x^-m (-1-y)^-c (1+y)^-n Gamma[1-a] Gamma[1-a-b-c] Gamma[b-c] Gamma[a+b+c] Gamma[a-b+2 c] Gamma[d] Pochhammer[1-a-b,n] Pochhammer[(a+b)/2,-(n/2)] Pochhammer[1/2 (1+a+b),-(n/2)] Pochhammer[1/2 (1-a+b-2 c),m-n/2] Pochhammer[1/2 (2-a+b-2 c),m-n/2] Pochhammer[b-c,m-n] Pochhammer[c,n] Pochhammer[1+b-c-d,m-n])/(m! n! Gamma[1-a-b] Gamma[b] Gamma[a+b] Gamma[1-a-c] Gamma[a+c] Gamma[-b+c+d] Pochhammer[1/2 (1-a+b-2 c),m-n] Pochhammer[1/2 (1-a+b-2 c),-(n/2)] Pochhammer[1-a+b-2 c,m-n] Pochhammer[1/2 (2-a+b-2 c),m-n] Pochhammer[1/2 (2-a+b-2 c),-(n/2)] Pochhammer[1/2 (a-b+2 c),n/2] Pochhammer[1/2 (1+a-b+2 c),n/2])+((-1)^n (-x)^(-b+c+n) x^-m (-1-y)^(a+b) (1+y)^(-a-b-c-n) Gamma[1-a] Gamma[1-a-b-c] Gamma[b-c] Gamma[a+b+c] Gamma[a-b+2 c] Gamma[d] Pochhammer[1-a-b,n] Pochhammer[(a+b)/2,-(n/2)] Pochhammer[1/2 (1+a+b),-(n/2)] Pochhammer[1/2 (1-a+b-2 c),m-n/2] Pochhammer[1/2 (2-a+b-2 c),m-n/2] Pochhammer[b-c,m-n] Pochhammer[c,n] Pochhammer[1+b-c-d,m-n])/(m! n! Gamma[b] Gamma[1-c] Gamma[1-a-c] Gamma[c] Gamma[a+c] Gamma[-b+c+d] Pochhammer[1/2 (1-a+b-2 c),m-n] Pochhammer[1/2 (1-a+b-2 c),-(n/2)] Pochhammer[1-a+b-2 c,m-n] Pochhammer[1/2 (2-a+b-2 c),m-n] Pochhammer[1/2 (2-a+b-2 c),-(n/2)] Pochhammer[1/2 (a-b+2 c),n/2] Pochhammer[1/2 (1+a-b+2 c),n/2])+((-x)^(-a-c) x^-m (-1-y)^-c (1+y)^-n Gamma[1-a] Gamma[-a+b-2 c] Gamma[1-a-b-c] Gamma[a+b+c] Gamma[d] Pochhammer[1-a-b,n] Pochhammer[(a+b)/2,-(n/2)] Pochhammer[1/2 (1+a+b),-(n/2)] Pochhammer[c,n] Pochhammer[a+c,m] Pochhammer[1/2 (1+a-b+2 c),m+n/2] Pochhammer[1/2 (2+a-b+2 c),m+n/2] Pochhammer[1+a+c-d,m])/(m! n! Gamma[1-a-b] Gamma[b] Gamma[a+b] Gamma[1-a-c] Gamma[-a-c+d] Pochhammer[1/2 (-a+b-2 c),-(n/2)] Pochhammer[1/2 (1-a+b-2 c),-(n/2)] Pochhammer[1/2 (1+a-b+2 c),m] Pochhammer[1/2 (1+a-b+2 c),n/2] Pochhammer[1+a-b+2 c,m+n] Pochhammer[1/2 (2+a-b+2 c),m] Pochhammer[1/2 (2+a-b+2 c),n/2])+((-x)^(-a-c) x^-m (-1-y)^(a+b) (1+y)^(-a-b-c-n) Gamma[1-a] Gamma[-a+b-2 c] Gamma[1-a-b-c] Gamma[a+b+c] Gamma[d] Pochhammer[1-a-b,n] Pochhammer[(a+b)/2,-(n/2)] Pochhammer[1/2 (1+a+b),-(n/2)] Pochhammer[c,n] Pochhammer[a+c,m] Pochhammer[1/2 (1+a-b+2 c),m+n/2] Pochhammer[1/2 (2+a-b+2 c),m+n/2] Pochhammer[1+a+c-d,m])/(m! n! Gamma[b] Gamma[1-c] Gamma[1-a-c] Gamma[c] Gamma[-a-c+d] Pochhammer[1/2 (-a+b-2 c),-(n/2)] Pochhammer[1/2 (1-a+b-2 c),-(n/2)] Pochhammer[1/2 (1+a-b+2 c),m] Pochhammer[1/2 (1+a-b+2 c),n/2] Pochhammer[1+a-b+2 c,m+n] Pochhammer[1/2 (2+a-b+2 c),m] Pochhammer[1/2 (2+a-b+2 c),n/2])-(x^m (-1-y)^(a+c+m) (1+y)^(-a-b-c-2 m-n) Gamma[1-a] Gamma[1-a-b-c] Gamma[b-c] Gamma[1-b+c] Gamma[a+b+c] Pochhammer[b,m+n] Pochhammer[a+b,2 m] Pochhammer[1-a-c,-m+n] Pochhammer[a+c,m])/(m! n! Gamma[1-b] Gamma[1-a-b] Gamma[b] Gamma[1+b-c] Gamma[c] Pochhammer[1+b-c,m+n] Pochhammer[d,m])-(x^m (-1-y)^(-b-m) (1+y)^-n Gamma[1-a] Gamma[1-a-b-c] Gamma[b-c] Gamma[1-b+c] Gamma[a+b+c] Pochhammer[b,m+n] Pochhammer[a+b,2 m] Pochhammer[1-a-c,-m+n] Pochhammer[a+c,m])/(m! n! Gamma[1-a-b] Gamma[1-a-c] Gamma[1+b-c] Gamma[c] Gamma[a+c] Pochhammer[1+b-c,m+n] Pochhammer[d,m])},{1/Abs[1-x]<1&&Sqrt[Abs[(-1+x)/y]]<-Abs[1-x]+Sqrt[1+1/Abs[1-x]] Abs[1-x]&&Abs[(-1+x)/y]<1/4,-(((1-x)^m (-y)^(-b-m) y^-n Gamma[1-a] Gamma[-b+c] Gamma[1-d] Gamma[a+b-d] Gamma[d] Gamma[1-a-b+d] Pochhammer[b,m+n] Pochhammer[a+b,2 m+n] Pochhammer[1+a+b-d,m+n])/(m! n! Gamma[1-a-b] Gamma[c] Gamma[1+b-d] Gamma[1+a+b-d] Gamma[-b+d] Gamma[-a-b+d] Pochhammer[1+b-c,m+n] Pochhammer[1+a+b-d,m] Pochhammer[d,m+n]))-((1-x)^-m (-1+x)^(-b-c-n+2 (c+n)) (-y)^-c y^-n Gamma[1-a] Gamma[b-c] Gamma[a-b+2 c] Gamma[a+b-d] Gamma[1+b-c-d] Gamma[d] Gamma[1-a-b+d] Pochhammer[b-c,m-n] Pochhammer[c,n] Pochhammer[1+a+c-d,n] Pochhammer[-a-c+d,m-n])/(m! n! Gamma[b] Gamma[1-a-c] Gamma[a+c] Gamma[1+b-d] Gamma[1+a+c-d] Gamma[-b+d] Gamma[-a-c+d] Pochhammer[1-a+b-2 c,m-2 n] Pochhammer[-b+c+d,n])+((1-x)^(-a-b+d-m) (-1+x)^(a-c-d-n+2 (c+n)) (-y)^-c y^-n Gamma[a-b+2 c] Gamma[a+b-d] Gamma[d] Gamma[1-a-b+d] Pochhammer[b-c,m-n] Pochhammer[c,n] Pochhammer[1+a+c-d,n] Pochhammer[-a-c+d,m-n])/(m! n! Gamma[a] Gamma[b] Gamma[1-b+c] Gamma[-b+c+d] Pochhammer[1-a+b-2 c,m-2 n] Pochhammer[-b+c+d,n])+((1-x)^(-a-b+d-m) (-1+x)^(b-c-d-n) (-y)^-c y^-n Gamma[-a+b-2 c] Gamma[a+b-d] Gamma[d] Gamma[1-a-b+d] Pochhammer[c,n] Pochhammer[a+c,m+n] Pochhammer[1+a+c-d,n] Pochhammer[-b+c+d,m+n])/(m! n! Gamma[a] Gamma[b] Gamma[1-a-c] Gamma[-a-c+d] Pochhammer[1+a-b+2 c,m+2 n] Pochhammer[-b+c+d,n])-((1-x)^-m (-1+x)^(-a-c-n) (-y)^-c y^-n Gamma[1-a] Gamma[-a+b-2 c] Gamma[a+b-d] Gamma[d] Gamma[1-a-b+d] Pochhammer[c,n] Pochhammer[a+c,m+n] Pochhammer[1+a+c-d,n] Pochhammer[-b+c+d,m+n])/(m! n! Gamma[b] Gamma[1-a-c] Gamma[1+b-d] Gamma[-b+d] Gamma[-a-c+d] Pochhammer[1+a-b+2 c,m+2 n] Pochhammer[-b+c+d,n])+((1-x)^-m (-1+x)^(1-d+n) (-y)^(-b+d) y^(-1-n) Gamma[1-a] Gamma[a+b-d] Gamma[-1+d] Gamma[d] Gamma[1-a-b+d] Gamma[-1-b+c+d] Pochhammer[2+a+b-2 d,n] Pochhammer[1+b-d,n] Pochhammer[-1+d,m-n] Pochhammer[-1-a-b+2 d,m-n])/(m! n! Gamma[b] Gamma[c] Gamma[1+a+b-d] Gamma[-b+d] Gamma[-a-b+d] Gamma[-1-a-b+2 d] Pochhammer[2+b-c-d,n] Pochhammer[-1-a-b+2 d,m-2 n])},{Abs[x/(-1+x)]<1&&Sqrt[Abs[-1+x]] Sqrt[Abs[y]]+Sqrt[Abs[x]] Sqrt[Abs[y]]<1&&Abs[(-1+x) y]<1,((-1)^n (1-x)^(-a+n) (x/(-1+x))^m y^n Pochhammer[a,m-n] Pochhammer[b,n] Pochhammer[c,n] Pochhammer[1+b-d,n] Pochhammer[-b+d,m-n])/(m! n! Pochhammer[d,m])},{Abs[x/(-1+x)]<1&&Sqrt[Abs[x]]/Sqrt[Abs[-1+x]]+Sqrt[Abs[y]]/Sqrt[Abs[-1+x]]<1&&Abs[y/(-1+x)]<1,((-1)^n (1-x)^(-b-n) (x/(-1+x))^m y^n Pochhammer[b,m+n] Pochhammer[c,n] Pochhammer[-a+d,m+n])/(m! n! Pochhammer[1-a,n] Pochhammer[d,m] Pochhammer[-a+d,n])},{Abs[x]<1&&4 Abs[x]<Abs[2+1/y+y]/(Abs[y]+Abs[1+y])&&Abs[y/(1+y)]<1,((-4)^m x^m (y/(1+y))^n (1+y)^-c Pochhammer[1-a-b,-2 m+n] Pochhammer[b,m] Pochhammer[(a+b)/2,m] Pochhammer[1/2 (1+a+b),m] Pochhammer[c,n])/(m! n! Pochhammer[1-a,-m+n] Pochhammer[d,m])},{Abs[x]/Abs[1+y]+Abs[y]/Abs[1+y]<1,(x^m (y/(1+y))^n (1+y)^(-b-m) Pochhammer[b,m+n] Pochhammer[1-a-c,-m+n] Pochhammer[a+c,m])/(m! n! Pochhammer[1-a,-m+n] Pochhammer[d,m])},{Abs[y]<1&&1+Abs[y]<Abs[(1+y)/Sqrt[x]],(4^m x^m (-y)^n (1+y)^(1-a-b-c-2 m) Pochhammer[1-a-b,-2 m+n] Pochhammer[b,m] Pochhammer[(a+b)/2,m] Pochhammer[1/2 (1+a+b),m] Pochhammer[1-a-c,-m+n] Pochhammer[a+c,m])/(m! n! Pochhammer[1-a,-m+n] Pochhammer[d,m])},{Abs[x/y]<1&&Sqrt[Abs[(-1+x)/y]]<1-Sqrt[Abs[x/y]]&&Abs[(-1+x)/y]<1&&Abs[x/(-1+x)]<1&&Sqrt[Abs[(-1+x)/y]]<1/(1+Sqrt[Abs[x/(-1+x)]])&&Abs[(-1+x)/y]<1,((-1)^m (1-x)^-b (x/(-1+x))^m ((-1+x)/y)^n (y/(1-x))^(-b-m) Gamma[1-a] Gamma[-b+c] Pochhammer[b,m+n] Pochhammer[a+b,m+n] Pochhammer[1+a+b-d,m+n])/(m! n! Gamma[1-a-b] Gamma[c] Pochhammer[1+b-c,m+n] Pochhammer[1+a+b-d,n] Pochhammer[d,m])+((1-x)^-b (x/(-1+x))^m ((-1+x)/y)^n (y/(1-x))^-c Gamma[1-a] Gamma[b-c] Pochhammer[c,n] Pochhammer[a+c,n] Pochhammer[1+a+c-d,n])/(m! n! Gamma[b] Gamma[1-a-c] Pochhammer[1-b+c,-m+n] Pochhammer[1+a+c-d,-m+n] Pochhammer[d,m])},{Abs[(-1+x)/x]<1&&Sqrt[Abs[x/y]]<1/(1+Sqrt[Abs[(-1+x)/x]])&&Abs[x/y]<1&&Abs[(-1+x)/x]<1&&Sqrt[Abs[x/y]]<1/(1+Sqrt[Abs[(-1+x)/x]])&&Abs[x/y]<1,((-1)^n (1-x)^-b ((-1+x)/x)^m (-(x/(-1+x)))^(-b+c+n) ((-1+x)/y)^n (y/(1-x))^-c Gamma[1-a] Gamma[b-c] Gamma[d] Gamma[-a-b+d] Pochhammer[b-c,m-n] Pochhammer[c,n] Pochhammer[a+c,n] Pochhammer[1+b-c-d,m-n] Pochhammer[1+a+c-d,n])/(m! n! Gamma[b] Gamma[1-a-c] Gamma[-a-c+d] Gamma[-b+c+d] Pochhammer[1+a+b-d,m])+((-1)^m (1-x)^-b (x/(-1+x))^m ((-1+x)/y)^n (y/(1-x))^(-b-m) Gamma[1-a] Gamma[-b+c] Pochhammer[b,m+n] Pochhammer[a+b,m+n] Pochhammer[1+a+b-d,m+n])/(m! n! Gamma[1-a-b] Gamma[c] Pochhammer[1+b-c,m+n] Pochhammer[1+a+b-d,n] Pochhammer[d,m])+((-1)^n (1-x)^-b ((-1+x)/x)^m (-(x/(-1+x)))^(a+c-d+n) ((-1+x)/y)^n (y/(1-x))^-c Gamma[1-a] Gamma[a+b-d] Gamma[d] Pochhammer[1-a-c,m-n] Pochhammer[c,n] Pochhammer[a+c,n] Pochhammer[1+a+c-d,n] Pochhammer[-a-c+d,m-n])/(m! n! Gamma[b] Gamma[1-a-c] Gamma[a+c] Pochhammer[1-a-b+d,m])},{4 Abs[x]<4&&Abs[1+y]<1&&1/4 Abs[(1+y)^2/(x y^2)]<1&&2 Abs[(1+1/y) Sqrt[(x y^2)/(1+y)^2]]<2&&4 Abs[(1+1/y) Sqrt[(x y^2)/(1+y)^2]]^2<-2+4/Abs[(1+y)^2/(x y^2)]+1/4 Abs[(1+y)^2/(x y^2)],(4^m x^m (1+y)^n Gamma[1-a] Gamma[1-a-b-c] Pochhammer[b,m+n] Pochhammer[(a+b)/2,m] Pochhammer[1/2 (1+a+b),m] Pochhammer[c,n] Pochhammer[a+c,m])/(m! n! Gamma[1-a-b] Gamma[1-a-c] Pochhammer[a+b+c,2 m+n] Pochhammer[d,m])-(2^(a-b+m-2 n) Sqrt[\[Pi]] (1-x)^-m (-1+x)^(-1-a) (1/((-1+x) y))^n (y-x y)^(a/2-b/2+m/2) Gamma[1-a] Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (a-b+2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b),-(m/2)+n] Pochhammer[(a+b)/2,m/2+n] Pochhammer[1/2 (2-a+b-2 c),-(m/2)] Pochhammer[1/2 (a-b+2 c),m/2] Pochhammer[1/2 (2+a+b-2 d),m/2] Pochhammer[1/2 (2+a+b-2 d),-(m/2)+n] Pochhammer[-(a/2)-b/2+d,-(m/2)] Pochhammer[-(a/2)-b/2+d,m/2+n])/(m! n! Gamma[1/2 (2-a-b)] Gamma[1+a-b] Gamma[b] Gamma[1/2 (1-a+b)] Gamma[c] Gamma[b-d]^2 Gamma[1-b+d]^2 Gamma[-(a/2)-b/2+d] Pochhammer[1/2,n] Pochhammer[1/2 (2-a-b),-(m/2)] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[(a+b)/2,m/2] Pochhammer[1/2 (2-a+b-2 c),-(m/2)+n] Pochhammer[1/2 (2+a+b-2 d),-(m/2)]^2 Pochhammer[-(a/2)-b/2+d,m/2]^2 Pochhammer[-(a/2)-b/2+d,-(m/2)+n])+(2^(a-b+m-2 n) Sqrt[\[Pi]] (1-x)^-m (-1+x)^(-1-a) x (1/((-1+x) y))^n (y-x y)^(a/2-b/2+m/2) Gamma[1-a] Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (a-b+2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b),-(m/2)+n] Pochhammer[(a+b)/2,m/2+n] Pochhammer[1/2 (2-a+b-2 c),-(m/2)] Pochhammer[1/2 (a-b+2 c),m/2] Pochhammer[1/2 (2+a+b-2 d),m/2] Pochhammer[1/2 (2+a+b-2 d),-(m/2)+n] Pochhammer[-(a/2)-b/2+d,-(m/2)] Pochhammer[-(a/2)-b/2+d,m/2+n])/(m! n! Gamma[1/2 (2-a-b)] Gamma[1+a-b] Gamma[b] Gamma[1/2 (1-a+b)] Gamma[c] Gamma[b-d]^2 Gamma[1-b+d]^2 Gamma[-(a/2)-b/2+d] Pochhammer[1/2,n] Pochhammer[1/2 (2-a-b),-(m/2)] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[(a+b)/2,m/2] Pochhammer[1/2 (2-a+b-2 c),-(m/2)+n] Pochhammer[1/2 (2+a+b-2 d),-(m/2)]^2 Pochhammer[-(a/2)-b/2+d,m/2]^2 Pochhammer[-(a/2)-b/2+d,-(m/2)+n])-(2^(a-b+m-2 n) Sqrt[\[Pi]] (1-x)^(-a-b+d-m) (-1+x)^(-1+b-d) (1/((-1+x) y))^n (y-x y)^(a/2-b/2+m/2) Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (a-b+2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b),-(m/2)+n] Pochhammer[(a+b)/2,m/2+n] Pochhammer[1/2 (2-a+b-2 c),-(m/2)] Pochhammer[1/2 (a-b+2 c),m/2] Pochhammer[1/2 (2+a+b-2 d),m/2] Pochhammer[1/2 (2+a+b-2 d),-(m/2)+n] Pochhammer[-(a/2)-b/2+d,-(m/2)] Pochhammer[-(a/2)-b/2+d,m/2+n])/(m! n! Gamma[a] Gamma[1/2 (2-a-b)] Gamma[1+a-b] Gamma[b] Gamma[1/2 (1-a+b)] Gamma[c] Gamma[b-d] Gamma[1-b+d] Gamma[-(a/2)-b/2+d] Pochhammer[1/2,n] Pochhammer[1/2 (2-a-b),-(m/2)] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[(a+b)/2,m/2] Pochhammer[1/2 (2-a+b-2 c),-(m/2)+n] Pochhammer[1/2 (2+a+b-2 d),-(m/2)]^2 Pochhammer[-(a/2)-b/2+d,m/2]^2 Pochhammer[-(a/2)-b/2+d,-(m/2)+n])+(2^(a-b+m-2 n) Sqrt[\[Pi]] (1-x)^(-a-b+d-m) (-1+x)^(-1+b-d) x (1/((-1+x) y))^n (y-x y)^(a/2-b/2+m/2) Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (a-b+2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b),-(m/2)+n] Pochhammer[(a+b)/2,m/2+n] Pochhammer[1/2 (2-a+b-2 c),-(m/2)] Pochhammer[1/2 (a-b+2 c),m/2] Pochhammer[1/2 (2+a+b-2 d),m/2] Pochhammer[1/2 (2+a+b-2 d),-(m/2)+n] Pochhammer[-(a/2)-b/2+d,-(m/2)] Pochhammer[-(a/2)-b/2+d,m/2+n])/(m! n! Gamma[a] Gamma[1/2 (2-a-b)] Gamma[1+a-b] Gamma[b] Gamma[1/2 (1-a+b)] Gamma[c] Gamma[b-d] Gamma[1-b+d] Gamma[-(a/2)-b/2+d] Pochhammer[1/2,n] Pochhammer[1/2 (2-a-b),-(m/2)] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[(a+b)/2,m/2] Pochhammer[1/2 (2-a+b-2 c),-(m/2)+n] Pochhammer[1/2 (2+a+b-2 d),-(m/2)]^2 Pochhammer[-(a/2)-b/2+d,m/2]^2 Pochhammer[-(a/2)-b/2+d,-(m/2)+n])-(2^(-2 c-2 n) (1-x)^-m (-1+x)^(-1-a) (1/((-1+x) y))^n (y-x y)^-c Gamma[1-a] Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (-a+b-2 c)] Gamma[1/2 (1-a+b-2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b-2 c),-(m/2)] Pochhammer[1/2 (1-a+b-2 c),-(m/2)] Pochhammer[c,n] Pochhammer[a+c,m+n] Pochhammer[1/2 (1+a-b+2 c),m/2] Pochhammer[1/2 (2+a-b+2 c),m/2] Pochhammer[1+a+c-d,n] Pochhammer[-b+c+d,m+n])/(m! n! Gamma[1+a-b] Gamma[b] Gamma[1/2 (-a+b)] Gamma[1/2 (1-a+b)] Gamma[1-a-c] Gamma[b-d]^2 Gamma[1-b+d]^2 Gamma[-a-c+d] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[1/2 (1+a-b+2 c),m/2+n] Pochhammer[1/2 (2+a-b+2 c),m/2+n] Pochhammer[-b+c+d,n])+(2^(-2 c-2 n) (1-x)^-m (-1+x)^(-1-a) x (1/((-1+x) y))^n (y-x y)^-c Gamma[1-a] Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (-a+b-2 c)] Gamma[1/2 (1-a+b-2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b-2 c),-(m/2)] Pochhammer[1/2 (1-a+b-2 c),-(m/2)] Pochhammer[c,n] Pochhammer[a+c,m+n] Pochhammer[1/2 (1+a-b+2 c),m/2] Pochhammer[1/2 (2+a-b+2 c),m/2] Pochhammer[1+a+c-d,n] Pochhammer[-b+c+d,m+n])/(m! n! Gamma[1+a-b] Gamma[b] Gamma[1/2 (-a+b)] Gamma[1/2 (1-a+b)] Gamma[1-a-c] Gamma[b-d]^2 Gamma[1-b+d]^2 Gamma[-a-c+d] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[1/2 (1+a-b+2 c),m/2+n] Pochhammer[1/2 (2+a-b+2 c),m/2+n] Pochhammer[-b+c+d,n])-(2^(-2 c-2 n) (1-x)^(-a-b+d-m) (-1+x)^(-1+b-d) (1/((-1+x) y))^n (y-x y)^-c Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (-a+b-2 c)] Gamma[1/2 (1-a+b-2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b-2 c),-(m/2)] Pochhammer[1/2 (1-a+b-2 c),-(m/2)] Pochhammer[c,n] Pochhammer[a+c,m+n] Pochhammer[1/2 (1+a-b+2 c),m/2] Pochhammer[1/2 (2+a-b+2 c),m/2] Pochhammer[1+a+c-d,n] Pochhammer[-b+c+d,m+n])/(m! n! Gamma[a] Gamma[1+a-b] Gamma[b] Gamma[1/2 (-a+b)] Gamma[1/2 (1-a+b)] Gamma[1-a-c] Gamma[b-d] Gamma[1-b+d] Gamma[-a-c+d] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[1/2 (1+a-b+2 c),m/2+n] Pochhammer[1/2 (2+a-b+2 c),m/2+n] Pochhammer[-b+c+d,n])+(2^(-2 c-2 n) (1-x)^(-a-b+d-m) (-1+x)^(-1+b-d) x (1/((-1+x) y))^n (y-x y)^-c Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (-a+b-2 c)] Gamma[1/2 (1-a+b-2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (-a+b-2 c),-(m/2)] Pochhammer[1/2 (1-a+b-2 c),-(m/2)] Pochhammer[c,n] Pochhammer[a+c,m+n] Pochhammer[1/2 (1+a-b+2 c),m/2] Pochhammer[1/2 (2+a-b+2 c),m/2] Pochhammer[1+a+c-d,n] Pochhammer[-b+c+d,m+n])/(m! n! Gamma[a] Gamma[1+a-b] Gamma[b] Gamma[1/2 (-a+b)] Gamma[1/2 (1-a+b)] Gamma[1-a-c] Gamma[b-d] Gamma[1-b+d] Gamma[-a-c+d] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[1/2 (1+a-b+2 c),m/2+n] Pochhammer[1/2 (2+a-b+2 c),m/2+n] Pochhammer[-b+c+d,n])+(2^(a-b+m-2 n) Sqrt[\[Pi]] (1-x)^-m (-1+x)^(-1-a) (1/((-1+x) y))^n (y-x y)^(1/2+a/2-b/2+m/2) Gamma[1-a] Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (-1+a-b+2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (1-a+b),-(m/2)+n] Pochhammer[1/2 (1+a+b),m/2+n] Pochhammer[1/2 (3-a+b-2 c),-(m/2)] Pochhammer[1/2 (-1+a-b+2 c),m/2] Pochhammer[1/2 (1+a+b-2 d),m/2] Pochhammer[1/2 (3+a+b-2 d),-(m/2)+n] Pochhammer[1/2 (1-a-b+2 d),-(m/2)] Pochhammer[1/2 (1-a-b+2 d),m/2+n])/(y m! n! Gamma[1/2 (1-a-b)] Gamma[1+a-b] Gamma[b] Gamma[1/2 (-a+b)] Gamma[c] Gamma[b-d]^2 Gamma[1-b+d]^2 Gamma[1/2 (-1-a-b+2 d)] Pochhammer[3/2,n] Pochhammer[1/2 (1-a-b),-(m/2)] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[1/2 (1+a+b),m/2] Pochhammer[1/2 (3-a+b-2 c),-(m/2)+n] Pochhammer[1/2 (1+a+b-2 d),-(m/2)] Pochhammer[1/2 (3+a+b-2 d),-(m/2)] Pochhammer[1/2 (-1-a-b+2 d),m/2] Pochhammer[1/2 (1-a-b+2 d),m/2] Pochhammer[1/2 (1-a-b+2 d),-(m/2)+n])+(2^(a-b+m-2 n) Sqrt[\[Pi]] (1-x)^(-a-b+d-m) (-1+x)^(-1+b-d) (1/((-1+x) y))^n (y-x y)^(1/2+a/2-b/2+m/2) Gamma[a-b] Gamma[1-a+b] Gamma[1/2 (-1+a-b+2 c)] Gamma[1+b-d] Gamma[a+b-d] Gamma[d] Gamma[-b+d] Gamma[1-a-b+d] Pochhammer[1/2 (1-a+b),-(m/2)+n] Pochhammer[1/2 (1+a+b),m/2+n] Pochhammer[1/2 (3-a+b-2 c),-(m/2)] Pochhammer[1/2 (-1+a-b+2 c),m/2] Pochhammer[1/2 (1+a+b-2 d),m/2] Pochhammer[1/2 (3+a+b-2 d),-(m/2)+n] Pochhammer[1/2 (1-a-b+2 d),-(m/2)] Pochhammer[1/2 (1-a-b+2 d),m/2+n])/(y m! n! Gamma[a] Gamma[1/2 (1-a-b)] Gamma[1+a-b] Gamma[b] Gamma[1/2 (-a+b)] Gamma[c] Gamma[b-d] Gamma[1-b+d] Gamma[1/2 (-1-a-b+2 d)] Pochhammer[3/2,n] Pochhammer[1/2 (1-a-b),-(m/2)] Pochhammer[1+a-b,m] Pochhammer[1/2 (-a+b),-(m/2)] Pochhammer[1/2 (1-a+b),-(m/2)] Pochhammer[1/2 (1+a+b),m/2] Pochhammer[1/2 (3-a+b-2 c),-(m/2)+n] Pochhammer[1/2 (1+a+b-2 d),-(m/2)] Pochhammer[1/2 (3+a+b-2 d),-(m/2)] Pochhammer[1/2 (-1-a-b+2 d),m/2] Pochhammer[1/2 (1-a-b+2 d),m/2] Pochhammer[1/2 (1-a-b+2 d),-(m/2)+n])}};


(* ::Section:: *)
(*H5*)


H5expose[p_]:=Module[{},

Return[If[p>20,Print["Total number of ACs is 20"];Abort[];,
H5series[Global`a,Global`b,Global`c,Global`x,Global`y,Global`m,Global`n][[p]]]]];



Options[H5ROC]={resolution->{100,3}};
H5ROC[args___] := h5core[H5ROC,args];

h5core[H5ROC,point___List,list_/;(IntegerQ[list]&&list>0),range_List,OptionsPattern[H1ROC]]:=Module[{i},
roc=H5series[a,b,c,x,y,m,n][[list,1]];
Show[ListPlot[{point},PlotStyle->Directive[PointSize[Medium],Red],PlotRange->{{range[[1]],range[[2]]},{range[[1]],range[[2]]}}],RegionPlot[roc,{x,range[[1]]-1,range[[2]]+1},{y,range[[1]]-1,range[[2]]+1},PlotPoints->OptionValue[resolution][[1]],MaxRecursion->OptionValue[resolution][[2]]],PlotRange->{{range[[1]],range[[2]]},{range[[1]],range[[2]]}},AspectRatio-> 1]
];


H5series[a_,b_,c_,x_,y_,m_,n_]={{Abs[x]<1/4&&Abs[y]<1&&Abs[y]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Min", "[", 
RowBox[{
FractionBox[
RowBox[{"1", "-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]], "+", 
RowBox[{"36", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], 
RowBox[{"54", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]], ",", 
FractionBox[
RowBox[{"1", "+", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]], "+", 
RowBox[{"36", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], 
RowBox[{"54", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]], ",", 
FractionBox[
RowBox[{"1", "-", 
RowBox[{"36", " ", 
RowBox[{"Abs", "[", "x", "]"}]}], "+", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]}], 
RowBox[{"54", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]]}], "]"}], 
RowBox[{
SqrtBox[
RowBox[{"Abs", "[", "x", "]"}]], "<", 
FractionBox["1", 
RowBox[{"2", " ", 
SqrtBox["3"]}]]}]},
{
FractionBox[
RowBox[{"1", "-", 
RowBox[{"36", " ", 
RowBox[{"Abs", "[", "x", "]"}]}], "+", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]}], 
RowBox[{"54", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)),(x^m y^n Pochhammer[a,2 m+n] Pochhammer[b,-m+n])/(m! n! Pochhammer[c,n])},{4 Abs[x]<1/4&&Abs[1-y]<1&&Abs[1-y]<-(1/3)-2/9 Sqrt[1-12 Abs[x]]+1/(54 Abs[x])+Sqrt[1-12 Abs[x]]/(54 Abs[x])&&4 Abs[x/(-1+y)]<4&&Abs[1-y]<1&&Abs[1-y]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Min", "[", 
RowBox[{
RowBox[{
FractionBox["1", "27"], " ", 
RowBox[{"(", 
RowBox[{"9", "-", 
RowBox[{"8", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]}], "-", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "3"}], "+", 
RowBox[{"4", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]], 
SqrtBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]]]}], ")"}]}], ",", 
RowBox[{
FractionBox["1", "27"], " ", 
RowBox[{"(", 
RowBox[{"9", "-", 
RowBox[{"8", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]}], "+", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "3"}], "+", 
RowBox[{"4", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]], 
SqrtBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]]]}], ")"}]}], ",", 
RowBox[{
FractionBox["1", "27"], " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "9"}], "-", 
RowBox[{"8", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]}], "+", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{"3", "+", 
RowBox[{"4", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]], 
SqrtBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]]]}], ")"}]}]}], "]"}], 
RowBox[{"3", "<", 
RowBox[{"4", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]}]}]},
{
RowBox[{
RowBox[{"-", 
FractionBox["1", "3"]}], "-", 
RowBox[{
FractionBox["8", "27"], " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]}], "+", 
FractionBox[
SqrtBox[
RowBox[{"3", "+", 
RowBox[{"4", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]}]}]], 
RowBox[{"9", " ", 
SqrtBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]]}]], "+", 
RowBox[{
FractionBox["4", "27"], " ", 
SqrtBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]], " ", 
SqrtBox[
RowBox[{"3", "+", 
RowBox[{"4", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}]], "]"}]}]}]]}]}], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)),((-4)^m x^m (1-y)^n Gamma[c] Gamma[-a-b+c] Pochhammer[a,2 m+n] Pochhammer[b,-m+n] Pochhammer[1/2 (1+a-c),m] Pochhammer[1/2 (2+a-c),m])/(m! n! Gamma[-a+c] Gamma[-b+c] Pochhammer[1+a+b-c,m+n] Pochhammer[-b+c,m])+((-4)^m x^m (1-y)^(-a-b+c-m+n) Gamma[a+b-c] Gamma[c] Pochhammer[1/2 (1+a-c),m] Pochhammer[1/2 (2+a-c),m] Pochhammer[-a+c,-2 m+n] Pochhammer[-b+c,m+n])/(m! n! Gamma[a] Gamma[b] Pochhammer[-b+c,m] Pochhammer[1-a-b+c,-m+n])},{4 Abs[x]<1/4&&Abs[1-y]<1&&Abs[1-y]<(2+2 (1-12 Abs[x])^(3/2)-36 Abs[x])/(108 Abs[x])&&1/4 Abs[(-1+y)/x]<1/4&&2 Sqrt[Abs[x]]<Min[Root[-16-16 Abs[(-1+y)/x]+(-64-72 Abs[(-1+y)/x]) #1^2+27 Abs[(-1+y)/x]^2 #1^4&,2],Root[-16+16 Abs[(-1+y)/x]+(-64+72 Abs[(-1+y)/x]) #1^2+27 Abs[(-1+y)/x]^2 #1^4&,2],Root[-16-16 Abs[(-1+y)/x]+(64+72 Abs[(-1+y)/x]) #1^2+27 Abs[(-1+y)/x]^2 #1^4&,2]]&&4 Abs[x]<1/4,(4^(-m+n) (1-y)^(-a-b+c+n) ((1-y)/x)^m (x/(-1+y))^(-a-b+c+n) Gamma[a+b-c] Gamma[c] Pochhammer[1+a+2 b-2 c,m-n] Pochhammer[a+b-c,m-n] Pochhammer[a+2 b-c,2 m-n])/(m! n! Gamma[a] Gamma[b] Pochhammer[1+a+2 b-2 c,m-2 n] Pochhammer[1/2 (a+2 b-c),m-n] Pochhammer[1/2 (1+a+2 b-c),m-n])+((-4)^m x^m (1-y)^n Gamma[c] Gamma[-a-b+c] Pochhammer[a,2 m+n] Pochhammer[b,-m+n] Pochhammer[1/2 (1+a-c),m] Pochhammer[1/2 (2+a-c),m])/(m! n! Gamma[-a+c] Gamma[-b+c] Pochhammer[1+a+b-c,m+n] Pochhammer[-b+c,m])},{Abs[x/y^2]<27/16&&Abs[x/y^2]<27/32+9/32 Sqrt[9-8/Abs[y]]+1/(4 Abs[y]^2)-9/(8 Abs[y])-Sqrt[9-8/Abs[y]]/(4 Abs[y])&&1/Abs[y]<1&&Abs[x y]<1/27&&Abs[x y]<Min[1/32 (27+(9-8/Abs[y])^(3/2)+8/Abs[y]^2-36/Abs[y]) Abs[y]^3,1/32 (27-(9+8/Abs[y])^(3/2)+8/Abs[y]^2+36/Abs[y]) Abs[y]^3]&&1/Abs[y]<1,((-1)^m x^m (-y)^(-a-2 m) y^-n Gamma[-a+b] Gamma[c] Pochhammer[a,2 m+n] Pochhammer[1+a-c,2 m+n])/(m! n! Gamma[b] Gamma[-a+c] Pochhammer[1+a-b,3 m+n])+(x^m (-y)^(-b+m) y^-n Gamma[a-b] Gamma[c] Pochhammer[b,-m+n] Pochhammer[1+b-c,-m+n])/(m! n! Gamma[a] Gamma[-b+c] Pochhammer[1-a+b,-3 m+n])},{1/Abs[1-y]<1&&16 Abs[x/(-1+y)^2]<27&&9 Abs[1-y]^2 (2+3 Abs[1-y])+32 Abs[x (-1+y)]<Abs[1-y] (1+Sqrt[(1+Abs[1-y]) (1+9 Abs[1-y])^3])&&27+Sqrt[(-9+1/Abs[1-y])^3 (-1+1/Abs[1-y])]>(1+32 Abs[x]+18 Abs[1-y])/Abs[1-y]^2,(4^m x^m (1-y)^(-b-n) (-1+y)^m Gamma[a-b] Gamma[c] Pochhammer[b,-m+n] Pochhammer[1/2 (1+a-c),m] Pochhammer[1/2 (2+a-c),m] Pochhammer[-a+c,-2 m+n])/(m! n! Gamma[a] Gamma[-b+c] Pochhammer[1-a+b,-3 m+n] Pochhammer[-b+c,m])+((-4)^m x^m (1-y)^(-a-n) (-1+y)^(-2 m) Gamma[-a+b] Gamma[c] Pochhammer[a,2 m+n] Pochhammer[1/2 (1+a-c),m] Pochhammer[1/2 (2+a-c),m] Pochhammer[-b+c,m+n])/(m! n! Gamma[b] Gamma[-a+c] Pochhammer[1+a-b,3 m+n] Pochhammer[-b+c,m])},{Abs[x/y^2]<27/16&&Abs[x/y^2]<1/32 (27+(9-8/Abs[y])^(3/2)+8/Abs[y]^2-36/Abs[y])&&1/Abs[y]<1&&1/27 Abs[1/(x y)]<1&&3 Abs[x/(-x y)^(2/3)]<9/(2 2^(1/3))&&27 Abs[x/(-x y)^(2/3)]^3<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Min", "[", 
RowBox[{
RowBox[{"Root", "[", 
RowBox[{
RowBox[{
RowBox[{
RowBox[{"-", "387420489"}], "-", 
RowBox[{"43046721", " ", 
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}]}], "-", 
RowBox[{"1594323", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "2"]}], "-", 
RowBox[{"19683", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "3"]}], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{"25509168", "+", 
RowBox[{"18895680", " ", 
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}]}], "+", 
RowBox[{"1767096", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "2"]}], "+", 
RowBox[{"61236", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "3"]}], "+", 
RowBox[{"729", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "4"]}]}], ")"}], " ", "#1"}], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "559872"}], "+", 
RowBox[{"352512", " ", 
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}]}], "-", 
RowBox[{"3456", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox["#1", "2"]}], "+", 
RowBox[{"4096", " ", 
SuperscriptBox["#1", "3"]}]}], "&"}], ",", "1"}], "]"}], ",", 
RowBox[{"Root", "[", 
RowBox[{
RowBox[{
RowBox[{"387420489", "-", 
RowBox[{"43046721", " ", 
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}]}], "+", 
RowBox[{"1594323", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "2"]}], "-", 
RowBox[{"19683", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "3"]}], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{"25509168", "-", 
RowBox[{"18895680", " ", 
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}]}], "+", 
RowBox[{"1767096", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "2"]}], "-", 
RowBox[{"61236", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "3"]}], "+", 
RowBox[{"729", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "4"]}]}], ")"}], " ", "#1"}], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{"559872", "+", 
RowBox[{"352512", " ", 
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}]}], "+", 
RowBox[{"3456", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox["#1", "2"]}], "+", 
RowBox[{"4096", " ", 
SuperscriptBox["#1", "3"]}]}], "&"}], ",", "2"}], "]"}], ",", 
RowBox[{"Root", "[", 
RowBox[{
RowBox[{
RowBox[{"387420489", "-", 
RowBox[{"43046721", " ", 
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}]}], "+", 
RowBox[{"1594323", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "2"]}], "-", 
RowBox[{"19683", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "3"]}], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{"25509168", "-", 
RowBox[{"18895680", " ", 
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}]}], "+", 
RowBox[{"1767096", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "2"]}], "-", 
RowBox[{"61236", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "3"]}], "+", 
RowBox[{"729", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "4"]}]}], ")"}], " ", "#1"}], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{"559872", "+", 
RowBox[{"352512", " ", 
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}]}], "+", 
RowBox[{"3456", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox["#1", "2"]}], "+", 
RowBox[{"4096", " ", 
SuperscriptBox["#1", "3"]}]}], "&"}], ",", "3"}], "]"}]}], "]"}], 
RowBox[{
FractionBox["1", "2"], "<", 
RowBox[{
FractionBox["1", "27"], " ", 
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}]}]}]},
{
RowBox[{"Root", "[", 
RowBox[{
RowBox[{
RowBox[{
RowBox[{"-", "387420489"}], "-", 
RowBox[{"43046721", " ", 
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}]}], "-", 
RowBox[{"1594323", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "2"]}], "-", 
RowBox[{"19683", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "3"]}], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{"25509168", "+", 
RowBox[{"18895680", " ", 
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}]}], "+", 
RowBox[{"1767096", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "2"]}], "+", 
RowBox[{"61236", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "3"]}], "+", 
RowBox[{"729", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "4"]}]}], ")"}], " ", "#1"}], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "559872"}], "+", 
RowBox[{"352512", " ", 
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}]}], "-", 
RowBox[{"3456", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["1", 
RowBox[{"x", " ", "y"}]], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox["#1", "2"]}], "+", 
RowBox[{"4096", " ", 
SuperscriptBox["#1", "3"]}]}], "&"}], ",", "1"}], "]"}], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)),((-1)^m x^m (-y)^(-a-2 m) y^-n Gamma[-a+b] Gamma[c] Pochhammer[a,2 m+n] Pochhammer[1+a-c,2 m+n])/(m! n! Gamma[b] Gamma[-a+c] Pochhammer[1+a-b,3 m+n])+(3^(-1-3 m+n) (1/(x y))^m (-y)^-b y^-n (-x y)^(1/3 (-a+b)+n/3) Gamma[1-b] Gamma[(a-b)/3] Gamma[c] Pochhammer[(a-b)/3,m-n/3] Pochhammer[1/3 (a+2 b),m+(2 n)/3] Pochhammer[1/3 (3+a+2 b-3 c),m+(2 n)/3])/(m! n! Gamma[a] Gamma[1-a/3-(2 b)/3] Gamma[-(a/3)-(2 b)/3+c] Pochhammer[1/3,m] Pochhammer[2/3,m] Pochhammer[1/3 (3-a-2 b),-((2 n)/3)] Pochhammer[(a-b)/3,-(n/3)] Pochhammer[1/3 (1+a-b),-(n/3)] Pochhammer[1/3 (2+a-b),-(n/3)] Pochhammer[1-a+b,n] Pochhammer[1/3 (a+2 b),(2 n)/3] Pochhammer[1/3 (3+a+2 b-3 c),(2 n)/3] Pochhammer[-(a/3)-(2 b)/3+c,-((2 n)/3)])+(3^(-(1/2)-a+b-3 m+n) \[Pi] (1/(x y))^m (-y)^-b y^-n (-x y)^(1/3 (-2-a+b)+n/3) Gamma[1-b] Gamma[a-b] Gamma[c] Pochhammer[1/3 (2+a-b),m-n/3] Pochhammer[1/3 (2+a+2 b),m+(2 n)/3] Pochhammer[1/3 (5+a+2 b-3 c),m+(2 n)/3])/(m! n! Gamma[a] Gamma[1/3 (1-a-2 b)] Gamma[(a-b)/3] Gamma[1/3 (1+a-b)] Gamma[-(a/3)-(2 (1+b))/3+c] Pochhammer[4/3,m] Pochhammer[5/3,m] Pochhammer[1/3 (1-a-2 b),-((2 n)/3)] Pochhammer[(a-b)/3,-(n/3)] Pochhammer[1/3 (1+a-b),-(n/3)] Pochhammer[1/3 (2+a-b),-(n/3)] Pochhammer[1-a+b,n] Pochhammer[1/3 (2+a+2 b),(2 n)/3] Pochhammer[1/3 (5+a+2 b-3 c),(2 n)/3] Pochhammer[1/3 (-2-a-2 b+3 c),-((2 n)/3)])-(2 3^(-(1/2)-a+b-3 m+n) \[Pi] (1/(x y))^m (-y)^-b y^-n (-x y)^(1/3 (-1-a+b)+n/3) Gamma[1-b] Gamma[a-b] Gamma[c] Pochhammer[1/3 (1+a-b),m-n/3] Pochhammer[1/3 (1+a+2 b),m+(2 n)/3] Pochhammer[1/3 (4+a+2 b-3 c),m+(2 n)/3])/(m! n! Gamma[a] Gamma[1/3 (2-a-2 b)] Gamma[(a-b)/3] Gamma[1/3 (2+a-b)] Gamma[1/3 (-1-a-2 b)+c] Pochhammer[2/3,m] Pochhammer[4/3,m] Pochhammer[1/3 (2-a-2 b),-((2 n)/3)] Pochhammer[(a-b)/3,-(n/3)] Pochhammer[1/3 (1+a-b),-(n/3)] Pochhammer[1/3 (2+a-b),-(n/3)] Pochhammer[1-a+b,n] Pochhammer[1/3 (1+a+2 b),(2 n)/3] Pochhammer[1/3 (4+a+2 b-3 c),(2 n)/3] Pochhammer[1/3 (-1-a-2 b+3 c),-((2 n)/3)])},{27+Sqrt[(-9+1/Abs[1-y])^3 (-1+1/Abs[1-y])]>(1+32 Abs[x]+18 Abs[1-y])/Abs[1-y]^2&&16 Abs[x/(-1+y)^2]<27&&1/Abs[1-y]<1&&1/27 Abs[1/(x (-1+y))]<1&&3 Abs[(x-x y)^(1/3)/(-1+y)]<9/(2 2^(1/3))&&27 Abs[(x-x y)^(1/3)/(-1+y)]^3<Root[-387420489+43046721 Abs[1/(x (-1+y))]-1594323 Abs[1/(x (-1+y))]^2+19683 Abs[1/(x (-1+y))]^3+(25509168+3424842 Abs[1/(x (-1+y))]-4374 Abs[1/(x (-1+y))]^2) #1+(-559872-2592 Abs[1/(x (-1+y))]+27 Abs[1/(x (-1+y))]^2) #1^2+4096 #1^3&,1],((-4)^m x^m (1-y)^(-a-n) (-1+y)^(-2 m) Gamma[-a+b] Gamma[c] Pochhammer[a,2 m+n] Pochhammer[1/2 (1+a-c),m] Pochhammer[1/2 (2+a-c),m] Pochhammer[-b+c,m+n])/(m! n! Gamma[b] Gamma[-a+c] Pochhammer[1+a-b,3 m+n] Pochhammer[-b+c,m])+((-1)^n 3^(-1-3 m+n) (1-y)^(-b-n) (1/(x (-1+y)))^m (x-x y)^(1/3 (-a+b)+n/3) Gamma[1-b] Gamma[(a-b)/3] Gamma[c] Pochhammer[(a-b)/3,m-n/3] Pochhammer[1/3 (a+2 b),m+(2 n)/3] Pochhammer[1/6 (3+a+2 b-3 c),n/3] Pochhammer[1/3 (3+a+2 b-3 c),m-n/3] Pochhammer[1/6 (6+a+2 b-3 c),n/3] Pochhammer[1/2 (1+a-c),-(n/2)] Pochhammer[1/2 (2+a-c),-(n/2)] Pochhammer[-a+c,n] Pochhammer[1/6 (-a-2 b+3 c),m+n/6] Pochhammer[1/6 (-a-2 b+3 c),-(n/3)] Pochhammer[1/6 (3-a-2 b+3 c),m+n/6] Pochhammer[1/6 (3-a-2 b+3 c),-(n/3)])/(m! n! Gamma[a] Gamma[1-a/3-(2 b)/3] Gamma[-(a/3)-(2 b)/3+c] Pochhammer[1/3,m] Pochhammer[2/3,m] Pochhammer[1/3 (3-a-2 b),-((2 n)/3)] Pochhammer[(a-b)/3,-(n/3)] Pochhammer[1/3 (1+a-b),-(n/3)] Pochhammer[1/3 (2+a-b),-(n/3)] Pochhammer[1-a+b,n] Pochhammer[1/3 (a+2 b),(2 n)/3] Pochhammer[1/6 (3+a+2 b-3 c),-(n/6)] Pochhammer[1/3 (3+a+2 b-3 c),-(n/3)] Pochhammer[1/6 (6+a+2 b-3 c),-(n/6)] Pochhammer[-(a/3)-(2 b)/3+c,n/3] Pochhammer[1/6 (-a-2 b+3 c),m-n/3] Pochhammer[1/6 (-a-2 b+3 c),n/6] Pochhammer[1/6 (3-a-2 b+3 c),m-n/3] Pochhammer[1/6 (3-a-2 b+3 c),n/6])-(2 (-1)^n 3^(-(1/2)-a+b-3 m+n) \[Pi] (1-y)^(-1-b-n) (1/(x (-1+y)))^m (x-x y)^(1/3 (2-a+b)+n/3) Gamma[1-b] Gamma[a-b] Gamma[c] Pochhammer[1/3 (1+a-b),m-n/3] Pochhammer[1/3 (1+a+2 b),m+(2 n)/3] Pochhammer[1/6 (1+a+2 b-3 c),n/3] Pochhammer[1/6 (4+a+2 b-3 c),n/3] Pochhammer[1/3 (4+a+2 b-3 c),m-n/3] Pochhammer[1/2 (1+a-c),-(n/2)] Pochhammer[1/2 (2+a-c),-(n/2)] Pochhammer[-a+c,n] Pochhammer[1/6 (2-a-2 b+3 c),m+n/6] Pochhammer[1/6 (2-a-2 b+3 c),-(n/3)] Pochhammer[1/6 (5-a-2 b+3 c),m+n/6] Pochhammer[1/6 (5-a-2 b+3 c),-(n/3)])/(x m! n! Gamma[a] Gamma[1/3 (2-a-2 b)] Gamma[(a-b)/3] Gamma[1/3 (2+a-b)] Gamma[1/3 (-1-a-2 b)+c] Pochhammer[2/3,m] Pochhammer[4/3,m] Pochhammer[1/3 (2-a-2 b),-((2 n)/3)] Pochhammer[(a-b)/3,-(n/3)] Pochhammer[1/3 (1+a-b),-(n/3)] Pochhammer[1/3 (2+a-b),-(n/3)] Pochhammer[1-a+b,n] Pochhammer[1/3 (1+a+2 b),(2 n)/3] Pochhammer[1/6 (1+a+2 b-3 c),-(n/6)] Pochhammer[1/6 (4+a+2 b-3 c),-(n/6)] Pochhammer[1/3 (4+a+2 b-3 c),-(n/3)] Pochhammer[1/3 (-1-a-2 b+3 c),n/3] Pochhammer[1/6 (2-a-2 b+3 c),m-n/3] Pochhammer[1/6 (2-a-2 b+3 c),n/6] Pochhammer[1/6 (5-a-2 b+3 c),m-n/3] Pochhammer[1/6 (5-a-2 b+3 c),n/6])+((-1)^n 3^(-(1/2)-a+b-3 m+n) \[Pi] (1-y)^(-1-b-n) (1/(x (-1+y)))^m (x-x y)^(1/3 (1-a+b)+n/3) Gamma[1-b] Gamma[a-b] Gamma[c] Pochhammer[1/3 (2+a-b),m-n/3] Pochhammer[1/3 (2+a+2 b),m+(2 n)/3] Pochhammer[1/6 (-1+a+2 b-3 c),n/3] Pochhammer[1/6 (2+a+2 b-3 c),n/3] Pochhammer[1/3 (5+a+2 b-3 c),m-n/3] Pochhammer[1/2 (1+a-c),-(n/2)] Pochhammer[1/2 (2+a-c),-(n/2)] Pochhammer[-a+c,n] Pochhammer[1/6 (4-a-2 b+3 c),m+n/6] Pochhammer[1/6 (4-a-2 b+3 c),-(n/3)] Pochhammer[1/6 (7-a-2 b+3 c),m+n/6] Pochhammer[1/6 (7-a-2 b+3 c),-(n/3)])/(x m! n! Gamma[a] Gamma[1/3 (1-a-2 b)] Gamma[(a-b)/3] Gamma[1/3 (1+a-b)] Gamma[-(a/3)-(2 (1+b))/3+c] Pochhammer[4/3,m] Pochhammer[5/3,m] Pochhammer[1/3 (1-a-2 b),-((2 n)/3)] Pochhammer[(a-b)/3,-(n/3)] Pochhammer[1/3 (1+a-b),-(n/3)] Pochhammer[1/3 (2+a-b),-(n/3)] Pochhammer[1-a+b,n] Pochhammer[1/3 (2+a+2 b),(2 n)/3] Pochhammer[1/6 (-1+a+2 b-3 c),-(n/6)] Pochhammer[1/6 (2+a+2 b-3 c),-(n/6)] Pochhammer[1/3 (5+a+2 b-3 c),-(n/3)] Pochhammer[1/3 (-2-a-2 b+3 c),n/3] Pochhammer[1/6 (4-a-2 b+3 c),m-n/3] Pochhammer[1/6 (4-a-2 b+3 c),n/6] Pochhammer[1/6 (7-a-2 b+3 c),m-n/3] Pochhammer[1/6 (7-a-2 b+3 c),n/6])},{Abs[x]<16 Abs[x]^2&&Abs[x]<16 Abs[x]^2 (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Min", "[", 
RowBox[{
FractionBox["32", 
RowBox[{"16", "+", 
RowBox[{"9", " ", 
RowBox[{"(", 
RowBox[{"8", "-", 
RowBox[{"3", " ", 
RowBox[{"Abs", "[", 
RowBox[{"4", "-", 
RowBox[{"4", " ", "y"}]}], "]"}]}]}], ")"}], " ", 
RowBox[{"Abs", "[", 
RowBox[{"4", "-", 
RowBox[{"4", " ", "y"}]}], "]"}]}], "+", 
SqrtBox[
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "4"}], "+", 
RowBox[{"Abs", "[", 
RowBox[{"4", "-", 
RowBox[{"4", " ", "y"}]}], "]"}]}], ")"}], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "4"}], "+", 
RowBox[{"9", " ", 
RowBox[{"Abs", "[", 
RowBox[{"4", "-", 
RowBox[{"4", " ", "y"}]}], "]"}]}]}], ")"}], "3"]}]]}]], ",", 
FractionBox[
RowBox[{"16", "-", 
RowBox[{"72", " ", 
RowBox[{"Abs", "[", 
RowBox[{"4", "-", 
RowBox[{"4", " ", "y"}]}], "]"}]}], "-", 
RowBox[{"27", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{"4", "-", 
RowBox[{"4", " ", "y"}]}], "]"}], "2"]}], "+", 
RowBox[{
SqrtBox[
RowBox[{"4", "+", 
RowBox[{"Abs", "[", 
RowBox[{"4", "-", 
RowBox[{"4", " ", "y"}]}], "]"}]}]], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"4", "+", 
RowBox[{"9", " ", 
RowBox[{"Abs", "[", 
RowBox[{"4", "-", 
RowBox[{"4", " ", "y"}]}], "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]}]}], 
RowBox[{"128", " ", 
RowBox[{"Abs", "[", 
RowBox[{"4", "-", 
RowBox[{"4", " ", "y"}]}], "]"}]}]]}], "]"}], 
RowBox[{
RowBox[{"9", " ", 
RowBox[{"Abs", "[", 
RowBox[{"4", "-", 
RowBox[{"4", " ", "y"}]}], "]"}]}], "<", "4"}]},
{
FractionBox[
RowBox[{"16", "-", 
RowBox[{"72", " ", 
RowBox[{"Abs", "[", 
RowBox[{"4", "-", 
RowBox[{"4", " ", "y"}]}], "]"}]}], "-", 
RowBox[{"27", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{"4", "-", 
RowBox[{"4", " ", "y"}]}], "]"}], "2"]}], "+", 
RowBox[{
SqrtBox[
RowBox[{"4", "+", 
RowBox[{"Abs", "[", 
RowBox[{"4", "-", 
RowBox[{"4", " ", "y"}]}], "]"}]}]], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"4", "+", 
RowBox[{"9", " ", 
RowBox[{"Abs", "[", 
RowBox[{"4", "-", 
RowBox[{"4", " ", "y"}]}], "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]}]}], 
RowBox[{"128", " ", 
RowBox[{"Abs", "[", 
RowBox[{"4", "-", 
RowBox[{"4", " ", "y"}]}], "]"}]}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\))&&Abs[4-4 y]<4&&Abs[-1+y]<1&&Abs[-1+y]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Min", "[", 
RowBox[{
RowBox[{"-", 
FractionBox[
RowBox[{"1", "+", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]], "-", 
RowBox[{"18", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], 
RowBox[{"54", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]]}], ",", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "-", 
RowBox[{"18", " ", 
RowBox[{"Abs", "[", "x", "]"}]}], "+", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]}], 
RowBox[{"54", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]]}], "]"}], 
RowBox[{
RowBox[{"12", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}], "<", 
RowBox[{"Abs", "[", "x", "]"}]}]},
{
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "-", 
RowBox[{"18", " ", 
RowBox[{"Abs", "[", "x", "]"}]}], "+", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]}], 
RowBox[{"54", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\))&&(9 Abs[-1+y])/Sqrt[Abs[x]]<4 Sqrt[3]&&Abs[(-1+y)^2/x]<16 (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Min", "[", 
RowBox[{
FractionBox[
RowBox[{"1", "+", 
RowBox[{"36", " ", 
RowBox[{"Abs", "[", "x", "]"}]}], "+", 
RowBox[{"378", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}], "+", 
RowBox[{"864", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}], "-", 
RowBox[{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]], " ", 
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"18", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}]}]}], 
RowBox[{"23328", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}]], ",", 
FractionBox[
RowBox[{"1", "+", 
SqrtBox[
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]], "-", 
RowBox[{"6", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{"6", "+", 
RowBox[{"5", " ", 
SqrtBox[
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}], "-", 
RowBox[{"9", " ", 
RowBox[{"(", 
RowBox[{"7", "+", 
RowBox[{"4", " ", 
SqrtBox[
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}]}], ")"}], " ", 
RowBox[{"Abs", "[", "x", "]"}]}], "+", 
RowBox[{"144", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}]}], ")"}]}]}], 
RowBox[{"23328", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}]]}], "]"}], 
RowBox[{
RowBox[{"12", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}], "<", 
RowBox[{"Abs", "[", "x", "]"}]}]},
{
FractionBox[
RowBox[{"1", "+", 
RowBox[{"36", " ", 
RowBox[{"Abs", "[", "x", "]"}]}], "+", 
RowBox[{"378", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}], "+", 
RowBox[{"864", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}], "-", 
RowBox[{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]], " ", 
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"18", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}]}]}], 
RowBox[{"23328", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\))&&Abs[(-1+y)^2/x]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Min", "[", 
RowBox[{
FractionBox[
RowBox[{"1", "+", 
SqrtBox[
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]], "-", 
RowBox[{"6", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{"6", "+", 
RowBox[{"5", " ", 
SqrtBox[
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}], "-", 
RowBox[{"9", " ", 
RowBox[{"(", 
RowBox[{"7", "+", 
RowBox[{"4", " ", 
SqrtBox[
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}]}], ")"}], " ", 
RowBox[{"Abs", "[", "x", "]"}]}], "+", 
RowBox[{"144", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}]}], ")"}]}]}], 
RowBox[{"1458", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}]], ",", 
RowBox[{"-", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]], "-", 
RowBox[{"6", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{"6", "+", 
RowBox[{"144", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}], "-", 
RowBox[{"5", " ", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}], "+", 
RowBox[{"9", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{"7", "-", 
RowBox[{"4", " ", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}]}], ")"}]}]}], ")"}]}]}], 
RowBox[{"1458", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}]]}]}], "]"}], 
RowBox[{
RowBox[{"12", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}], "<", 
RowBox[{"Abs", "[", "x", "]"}]}]},
{
RowBox[{"-", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]], "-", 
RowBox[{"6", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{"6", "+", 
RowBox[{"144", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}], "-", 
RowBox[{"5", " ", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}], "+", 
RowBox[{"9", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{"7", "-", 
RowBox[{"4", " ", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}]}], ")"}]}]}], ")"}]}]}], 
RowBox[{"1458", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}]]}], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)),-(((-1)^n 2^(-2-2 a+2 c-4 m) Sqrt[\[Pi]] (-x)^(1/2-a/2+c/2) x^(-1-m) (1-y)^n Gamma[1-b] Gamma[1+a+b-c] Gamma[1/2 (-1+c)] Gamma[c/2] Gamma[c] Gamma[-a-b+c] Pochhammer[a,n] Pochhammer[1/2 (3+a+2 b-3 c),m] Pochhammer[(3-c)/2,-(n/2)] Pochhammer[1/2 (1+a-c),m] Pochhammer[1/2 (1+a+2 b-c),m+n] Pochhammer[1-c/2,-(n/2)] Pochhammer[1/2 (-1+c),n/2] Pochhammer[c/2,n/2] Pochhammer[1/2 (1-a-2 b+c),m-n])/(m! n! Gamma[a/2] Gamma[(1+a)/2] Gamma[1/2 (2+a-c)] Gamma[1/2 (1+a+2 b-c)] Gamma[-a+c] Gamma[1/2 (1-a-2 b+c)] Gamma[1/2 (-1-a-2 b+3 c)] Pochhammer[1/2,m] Pochhammer[a/2,n/2] Pochhammer[(1+a)/2,n/2] Pochhammer[(3-c)/2,m-n/2] Pochhammer[1-c/2,m-n/2]))+((-1)^n 2^(-3-2 a+2 c-4 m) Sqrt[\[Pi]] (-x)^(-(a/2)+c/2) x^(-1-m) (1-y)^n Gamma[1-b] Gamma[1+a+b-c] Gamma[1/2 (-2+c)] Gamma[1/2 (-1+c)] Gamma[c] Gamma[-a-b+c] Pochhammer[a,n] Pochhammer[1/2 (4+a+2 b-3 c),m] Pochhammer[(3-c)/2,-(n/2)] Pochhammer[1/2 (2+a-c),m] Pochhammer[1/2 (2+a+2 b-c),m+n] Pochhammer[2-c/2,-(n/2)] Pochhammer[1/2 (-2+c),n/2] Pochhammer[1/2 (-1+c),n/2] Pochhammer[1/2 (2-a-2 b+c),m-n])/(m! n! Gamma[a/2] Gamma[(1+a)/2] Gamma[1/2 (1+a-c)] Gamma[1/2 (a+2 b-c)] Gamma[-a+c] Gamma[1/2 (-a-2 b+c)] Gamma[1/2 (-2-a-2 b+3 c)] Pochhammer[3/2,m] Pochhammer[a/2,n/2] Pochhammer[(1+a)/2,n/2] Pochhammer[(3-c)/2,m-n/2] Pochhammer[2-c/2,m-n/2])+((-1)^n 2^(-2 a-4 m-2 n) Sqrt[\[Pi]] (-x)^(-(a/2)-n/2) x^-m (1-y)^n Gamma[1-b] Gamma[(1-c)/2] Gamma[1+a+b-c] Gamma[1-c/2] Gamma[c] Gamma[-a-b+c] Pochhammer[a/2,m+n/2] Pochhammer[a,n] Pochhammer[a/2+b,m+(3 n)/2] Pochhammer[(1-c)/2,-(n/2)] Pochhammer[1+a/2+b-c,m+n/2] Pochhammer[1-c/2,-(n/2)] Pochhammer[c/2,n/2] Pochhammer[(1+c)/2,n/2] Pochhammer[-(a/2)-b+c,m-n/2])/(m! n! Gamma[(1+a)/2] Gamma[1-a/2-b] Gamma[1/2 (1+a-c)] Gamma[1/2 (2+a-c)] Gamma[1+a/2+b-c] Gamma[-a+c] Gamma[-(a/2)-b+c] Pochhammer[1/2,m] Pochhammer[a/2,n/2] Pochhammer[(1+a)/2,n/2] Pochhammer[1-a/2-b,-((3 n)/2)] Pochhammer[a/2+b,(3 n)/2] Pochhammer[1+a/2+b-c,n/2]^2 Pochhammer[c/2,m+n/2] Pochhammer[(1+c)/2,m+n/2] Pochhammer[-(a/2)-b+c,-(n/2)]^2)-((-1)^m 2^(-2+2 a+4 b-2 c+2 m-4 n) Sqrt[\[Pi]] (-x)^(1/2+a/2+b-c/2+m) x^(-1-n) (1-y)^(-a-b+c) ((1-y)/x)^m (x/(-1+y))^(-a-b+c) Gamma[a+b-c] Gamma[1/2 (-1+c)] Gamma[c/2] Gamma[c] Gamma[1-a-2 b+c] Gamma[1-a-b+c] Gamma[-a-2 b+2 c] Pochhammer[1/2 (3+a+2 b-3 c),n] Pochhammer[(3-c)/2,-(m/2)] Pochhammer[1/2 (1+a-c),n] Pochhammer[1/2 (1+a+2 b-c),m+n] Pochhammer[1-c/2,-(m/2)] Pochhammer[1/2 (-1+c),m/2] Pochhammer[c/2,m/2] Pochhammer[1/2 (1-a-2 b+c),-m+n])/(m! n! Gamma[a] Gamma[b] Gamma[1/2 (1-a+c)] Gamma[1/2 (1-a-2 b+c)] Gamma[1/2 (2-a-2 b+c)] Gamma[-(a/2)-b+c] Gamma[1/2-a/2-b+c] Gamma[1/2 (-1-a-2 b+3 c)] Pochhammer[1/2,n] Pochhammer[1+a+2 b-2 c,m] Pochhammer[(3-c)/2,-(m/2)+n] Pochhammer[1-c/2,-(m/2)+n] Pochhammer[-(a/2)-b+c,-(m/2)] Pochhammer[1/2-a/2-b+c,-(m/2)])+((-1)^m 2^(-3+2 a+4 b-2 c+2 m-4 n) Sqrt[\[Pi]] (-x)^(a/2+b-c/2+m) x^(-1-n) (1-y)^(-a-b+c) ((1-y)/x)^m (x/(-1+y))^(-a-b+c) Gamma[a+b-c] Gamma[1/2 (-2+c)] Gamma[1/2 (-1+c)] Gamma[c] Gamma[1-a-2 b+c] Gamma[1-a-b+c] Gamma[-a-2 b+2 c] Pochhammer[1/2 (4+a+2 b-3 c),n] Pochhammer[(3-c)/2,-(m/2)] Pochhammer[1/2 (2+a-c),n] Pochhammer[1/2 (2+a+2 b-c),m+n] Pochhammer[2-c/2,-(m/2)] Pochhammer[1/2 (-2+c),m/2] Pochhammer[1/2 (-1+c),m/2] Pochhammer[1/2 (2-a-2 b+c),-m+n])/(m! n! Gamma[a] Gamma[b] Gamma[1/2 (-a+c)] Gamma[1/2 (-a-2 b+c)] Gamma[1/2 (1-a-2 b+c)] Gamma[-(a/2)-b+c] Gamma[1/2-a/2-b+c] Gamma[1/2 (-2-a-2 b+3 c)] Pochhammer[3/2,n] Pochhammer[1+a+2 b-2 c,m] Pochhammer[(3-c)/2,-(m/2)+n] Pochhammer[2-c/2,-(m/2)+n] Pochhammer[-(a/2)-b+c,-(m/2)] Pochhammer[1/2-a/2-b+c,-(m/2)])+(2^(2 a+4 b-4 c-4 n) Sqrt[\[Pi]] (-x)^(a/2+b-c+m/2) x^-n (1-y)^(-a-b+c) ((1-y)/x)^m (x/(-1+y))^(-a-b+c) Gamma[(1-c)/2] Gamma[a+b-c] Gamma[1-c/2] Gamma[c] Gamma[1-a-2 b+c] Gamma[1-a-b+c] Gamma[-a-2 b+2 c] Pochhammer[a/2,m/2+n] Pochhammer[a/2+b,(3 m)/2+n] Pochhammer[(1-c)/2,-(m/2)] Pochhammer[1+a/2+b-c,m/2+n] Pochhammer[1-c/2,-(m/2)] Pochhammer[c/2,m/2] Pochhammer[(1+c)/2,m/2] Pochhammer[-(a/2)-b+c,-(m/2)+n])/(m! n! Gamma[1-a/2] Gamma[a] Gamma[1-a/2-b] Gamma[b] Gamma[1/2 (1-a-2 b+c)] Gamma[1/2 (2-a-2 b+c)] Gamma[-(a/2)-b+c] Gamma[1/2-a/2-b+c] Pochhammer[1/2,n] Pochhammer[1-a/2,-(m/2)] Pochhammer[a/2,m/2] Pochhammer[1-a/2-b,-((3 m)/2)] Pochhammer[a/2+b,(3 m)/2] Pochhammer[1+a+2 b-2 c,m] Pochhammer[1+a/2+b-c,m/2] Pochhammer[c/2,m/2+n] Pochhammer[(1+c)/2,m/2+n] Pochhammer[-(a/2)-b+c,-(m/2)]^2 Pochhammer[1/2-a/2-b+c,-(m/2)])+((-1)^n 2^(-1-2 a-4 m-2 n) Sqrt[\[Pi]] (-x)^(1/2-a/2-n/2) x^(-1-m) (1-y)^n Gamma[1-b] Gamma[(1-c)/2] Gamma[1+a+b-c] Gamma[-(c/2)] Gamma[c] Gamma[-a-b+c] Pochhammer[a,n] Pochhammer[(1+a)/2,m+n/2] Pochhammer[1/2 (1+a+2 b),m+(3 n)/2] Pochhammer[1/2 (3+a+2 b-2 c),m+n/2] Pochhammer[(1-c)/2,-(n/2)] Pochhammer[-(c/2),-(n/2)] Pochhammer[(1+c)/2,n/2] Pochhammer[(2+c)/2,n/2] Pochhammer[1/2-a/2-b+c,m-n/2])/(m! n! Gamma[a/2] Gamma[1/2 (1-a-2 b)] Gamma[1/2 (1+a+2 b-2 c)] Gamma[1/2 (1+a-c)] Gamma[1/2 (2+a-c)] Gamma[-a+c] Gamma[-(1/2)-a/2-b+c] Pochhammer[3/2,m] Pochhammer[a/2,n/2] Pochhammer[(1+a)/2,n/2] Pochhammer[1/2 (1-a-2 b),-((3 n)/2)] Pochhammer[1/2 (1+a+2 b),(3 n)/2] Pochhammer[1/2 (1+a+2 b-2 c),n/2] Pochhammer[1/2 (3+a+2 b-2 c),n/2] Pochhammer[(1+c)/2,m+n/2] Pochhammer[(2+c)/2,m+n/2] Pochhammer[-(1/2)-a/2-b+c,-(n/2)] Pochhammer[1/2-a/2-b+c,-(n/2)])+(2^(-1+2 a+4 b-4 c-4 n) Sqrt[\[Pi]] (-x)^(1/2+a/2+b-c+m/2) x^(-1-n) (1-y)^(-a-b+c) ((1-y)/x)^m (x/(-1+y))^(-a-b+c) Gamma[(1-c)/2] Gamma[a+b-c] Gamma[-(c/2)] Gamma[c] Gamma[1-a-2 b+c] Gamma[1-a-b+c] Gamma[-a-2 b+2 c] Pochhammer[(1+a)/2,m/2+n] Pochhammer[1/2 (1+a+2 b),(3 m)/2+n] Pochhammer[1/2 (3+a+2 b-2 c),m/2+n] Pochhammer[(1-c)/2,-(m/2)] Pochhammer[-(c/2),-(m/2)] Pochhammer[(1+c)/2,m/2] Pochhammer[(2+c)/2,m/2] Pochhammer[1/2-a/2-b+c,-(m/2)+n])/(m! n! Gamma[(1-a)/2] Gamma[a] Gamma[1/2 (1-a-2 b)] Gamma[b] Gamma[1/2 (1-a-2 b+c)] Gamma[1/2 (2-a-2 b+c)] Gamma[-(1/2)-a/2-b+c] Gamma[-(a/2)-b+c] Pochhammer[3/2,n] Pochhammer[(1-a)/2,-(m/2)] Pochhammer[(1+a)/2,m/2] Pochhammer[1/2 (1-a-2 b),-((3 m)/2)] Pochhammer[1/2 (1+a+2 b),(3 m)/2] Pochhammer[1+a+2 b-2 c,m] Pochhammer[1/2 (3+a+2 b-2 c),m/2] Pochhammer[(1+c)/2,m/2+n] Pochhammer[(2+c)/2,m/2+n] Pochhammer[-(1/2)-a/2-b+c,-(m/2)] Pochhammer[-(a/2)-b+c,-(m/2)] Pochhammer[1/2-a/2-b+c,-(m/2)])},{Abs[x]<16 Abs[x]^2&&1/Abs[1-y]<1&&(9 Abs[-1+y])/Sqrt[Abs[x]]<4 Sqrt[3]&&Abs[(-1+y)^2/x]<16 (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Min", "[", 
RowBox[{
FractionBox[
RowBox[{"1", "+", 
RowBox[{"36", " ", 
RowBox[{"Abs", "[", "x", "]"}]}], "+", 
RowBox[{"378", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}], "+", 
RowBox[{"864", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}], "-", 
RowBox[{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]], " ", 
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"18", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}]}]}], 
RowBox[{"23328", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}]], ",", 
FractionBox[
RowBox[{"1", "+", 
SqrtBox[
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]], "-", 
RowBox[{"6", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{"6", "+", 
RowBox[{"5", " ", 
SqrtBox[
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}], "-", 
RowBox[{"9", " ", 
RowBox[{"(", 
RowBox[{"7", "+", 
RowBox[{"4", " ", 
SqrtBox[
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}]}], ")"}], " ", 
RowBox[{"Abs", "[", "x", "]"}]}], "+", 
RowBox[{"144", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}]}], ")"}]}]}], 
RowBox[{"23328", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}]]}], "]"}], 
RowBox[{
RowBox[{"12", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}], "<", 
RowBox[{"Abs", "[", "x", "]"}]}]},
{
FractionBox[
RowBox[{"1", "+", 
RowBox[{"36", " ", 
RowBox[{"Abs", "[", "x", "]"}]}], "+", 
RowBox[{"378", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}], "+", 
RowBox[{"864", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}], "-", 
RowBox[{
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]], " ", 
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"18", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}]}]}], 
RowBox[{"23328", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\))&&Abs[(-1+y)^2/x]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Min", "[", 
RowBox[{
FractionBox[
RowBox[{"1", "+", 
SqrtBox[
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]], "-", 
RowBox[{"6", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{"6", "+", 
RowBox[{"5", " ", 
SqrtBox[
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}], "-", 
RowBox[{"9", " ", 
RowBox[{"(", 
RowBox[{"7", "+", 
RowBox[{"4", " ", 
SqrtBox[
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}]}], ")"}], " ", 
RowBox[{"Abs", "[", "x", "]"}]}], "+", 
RowBox[{"144", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}]}], ")"}]}]}], 
RowBox[{"1458", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}]], ",", 
RowBox[{"-", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]], "-", 
RowBox[{"6", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{"6", "+", 
RowBox[{"144", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}], "-", 
RowBox[{"5", " ", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}], "+", 
RowBox[{"9", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{"7", "-", 
RowBox[{"4", " ", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}]}], ")"}]}]}], ")"}]}]}], 
RowBox[{"1458", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}]]}]}], "]"}], 
RowBox[{
RowBox[{"12", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}], "<", 
RowBox[{"Abs", "[", "x", "]"}]}]},
{
RowBox[{"-", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]], "-", 
RowBox[{"6", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{"6", "+", 
RowBox[{"144", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "2"]}], "-", 
RowBox[{"5", " ", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}], "+", 
RowBox[{"9", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{"7", "-", 
RowBox[{"4", " ", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}]}], ")"}]}]}], ")"}]}]}], 
RowBox[{"1458", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], "3"]}]]}], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\))&&9 Abs[1-y]^2 (2+3 Abs[1-y])+2 Abs[(-1+y)^2/x]<Abs[1-y] (1+Sqrt[(1+Abs[1-y]) (1+9 Abs[1-y])^3]),((-1)^n 2^(-2 a-4 m-2 n) Sqrt[\[Pi]] (-x)^(-(a/2)-n/2) x^-m (1-y)^n Gamma[1-b] Gamma[(1-c)/2] Gamma[1+a+b-c] Gamma[1-c/2] Gamma[c] Gamma[-a-b+c] Pochhammer[a/2,m+n/2] Pochhammer[a,n] Pochhammer[a/2+b,m+(3 n)/2] Pochhammer[(1-c)/2,-(n/2)] Pochhammer[1+a/2+b-c,m+n/2] Pochhammer[1-c/2,-(n/2)] Pochhammer[c/2,n/2] Pochhammer[(1+c)/2,n/2] Pochhammer[-(a/2)-b+c,m-n/2])/(m! n! Gamma[(1+a)/2] Gamma[1-a/2-b] Gamma[1/2 (1+a-c)] Gamma[1/2 (2+a-c)] Gamma[1+a/2+b-c] Gamma[-a+c] Gamma[-(a/2)-b+c] Pochhammer[1/2,m] Pochhammer[a/2,n/2] Pochhammer[(1+a)/2,n/2] Pochhammer[1-a/2-b,-((3 n)/2)] Pochhammer[a/2+b,(3 n)/2] Pochhammer[1+a/2+b-c,n/2]^2 Pochhammer[c/2,m+n/2] Pochhammer[(1+c)/2,m+n/2] Pochhammer[-(a/2)-b+c,-(n/2)]^2)+(2^(2 a+4 b-4 c-4 n) Sqrt[\[Pi]] (-x)^(a/2+b-c+m/2) x^-n (1-y)^(-a-b+c) ((1-y)/x)^m (x/(-1+y))^(-a-b+c) Gamma[(1-c)/2] Gamma[a+b-c] Gamma[1-c/2] Gamma[c] Gamma[1-a-2 b+c] Gamma[1-a-b+c] Gamma[-a-2 b+2 c] Pochhammer[a/2,m/2+n] Pochhammer[a/2+b,(3 m)/2+n] Pochhammer[(1-c)/2,-(m/2)] Pochhammer[1+a/2+b-c,m/2+n] Pochhammer[1-c/2,-(m/2)] Pochhammer[c/2,m/2] Pochhammer[(1+c)/2,m/2] Pochhammer[-(a/2)-b+c,-(m/2)+n])/(m! n! Gamma[1-a/2] Gamma[a] Gamma[1-a/2-b] Gamma[b] Gamma[1/2 (1-a-2 b+c)] Gamma[1/2 (2-a-2 b+c)] Gamma[-(a/2)-b+c] Gamma[1/2-a/2-b+c] Pochhammer[1/2,n] Pochhammer[1-a/2,-(m/2)] Pochhammer[a/2,m/2] Pochhammer[1-a/2-b,-((3 m)/2)] Pochhammer[a/2+b,(3 m)/2] Pochhammer[1+a+2 b-2 c,m] Pochhammer[1+a/2+b-c,m/2] Pochhammer[c/2,m/2+n] Pochhammer[(1+c)/2,m/2+n] Pochhammer[-(a/2)-b+c,-(m/2)]^2 Pochhammer[1/2-a/2-b+c,-(m/2)])+((-1)^n 2^(-1-2 a-4 m-2 n) Sqrt[\[Pi]] (-x)^(1/2-a/2-n/2) x^(-1-m) (1-y)^n Gamma[1-b] Gamma[(1-c)/2] Gamma[1+a+b-c] Gamma[-(c/2)] Gamma[c] Gamma[-a-b+c] Pochhammer[a,n] Pochhammer[(1+a)/2,m+n/2] Pochhammer[1/2 (1+a+2 b),m+(3 n)/2] Pochhammer[1/2 (3+a+2 b-2 c),m+n/2] Pochhammer[(1-c)/2,-(n/2)] Pochhammer[-(c/2),-(n/2)] Pochhammer[(1+c)/2,n/2] Pochhammer[(2+c)/2,n/2] Pochhammer[1/2-a/2-b+c,m-n/2])/(m! n! Gamma[a/2] Gamma[1/2 (1-a-2 b)] Gamma[1/2 (1+a+2 b-2 c)] Gamma[1/2 (1+a-c)] Gamma[1/2 (2+a-c)] Gamma[-a+c] Gamma[-(1/2)-a/2-b+c] Pochhammer[3/2,m] Pochhammer[a/2,n/2] Pochhammer[(1+a)/2,n/2] Pochhammer[1/2 (1-a-2 b),-((3 n)/2)] Pochhammer[1/2 (1+a+2 b),(3 n)/2] Pochhammer[1/2 (1+a+2 b-2 c),n/2] Pochhammer[1/2 (3+a+2 b-2 c),n/2] Pochhammer[(1+c)/2,m+n/2] Pochhammer[(2+c)/2,m+n/2] Pochhammer[-(1/2)-a/2-b+c,-(n/2)] Pochhammer[1/2-a/2-b+c,-(n/2)])+(2^(-1+2 a+4 b-4 c-4 n) Sqrt[\[Pi]] (-x)^(1/2+a/2+b-c+m/2) x^(-1-n) (1-y)^(-a-b+c) ((1-y)/x)^m (x/(-1+y))^(-a-b+c) Gamma[(1-c)/2] Gamma[a+b-c] Gamma[-(c/2)] Gamma[c] Gamma[1-a-2 b+c] Gamma[1-a-b+c] Gamma[-a-2 b+2 c] Pochhammer[(1+a)/2,m/2+n] Pochhammer[1/2 (1+a+2 b),(3 m)/2+n] Pochhammer[1/2 (3+a+2 b-2 c),m/2+n] Pochhammer[(1-c)/2,-(m/2)] Pochhammer[-(c/2),-(m/2)] Pochhammer[(1+c)/2,m/2] Pochhammer[(2+c)/2,m/2] Pochhammer[1/2-a/2-b+c,-(m/2)+n])/(m! n! Gamma[(1-a)/2] Gamma[a] Gamma[1/2 (1-a-2 b)] Gamma[b] Gamma[1/2 (1-a-2 b+c)] Gamma[1/2 (2-a-2 b+c)] Gamma[-(1/2)-a/2-b+c] Gamma[-(a/2)-b+c] Pochhammer[3/2,n] Pochhammer[(1-a)/2,-(m/2)] Pochhammer[(1+a)/2,m/2] Pochhammer[1/2 (1-a-2 b),-((3 m)/2)] Pochhammer[1/2 (1+a+2 b),(3 m)/2] Pochhammer[1+a+2 b-2 c,m] Pochhammer[1/2 (3+a+2 b-2 c),m/2] Pochhammer[(1+c)/2,m/2+n] Pochhammer[(2+c)/2,m/2+n] Pochhammer[-(1/2)-a/2-b+c,-(m/2)] Pochhammer[-(a/2)-b+c,-(m/2)] Pochhammer[1/2-a/2-b+c,-(m/2)])+((-1)^n 2^(-1-2 n) (-x)^(1/2 (a+2 b-c)) x^-n (1-y)^(-a-b+c-m) (x/(-1+y))^(-1-a-b+c) (-1+y)^(1-c+2 n) Gamma[1/2 (6+a+2 b-3 c)] Gamma[1-c] Gamma[1/2 (2+a-c)] Gamma[a+b-c] Gamma[c]^2 Gamma[1-a-b+c] Pochhammer[1/2 (4+a+2 b-3 c),n] Pochhammer[1/2 (2+a-c),n] Pochhammer[-2+c,m-2 n] Pochhammer[1/2 (-2-a-2 b+3 c),m-n])/(m! n! Gamma[a] Gamma[b] Gamma[1/2 (a+2 b-3 c)] Gamma[3-c] Gamma[(a-c)/2] Gamma[1/2 (2-a+c)] Gamma[1/2 (2-a-2 b+3 c)] Pochhammer[3/2,n] Pochhammer[1/2 (-4-a-2 b+3 c),m-3 n])-((-1)^m 2^(-1-2 m) (-x)^(1/2 (-a+c)) x^(-1-m) (1-y)^-n (-1+y)^(2-c+2 m) Gamma[1-b] Gamma[1/2 (6+a+2 b-3 c)] Gamma[1-c] Gamma[1/2 (2+a-c)] Gamma[a+b-c] Gamma[c]^2 Gamma[1-a-b+c] Pochhammer[1/2 (4+a+2 b-3 c),m] Pochhammer[1/2 (2+a-c),m] Pochhammer[-2+c,-2 m+n] Pochhammer[1/2 (-2-a-2 b+3 c),-m+n])/(m! n! Gamma[a] Gamma[1/2 (a+2 b-3 c)] Gamma[3-c] Gamma[a-c] Gamma[1/2 (a+2 b-c)] Gamma[1-a+c] Gamma[1/2 (2-a-2 b+c)] Gamma[1/2 (2-a-2 b+3 c)] Pochhammer[3/2,m] Pochhammer[1/2 (-4-a-2 b+3 c),-3 m+n])-((-1)^n 2^(-1-2 n) (-x)^(1/2 (1+a+2 b-c)) x^-n (1-y)^(-a-b+c-m) (x/(-1+y))^(-1-a-b+c) (-1+y)^(-c+2 n) Cos[1/2 (a+2 b-3 c) \[Pi]] Cos[1/2 (a-c) \[Pi]] Cos[1/2 (a+2 b-c) \[Pi]] Gamma[1/2 (3+a+2 b-3 c)] Gamma[1-c] Gamma[1/2 (1+a-c)] Gamma[a+b-c] Gamma[1/2 (1+a+2 b-c)] Gamma[c]^2 Gamma[1/2 (1-a-2 b+c)] Gamma[1-a-b+c] Pochhammer[1/2 (3+a+2 b-3 c),n] Pochhammer[1/2 (1+a-c),n] Pochhammer[-1+c,m-2 n] Pochhammer[1/2 (-1-a-2 b+3 c),m-n])/(\[Pi]^3 m! n! Gamma[a] Gamma[b] Gamma[2-c] Pochhammer[1/2,n] Pochhammer[1/2 (-1-a-2 b+3 c),m-3 n])-((-1)^m 2^(-1-2 m) (-x)^(1/2 (1-a+c)) x^(-1-m) (1-y)^-n (-1+y)^(1-c+2 m) Cos[1/2 (a+2 b-3 c) \[Pi]] Cos[1/2 (a+2 b-c) \[Pi]]^2 Gamma[1-b] Gamma[1/2 (3+a+2 b-3 c)] Gamma[1-c] Gamma[1/2 (1+a-c)] Gamma[a+b-c] Gamma[1/2 (1+a+2 b-c)] Gamma[c]^2 Gamma[1/2 (1-a-2 b+c)] Gamma[1-a-b+c] Pochhammer[1/2 (3+a+2 b-3 c),m] Pochhammer[1/2 (1+a-c),m] Pochhammer[-1+c,-2 m+n] Pochhammer[1/2 (-1-a-2 b+3 c),-m+n])/(\[Pi]^3 m! n! Gamma[a] Gamma[2-c] Gamma[a-c] Gamma[1-a+c] Pochhammer[1/2,m] Pochhammer[1/2 (-1-a-2 b+3 c),-3 m+n])},{4 Abs[1/4+x]<1&&Abs[y]/8<(2+2 (1-3 Abs[1/4+x])^(3/2)-9 Abs[1/4+x])/(27-108 Abs[1/4+x])&&8 Abs[y/(1+4 x)^2]<1&&(2 (2 (1+Sqrt[1+3 Abs[1/4+x]])+Abs[1/4+x] (9+6 Sqrt[1+3 Abs[1/4+x]])) Abs[y])/Abs[1+4 x]^2<1,-(((-1)^(-2 n) 2^(-1+a-2 m+n) Sqrt[\[Pi]] (1+4 x)^(1/2-a-b+m-2 n) y^n Gamma[1-b] Pochhammer[1-a-2 b,2 m-3 n] Pochhammer[a+2 b,3 n] Sec[(a+b) \[Pi]])/(m! n! Gamma[a] Gamma[3/2-a-b] Pochhammer[3/2-a-b,m-2 n] Pochhammer[c,n]))+((-1)^(-2 n) 2^(-a-2 (b+m)-3 n) Sqrt[\[Pi]] (1+4 x)^m y^n Gamma[1-b] Pochhammer[a,2 m+n] Pochhammer[a+2 b,3 n] Sec[(a+b) \[Pi]])/(m! n! Gamma[1-a-2 b] Gamma[1/2+a+b] Pochhammer[1/2+a+b,m+2 n] Pochhammer[c,n])},{Abs[1/4+x]<1/4&&Abs[y]/8<4/27&&Abs[y]/8<(2+2 (1-3 Abs[1/4+x])^(3/2)-9 Abs[1/4+x])/(27-108 Abs[1/4+x])&&Sqrt[2] Abs[(1+4 x) Sqrt[y/(1+4 x)^2]]<8/(3 Sqrt[3])&&1/8 Abs[(1+4 x)^2/y]<1&&1/8 Abs[(1+4 x)^2/y]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Min", "[", 
RowBox[{
RowBox[{
FractionBox["1", "2048"], 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "2048"}], "+", 
RowBox[{"12096", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}], "-", 
RowBox[{"15552", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "4"]}], "+", 
RowBox[{"5832", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "6"]}], "+", 
RowBox[{"9", " ", 
SqrtBox["2"], " ", 
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], " ", 
RowBox[{"(", 
RowBox[{"8", "-", 
RowBox[{"6", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "16"}], "+", 
RowBox[{"18", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]}]}], ")"}]}], ",", 
RowBox[{
FractionBox["1", "2048"], 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "2048"}], "+", 
RowBox[{"12096", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}], "-", 
RowBox[{"15552", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "4"]}], "+", 
RowBox[{"5832", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "6"]}], "+", 
RowBox[{"9", " ", 
SqrtBox["2"], " ", 
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "8"}], "+", 
RowBox[{"6", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "16"}], "+", 
RowBox[{"18", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]}]}], ")"}]}], ",", 
RowBox[{
FractionBox["1", "2048"], 
RowBox[{"(", 
RowBox[{"2048", "+", 
RowBox[{"12096", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}], "+", 
RowBox[{"15552", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "4"]}], "+", 
RowBox[{"5832", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "6"]}], "-", 
RowBox[{"9", " ", 
SqrtBox["2"], " ", 
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], " ", 
RowBox[{"(", 
RowBox[{"8", "+", 
RowBox[{"6", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"16", "+", 
RowBox[{"18", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]}]}], ")"}]}]}], "]"}], 
RowBox[{
FractionBox["4", "3"], "<", 
RowBox[{
SqrtBox["2"], " ", 
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}]}]}]},
{
RowBox[{
FractionBox["1", "2048"], 
RowBox[{"(", 
RowBox[{"2048", "+", 
RowBox[{"12096", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}], "+", 
RowBox[{"15552", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "4"]}], "+", 
RowBox[{"5832", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "6"]}], "-", 
RowBox[{"9", " ", 
SqrtBox["2"], " ", 
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], " ", 
RowBox[{"(", 
RowBox[{"8", "+", 
RowBox[{"6", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"16", "+", 
RowBox[{"18", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]}]}], ")"}]}], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\))&&2 Sqrt[2] Abs[(1+4 x) Sqrt[y/(1+4 x)^2]]<16/(3 Sqrt[3])&&1/8 Abs[(1+4 x)^2/y]<1&&1/8 Abs[(1+4 x)^2/y]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Min", "[", 
RowBox[{
RowBox[{
FractionBox["1", "131072"], 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "131072"}], "+", 
RowBox[{"774144", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}], "-", 
RowBox[{"995328", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "4"]}], "+", 
RowBox[{"373248", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "6"]}], "+", 
RowBox[{"18", " ", 
SqrtBox["2"], " ", 
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], " ", 
RowBox[{"(", 
RowBox[{"32", "-", 
RowBox[{"24", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "64"}], "+", 
RowBox[{"72", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]}]}], ")"}]}], ",", 
RowBox[{
FractionBox["1", "131072"], 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "131072"}], "+", 
RowBox[{"774144", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}], "-", 
RowBox[{"995328", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "4"]}], "+", 
RowBox[{"373248", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "6"]}], "+", 
RowBox[{"18", " ", 
SqrtBox["2"], " ", 
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "32"}], "+", 
RowBox[{"24", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "64"}], "+", 
RowBox[{"72", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]}]}], ")"}]}], ",", 
RowBox[{
FractionBox["1", "131072"], 
RowBox[{"(", 
RowBox[{"131072", "+", 
RowBox[{"774144", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}], "+", 
RowBox[{"995328", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "4"]}], "+", 
RowBox[{"373248", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "6"]}], "-", 
RowBox[{"18", " ", 
SqrtBox["2"], " ", 
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], " ", 
RowBox[{"(", 
RowBox[{"32", "+", 
RowBox[{"24", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"64", "+", 
RowBox[{"72", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]}]}], ")"}]}]}], "]"}], 
RowBox[{
FractionBox["8", "3"], "<", 
RowBox[{"2", " ", 
SqrtBox["2"], " ", 
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}]}]}]},
{
RowBox[{
FractionBox["1", "131072"], 
RowBox[{"(", 
RowBox[{"131072", "+", 
RowBox[{"774144", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}], "+", 
RowBox[{"995328", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "4"]}], "+", 
RowBox[{"373248", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "6"]}], "-", 
RowBox[{"18", " ", 
SqrtBox["2"], " ", 
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], " ", 
RowBox[{"(", 
RowBox[{"32", "+", 
RowBox[{"24", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"64", "+", 
RowBox[{"72", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
RowBox[{
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], " ", 
SqrtBox[
FractionBox["y", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}], ")"}], "2"]]]}], "]"}], "2"]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]}]}], ")"}]}], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)),((-1)^(-2 n) 2^(-a-2 (b+m)-3 n) Sqrt[\[Pi]] (1+4 x)^m y^n Gamma[1-b] Pochhammer[a,2 m+n] Pochhammer[a+2 b,3 n] Sec[(a+b) \[Pi]])/(m! n! Gamma[1-a-2 b] Gamma[1/2+a+b] Pochhammer[1/2+a+b,m+2 n] Pochhammer[c,n])+((-1)^(2 m) 2^(1/4 (-3-2 a-6 b+6 m-12 n)) \[Pi] (1+4 x)^(5/2-a-b+m) (-((1+4 x)^2/y))^n (y/(1+4 x)^2)^(1/4 (3-2 a-2 b+2 m)) Gamma[1-b] Gamma[c] Pochhammer[1/2 (1-a-2 b),m] Pochhammer[1/12 (3+2 a-2 b),m/6] Pochhammer[1/12 (3+2 a-2 b),-(m/2)+n] Pochhammer[1/4 (3+2 a-2 b),1/2 (m+6 n)] Pochhammer[1/12 (7+2 a-2 b),-(m/2)] Pochhammer[1/12 (11+2 a-2 b),-(m/2)] Pochhammer[1/12 (15+2 a-2 b),-(m/2)] Pochhammer[1/12 (15+2 a-2 b),m/6+n] Pochhammer[1-a/2-b,m] Pochhammer[1/12 (-3-2 a+2 b),m/2] Pochhammer[1/12 (1-2 a+2 b),m/2] Pochhammer[1/12 (5-2 a+2 b),m/2] Pochhammer[1/12 (9-2 a+2 b),-(m/6)] Pochhammer[1/4 (1+2 a+2 b),-(m/2)+n] Pochhammer[1/4 (5+2 a+2 b-4 c),-(m/2)+n] Sec[(a+b) \[Pi]])/(y m! n! Gamma[a] Gamma[3/2-a-b] Gamma[1/4 (-1+2 a+2 b)] Gamma[1/4 (-1-2 a-2 b+4 c)] Pochhammer[3/2,n] Pochhammer[1-a-2 b,2 m] Pochhammer[1/12 (3+2 a-2 b),m/6+n] Pochhammer[1/4 (3+2 a-2 b),m/2] Pochhammer[1/4 (3+2 a-2 b),-((3 m)/2)+3 n] Pochhammer[1/12 (15+2 a-2 b),m/6] Pochhammer[1/12 (15+2 a-2 b),-(m/2)+n] Pochhammer[3/2-a-b,m] Pochhammer[1/12 (-3-2 a+2 b),-(m/6)] Pochhammer[1/4 (1-2 a+2 b),-(m/2)] Pochhammer[1/4 (-1+2 a+2 b),-(m/2)] Pochhammer[1/4 (1+2 a+2 b),-(m/2)] Pochhammer[1/4 (5+2 a+2 b-4 c),-(m/2)] Pochhammer[1/4 (-1-2 a-2 b+4 c),m/2])-((-1)^-m 2^(1/4 (-1-2 a-6 b+2 m-12 n)) \[Pi] (1+4 x)^(1/2-a-b+m) (-((1+4 x)^2/y))^n (y/(1+4 x)^2)^(1/4 (1-2 a-2 b+2 m)) Gamma[1-b] Gamma[c] Pochhammer[1/2 (1-a-2 b),m] Pochhammer[1/4 (1+2 a-2 b),-((3 m)/2)] Pochhammer[1/4 (1+2 a-2 b),1/2 (m+6 n)] Pochhammer[1-a/2-b,m] Pochhammer[1/4 (3-2 a+2 b),(3 m)/2] Pochhammer[a+2 b,-2 m] Pochhammer[1/4 (-1+2 a+2 b),-(m/2)+n] Pochhammer[1/4 (3+2 a+2 b-4 c),-(m/2)+n] Sec[(a+b) \[Pi]])/(m! n! Gamma[a] Gamma[3/2-a-b] Gamma[1/4 (1+2 a+2 b)] Gamma[1/4 (1-2 a-2 b+4 c)] Pochhammer[1/2,n] Pochhammer[1/4 (1+2 a-2 b),m/2] Pochhammer[1/4 (1+2 a-2 b),-((3 m)/2)+3 n] Pochhammer[1/4 (3-2 a+2 b),-(m/2)] Pochhammer[1/4 (3+2 a+2 b-4 c),-(m/2)] Pochhammer[1/4 (1-2 a-2 b+4 c),m/2])},{4 Abs[x]^2>Abs[x]&&27 Abs[y^2/x]<16&&18 Abs[x]^2 (1+2 (1+12 Abs[x])^(3/2)+81 Abs[x]^2 Abs[y^2/x])<Abs[x] (1+864 Abs[x]^2+864 Abs[x]^3+(1+12 Abs[x])^(3/2)),((-1)^-n 2^(-a-2 m-2 n) Sqrt[\[Pi]] (-x)^-m x^(-(a/2)-n) y^(2 n) Gamma[1-b] Pochhammer[a/2,m+n] Pochhammer[a/2+b,m+3 n])/(m! n! Gamma[(1+a)/2] Gamma[1-a/2-b] Pochhammer[1/2,m] Pochhammer[1/2,n] Pochhammer[c,2 n])-((-1)^-n 2^(-a+2 n-2 (m+2 n)) Sqrt[\[Pi]] (-x)^-m x^(1/2 (-1-a-2 n)) y^(2 n) Gamma[1-b] Pochhammer[(1+a)/2,m+n] Pochhammer[1/2 (1+a+2 b),m+3 n])/(m! n! Gamma[a/2] Gamma[1/2 (1-a-2 b)] Pochhammer[1/2,n] Pochhammer[3/2,m] Pochhammer[c,2 n])+((-1)^n 2^(-1-a-2 m-2 n) Sqrt[\[Pi]] (-x)^-m x^(-1-a/2-n) y^(1+2 n) Gamma[1+a] Gamma[1-b] Gamma[c] Pochhammer[(2+a)/2,m+n] Pochhammer[2+a/2+b,m+3 n])/(m! n! Gamma[a] Gamma[(1+a)/2] Gamma[-1-a/2-b] Gamma[1+c] Pochhammer[3/2,m] Pochhammer[3/2,n] Pochhammer[1+c,2 n])-((-1)^n 2^(-1-a-2 m-2 n) Sqrt[\[Pi]] (-x)^-m x^(1/2 (-1-a-2 n)) y^(1+2 n) Gamma[1+a] Gamma[1-b] Gamma[c] Pochhammer[(1+a)/2,m+n] Pochhammer[1/2 (3+a+2 b),m+3 n])/(m! n! Gamma[a] Gamma[(2+a)/2] Gamma[1/2 (-1-a-2 b)] Gamma[1+c] Pochhammer[1/2,m] Pochhammer[3/2,n] Pochhammer[1+c,2 n])},{1/Abs[1+4 x]<1&&Abs[y^2/(1+4 x)]<(4 (128+288 Abs[1+4 x]+189 Abs[1+4 x]^2+27 Abs[1+4 x]^3-2 (4+3 Abs[1+4 x])^(3/2) (8+9 Abs[1+4 x])))/(729 (1+1/Abs[1+4 x])^2 Abs[1+4 x]^3),-(((-1)^(-2 n) 2^(-1+a-2 n) \[Pi] (-1-4 x)^(-(1/2)+a/2+b+(3 n)/2) (1+4 x)^(1/2-a-b-m-2 n) y^n Gamma[1-b] Pochhammer[a/2,m+n/2] Pochhammer[1/2-a/2-b,m-(3 n)/2] Pochhammer[a+2 b,3 n] Sec[a \[Pi]+b \[Pi]])/(m! n! Gamma[1-a/2] Gamma[a] Gamma[1-a/2-b] Pochhammer[1/2,m] Pochhammer[1-a/2,-(n/2)] Pochhammer[a/2,n/2] Pochhammer[c,n]))+((-1)^(-2 n) 2^(a-2 n) \[Pi] (-1-4 x)^(-1+a/2+b+(3 n)/2) (1+4 x)^(1/2-a-b-m-2 n) y^n Gamma[1-b] Pochhammer[1/2+a/2,m+n/2] Pochhammer[1-a/2-b,m-(3 n)/2] Pochhammer[a+2 b,3 n] Sec[a \[Pi]+b \[Pi]])/(m! n! Gamma[1/2-a/2] Gamma[a] Gamma[1/2-a/2-b] Pochhammer[3/2,m] Pochhammer[1/2-a/2,-(n/2)] Pochhammer[1/2+a/2,n/2] Pochhammer[c,n])-(2^(1-a-2 b-2 n) \[Pi] (-1-4 x)^(-(1/2)-a/2-n/2) (1+4 x)^-m y^n Gamma[1-b] Pochhammer[1/2+a/2,m+n/2] Pochhammer[1-a/2-b,m-(3 n)/2] Pochhammer[a+2 b,3 n] Sec[a \[Pi]+b \[Pi]])/(m! n! Gamma[a/2] Gamma[1-a-2 b] Gamma[a/2+b] Pochhammer[3/2,m] Pochhammer[1-a/2-b,-((3 n)/2)] Pochhammer[a/2+b,(3 n)/2] Pochhammer[c,n])+(2^(-a-2 b-2 n) \[Pi] (-1-4 x)^(-(a/2)-n/2) (1+4 x)^-m y^n Gamma[1-b] Pochhammer[a/2,m+n/2] Pochhammer[1/2-a/2-b,m-(3 n)/2] Pochhammer[a+2 b,3 n] Sec[a \[Pi]+b \[Pi]])/(m! n! Gamma[1/2+a/2] Gamma[1-a-2 b] Gamma[1/2+a/2+b] Pochhammer[1/2,m] Pochhammer[1/2-a/2-b,-((3 n)/2)] Pochhammer[1/2+a/2+b,(3 n)/2] Pochhammer[c,n])},{4 Abs[x/(1+4 x)]<1&&Abs[y/Sqrt[1+4 x]]<1&&Abs[y/Sqrt[1+4 x]]^2<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Min", "[", 
RowBox[{
FractionBox[
RowBox[{"8", "+", 
RowBox[{"48", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}], "+", 
RowBox[{"6144", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}], "2"]}], "-", 
RowBox[{"32768", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}], "3"]}], "-", 
RowBox[{"8", " ", 
SqrtBox[
RowBox[{"1", "-", 
RowBox[{"4", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}]}]], " ", 
SqrtBox[
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
RowBox[{"16", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}]}], ")"}], "3"]}]], " ", 
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"32", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}]}], ")"}]}]}], 
RowBox[{"11664", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}], "2"]}]], ",", 
FractionBox[
RowBox[{"8", "+", 
RowBox[{"48", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}], "+", 
RowBox[{"6144", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}], "2"]}], "-", 
RowBox[{"32768", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}], "3"]}], "+", 
RowBox[{"8", " ", 
SqrtBox[
RowBox[{"1", "-", 
RowBox[{"4", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}]}]], " ", 
SqrtBox[
RowBox[{"-", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
RowBox[{"16", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}]}], ")"}], "3"]}]], " ", 
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"32", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}]}], ")"}]}]}], 
RowBox[{"11664", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}], "2"]}]], ",", 
FractionBox[
RowBox[{"2", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}], "+", 
RowBox[{"1536", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}], "2"]}], "+", 
RowBox[{"8192", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}], "3"]}], "-", 
RowBox[{"2", " ", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"4", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}]}]], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"16", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]], " ", 
RowBox[{"Abs", "[", 
RowBox[{"1", "-", 
RowBox[{"32", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}]}], "]"}]}]}], 
RowBox[{"2916", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}], "2"]}]]}], "]"}], 
RowBox[{
RowBox[{"4", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}], "<", 
FractionBox["1", "4"]}]},
{
FractionBox[
RowBox[{"2", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}], "+", 
RowBox[{"1536", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}], "2"]}], "+", 
RowBox[{"8192", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}], "3"]}], "-", 
RowBox[{"2", " ", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"4", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}]}]], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "+", 
RowBox[{"16", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]], " ", 
RowBox[{"Abs", "[", 
RowBox[{"1", "-", 
RowBox[{"32", " ", 
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}]}]}], "]"}]}]}], 
RowBox[{"2916", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox["x", 
RowBox[{"1", "+", 
RowBox[{"4", " ", "x"}]}]], "]"}], "2"]}]], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)),((-1)^n 4^m (x/(1+4 x))^m (1+4 x)^(1/2 (-a-n)) y^n Pochhammer[a/2,m+n/2] Pochhammer[a,n] Pochhammer[1/2 (1-a-2 b),m-(3 n)/2])/(m! n! Pochhammer[a/2,n/2] Pochhammer[1/2 (1-a-2 b),-((3 n)/2)] Pochhammer[1-b,m-n] Pochhammer[c,n])},{4 Abs[x]<1&&Abs[y/(1+4 x)^2]<1&&Abs[y/(1+4 x)^2]<(\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Min", "[", 
RowBox[{
FractionBox[
RowBox[{"2", " ", 
RowBox[{"(", 
RowBox[{"1", "+", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]], "+", 
RowBox[{"36", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}]}], 
RowBox[{"27", " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"2", " ", 
SqrtBox[
RowBox[{"Abs", "[", "x", "]"}]]}], "+", 
RowBox[{"8", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], 
RowBox[{"3", "/", "2"}]]}]}], ")"}], "2"]}]], ",", 
FractionBox[
RowBox[{"2", "-", 
RowBox[{"2", " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{"1", "-", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], 
RowBox[{"3", "/", "2"}]]}], "+", 
RowBox[{"72", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], 
RowBox[{"27", " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"2", " ", 
SqrtBox[
RowBox[{"Abs", "[", "x", "]"}]]}], "+", 
RowBox[{"8", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", "x", "]"}], 
RowBox[{"3", "/", "2"}]]}]}], ")"}], "2"]}]], ",", 
FractionBox[
RowBox[{"1", "+", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]], "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "3"}], "+", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}], ")"}]}]}], 
RowBox[{"54", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
RowBox[{"4", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], "2"]}]], ",", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]], "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{"3", "+", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}], ")"}]}]}], 
RowBox[{"54", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
RowBox[{"4", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], "2"]}]]}], "]"}], 
RowBox[{
RowBox[{"2", " ", 
SqrtBox[
RowBox[{"Abs", "[", "x", "]"}]]}], "<", 
FractionBox["1", 
SqrtBox["3"]]}]},
{
RowBox[{"Min", "[", 
RowBox[{
FractionBox[
RowBox[{"1", "+", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]], "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{
RowBox[{"-", "3"}], "+", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}], ")"}]}]}], 
RowBox[{"54", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
RowBox[{"4", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], "2"]}]], ",", 
FractionBox[
RowBox[{
RowBox[{"-", "1"}], "+", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]], "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
RowBox[{"(", 
RowBox[{"3", "+", 
SqrtBox[
RowBox[{"1", "+", 
RowBox[{"12", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}]]}], ")"}]}]}], 
RowBox[{"54", " ", 
RowBox[{"Abs", "[", "x", "]"}], " ", 
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", 
RowBox[{"4", " ", 
RowBox[{"Abs", "[", "x", "]"}]}]}], ")"}], "2"]}]]}], "]"}], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)),((-1)^n 4^m (-x)^m (1+4 x)^(1/2-a-b-2 n) y^n Pochhammer[a,n] Pochhammer[1/2 (1-a-2 b),m-(3 n)/2] Pochhammer[1-a/2-b,m-(3 n)/2])/(m! n! Pochhammer[1/2 (1-a-2 b),-((3 n)/2)] Pochhammer[1-b,m-n] Pochhammer[1-a/2-b,-((3 n)/2)] Pochhammer[c,n])},{16 Abs[x (-1+y)]<4&&16 Abs[x (-1+y)]<(-8-20 Abs[y/(-1+y)]+Abs[y/(-1+y)]^2+8 Sqrt[Abs[y/(-1+y)]] Sqrt[8+Abs[y/(-1+y)]]+Abs[y/(-1+y)]^(3/2) Sqrt[8+Abs[y/(-1+y)]])/(2 (-1+Abs[y/(-1+y)])^3)&&Abs[y/(-1+y)]<1,1/(m! n! Pochhammer[c,n]) 16^m x^m (1-y)^(-b+m) (y/(-1+y))^n Pochhammer[a/2,m] Pochhammer[(1+a)/2,m] Pochhammer[b,-m+n] Pochhammer[1/2 (1+a-c),m] Pochhammer[1/2 (2+a-c),m] Pochhammer[-a+c,-2 m+n]},{Abs[x/(-1+y)^2]<1/4&&Abs[x/(-1+y)^2]<1/32 (8+20 Abs[y/(-1+y)]-Abs[y/(-1+y)]^2-Sqrt[Abs[y/(-1+y)]] (8+Abs[y/(-1+y)])^(3/2))&&Abs[y/(-1+y)]<1,((-1)^m x^m (1-y)^(-a-2 m) (y/(-1+y))^n Pochhammer[a,2 m+n] Pochhammer[-b+c,m+n])/(m! n! Pochhammer[1-b,m] Pochhammer[c,n] Pochhammer[-b+c,m])},{1/(4 Abs[x])<1&&8 Abs[(x^(3/2) y)/(1+4 x)^2]<2/(3 Sqrt[3])&&8 Abs[(x^(3/2) y)/(1+4 x)^2]<Min[Root[4096+(-55296-64/Abs[x]^3+1152/Abs[x]^2-55296/Abs[x]) #1^2+(186624+729/Abs[x]^4-11664/Abs[x]^3+69984/Abs[x]^2-186624/Abs[x]) #1^4&,3],Root[4096+(-55296-64/Abs[x]^3+1152/Abs[x]^2-55296/Abs[x]) #1^2+(186624+729/Abs[x]^4-11664/Abs[x]^3+69984/Abs[x]^2-186624/Abs[x]) #1^4&,4]],((-1)^n 2^(-1+a+2 b-2 m+3 n) Sqrt[\[Pi]] (-x)^-m x^(1/2 (-2+a+2 b+3 n)) (1+4 x)^(1/2-a-b-2 n) y^n Gamma[1-b] Pochhammer[a,n] (Sqrt[x] Gamma[a/2] Gamma[1/2 (1-a-2 b)] Pochhammer[3/2,m] Pochhammer[(1-a)/2,m-n/2] Pochhammer[1-a/2,-(n/2)] Pochhammer[a/2,n/2] Pochhammer[1/2 (1-a-2 b),m-(3 n)/2]-Gamma[(1+a)/2] Gamma[1-a/2-b] Pochhammer[1/2,m] Pochhammer[(1-a)/2,-(n/2)] Pochhammer[1-a/2,m-n/2] Pochhammer[(1+a)/2,n/2] Pochhammer[1-a/2-b,m-(3 n)/2]))/(m! n! Gamma[a/2] Gamma[(1+a)/2] Gamma[1/2 (1-a-2 b)] Gamma[1-a/2-b] Pochhammer[1/2,m] Pochhammer[3/2,m] Pochhammer[(1-a)/2,-(n/2)] Pochhammer[1-a/2,-(n/2)] Pochhammer[a/2,n/2] Pochhammer[(1+a)/2,n/2] Pochhammer[1/2 (1-a-2 b),-((3 n)/2)] Pochhammer[1-a/2-b,-((3 n)/2)] Pochhammer[c,n])},{Abs[1+1/(4 x)]<1&&27/16 Abs[y/Sqrt[-x]]<4&&(1/16 Abs[1+1/(4 x)] (96+3 Abs[1+1/(4 x)]+2 Abs[1+1/(4 x)]^2)+729/256 Abs[y/Sqrt[-x]]^2<8+1/8 Sqrt[(-4+Abs[1+1/(4 x)])^3 (-1+Abs[1+1/(4 x)])] (8+Abs[1+1/(4 x)])||6 Abs[1+1/(4 x)]+3/16 Abs[1+1/(4 x)]^2+1/8 Abs[1+1/(4 x)]^3+1/8 Sqrt[(-4+Abs[1+1/(4 x)])^3 (-1+Abs[1+1/(4 x)])] (8+Abs[1+1/(4 x)])+729/256 Abs[y/Sqrt[-x]]^2<8||(-((729 y^2)/(256 x))+6 Abs[1+1/(4 x)]+3/16 Abs[1+1/(4 x)]^2+1/8 Abs[1+1/(4 x)]^3+1/8 Sqrt[(-4+Abs[1+1/(4 x)])^3 (-1+Abs[1+1/(4 x)])] (8+Abs[1+1/(4 x)])>8&&729/16 Abs[y/Sqrt[-x]]^2<=27))&&Abs[y^6/x^3]/2176782336<16777216/205891132094649&&Abs[(1+4 x)^4/(x^3 y^2)]/4096<1&&Abs[(1+4 x)^4/(x^3 y^2)]/4096<Min[Root[281474976710656-200385994162176 Abs[y^6/x^3]^(1/3)+53496602689536 Abs[y^6/x^3]^(2/3)-6347497291776 Abs[y^6/x^3]+282429536481 Abs[y^6/x^3]^(4/3)+(-1125899906842624-1277357633568768 Abs[y^6/x^3]^(1/3)+12181668102144 Abs[y^6/x^3]^(2/3)-496306372608 Abs[y^6/x^3]) #1+(1688849860263936+68444598829056 Abs[y^6/x^3]^(1/3)-7101090889728 Abs[y^6/x^3]^(2/3)+293534171136 Abs[y^6/x^3]) #1^2+(-1125899906842624-15668040695808 Abs[y^6/x^3]^(1/3)+1649267441664 Abs[y^6/x^3]^(2/3)-68719476736 Abs[y^6/x^3]) #1^3+281474976710656 #1^4&,1],Root[281474976710656-200385994162176 Abs[y^6/x^3]^(1/3)+53496602689536 Abs[y^6/x^3]^(2/3)-6347497291776 Abs[y^6/x^3]+282429536481 Abs[y^6/x^3]^(4/3)+(-1125899906842624-1277357633568768 Abs[y^6/x^3]^(1/3)+12181668102144 Abs[y^6/x^3]^(2/3)-496306372608 Abs[y^6/x^3]) #1+(1688849860263936+68444598829056 Abs[y^6/x^3]^(1/3)-7101090889728 Abs[y^6/x^3]^(2/3)+293534171136 Abs[y^6/x^3]) #1^2+(-1125899906842624-15668040695808 Abs[y^6/x^3]^(1/3)+1649267441664 Abs[y^6/x^3]^(2/3)-68719476736 Abs[y^6/x^3]) #1^3+281474976710656 #1^4&,2]],(2^(-2 (a+b+2 n)) 27^n Sqrt[\[Pi]] (1+1/(4 x))^m (-x)^(1/2 (-a-n)) y^n Gamma[1-b] Pochhammer[a/2,m+n/2] Pochhammer[a,n] Pochhammer[a/2+b,m+(3 n)/2] Pochhammer[1/3 (a+2 b),n] Pochhammer[1/3 (1+a+2 b),n] Pochhammer[1/3 (2+a+2 b),n] Sec[(a+b) \[Pi]])/(m! n! Gamma[1-a-2 b] Gamma[1/2+a+b] Pochhammer[a/2,n/2] Pochhammer[a/2+b,(3 n)/2] Pochhammer[1/2+a+b,m+2 n] Pochhammer[c,n])-((-1)^m 2^(-(15/2)-a-b+3 m-12 n) \[Pi]^(3/2) (1+1/(4 x))^m (-x)^(1/2 (-1+a+2 b)) (1+4 x)^(1/2-a-b) (-((1+4 x)^4/(x^3 y^2)))^(1+n) ((x^3 y^2)/(1+4 x)^4)^(1/8 (3-2 a-2 b+2 m)) Gamma[-(3/4)] Gamma[-(1/4)] Gamma[1-b] Gamma[c/2] Gamma[(1+c)/2] Pochhammer[1/2 (1-a-2 b),m] Pochhammer[1/8 (-1+2 a-2 b),m/4] Pochhammer[1/24 (19+2 a-2 b),-(m/4)] Pochhammer[1/24 (19+2 a-2 b),m/12+n] Pochhammer[1/24 (27+2 a-2 b),-(m/4)] Pochhammer[1/24 (27+2 a-2 b),m/12+n] Pochhammer[1/24 (35+2 a-2 b),-(m/4)] Pochhammer[1/24 (35+2 a-2 b),m/12+n] Pochhammer[1/24 (-11-2 a+2 b),m/4] Pochhammer[1/24 (-3-2 a+2 b),m/4] Pochhammer[1/24 (5-2 a+2 b),m/4] Pochhammer[1/8 (9-2 a+2 b),-(m/4)] Pochhammer[1/8 (9-2 a+2 b),(3 m)/4+n] Pochhammer[1/6 (1+a+2 b),-(m/3)] Pochhammer[1/6 (3+a+2 b),-(m/3)] Pochhammer[1/6 (5+a+2 b),-(m/3)] Pochhammer[1/8 (5+2 a+2 b),-(m/4)+n] Pochhammer[1/8 (9+2 a+2 b),-(m/4)+n] Pochhammer[1/8 (9+2 a+2 b-4 c),-(m/4)+n] Pochhammer[1/8 (13+2 a+2 b-4 c),-(m/4)+n] Sec[(a+b) \[Pi]])/(m! n! Gamma[a] Gamma[1/8 (-1-2 a-2 b)] Gamma[3/2-a-b] Gamma[1/8 (-1+2 a+2 b)] Gamma[1/8 (1+2 a+2 b)] Gamma[1/8 (3+2 a+2 b)] Gamma[1/8 (-5-2 a-2 b+4 c)] Gamma[1/8 (-1-2 a-2 b+4 c)] Pochhammer[5/4,n] Pochhammer[3/2,n] Pochhammer[7/4,n] Pochhammer[1/8 (-1-2 a-2 b),m/4] Pochhammer[1/8 (-1+2 a-2 b),-((3 m)/4)] Pochhammer[1/24 (19+2 a-2 b),m/12] Pochhammer[1/24 (19+2 a-2 b),-(m/4)+n] Pochhammer[1/24 (27+2 a-2 b),m/12] Pochhammer[1/24 (27+2 a-2 b),-(m/4)+n] Pochhammer[1/24 (35+2 a-2 b),m/12] Pochhammer[1/24 (35+2 a-2 b),-(m/4)+n] Pochhammer[3/2-a-b,m] Pochhammer[1/24 (-11-2 a+2 b),-(m/12)] Pochhammer[1/24 (-3-2 a+2 b),-(m/12)] Pochhammer[1/24 (5-2 a+2 b),-(m/12)] Pochhammer[1/8 (9-2 a+2 b),(3 m)/4] Pochhammer[1/8 (9-2 a+2 b),-(m/4)+n] Pochhammer[1/8 (-1+2 a+2 b),-(m/4)] Pochhammer[1/8 (1+2 a+2 b),-(m/4)] Pochhammer[1/8 (3+2 a+2 b),-(m/4)] Pochhammer[1/8 (5+2 a+2 b),-(m/4)] Pochhammer[1/8 (9+2 a+2 b),-(m/4)] Pochhammer[1/8 (9+2 a+2 b-4 c),-(m/4)] Pochhammer[1/8 (13+2 a+2 b-4 c),-(m/4)] Pochhammer[1/8 (-5-2 a-2 b+4 c),m/4] Pochhammer[1/8 (-1-2 a-2 b+4 c),m/4])-((-1)^m 2^(-(9/2)-a-b+3 m-12 n) \[Pi]^(3/2) (1+1/(4 x))^m (-x)^(1/2 (-1+a+2 b)) (1+4 x)^(1/2-a-b) (-((1+4 x)^4/(x^3 y^2)))^(1+n) ((x^3 y^2)/(1+4 x)^4)^(1/8 (5-2 a-2 b+2 m)) Gamma[-(1/4)] Gamma[1/4] Gamma[1-b] Gamma[c/2] Gamma[(1+c)/2] Pochhammer[1/2 (1-a-2 b),m] Pochhammer[1/8 (1+2 a-2 b),m/4] Pochhammer[1/24 (13+2 a-2 b),-(m/4)] Pochhammer[1/24 (13+2 a-2 b),m/12+n] Pochhammer[1/24 (21+2 a-2 b),-(m/4)] Pochhammer[1/24 (21+2 a-2 b),m/12+n] Pochhammer[1/24 (29+2 a-2 b),-(m/4)] Pochhammer[1/24 (29+2 a-2 b),m/12+n] Pochhammer[1/24 (-5-2 a+2 b),m/4] Pochhammer[1/24 (3-2 a+2 b),m/4] Pochhammer[1/8 (7-2 a+2 b),-(m/4)] Pochhammer[1/8 (7-2 a+2 b),(3 m)/4+n] Pochhammer[1/24 (11-2 a+2 b),m/4] Pochhammer[1/6 (1+a+2 b),-(m/3)] Pochhammer[1/6 (3+a+2 b),-(m/3)] Pochhammer[1/6 (5+a+2 b),-(m/3)] Pochhammer[1/8 (3+2 a+2 b),-(m/4)+n] Pochhammer[1/8 (7+2 a+2 b),-(m/4)+n] Pochhammer[1/8 (7+2 a+2 b-4 c),-(m/4)+n] Pochhammer[1/8 (11+2 a+2 b-4 c),-(m/4)+n] Sec[(a+b) \[Pi]])/(m! n! Gamma[a] Gamma[1/8 (1-2 a-2 b)] Gamma[3/2-a-b] Gamma[1/8 (-1+2 a+2 b)] Gamma[1/8 (1+2 a+2 b)] Gamma[1/8 (5+2 a+2 b)] Gamma[1/8 (-3-2 a-2 b+4 c)] Gamma[1/8 (1-2 a-2 b+4 c)] Pochhammer[3/4,n] Pochhammer[5/4,n] Pochhammer[3/2,n] Pochhammer[1/8 (1-2 a-2 b),m/4] Pochhammer[1/8 (1+2 a-2 b),-((3 m)/4)] Pochhammer[1/24 (13+2 a-2 b),m/12] Pochhammer[1/24 (13+2 a-2 b),-(m/4)+n] Pochhammer[1/24 (21+2 a-2 b),m/12] Pochhammer[1/24 (21+2 a-2 b),-(m/4)+n] Pochhammer[1/24 (29+2 a-2 b),m/12] Pochhammer[1/24 (29+2 a-2 b),-(m/4)+n] Pochhammer[3/2-a-b,m] Pochhammer[1/24 (-5-2 a+2 b),-(m/12)] Pochhammer[1/24 (3-2 a+2 b),-(m/12)] Pochhammer[1/8 (7-2 a+2 b),(3 m)/4] Pochhammer[1/8 (7-2 a+2 b),-(m/4)+n] Pochhammer[1/24 (11-2 a+2 b),-(m/12)] Pochhammer[1/8 (-1+2 a+2 b),-(m/4)] Pochhammer[1/8 (1+2 a+2 b),-(m/4)] Pochhammer[1/8 (3+2 a+2 b),-(m/4)] Pochhammer[1/8 (5+2 a+2 b),-(m/4)] Pochhammer[1/8 (7+2 a+2 b),-(m/4)] Pochhammer[1/8 (7+2 a+2 b-4 c),-(m/4)] Pochhammer[1/8 (11+2 a+2 b-4 c),-(m/4)] Pochhammer[1/8 (-3-2 a-2 b+4 c),m/4] Pochhammer[1/8 (1-2 a-2 b+4 c),m/4])+((-1)^m 2^(-(5/2)-a-b+3 m-12 n) \[Pi]^(3/2) (1+1/(4 x))^m (-x)^(-(7/2)+a/2+b) (1+4 x)^(9/2-a-b) (-((1+4 x)^4/(x^3 y^2)))^n ((x^3 y^2)/(1+4 x)^4)^(1/8 (7-2 a-2 b+2 m)) Gamma[-(1/4)] Gamma[1/4] Gamma[1-b] Gamma[c/2] Gamma[(1+c)/2] Pochhammer[1/2 (1-a-2 b),m] Pochhammer[1/8 (3+2 a-2 b),m/4] Pochhammer[1/24 (7+2 a-2 b),-(m/4)] Pochhammer[1/24 (7+2 a-2 b),m/12+n] Pochhammer[1/24 (15+2 a-2 b),-(m/4)] Pochhammer[1/24 (15+2 a-2 b),m/12+n] Pochhammer[1/24 (23+2 a-2 b),-(m/4)] Pochhammer[1/24 (23+2 a-2 b),m/12+n] Pochhammer[1/24 (1-2 a+2 b),m/4] Pochhammer[1/8 (5-2 a+2 b),-(m/4)] Pochhammer[1/8 (5-2 a+2 b),(3 m)/4+n] Pochhammer[1/24 (9-2 a+2 b),m/4] Pochhammer[1/24 (17-2 a+2 b),m/4] Pochhammer[1/6 (1+a+2 b),-(m/3)] Pochhammer[1/6 (3+a+2 b),-(m/3)] Pochhammer[1/6 (5+a+2 b),-(m/3)] Pochhammer[1/8 (1+2 a+2 b),-(m/4)+n] Pochhammer[1/8 (5+2 a+2 b),-(m/4)+n] Pochhammer[1/8 (5+2 a+2 b-4 c),-(m/4)+n] Pochhammer[1/8 (9+2 a+2 b-4 c),-(m/4)+n] Sec[(a+b) \[Pi]])/(y^2 m! n! Gamma[a] Gamma[1/8 (3-2 a-2 b)] Gamma[3/2-a-b] Gamma[1/8 (-1+2 a+2 b)] Gamma[1/8 (3+2 a+2 b)] Gamma[1/8 (5+2 a+2 b)] Gamma[1/8 (-1-2 a-2 b+4 c)] Gamma[1/8 (3-2 a-2 b+4 c)] Pochhammer[1/2,n] Pochhammer[3/4,n] Pochhammer[5/4,n] Pochhammer[1/8 (3-2 a-2 b),m/4] Pochhammer[1/8 (3+2 a-2 b),-((3 m)/4)] Pochhammer[1/24 (7+2 a-2 b),m/12] Pochhammer[1/24 (7+2 a-2 b),-(m/4)+n] Pochhammer[1/24 (15+2 a-2 b),m/12] Pochhammer[1/24 (15+2 a-2 b),-(m/4)+n] Pochhammer[1/24 (23+2 a-2 b),m/12] Pochhammer[1/24 (23+2 a-2 b),-(m/4)+n] Pochhammer[3/2-a-b,m] Pochhammer[1/24 (1-2 a+2 b),-(m/12)] Pochhammer[1/8 (5-2 a+2 b),(3 m)/4] Pochhammer[1/8 (5-2 a+2 b),-(m/4)+n] Pochhammer[1/24 (9-2 a+2 b),-(m/12)] Pochhammer[1/24 (17-2 a+2 b),-(m/12)] Pochhammer[1/8 (-1+2 a+2 b),-(m/4)] Pochhammer[1/8 (1+2 a+2 b),-(m/4)] Pochhammer[1/8 (3+2 a+2 b),-(m/4)] Pochhammer[1/8 (5+2 a+2 b),-(m/4)]^2 Pochhammer[1/8 (5+2 a+2 b-4 c),-(m/4)] Pochhammer[1/8 (9+2 a+2 b-4 c),-(m/4)] Pochhammer[1/8 (-1-2 a-2 b+4 c),m/4] Pochhammer[1/8 (3-2 a-2 b+4 c),m/4])+((-1)^m 2^(-(1/2)-a-b+3 m-12 n) \[Pi]^(3/2) (1+1/(4 x))^m (-x)^(1/2 (1+a+2 b)) (1+4 x)^(1/2-a-b) (-((1+4 x)^4/(x^3 y^2)))^n ((x^3 y^2)/(1+4 x)^4)^(1/8 (1-2 a-2 b+2 m)) Gamma[1/4] Gamma[3/4] Gamma[1-b] Gamma[c/2] Gamma[(1+c)/2] Pochhammer[1/2 (1-a-2 b),m] Pochhammer[1/24 (1+2 a-2 b),-(m/4)] Pochhammer[1/24 (1+2 a-2 b),m/12+n] Pochhammer[1/8 (5+2 a-2 b),m/4] Pochhammer[1/24 (9+2 a-2 b),-(m/4)] Pochhammer[1/24 (9+2 a-2 b),m/12+n] Pochhammer[1/24 (17+2 a-2 b),-(m/4)] Pochhammer[1/24 (17+2 a-2 b),m/12+n] Pochhammer[1/8 (3-2 a+2 b),-(m/4)] Pochhammer[1/8 (3-2 a+2 b),(3 m)/4+n] Pochhammer[1/24 (7-2 a+2 b),m/4] Pochhammer[1/24 (15-2 a+2 b),m/4] Pochhammer[1/24 (23-2 a+2 b),m/4] Pochhammer[1/6 (1+a+2 b),-(m/3)] Pochhammer[1/6 (3+a+2 b),-(m/3)] Pochhammer[1/6 (5+a+2 b),-(m/3)] Pochhammer[1/8 (-1+2 a+2 b),-(m/4)+n] Pochhammer[1/8 (3+2 a+2 b),-(m/4)+n] Pochhammer[1/8 (3+2 a+2 b-4 c),-(m/4)+n] Pochhammer[1/8 (7+2 a+2 b-4 c),-(m/4)+n] Sec[(a+b) \[Pi]])/(x m! n! Gamma[a] Gamma[1/8 (5-2 a-2 b)] Gamma[3/2-a-b] Gamma[1/8 (1+2 a+2 b)] Gamma[1/8 (3+2 a+2 b)] Gamma[1/8 (5+2 a+2 b)] Gamma[1/8 (1-2 a-2 b+4 c)] Gamma[1/8 (5-2 a-2 b+4 c)] Pochhammer[1/4,n] Pochhammer[1/2,n] Pochhammer[3/4,n] Pochhammer[1/8 (5-2 a-2 b),m/4] Pochhammer[1/24 (1+2 a-2 b),m/12] Pochhammer[1/24 (1+2 a-2 b),-(m/4)+n] Pochhammer[1/8 (5+2 a-2 b),-((3 m)/4)] Pochhammer[1/24 (9+2 a-2 b),m/12] Pochhammer[1/24 (9+2 a-2 b),-(m/4)+n] Pochhammer[1/24 (17+2 a-2 b),m/12] Pochhammer[1/24 (17+2 a-2 b),-(m/4)+n] Pochhammer[3/2-a-b,m] Pochhammer[1/8 (3-2 a+2 b),(3 m)/4] Pochhammer[1/8 (3-2 a+2 b),-(m/4)+n] Pochhammer[1/24 (7-2 a+2 b),-(m/12)] Pochhammer[1/24 (15-2 a+2 b),-(m/12)] Pochhammer[1/24 (23-2 a+2 b),-(m/12)] Pochhammer[1/8 (-1+2 a+2 b),-(m/4)] Pochhammer[1/8 (1+2 a+2 b),-(m/4)] Pochhammer[1/8 (3+2 a+2 b),-(m/4)]^2 Pochhammer[1/8 (5+2 a+2 b),-(m/4)] Pochhammer[1/8 (3+2 a+2 b-4 c),-(m/4)] Pochhammer[1/8 (7+2 a+2 b-4 c),-(m/4)] Pochhammer[1/8 (1-2 a-2 b+4 c),m/4] Pochhammer[1/8 (5-2 a-2 b+4 c),m/4])+((-1)^m 2^(-(7/2)-a-b+3 m-12 n) \[Pi]^(3/2) (1+1/(4 x))^m (-x)^(-3+a/2+b) x (1+4 x)^(5/2-a-b) (-((1+4 x)^4/(x^3 y^2)))^n ((x^3 y^2)/(1+4 x)^4)^(1/8 (5-2 a-2 b+2 m)) Gamma[1/4] Gamma[3/4] Gamma[1-b] Gamma[c] Gamma[(1+c)/2] Gamma[(2+c)/2] Pochhammer[1/24 (1+2 a-2 b),-(m/4)] Pochhammer[1/24 (1+2 a-2 b),m/12+n] Pochhammer[1/8 (5+2 a-2 b),m/4] Pochhammer[1/24 (9+2 a-2 b),-(m/4)] Pochhammer[1/24 (9+2 a-2 b),m/12+n] Pochhammer[1/24 (17+2 a-2 b),-(m/4)] Pochhammer[1/24 (17+2 a-2 b),m/12+n] Pochhammer[-1-a/2-b,m] Pochhammer[1/8 (3-2 a+2 b),-(m/4)] Pochhammer[1/8 (3-2 a+2 b),(3 m)/4+n] Pochhammer[1/24 (7-2 a+2 b),m/4] Pochhammer[1/24 (15-2 a+2 b),m/4] Pochhammer[1/24 (23-2 a+2 b),m/4] Pochhammer[1/6 (4+a+2 b),-(m/3)] Pochhammer[1/6 (6+a+2 b),-(m/3)] Pochhammer[1/6 (8+a+2 b),-(m/3)] Pochhammer[1/8 (-1+2 a+2 b),-(m/4)+n] Pochhammer[1/8 (3+2 a+2 b),-(m/4)+n] Pochhammer[1/8 (3+2 a+2 b-4 c),-(m/4)+n] Pochhammer[1/8 (7+2 a+2 b-4 c),-(m/4)+n] Sec[(a+b) \[Pi]])/(y m! n! Gamma[a] Gamma[1/8 (9-2 a-2 b)] Gamma[-(1/2)-a-b] Gamma[1/8 (5+2 a+2 b)] Gamma[1/8 (7+2 a+2 b)] Gamma[1/8 (9+2 a+2 b)] Gamma[1+c] Gamma[1/8 (1-2 a-2 b+4 c)] Gamma[1/8 (5-2 a-2 b+4 c)] Pochhammer[1/4,n] Pochhammer[1/2,n] Pochhammer[3/4,n] Pochhammer[1/8 (9-2 a-2 b),m/4] Pochhammer[1/24 (1+2 a-2 b),m/12] Pochhammer[1/24 (1+2 a-2 b),-(m/4)+n] Pochhammer[1/8 (5+2 a-2 b),-((3 m)/4)] Pochhammer[1/24 (9+2 a-2 b),m/12] Pochhammer[1/24 (9+2 a-2 b),-(m/4)+n] Pochhammer[1/24 (17+2 a-2 b),m/12] Pochhammer[1/24 (17+2 a-2 b),-(m/4)+n] Pochhammer[-(1/2)-a-b,m] Pochhammer[1/8 (3-2 a+2 b),(3 m)/4] Pochhammer[1/8 (3-2 a+2 b),-(m/4)+n] Pochhammer[1/24 (7-2 a+2 b),-(m/12)] Pochhammer[1/24 (15-2 a+2 b),-(m/12)] Pochhammer[1/24 (23-2 a+2 b),-(m/12)] Pochhammer[1/8 (-1+2 a+2 b),-(m/4)] Pochhammer[1/8 (3+2 a+2 b),-(m/4)] Pochhammer[1/8 (5+2 a+2 b),-(m/4)] Pochhammer[1/8 (7+2 a+2 b),-(m/4)] Pochhammer[1/8 (9+2 a+2 b),-(m/4)] Pochhammer[1/8 (3+2 a+2 b-4 c),-(m/4)] Pochhammer[1/8 (7+2 a+2 b-4 c),-(m/4)] Pochhammer[1/8 (1-2 a-2 b+4 c),m/4] Pochhammer[1/8 (5-2 a-2 b+4 c),m/4])},{Abs[(-1+y)^2/x]<4&&Abs[y/(Sqrt[x/(-1+y)^2] (-1+y))]<4&&Abs[y/(Sqrt[x/(-1+y)^2] (-1+y))]^2<4 (\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"Min", "[", 
RowBox[{
RowBox[{"Root", "[", 
RowBox[{
RowBox[{
RowBox[{
RowBox[{"-", "256"}], "-", 
RowBox[{"256", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "-", 
RowBox[{"96", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "2"]}], "-", 
RowBox[{"16", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "3"]}], "-", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "4"], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{"128", "+", 
RowBox[{"1280", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "-", 
RowBox[{"664", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "2"]}], "+", 
RowBox[{"12", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "3"]}]}], ")"}], " ", "#1"}], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "16"}], "-", 
RowBox[{"544", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "-", 
RowBox[{"48", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox["#1", "2"]}], "+", 
RowBox[{"64", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], " ", 
SuperscriptBox["#1", "3"]}]}], "&"}], ",", "1"}], "]"}], ",", 
RowBox[{"Root", "[", 
RowBox[{
RowBox[{
RowBox[{
RowBox[{"-", "256"}], "-", 
RowBox[{"256", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "-", 
RowBox[{"96", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "2"]}], "-", 
RowBox[{"16", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "3"]}], "-", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "4"], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{"128", "+", 
RowBox[{"1280", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "-", 
RowBox[{"664", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "2"]}], "+", 
RowBox[{"12", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "3"]}]}], ")"}], " ", "#1"}], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "16"}], "-", 
RowBox[{"544", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "-", 
RowBox[{"48", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox["#1", "2"]}], "+", 
RowBox[{"64", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], " ", 
SuperscriptBox["#1", "3"]}]}], "&"}], ",", "2"}], "]"}], ",", 
RowBox[{"Root", "[", 
RowBox[{
RowBox[{
RowBox[{
RowBox[{"-", "256"}], "-", 
RowBox[{"256", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "-", 
RowBox[{"96", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "2"]}], "-", 
RowBox[{"16", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "3"]}], "-", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "4"], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{"128", "+", 
RowBox[{"1280", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "-", 
RowBox[{"664", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "2"]}], "+", 
RowBox[{"12", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "3"]}]}], ")"}], " ", "#1"}], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "16"}], "-", 
RowBox[{"544", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "-", 
RowBox[{"48", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox["#1", "2"]}], "+", 
RowBox[{"64", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], " ", 
SuperscriptBox["#1", "3"]}]}], "&"}], ",", "3"}], "]"}], ",", 
RowBox[{"Root", "[", 
RowBox[{
RowBox[{
RowBox[{
RowBox[{"-", "256"}], "+", 
RowBox[{"256", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "-", 
RowBox[{"96", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "2"]}], "+", 
RowBox[{"16", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "3"]}], "-", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "4"], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "128"}], "+", 
RowBox[{"1280", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "+", 
RowBox[{"664", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "2"]}], "+", 
RowBox[{"12", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "3"]}]}], ")"}], " ", "#1"}], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "16"}], "+", 
RowBox[{"544", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "-", 
RowBox[{"48", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox["#1", "2"]}], "+", 
RowBox[{"64", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], " ", 
SuperscriptBox["#1", "3"]}]}], "&"}], ",", "1"}], "]"}]}], "]"}], 
RowBox[{
RowBox[{"27", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "<", "4"}]},
{
RowBox[{"Root", "[", 
RowBox[{
RowBox[{
RowBox[{
RowBox[{"-", "256"}], "+", 
RowBox[{"256", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "-", 
RowBox[{"96", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "2"]}], "+", 
RowBox[{"16", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "3"]}], "-", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "4"], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "128"}], "+", 
RowBox[{"1280", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "+", 
RowBox[{"664", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "2"]}], "+", 
RowBox[{"12", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "3"]}]}], ")"}], " ", "#1"}], "+", 
RowBox[{
RowBox[{"(", 
RowBox[{
RowBox[{"-", "16"}], "+", 
RowBox[{"544", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}]}], "-", 
RowBox[{"48", " ", 
SuperscriptBox[
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], "2"]}]}], ")"}], " ", 
SuperscriptBox["#1", "2"]}], "+", 
RowBox[{"64", " ", 
RowBox[{"Abs", "[", 
FractionBox[
SuperscriptBox[
RowBox[{"(", 
RowBox[{
RowBox[{"-", "1"}], "+", "y"}], ")"}], "2"], "x"], "]"}], " ", 
SuperscriptBox["#1", "3"]}]}], "&"}], ",", "1"}], "]"}], 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)),-((2^(-a-2 m-n) Sqrt[\[Pi]] (1-y)^-a (x/(-1+y)^2)^(1/2 (-1-a-n)) (-((-1+y)^2/x))^m (y/(-1+y))^n Gamma[1-b] Pochhammer[a,n] Pochhammer[(1+a)/2,m+n/2] Pochhammer[1/2 (1+a+2 b),m+n/2] Pochhammer[1/2 (3+a+2 b-2 c),m+n/2] Pochhammer[1/2 (3+a+2 b-2 c),-(n/2)] Pochhammer[-(1/2)-a/2-b+c,n/2])/(m! n! Gamma[a/2] Gamma[1/2 (1-a-2 b)] Pochhammer[3/2,m] Pochhammer[a/2,n/2] Pochhammer[(1+a)/2,n/2] Pochhammer[1/2 (1-a-2 b),-(n/2)] Pochhammer[1/2 (1+a+2 b),n/2] Pochhammer[1/2 (3+a+2 b-2 c),m-n/2] Pochhammer[1/2 (3+a+2 b-2 c),n/2] Pochhammer[c,n] Pochhammer[-(1/2)-a/2-b+c,-(n/2)]))+(2^(-a-2 m-n) Sqrt[\[Pi]] (1-y)^-a (x/(-1+y)^2)^(1/2 (-a-n)) (-((-1+y)^2/x))^m (y/(-1+y))^n Gamma[1-b] Pochhammer[a/2,m+n/2] Pochhammer[a,n] Pochhammer[a/2+b,m+n/2] Pochhammer[1+a/2+b-c,m+n/2] Pochhammer[1+a/2+b-c,-(n/2)] Pochhammer[-(a/2)-b+c,n/2])/(m! n! Gamma[(1+a)/2] Gamma[1-a/2-b] Pochhammer[1/2,m] Pochhammer[a/2,n/2] Pochhammer[(1+a)/2,n/2] Pochhammer[1-a/2-b,-(n/2)] Pochhammer[a/2+b,n/2] Pochhammer[1+a/2+b-c,m-n/2] Pochhammer[1+a/2+b-c,n/2] Pochhammer[c,n] Pochhammer[-(a/2)-b+c,-(n/2)])}};


(* ::Section:: *)
(*End package*)


End[]


EndPackage[]
