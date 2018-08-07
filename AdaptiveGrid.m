(* ::Package:: *)

(*Fabian K\[ODoubleDot]ssel, \[Copyright] 2018*)
BeginPackage[ "AdaptiveGrid`"]

evaluateOnAdaptiveGrid::usage = 
	"evaluateOnAdaptiveGrid[f_,initgrid_,depth_] computes a function f on an initial grid startgrid that gets refined around the zero transition of f by subdividing to a depth depth. Returns a list of rectangle with the function evaluated at every corner: {{{x0,y0}, f[x0,y0]},{{x1,y0}, f[x1,y0]},{{x1,y1}, f[x1,y1]},{{x0,y1}, f[x0,y1]}}."

sameSign::usage = "Returns true if all values at the corner of a face have the same sign. If  at least one value is zero, it returns zero."

Begin[ "Private`"]

(*A point is defined by a pair {x,y}.
A face is defined by a list of points starting from the bottom left corner in a CCW order:
{{x0,y0}, {x1,y0}, {x1,y1}, {x0,y1}}*)

(*Returns list of faces of a grid or evaluated grid. Assumes an ordered grid that can be obtained for example from Table[{x,y},{x,0,10,1},{y,0,5,0.5}]*)
faces[g_]:=Flatten[
		Table[
			{g[[1+i,1+j]],g[[2+i,1+j]],g[[2+i,2+j]],g[[1+i,2+j]]},
			{i,0,Dimensions[g][[1]]-2},{j,0,Dimensions[g][[2]]-2}
		],
	1]

(*Evaluate a function on a grid*)
evaluateGrid[f_,grid_]:=Map[{#,Apply[f,#]}&,grid,{2}]

(*Evaluate function f on the corners of a face g with four grid points {{x0,y0},...} and returns {{{x0,y0}, f[x0,y0]},...}*)
evaluateFace[f_,face_]:=MapThread[List,{face,Map[Apply@f,face]}]

(*Sums up the signs of a function evaluated at the corner of a face. If it is -4 or 4, all signs are the same. CAUTION: 0 has sign 0, and this function is always False if one corner evaluates to 0.*)
sameSign[e_]:=AllTrue[Map[#==0&,e[[All,2]]],TrueQ]||(Abs@Total@Map[Sign,e[[All,2]]])==4

(*Given an evaluated face e and a function f, it returns four new evaluated faces, subdividing the original one. Assumes that [1,1] is the bottom lower corner*)
subdivide[f_,e_]:=Block[
	{
		dx=(e[[2,1,1]]-e[[1,1,1]])/2,
		dy=(e[[4,1,2]]-e[[1,1,2]])/2,
		newp
	},
	(*construct new grid points*)
	newp=Map[{#,Apply[f,#]}&,
		{
			{{e[[1,1,1]],e[[1,1,2]]+dy}},
			Table[{e[[1,1,1]]+dx,e[[1,1,2]]+i dy},{i,0,2}],
			{{e[[2,1,1]],e[[1,1,2]]+dy}}
		},
		{2}];
	(*insert old points*)
	newp=Insert[newp,e[[1]],{1,1}];
	newp=Insert[newp,e[[2]],{3,1}];	
	newp=Insert[newp,e[[4]],{1,3}];
	newp=Insert[newp,e[[3]],{3,3}];
	faces[newp]
]

refine[lim_,l_,f_,e_]:=
	If[l>=lim,e,
		Block[
		{
			sub=Map[sameSign,e]
		},
		FlattenAt[
			MapThread[
				If[#1,
					#2,
					refine[lim,l+1,f,subdivide[f,#2]]
				]&,
			{sub,e}],
			Position[sub,False]
		]
		]
	]
	
(* This function subdivides grid cells, that contain points where f evaluates to zero.
Given a initial grid initgrid it evaluates the function f on the grid and refines it until a depth
The initial grid must be fine enough, so that at leat one cell contains a change of sign.
*)
evaluateOnAdaptiveGrid[f_,initgrid_,depth_]:=
	refine[depth,1,f,Map[evaluateFace[f,#]&,faces@initgrid]]

End[]

EndPackage[]
