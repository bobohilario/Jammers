(* Wolfram Language Package *)

BeginPackage["Jammers`"]

Jammers`JammerDashboard

Jammers`DeployDashboard

Begin["`Private`"] 

dashboardlocation[league_]:=URLBuild[{leagueDir[league],"dashboard"}]

JammerDashboard[league_]:=(
PacletDataRebuild[];
Notebook[{
	leagueTitleCell[league],
	linksCell[league],
	Cell["Leaderboard","Section"],
	leagueLeaderboardGrid[league],
	Cell["Game History","Section"],
	leagueGameHistory[league]
},System`ClickToCopyEnabled->False])

leagueTitleCell[league_]:=Cell[league,"Title"]


leagueLeaderboardGrid[league_]:=With[{data=getJammerData["Players",league]},
	Cell[
	BoxData[ToBoxes[
		Style[Grid[Join[
		{Style[#,16]&/@Prepend[Keys[First[data]],""]},
		Join[Transpose[{Style[#,16]&/@Keys[data]}],Values/@Values[data],2]
		],Alignment->Left,Frame->All],
		FontFamily->"Source Sans Pro"
		]
		]],"Output"]
]
	
	
leagueGameHistory[league_]:=With[{data=getJammerData["Games",league]},
	If[Length[data]>0,
	Cell[CellGroupData[
		KeyValueMap[gameRow,getJammerData["Games",league]]
	,Open],"Output"],
	Nothing
	]
]
	
linksCell[league_]:=Cell[
	BoxData@ToBoxes@Column[
		{
		Hyperlink["Add Player",URL["forms/newplayer"]],
		
		Hyperlink["Determine Handicaps",URL["forms/handicaps"]],
		Hyperlink["Record Game Results",URL["forms/newgame"]]	
			
		}],
		"Output"
]

DeployDashboard[args___]:=deployJammerDashboard[args]
deployJammerDashboard[league_]:=With[{co=CloudObject[dashboardlocation[league]]},
	If[FileExistsQ[co],
		DeleteObject[CloudObject[dashboardlocation[league]]]
	];
	Pause[1];
	CloudDeploy[JammerDashboard[league], co,Permissions->"Public"]
]


gameRow[game_,gameData_]:=
	Cell[BoxData[ToBoxes[
		Row[{
			Style["Game ",FontFamily->"Source Sans Pro"],
			game,": ",
			Style[StringRiffle[Values[gameData["t1"]]," & "],FontFamily->"Source Sans Pro"]," ",
			Style[gameData["Score"]["t1"],Bold,16],
			" - ",
			Style[StringRiffle[Values[gameData["t2"]]," & "], FontFamily->"Source Sans Pro"]," ",
			Style[gameData["Score"]["t2"],Bold,16],"   ",
			Hyperlink["Edit \[RightGuillemet]",URL["forms/editgame?game="<>ToString[game]]]
			
		}]
	]],"Output"]
	
	


End[]

EndPackage[]