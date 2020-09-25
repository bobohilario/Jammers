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


leagueLeaderboardGrid[league_]:=With[{data=MapAt[N, ReverseSortBy[getJammerData["Players",league], #Handicap &], {All, "Handicap"}]},
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
		Hyperlink[Style["Add Player",20],URL["forms/newplayer"]],
		
		Hyperlink[Style["Determine Handicaps",20],URL["forms/handicaps"]],
		Hyperlink[Style["Random Matchup",20],URL["forms/matchup"]],
		Hyperlink[Style["Record Game Results",20],URL["forms/newgame"]]	
			
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
			Style[StringRiffle[Values[gameData["t1"]]," & "],FontFamily->"Source Sans Pro",If[gameData["Winner"]==="Team 1",Bold,Plain]]," ",
			Style[gameData["Score"]["t1"],Bold,16],
			" - ",
			Style[StringRiffle[Values[gameData["t2"]]," & "], FontFamily->"Source Sans Pro",If[gameData["Winner"]==="Team 2",Bold,Plain]]," ",
			Style[gameData["Score"]["t2"],Bold,16],"   ",
			Hyperlink["Edit \[RightGuillemet]",URL["forms/editgame?game="<>ToString[game]]]
			
		}]
	]],"Output"]
	
	


End[]

EndPackage[]