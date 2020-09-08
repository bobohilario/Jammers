(* Wolfram Language Package *)

BeginPackage["Jammers`"]

Jammers`DashboardNotebook

Jammers`DeployDashboard

Begin["`Private`"] 

dashboardlocation[league_]:=URLBuild[{leagueDir[league],"dashboard"}]

DashboardNotebook[league_]:=Notebook[{
	leagueTitleCell[league],
	linksCell[league],
	Cell["Leaderboard","Section"],
	leagueLeaderboard[league],
	Cell["Game History","Section"],
	leagueGameHistory[league]
},System`ClickToCopyEnabled->False]

leagueTitleCell[league_]:=Cell[league,"Title"]

leagueLeaderboard[league_]:=Cell[
	BoxData[ToBoxes[Rasterize[Dataset[MapAt[N,getJammerData["Players",league],{All,"Handicap"}]]]]],"Output"]

leagueGameHistory[league_]:=Cell[CellGroupData[
	KeyValueMap[gameRow,getJammerData["Games",league]]
,Open],"Output"]
	
linksCell[league_]:=Cell[
	BoxData@ToBoxes@Column[
		{
		Hyperlink["Add Player",URL["forms/newplayer"]],
		
		Hyperlink["Record Game",URL["forms/newgame"]]	
			
		}],
		"Output"
]

DeployDashboard[args___]:=deployDashboard[args]
deployDashboard[league_]:=(DeleteObject[CloudObject[dashboardlocation[league]]];
	CloudDeploy[DashboardNotebook[league],dashboardlocation[league],Permissions->"Public"])


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