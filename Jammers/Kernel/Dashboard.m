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
}]

leagueTitleCell[league_]:=Cell[league,"Title"]

leagueLeaderboard[league_]:=Cell[
	BoxData[ToBoxes[Dataset[getJammerData["Players",league]]]],"Output"]

leagueGameHistory[league_]:=Cell[
	BoxData[ToBoxes[Dataset[getJammerData["Games",league]]]],"Output"]
	
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

End[]

EndPackage[]