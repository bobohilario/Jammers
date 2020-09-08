(* Wolfram Language Package *)

BeginPackage["Jammers`"]

Jammers`DeployCode
Jammers`DeployGameForm
Jammers`DeployPlayerForm

Jammers`CreateLeague


Begin["`Private`"] 

$localkerneldir=FileNameDrop[$InputFileName];


DeployCode[]:=(
Quiet[DeleteObject[CloudObject[URLBuild[{$basedir,"code"}]]]];
CopyDirectory[
	$localkerneldir,
	CloudObject[URLBuild[{$basedir,"code"}]]];
	CloudEvaluate[
		Clear["Jammers`","Jammers`Private`"];
		Get[FileNameJoin[{$basedir,"code","Jammers.m"}]];
		DumpSave[URLBuild[{$basedir,"codedump.mx"}],{"Jammers`","Jammers`Private`"}]
	]
)


DeployGameForm[league_]:=With[{dumpfile=$dumppath,l=league},
	CloudDeploy[Delayed[
		Clear["Jammers`","Jammers`Private`"];
		Get[dumpfile];
		With[{players=playerList[league]},
		
		FormFunction[{
		{"t1","Team 1"}->CompoundElement[{"player1" -> players, "player2" -> players}],
		{"t2","Team 2"}->CompoundElement[{"player1" -> players, "player2" -> players}],
		"Score"->CompoundElement[{{"t1","Team 1"} -> "Integer", {"t2","Team 2"} -> "Integer"}]
	},
		(
		Clear["Jammers`","Jammers`Private`"];
		Get[dumpfile];
		With[{n=gameCount[l]},
			updateJammerData["Games",l,<|(n+1)->#|>];
			updatePlayersData[l,#];
		
		HTTPRedirect[deployDashboard[league]]
		];
		)&,
		AppearanceRules->{"Title"->"Record Game Results"}
	]]],
	URLBuild[{leagueDir[l],"forms","newgame"}],Permissions->"Public"
	]
]



DeployPlayerForm[league_]:=With[{dumpfile=$dumppath},
	CloudDeploy[FormFunction[{
		"Name"->"String"
	},
		(
		Clear["Jammers`","Jammers`Private`"];
		Get[dumpfile];
		updateJammerData["Players",league,<|#Name->$newPlayerData|>];
		
		HTTPRedirect[deployDashboard[league]]
		
		)&,
		AppearanceRules->{"Title"->"Add a new player"}
	],
	URLBuild[{leagueDir[league],"forms","newplayer"}],Permissions->"Public"
	]
]

Jammers`CreateLeague[l_]:=CloudEvaluate[createLeague[l]]

End[]
EndPackage[]