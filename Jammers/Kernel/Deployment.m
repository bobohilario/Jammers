(* Wolfram Language Package *)

BeginPackage["Jammers`"]

Jammers`DeployCode
Jammers`DeployGameForm
Jammers`DeployHandicapForm
Jammers`DeployEditGameForm
Jammers`DeployPlayerForm

Jammers`RandomGameForm
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
		"Score"->CompoundElement[{{"t1","Team 1"} -> "Integer", {"t2","Team 2"} -> "Integer"}],
		"Winner"->{"Team 1","Team 2"}
	},
		(
		Clear["Jammers`","Jammers`Private`"];
		Get[dumpfile];
		With[{n=gameCount[l]},
			updateJammerData["Games",l,<|(n+1)->#|>];
			updatePlayersRecords[l,#];
			updateHandicaps[league];
		
		HTTPRedirect[deployJammerDashboard[league]]
		]
		)&,
		AppearanceRules->{"Title"->"Record Game Results"}
	]]],
	URLBuild[{leagueDir[l],"forms","newgame"}],Permissions->"Public"
	]
]


DeployEditGameForm[league_]:=With[{dumpfile=$dumppath,l=league},
	CloudDeploy[
		APIFunction[{"game"->"Integer"},
		(Clear["Jammers`","Jammers`Private`"];
		Get[dumpfile];
		With[{players=playerList[l],g=#game,data=getJammerData["Games",l][#game]},
		
		FormFunction[{
		{"t1","Team 1"}-><|"Interpreter" -> 
		  CompoundElement[{"player1" -> players, "player2" -> players}], 
		 "Input" -> data["t1"]
		 |>,
		{"t2","Team 2"}-><|"Interpreter" -> 
		  CompoundElement[{"player1" -> players, "player2" -> players}], 
		 "Input" -> data["t2"]
		 |>,
		"Score"-><|"Interpreter" -> 
		  CompoundElement[{{"t1","Team 1"} -> "Integer", {"t2","Team 2"} -> "Integer"}], 
		 "Input" -> data["Score"]
		 |>,
		"Winner"-><|"Interpreter" ->{"Team 1","Team 2"}, 
		 "Input" -> data["Winner"]
		 |>
		},
			(
			Clear["Jammers`","Jammers`Private`"];
			Get[dumpfile];
			Block[{$editinggame=True,$oldwinner=data["Winner"]},
				updateJammerData["Games",l,<|g->#|>];
				If[data["Winner"]=!=#Winner,
					updatePlayersRecords[l,#]
				];
				updateHandicaps[l];
				HTTPRedirect[deployJammerDashboard[league]]
			]
			)&,
			AppearanceRules->{"Title"->"Edit Game Results"}
			]
		])&],
	URLBuild[{leagueDir[l],"forms","editgame"}],Permissions->"Public"
	]
]

DeployHandicapForm[league_]:=With[{dumpfile=$dumppath,l=league},
	CloudDeploy[Delayed[
		Clear["Jammers`","Jammers`Private`"];
		Get[dumpfile];
		With[{players=playerList[league]},
		
		FormFunction[{
		{"t1","Team 1"}->CompoundElement[{"player1" -> players, "player2" -> players}],
		{"t2","Team 2"}->CompoundElement[{"player1" -> players, "player2" -> players}]
	},
		(
		Clear["Jammers`","Jammers`Private`"];
		Get[dumpfile];
		AssociationThread[{"Team 1 Target","Team 2 Target"},handicapGame[league,Values[#t1],Values[#t2]]]
		
		)&,
		AppearanceRules->{"Title"->"Determine Handicap"}
	]]],
	URLBuild[{leagueDir[l],"forms","handicaps"}],Permissions->"Public"
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
		HTTPRedirect[deployJammerDashboard[league]]
		
		)&,
		AppearanceRules->{"Title"->"Add a new player"}
	],
	URLBuild[{leagueDir[league],"forms","newplayer"}],Permissions->"Public"
	]
]


Jammers`RandomGameForm[league_]:=With[{dumpfile=$dumppath},
	CloudDeploy[
	Delayed[
		(
		Clear["Jammers`","Jammers`Private`"];
		Get[dumpfile];
		
		With[{playerdata=getJammerData["Players",league],chosen=Lookup[getJammerData["CurrentPlayers",league],"CurrentPlayers",{}]},
			FormFunction[{"p"-><|"Interpreter" -> AnySubset[Keys[playerdata]], "Input" -> chosen|>},
				(updateJammerData["CurrentPlayers",league,<|"CurrentPlayers"->#p|>];
				chooseRandomGame[#p,playerdata])&
				,
				AppearanceRules->{"Title"->"Create a Random Matchup"}
			]
		
		]
		
		)
	],
	URLBuild[{leagueDir[league],"forms","matchup"}],Permissions->"Public"
	]
]


Jammers`CreateLeague[l_]:=CloudEvaluate[createLeague[l]]

End[]
EndPackage[]