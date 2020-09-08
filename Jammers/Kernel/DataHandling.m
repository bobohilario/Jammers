(* Wolfram Language Package *)

BeginPackage["Jammers`"]

Begin["`Private`"] 
getJammerData[datatype_,league_]:=If[!$CloudEvaluation,
	CloudEvaluate,Identity][With[{file=jammerDataFile[datatype,league]},
	If[FileExistsQ[file],
		Get[file],
		<||>
	]
]]



jammerDataDir[datatype_,league_]:=FileNameJoin[{leagueDir[league],datatype}]
jammerDataFile[datatype_,league_]:=FileNameJoin[{jammerDataDir[datatype,league],"current"}]
backupJammerDataFile[datatype_,league_]:=FileNameJoin[{jammerDataDir[datatype,league],DateString[{"Year", "Month", "Day", "Hour", "Minute"}]}]


updateJammerData[datatype_,league_,newdata_]:=Block[{olddata=getJammerData[datatype,league],data,file=jammerDataFile[datatype,league]},
	data=Association[olddata,newdata];
	Quiet@CreateDirectory[FileNameDrop[file],CreateIntermediateDirectories->True];
	If[AssociationQ[data],
		backupJammerData[datatype,league];
		Put[data,file];
		data
		,
		Put[newdata,file]
	]
]
	
backupJammerData[datatype_,league_]:=With[{file=jammerDataFile[datatype,league]},
	If[FileExistsQ[file],
		CopyFile[file,backupJammerDataFile[datatype,league],OverwriteTarget->True]
	]
]


updatePlayersData[league_,KeyValuePattern[{"t1"->t1_,"t2"->t2_,"Score"->sc_}]]:=Block[{
	handicapScores=handicapGame[league,Values@t1,Values@t2],diff},
	(* if diff > 0 team 2 wins *)
	diff=(handicapScores[[1]]-sc["t1"])-(handicapScores[[2]]-sc["t2"]);
	updatePlayerData[league,diff,#]&/@Values[t1];
	updatePlayerData[league,-diff,#]&/@Values[t2];
	
]

updatePlayerData[league_,difference_,player_]:=Block[{data=getJammerData["Players",league][player]},
	data["Games"]=data["Games"]+1;
	If[difference>0,
		data["Losses"]=data["Losses"]+1;
		,
		data["Wins"]=data["Wins"]+1;
	];
	data["Handicap"]=data["Handicap"]+difference/4;
	
	updateJammerData["Players",league,<|player->data|>]
	
]

$newPlayerData=<|"Games"->0,"Wins"->0,"Losses"->0,"Handicap"->0,"LastGame"->0|>;

End[]

EndPackage[]