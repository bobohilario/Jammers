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
		Put[newdata,file];
		newdata
	]
]
	
backupJammerData[datatype_,league_]:=With[{file=jammerDataFile[datatype,league]},
	If[FileExistsQ[file],
		CopyFile[file,backupJammerDataFile[datatype,league],OverwriteTarget->True]
	]
]


updatePlayersRecords[league_,KeyValuePattern[{"t1"->t1_,"t2"->t2_,"Score"->sc_,"Winner"->win_}]]:=Block[{},
Put[win,"debugLog"];
	updatePlayerRecords[league,win==="Team 1",#]&/@Values[t1];
	updatePlayerRecords[league,win==="Team 2",#]&/@Values[t2];
	
]

$editinggame=False;
updatePlayerRecords[league_,win_,player_]:=Block[{data=getJammerData["Players",league][player]},
PutAppend["w"->win,"debugLog"];
	data["Games"]=data["Games"]+1;
	data["LastGame"->Now];
	If[TrueQ[win],
		data["Wins"]=data["Wins"]+1,
		data["Losses"]=data["Losses"]+1;
	];
	updateJammerData["Players",league,<|player->data|>]
]/;!TrueQ[$editinggame]

updatePlayerRecords[league_,win_,player_]:=Block[{data=getJammerData["Players",league][player]},
	If[TrueQ[win],
		data["Wins"]=data["Wins"]+1;
		data["Losses"]=data["Losses"]-1;,
		data["Losses"]=data["Losses"]+1;
		data["Wins"]=data["Wins"]-1;
	];
	updateJammerData["Players",league,<|player->data|>]
]/;TrueQ[$editinggame]

$newPlayerData=<|"Games"->0,"Wins"->0,"Losses"->0,"Handicap"->0,"LastGame"->0|>;

End[]

EndPackage[]