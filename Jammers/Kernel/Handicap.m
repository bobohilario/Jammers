(* Wolfram Language Package *)

BeginPackage["Jammers`"]

Begin["`Private`"] 

handicapGame[league_,t1_,t2_]:=handicapgame[getJammerData["Players",league],t1,t2]
	
handicapgame[playedata_,t1_,t2_]:=21+{round@Total[Lookup[Lookup[playedata,t1,<||>],"Handicap",0]],
		round@Total[Lookup[Lookup[playedata,t2,<||>],"Handicap",0]]
	}
	
updateHandicaps[league_]:=Block[{caps=computeHandicaps[league],playerdata=getJammerData["Players",league]},
	playerdata=Association[KeyValueMap[#1->Association[#2,"Handicap"->Lookup[caps,#1,0]]&,playerdata]];
	If[AssociationQ[playerdata],
		updateJammerData["Players",league,playerdata],
		$Failed
	]
]



computeHandicaps[league_]:=Block[{gamedata=getJammerData["Games",league],caps=<||>,games=<||>},
	({caps,games}=Echo[computeHandicaps[{caps,games},#]])&/@gamedata;
	caps/games
]
	
computeHandicaps[{caps_,games_},KeyValuePattern[{"t1"->t1_,"t2"->t2_,"Score"->sc_}]]:=Block[{adjustment=
	(sc["t1"]-sc["t2"])/4,new=caps,g=games},
	(new[#]=Lookup[caps,#,0]+adjustment;g[#]=Lookup[games,#,0]+1)&@t1["player1"];
	(new[#]=Lookup[caps,#,0]+adjustment;g[#]=Lookup[games,#,0]+1)&@t1["player2"];
	(new[#]=Lookup[caps,#,0]-adjustment;g[#]=Lookup[games,#,0]+1)&@t2["player1"];
	(new[#]=Lookup[caps,#,0]-adjustment;g[#]=Lookup[games,#,0]+1)&@t2["player2"];
	{new,g}
]


chooseRandomGame[current_,playerdata_]:=With[{r=Partition[RandomSample[current, 4], 2]},
AssociationThread[{r[[1]],r[[2]]},handicapgame[playerdata,r[[1]],r[[2]]]]
]

End[]

EndPackage[]