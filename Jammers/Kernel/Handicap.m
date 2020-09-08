(* Wolfram Language Package *)

BeginPackage["Jammers`"]

Begin["`Private`"] 

handicapGame[league_,t1_,t2_]:=With[{data=getJammerData["Players",league]},
	21+{round@Total[Lookup[Lookup[data,t1,<||>],"Handicap",0]],
		round@Total[Lookup[Lookup[data,t2,<||>],"Handicap",0]]
	}
	
]
	
	
updateHandicaps[league_]:=Block[{caps=computeHandicaps[league],playerdata=getJammerData["Players",league]},
	playerdata=Association[KeyValueMap[#1->Association[#2,"Handicap"->Lookup[caps,#1,0]]&,playerdata]];
	If[AssociationQ[playerdata],
		updateJammerData["Players",league,newdata],
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


End[]

EndPackage[]