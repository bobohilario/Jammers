(* Wolfram Language Package *)

BeginPackage["Jammers`"]

Begin["`Private`"] 

handicapGame[league_,t1_,t2_]:=With[{data=getJammerData["Players",league]},
	21+{round@Total[Lookup[Lookup[data,t1,<||>],"Handicap",0]],
		round@Total[Lookup[Lookup[data,t2,<||>],"Handicap",0]]
	}
	
]
	
	
End[]

EndPackage[]