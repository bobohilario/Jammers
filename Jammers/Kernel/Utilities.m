(* Wolfram Language Package *)

BeginPackage["Jammers`"]

Begin["`Private`"] 


(* file layout *)
$basedir="Jammers";
$dumppath=URLBuild[{$basedir,"codedump.mx"}];
leagueDir[league_]:=FileNameJoin[{$basedir,stringhash[league]}]


createLeague[league_]:=CreateDirectory[leagueDir[league],CreateIntermediateDirectories->True]
leagueExistsQ[league_]:=FileExistsQ[leagueDir[league]]

(* tools *)
stringhash[expr_]:=IntegerString[Hash[expr],16]
round[x_] := Round[x - Sign[x]/10^10]



gameCount[league_]:=Length[getJammerData["Games",league]]
playerList[league_]:=Keys[getJammerData["Players",league]]

End[]

EndPackage[]