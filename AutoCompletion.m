(* ::Package:: *)

(*  Mathematica Package *)

(*  :Name: AutoCompletion` *)
(*  :Title: Enable TAB-autocompletion for Mathematica < 9.0 *)
(*  :Author: Federico Stra *)
(*  :Summary: *)
(*  :Context: Let` *)
(*  :Package Version: 1.0 *)
(*  :Copyright: Copyright 2018, Federico Stra *)
(*  :History: *)
(*  :Keywords: Notebook, FrontEnd, autocomplete, input *)
(*  :Source: *)
(*  :Warnings: *)
(*  :Mathematica Version: 8.0 *)
(*  :Limitations: *)
(*  :Discussion: *)


BeginPackage["AutoCompletion`"]

Unprotect[AutoComplete]

ClearAll[AutoComplete]

AutoComplete::usage = "\
AutoComplete[\!\(\*StyleBox[\"flag\", \"TI\"]\)] \
turns autocompletion on or off by passing \
\!\(\*StyleBox[\"flag\", \"TI\"]\) \
equal to True or False."


Begin["`Private`"]


AutoComplete[True] := SetOptions[SelectedNotebook[], 
   NotebookEventActions -> {{"KeyDown", "\t"} :> 
      Module[{nb = SelectedNotebook[], prefix, oldCompletions, 
        completions, pos}, SelectionMove[nb, All, Word]; 
        prefix = NotebookRead[nb]; oldCompletions = 
         "AutoComplete" /. Options[nb]; If[oldCompletions === 
          "AutoComplete", oldCompletions = {}]; 
        If[MemberQ[oldCompletions, prefix], completions = 
          oldCompletions, completions = Names[StringJoin[prefix, 
             "*"]]; SetOptions[nb, "AutoComplete" -> completions]]; 
        If[completions === {}, SelectionMove[nb, After, Word], 
         pos = Position[completions, prefix, {1}, 1]; 
          If[pos === {}, pos = 0, pos = pos[[1,1]]]; 
          If[CurrentValue["ControlKey"], pos = Mod[pos - 1, 
             Length[completions], 1], pos = Mod[pos + 1, 
             Length[completions], 1]]; NotebookWrite[nb, 
           completions[[pos]]]]]}]
AutoComplete[False] := SetOptions[SelectedNotebook[], 
   NotebookEventActions -> Cases[NotebookEventActions /. Options[nb], 
     Except[HoldPattern[{"KeyDown", "\t"} :> _]]], 
   "AutoComplete" -> {}]


End[]


SetAttributes[AutoComplete, ReadProtected]

Protect[AutoComplete]

EndPackage[]
