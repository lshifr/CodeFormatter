(* ::Package:: *)

(* Mathematica Package *)

(* :Title: CodeFormatter   *) 

(* :Author: Leonid B. Shifrin    *)

(* :Summary:  Pretty-printer for Mathematica code in box form  *)

(* :Context: CodeFormatter`  *)

(* :Package version: 1.0   *)

(* :Copyright:  Copyright 2012, Leonid B.Shifrin.
	The package is released under MIT Open Source license     	
*)

(*  :History:  Version 1.0  March 2012    *)

(*  :Keywords: code, format, pretty-print *)

(*  :Mathematica version:  8.0  *)



BeginPackage["CodeFormatter`"]
(* Exported symbols added here with SymbolName::usage *) 

FullCodeFormat::usage = 
"FullCodeFormat[code_]  takes some Mathematica expression (in most cases that would
probably be some code) in the box form, and attempts to format this expression to 
make it easier to read and more understandable, by inserting tabs and new lines. 
The formatted boxed expression is then returned";

FullCodeFormatCompact::usage = 
"FullCodeFormatCompact[code_] works like FullCodeFormat[code_], but uses setting which 
make the resulting formatted expression more compact-looking";



SEFormat::usage = 
"SEFormat[code] formats code in a custom way applicable to use in mathematica.stackexchange.com 
posts ";


CodeFormatterMakeBoxes::usage = "CodeFormatterMakeBoxes[expr] returns a simplified box representation
for expr. The formatter can use it to handle code which it can't handle using MakeBoxes";

CodeFormatterPrint::usage = "CodeFormatterPrint[f] pretty-prints definitions (OwnValues, DownValues, UpValues 
and SubValues), associated with the symbol";

CodeFormatterSpelunk::usage = "CodeFormatterSpelunk[f] pretty-prints main definitions (OwnValues, 
DownValues, UpValues and SubValues) for symbol f, using short names for all symbols in the code

CodeFormatterSpelunk[f,boxFunction] uses a custom function boxFunction to convert definitions to
boxes. The default value is CodeFormatterMakeBoxes, but one can also try MakeBoxes - if the latter
works, it may produce somewhat better result
";


CodeFormatted::usage = "CodeFormatted[code]  prints a cell with the formatted code. This function 
has an attribute HoldAll";

Begin["`Private`"]
(* Implementation of the package *)


$supportedBoxes = {StyleBox, TagBox, FractionBox,ItemBox (*,DynamicModuleBox*)};
$combineTopLevelStatements = True;
$maxLineLength = 70;
$alignClosingBracket = True;
$alignClosingList = True;
$alignClosingParen = True;
$alwaysBreakCompoundExpressions = False;
$alignClosingScopingBracket = True;
$alignClosingIfBracket = True; 
$defaultTabWidth = 4;
$useSpacesForTabs = True;
$overallTab = 4;


$inlSymbol = "\[IndentingNewLine]";

ClearAll[preprocess];
preprocess[boxes_] :=
      boxes //.
                RowBox[{("\t" | "\n") .., expr___}]                            :> expr /.
                timesBox : RowBox[{_, e_String /; StringMatchQ[e, " " ..], _}] :> ReplacePart[timesBox, {1, 2} -> "$TimesMark$"] /.
                s_String /; StringMatchQ[s, (Whitespace | $inlSymbol | "") ..] :> Sequence[] /.
                "$TimesMark$"                                                  -> " " //.
                {
                 RowBox[{r_RowBox}]                                            :> r,
                 RowBox[{}]                                                    :> Sequence[],
                 RowBox[{"Association", "[", inner__, "]"}]                    :> RowBox[{"<|", inner, "|>"}]
                };


ClearAll[$blocks, blockQ];
$blocks = {
   ModuleBlock, BlockBlock, WithBlock, SetBlock, SetDelayedBlock, 
   CompoundExpressionBlock, GeneralHeadBlock, HeadBlock, ElemBlock, 
   GeneralBlock, ParenBlock, ListBlock, PatternBlock, PatternTestBlock, 
   FunctionBlock, AlternativesBlock, StatementBlock, NewlineBlock, 
   MultilineBlock, FinalTabBlock, GeneralSplitHeadBlock, EmptySymbol, 
   SemicolonSeparatedGeneralBlock, SuppressedCompoundExpressionBlock, 
   GeneralBoxBlock, TagSetDelayedBlock, TagSetBlock, ApplyBlock, 
   ApplyLevel1Block, RuleBlock, RuleDelayedBlock, MapBlock, FunApplyBlock, 
   IfBlock, IfBlock, IfCommentBlock,DataArrayBlock,ReplaceAllBlock,
	ReplaceRepeatedBlock, CustomTabBlock
   };
   
blockQ[block_Symbol] :=
    MemberQ[$blocks, block];


ClearAll[boxQ, boxNArgs]

boxQ[box_Symbol] :=
    MemberQ[$supportedBoxes, box];


boxNArgs[_StyleBox] = 1;
boxNArgs[_FractionBox] = 2;
boxNArgs[_TagBox] = 1;
boxNArgs[_ItemBox]=2;


boxNArgs[_] :=
    Throw[$Failed, boxNArgs];


ClearAll[strsymQ];
strsymQ[s_String] :=
    StringMatchQ[ s, (LetterCharacter | "$") ~~ ((WordCharacter | "$" | "`") ...)];
 
ClearAll[preformat];
(* SetAttributes[preformat,HoldAll]; *)

preformat[expr : (box_?boxQ[args___])] :=
    With[ {n = boxNArgs[expr]},
        GeneralBoxBlock[box,
         n,
         Sequence @@ Map[preformat, Take[{args}, n]],
         Sequence @@ Drop[{args}, n]
         ]
    ];



preformat[CustomTabBlock[expr_,width_]]:=
	TabBlock[preformat[expr],True,width];


preformat[
   RowBox[{
      head : ("Module" | "Block" | "With"), 
      "[", RowBox[{decl_RowBox, ",", body_RowBox}], "]"
   }]
] :=
    (head /. {"Module" -> ModuleBlock, "Block" -> BlockBlock, "With" -> WithBlock})[
		preformat@decl, 
        preformat@body
    ];

preformat[RowBox[{lhs_, assignment : (":=" | "="), rhs_}]] :=
    (assignment /. {":=" :> SetDelayedBlock, "=" :> SetBlock})[
		preformat[lhs], 
		preformat[rhs]
	];

preformat[
   RowBox[{s_String?strsymQ, "/:", lhs_, assignment : (":=" | "="), 
     rhs_}]] :=
    (assignment /. {":=" :> TagSetDelayedBlock, 
        "=" :> TagSetBlock})[s, preformat[lhs], preformat[rhs]];

preformat[RowBox[{fn_, "@@", expr_}]] :=
    ApplyBlock[preformat@fn, preformat@expr];

preformat[RowBox[{fn_, "@@@", expr_}]] :=
    ApplyLevel1Block[preformat@fn, preformat@expr];

preformat[RowBox[{fn_, "/@", expr_}]] :=
    MapBlock[preformat@fn, preformat@expr];

preformat[RowBox[{fn_, "@", expr_}]] :=
    FunApplyBlock[preformat@fn, preformat@expr];

preformat[RowBox[{lhs_, "\[Rule]", rhs_}]] :=
    RuleBlock[preformat@lhs, preformat@rhs];

preformat[RowBox[{lhs_, "\[RuleDelayed]", rhs_}]] :=
    RuleDelayedBlock[preformat@lhs, preformat@rhs];

preformat[RowBox[{lhs_,"//.","\[VeryThinSpace]",rhs_}]]:=
    ReplaceRepeatedBlock[preformat[lhs],preformat[rhs]];

preformat[RowBox[{lhs_,"/.","\[VeryThinSpace]",rhs_}]]:=
    ReplaceAllBlock[preformat[lhs],preformat[rhs]];

preformat[RowBox[{p_, "?", test_}]] :=
    PatternTestBlock[preformat[p], preformat[test]];

preformat[RowBox[{body_, "&"}]] :=
    FunctionBlock[preformat[body]];

preformat[RowBox[alts : {PatternSequence[_, "|"] .., _}]] :=
    AlternativesBlock @@ Map[preformat, alts[[1 ;; -1 ;; 2]]];

preformat[RowBox[{"If", "[", RowBox[{cond_, ",", iftrue_, ",", iffalse_}], "]"}]] :=
    IfBlock[preformat@cond, preformat@iftrue, preformat@iffalse];



preformat[RowBox[elems : {PatternSequence[_, ";"] ..}]] :=
    SuppressedCompoundExpressionBlock @@ Map[
      Map[preformat, StatementBlock @@ DeleteCases[#, ";"]] &,
      Split[elems, # =!= ";" &]];

preformat[RowBox[elems : {PatternSequence[_, ";"] .., _}]] :=
    CompoundExpressionBlock @@ Map[
      Map[preformat, StatementBlock @@ DeleteCases[#, ";"]] &,
      Split[elems, # =!= ";" &]];

preformat[RowBox[elems_List]] /; ! FreeQ[elems, "\n" | "\t", 1] :=
    preformat[RowBox[DeleteCases[elems, "\n" | "\t"]]];

preformat[RowBox[{"{", elems___, "}"}]] :=
    ListBlock @@ Map[preformat, {elems}];

preformat[RowBox[{"<|",elems___,"|>"}]]:=
	DataArrayBlock @@ Map[preformat, {elems}];

preformat[RowBox[{"(", elems__, ")"}]] :=
    ParenBlock @@ Map[preformat, {elems}];

preformat[RowBox[{p_String?strsymQ, ":", elem_}]] :=
    PatternBlock[p, preformat@elem];

preformat[RowBox[{p_String?strsymQ, ":", elem_, ":", def_}]] :=
    PatternBlock[p, preformat@elem, preformat@def];

preformat[RowBox[{head_, "[", elems___, "]"}]] :=
    GeneralHeadBlock[preformat@head, 
     Sequence @@ Map[preformat, {elems}]];

preformat[RowBox[elems : {PatternSequence[_, ","] .., _}]] :=
    SemicolonSeparatedGeneralBlock @@ 
     Map[preformat, DeleteCases[elems, ","]];

preformat[RowBox[elems_List]] :=
    GeneralBlock @@ Map[preformat, elems];

preformat[block_?blockQ[args_]] :=
    block @@ Map[preformat, {args}];

preformat[a_?AtomQ] := a;

preformat[expr_] :=
  Throw[{$Failed, expr}, preformat];





ClearAll[processPreformatted];
processPreformatted[GeneralBlock[blocks__]]/;$combineTopLevelStatements :=
    GeneralBlock[Sequence @@ Map[processPreformatted, {blocks}]];

processPreformatted[
   preformatted : SemicolonSeparatedGeneralBlock[elems___]
]/;$combineTopLevelStatements  :=
    GeneralBoxBlock[RowBox[{##}] &, Length[{elems}], elems];

processPreformatted[arg_] :=  arg;




ClearAll[tabify];
tabify[expr_] /; ! FreeQ[expr, TabBlock[_]|TabBlock[_,_]] :=
    tabify[expr //. TabBlock[sub_] :> TabBlock[sub, True,$tabWidth]];

tabify[(block_?blockQ /; ! MemberQ[{TabBlock, FinalTabBlock}, block])[elems___]] :=
    block @@ Map[tabify, {elems}];

tabify[TabBlock[FinalTabBlock[el_, flag_,fwidth_:$tabWidth], tflag_,width_:$tabWidth]] :=
    FinalTabBlock[tabify[TabBlock[el, tflag,width]], flag,fwidth];

tabify[TabBlock[NewlineBlock[el_, flag_], _,width_:$tabWidth]] :=
    tabify[NewlineBlock[TabBlock[el, True,width], flag]];

tabify[TabBlock[t_TabBlock, flag_,width_:$tabWidth]] :=
    tabify[TabBlock[tabify[t], flag,width]];

tabify[TabBlock[GeneralBoxBlock[box_, n_, args___], flag_,width_:$tabWidth]] :=
    GeneralBoxBlock[box, n,
     Sequence @@ Map[tabify[TabBlock[#, flag,width]] &, Take[{args}, n]],
     Sequence @@ Drop[{args}, n]
     ];

tabify[TabBlock[(block_?blockQ /; ! MemberQ[{TabBlock}, block])[ elems___], flag_,width_:$tabWidth]] :=
    FinalTabBlock[
     block @@ Map[tabify@TabBlock[#, False,width] &, {elems}],
     flag,
     width];

tabify[TabBlock[a_?AtomQ, flag_,width_:$tabWidth]] :=
    FinalTabBlock[a, flag,width];

tabify[expr_] :=  expr;



ClearAll[isNextNewline];
isNextNewline[_NewlineBlock] := True;

isNextNewline[block : (_?blockQ | TabBlock)[fst_, ___]] :=
    isNextNewline[fst];

isNextNewline[_] := False;



ClearAll[postformat];
postformat[GeneralBlock[elems__]] :=
    RowBox[postformat /@ {elems}];
(* Note: BlankSequence in body intentional, to allow for closing element *)
postformat[(head : ModuleBlock | BlockBlock | WithBlock)[vars_, body__]] :=
    RowBox[{
      head /. {ModuleBlock -> "Module", BlockBlock -> "Block", WithBlock -> "With"},
      "[",
      RowBox[{
        postformat[vars], ",", 
        Sequence @@ (Map[postformat, {body}] //. 
           EmptySymbol[] :> Sequence[])
        }],
      "]"}
     ];

postformat[(head : SetBlock | SetDelayedBlock | RuleBlock | RuleDelayedBlock)[lhs_, rhs_]] :=
    RowBox[{
      postformat@lhs, head /. {
        SetBlock -> "=", SetDelayedBlock -> ":=", 
        RuleBlock -> "\[Rule]", RuleDelayedBlock -> "\[RuleDelayed]"
        },
      postformat@rhs
      }];

postformat[(head : TagSetBlock | TagSetDelayedBlock)[s_, lhs_, rhs_]] :=
    RowBox[{
	  postformat@s, "/:", postformat@lhs, 
      head /. {TagSetBlock -> "=", TagSetDelayedBlock -> ":="}, 
      postformat@rhs}];

postformat[(head :  MapBlock | ApplyLevel1Block | ApplyBlock | FunApplyBlock)[f_,  expr_]] :=
    RowBox[{
      postformat@f, head /. {
        ApplyBlock -> "@@", ApplyLevel1Block -> "@@@", 
		MapBlock -> "/@", FunApplyBlock -> "@"
        },
      postformat@expr
      }];

postformat[ReplaceAllBlock[lhs_,rhs_]]:=
	RowBox[{postformat@lhs,"/.","\[VeryThinSpace]",postformat@rhs}]

postformat[ReplaceRepeatedBlock[lhs_,rhs_]]:=
	RowBox[{postformat@lhs,"//.","\[VeryThinSpace]",postformat@rhs}]

postformat[AlternativesBlock[elems__]] :=
    RowBox[Riffle[postformat /@ {elems}, "|"]];

postformat[FunctionBlock[body_]] :=
    RowBox[{postformat@body, "&"}];

postformat[PatternTestBlock[p_, body_]] :=
    RowBox[{postformat@p, "?", postformat@body}];

postformat[CompoundExpressionBlock[elems__]] :=
    RowBox[Riffle[postformat /@ {elems}, ";"]];

(* Note: fragile! *)

postformat[IfBlock[if_, cond_, iftrue_, ifcomment_, iffalse_, closingElement_]] /; ! FreeQ[ifcomment, IfCommentBlock] :=
    RowBox[{postformat@if, "[",
      RowBox[{postformat@cond, ",", postformat@iftrue, ",", 
        postformat@ifcomment, postformat@iffalse, 
        postformat@closingElement //. EmptySymbol[] :> Sequence[]}],
      "]"}];

postformat[IfBlock[cond_, iftrue_, iffalse_]] :=
    RowBox[{"If", "[",
      RowBox[{postformat@cond, ",", postformat@iftrue, ",", 
        postformat@iffalse}],
      "]"}];

postformat[IfCommentBlock[]] :=
    RowBox[{"(*", " ", "else", " ", "*)"}];

postformat[SuppressedCompoundExpressionBlock[elems__]] :=
    RowBox[Append[Riffle[postformat /@ {elems}, ";"], ";"]];

postformat[ListBlock[elems___]] :=
    RowBox[{"{", 
      Sequence @@ (Map[postformat, {elems}] //. 
         EmptySymbol[] :> Sequence[]), "}"}];

postformat[DataArrayBlock[elems___]] :=
    RowBox[{"<|", 
      Sequence @@ (Map[postformat, {elems}] //. 
         EmptySymbol[] :> Sequence[]), "|>"}];

postformat[ParenBlock[elems__]] :=
    RowBox[{"(", 
      Sequence @@ (Map[postformat, {elems}] //. 
         EmptySymbol[] :> Sequence[]), ")"}];

postformat[PatternBlock[name_, pt_]] :=
    RowBox[{postformat@name, ":", postformat@pt}];

postformat[PatternBlock[name_, pt_, def_]] :=
    RowBox[{postformat@name, ":", postformat@pt, ":", postformat@def}];

postformat[GeneralHeadBlock[head_, elems___]] :=
    RowBox[{postformat@head, "[", 
      Sequence @@ Riffle[postformat /@ {elems}, ","], "]"}];

postformat[GeneralSplitHeadBlock[head_, elems___, Tabbed[]]] :=
    RowBox[{postformat@head, "[", 
      Sequence @@ Riffle[postformat /@ {elems}, ","],(*"\n","\t", *)
      "]"}];

postformat[GeneralSplitHeadBlock[head_, elems___]] :=
    With[ {formattedElems = postformat /@ {elems}},
        RowBox[{postformat@head, "[",
          Sequence @@ Riffle[Most[formattedElems], ","],
          Last[formattedElems] //. EmptySymbol[] :> Sequence[], "]"}]
    ];

postformat[GeneralBlock[elems___]] :=
    RowBox[Riffle[postformat /@ {elems}, ","]];

postformat[StatementBlock[elem_]] :=
    postformat[elem];

postformat[MultilineBlock[elems__]] :=
    RowBox[Riffle[postformat /@ {elems}, "\n"]];



postformat[NewlineBlock[elem_?isNextNewline, False]] :=
    postformat@elem;


postformat[SemicolonSeparatedGeneralBlock[elems__]] :=
    RowBox[Riffle[postformat /@ {elems}, ","]];

postformat[NewlineBlock[elem_, _]] :=
    RowBox[{"\n", postformat@elem}];

postformat[GeneralBoxBlock[box_, n_, args___]] :=
    box[
     Sequence @@ Map[postformat, Take[{args}, n]],
     Sequence @@ Drop[{args}, n]
     ];




postformat[FinalTabBlock[expr_, True,width_]] :=
    RowBox[{
		If[$useSpacesForTabs,StringJoin[ConstantArray[" ",{width}]],"\t"], 
		postformat@expr
	}];

postformat[FinalTabBlock[expr_, False,_]] :=
    postformat@expr;

postformat[EmptySymbol[]] :=
    EmptySymbol[];

postformat[a_?AtomQ] :=  a;


postformat[arg_] :=
    Throw[{$Failed, arg}, postformat];




Clear[maxLen];
maxLen[boxes : (_RowBox | _?boxQ[___])] :=
    Max@Replace[
      Split[
       Append[Cases[boxes, s_String, Infinity], "\n"], # =!= "\n" &],
      {s___, ("\t" | " ") ..., "\n"} :> 
       Total[{s} /. {"\t" -> $tabWidth, ss_ :> StringLength[ss]}],
      {1}];


maxLen[expr_] :=
    With[ {boxes = postformat@expr},
        maxLen[boxes] /; MatchQ[boxes, (_RowBox | _?boxQ[___])]
    ];

maxLen[expr_] :=
    Throw[{$Failed, expr}, maxLen];
  
  


ClearAll[$closingElementRules];
$closingElementRules = {
   "Bracket" :> $alignClosingBracket ,
   "List" :> $alignClosingList,
   "Parenthesis" :> $alignClosingParen, 
   "ScopingBracket" :> $alignClosingScopingBracket,
   "IfBracket" :> $alignClosingIfBracket
   };  

ClearAll[closingElement];
closingElement[type_String] :=
    Unevaluated[
		If[TrueQ@type,
             NewlineBlock[EmptySymbol[], True],
             (* else *)
             EmptySymbol[]
        ]
    ] /. $closingElementRules; 
     
     


ClearAll[needSplitQ];
needSplitQ[expr_, currentTab_] :=
    maxLen[expr] > $maxLineLength - currentTab;
 
  
ClearAll[format];

format[expr_] :=
    format[expr, 0];

format[expr : GeneralBoxBlock[box_, n_, args___], currentTab_] :=
    With[ {splitQ = needSplitQ[expr, currentTab]},
        GeneralBoxBlock[box, n,
         If[ n > 0,
             format[First@{args}, currentTab],
             Sequence @@ {}
         ],
         Sequence @@ 
          Map[format[If[ splitQ,
                         NewlineBlock[#, False],
                         #
                     ], currentTab] &, 
           Take[{args}, {2, n}]],
         Sequence @@ Drop[{args}, n]
         ]
    ];


format[TabBlock[expr_], currentTab_] :=
    TabBlock[format[expr, currentTab + $tabWidth]];

format[TabBlock[expr_,True,width_],currentTab_]:=
	TabBlock[format[expr,currentTab+width],True,width];

format[NewlineBlock[expr_, flag_], currentTab_] :=
    NewlineBlock[format[expr, currentTab], flag];

format[block_?blockQ[left___, sc : (_ModuleBlock | _BlockBlock | _WithBlock), right___], currentTab_] :=
    format[block[left, NewlineBlock[sc, True], right], currentTab];

format[(head : ModuleBlock | BlockBlock | WithBlock)[vars_, body_], 
   currentTab_] :=
    head[
     format[vars, currentTab],
     format[NewlineBlock[TabBlock[body], False], currentTab],
     closingElement["ScopingBracket"]
     ];

format[(head : SetDelayedBlock)[lhs_, rhs_], currentTab_] :=
    head[
     format[lhs, currentTab],
     format[NewlineBlock[TabBlock[rhs], False], currentTab]
     ];


format[(head : (ReplaceAllBlock|ReplaceRepeatedBlock))[lhs_, rhs_], currentTab_] :=
    head[
     format[lhs, currentTab],
     format[NewlineBlock[TabBlock[rhs], False], currentTab]
     ];

format[TagSetDelayedBlock[s_, lhs_, rhs_], currentTab_] :=
    TagSetDelayedBlock[
     format[s, currentTab],
     format[lhs, currentTab],
     format[NewlineBlock[TabBlock[rhs], False], currentTab]
     ];

format[expr : (head : (SetBlock | RuleBlock | RuleDelayedBlock))[lhs_,
       rhs_], currentTab_] /; needSplitQ[expr, currentTab] :=
    head[
     format[lhs, currentTab],
     format[NewlineBlock[TabBlock[rhs], False], currentTab]
     ];


format[(ce : (CompoundExpressionBlock | SuppressedCompoundExpressionBlock))[elems__], currentTab_] :=
    With[ {formatted = Map[format[#, currentTab] &, {elems}]},
        (ce @@ Map[NewlineBlock[#, False] &, formatted]) /;
         $alwaysBreakCompoundExpressions || !FreeQ[formatted, NewlineBlock]
    ];


format[StatementBlock[el_], currentTab_] :=
    StatementBlock[format[el, currentTab]];

format[expr : IfBlock[cond_, iftrue_, iffalse_], currentTab_] /; 
   needSplitQ[expr, currentTab] :=
    With[ {formatF = 
       format[TabBlock@NewlineBlock[#, False], currentTab] &},
        IfBlock[
         format["If", currentTab],
         formatF@cond,
         formatF@iftrue,
         formatF@IfCommentBlock[],
         formatF@iffalse,
         closingElement["IfBracket"]
         ]
    ];

format[expr : GeneralHeadBlock[head_, elems___], currentTab_] :=
    With[ {splitQ = needSplitQ[expr, currentTab]},
        GeneralSplitHeadBlock(* GeneralHeadBlock *)[
          format[head, currentTab],
          Sequence @@ Map[
            format[If[ splitQ,
                       TabBlock@NewlineBlock[#, False],
                       #
                   ], 
              currentTab] &,
            {elems}],
          closingElement["Bracket"]
          ] /; splitQ
    ];

format[expr : (ListBlock[elems___]), currentTab_] /; needSplitQ[expr, currentTab] :=
    NewlineBlock[
     ListBlock[
      Sequence @@ Map[format[TabBlock@NewlineBlock[#, False], currentTab] &, {elems}],
      closingElement["List"]
      ],
     True];

format[expr : (ParenBlock[elems___]), currentTab_] /; needSplitQ[expr, currentTab] :=
    NewlineBlock[
     ParenBlock[
      Sequence @@  Map[format[TabBlock@NewlineBlock[#, False], currentTab] &, {elems}],
      closingElement["Parenthesis"]
      ],
     True];

format[expr : ((head : (ApplyBlock | ApplyLevel1Block | MapBlock |  FunApplyBlock))[f_, e_]), currentTab_] /; 
   needSplitQ[expr, currentTab] :=
    head[
     format[f, currentTab],
     format[TabBlock@NewlineBlock[e, False], currentTab]
     ];

(* For a generic block, it is not obvious that we have to tab, so we \
don't*)
format[expr : (block_?blockQ[elems___]), currentTab_] :=
    With[ {splitQ = needSplitQ[expr, currentTab]},
        block @@ Map[
          format[If[ splitQ,
                     NewlineBlock[#, False],
                     #
                 ], currentTab] &,
          {elems}]
    ];

format[a_?AtomQ, _] := a;
 
    
$tabWidth = 4;


    
ClearAll[FullCodeFormat, FullCodeFormatCompact];
FullCodeFormat[boxes_] :=
Block[{$tabWidth = $defaultTabWidth},
    postformat@
     tabify@format@processPreformatted@preformat@preprocess@boxes
];

FullCodeFormatCompact[boxes_] :=
    Block[ {$alignClosingBracket = False,
      $alignClosingList = False,
      $alignClosingParen = False,
      $alignClosingScopingBracket = False,
      $alignClosingIfBracket = False},
        FullCodeFormat[boxes]
    ];    
     

SEFormat[boxes_,lineWidth_,tabWidth_, overallTab_]:=
	Block[{$defaultTabWidth = tabWidth, $maxLineLength = lineWidth,$overallTab= overallTab ,
		$useSpacesForTabs  = True},
		FullCodeFormat@CustomTabBlock[boxes,$overallTab]
	];
	
	
	
(*============================================================================================*)
(*=========			 Simplified version of MakeBoxes 		==============*)
(*============================================================================================*)
	
	
ClearAll[$infixRules, infixForm, infixQ, $multiInfixRules, multiInfixQ, multiInfixFormSeparator];

$infixRules = {
	Set -> "=", 
	SetDelayed -> ":=", 
	Rule -> "->", 
   	RuleDelayed -> ":>", 
   	Map -> "/@", 
   	Apply -> "@@", 
   	Pattern -> ":", 
   	PatternTest -> "?", 
   	Condition -> "/;", 
   	Optional -> ":", 
   	ReplaceAll -> "/.", 
   	ReplaceRepeated -> "//."
   };
   
$multiInfixRules = {
	Alternatives -> "|", 
	And -> "&&", 
	Or -> "||", 
   	Plus -> "+", 
   	Times -> "*", 
   	CompoundExpression -> ";", 
   	Less -> "<", 
   	LessEqual -> "<=", 
   	Greater -> ">", 
   	GreaterEqual -> ">=", 
   	Equal -> "==", 
   	Unequal -> "!=", 
   	SameQ -> "===", 
   	UnsameQ -> "=!="
   };
   
infixForm[s_] := s /. $infixRules;

SetAttributes[{infixQ, multiInfixQ}, HoldAll];
infixQ[s_Symbol] := MemberQ[$infixRules[[All, 1]], HoldPattern[s]];

multiInfixQ[s_Symbol] := 
  MemberQ[$multiInfixRules[[All, 1]], HoldPattern[s]];

multiInfixFormSeparator[s_Symbol] := s /. $multiInfixRules;



ClearAll[boxesRiffle];
SetAttributes[boxesRiffle, HoldRest];
boxesRiffle[separator_, elems___] := 
  RowBox@Riffle[Map[makeBoxes, Unevaluated[{elems}]], separator];


ClearAll[makeBoxes];
SetAttributes[makeBoxes, HoldAllComplete];
makeBoxes[args___] := boxesRiffle[",", args];

makeBoxes[List[args___]] := RowBox[{"{", makeBoxes[args], "}"}];

makeBoxes[ Verbatim[Pattern][sym_Symbol, 
    bl : (Verbatim[Blank][___] | Verbatim[BlankSequence][___] | 
       Verbatim[BlankNullSequence][___])]] := 
  makeBoxes[sym] <> ToString[Unevaluated@bl];

makeBoxes[Function[body_]] := RowBox[{makeBoxes[body], "&"}];

makeBoxes[Slot[i_Integer]] := "#" <> ToString[i];

makeBoxes[SlotSequence[i_Integer]] := "##" <> ToString[i];

makeBoxes[(h_Symbol?infixQ)[lhs_, rhs_]] := 
  RowBox[{makeBoxes[lhs], infixForm[h], makeBoxes[rhs]}];

makeBoxes[(h_Symbol?multiInfixQ)[args___]] := 
  RowBox[{"(", boxesRiffle[multiInfixFormSeparator[h], args], ")"}];

makeBoxes[head_[elems___]] := 
  RowBox[{makeBoxes[head], "[", makeBoxes[elems], "]"}];

makeBoxes[s_ /; AtomQ[Unevaluated[s]]] := MakeBoxes[s];	
	

ClearAll[CodeFormatterMakeBoxes];
SetAttributes[CodeFormatterMakeBoxes,HoldAllComplete];
CodeFormatterMakeBoxes[args__]:=makeBoxes[args]
	
	
	
(*============================================================================================*)
(*=========		 Printing definitions and spelunking 			==============*)
(*============================================================================================*)	
	
	
ClearAll[prn];
prn = CellPrint[Cell[BoxData[#], "Input"]] &;
	
	
ClearAll[getDefContexts];
getDefContexts[f_Symbol] := 
  Union@Flatten@Map[getDefContexts[f, #] &, globalProperties[]];
  
getDefContexts[f_Symbol, prop_] :=
  Union[Cases[prop[f], s_Symbol :> Context[s], Infinity, Heads -> True]];


ClearAll[globalProperties];
globalProperties[] := {DownValues, SubValues, UpValues, OwnValues};


ClearAll[$boxFunction];
$boxFunction = CodeFormatterMakeBoxes;


ClearAll[defBoxes];
defBoxes[f_Symbol] := 
  Flatten@Map[defBoxes[f, #] &, globalProperties[]];

defBoxes[f_Symbol, prop_] :=
Cases[prop[f], 
  Verbatim[RuleDelayed][Verbatim[HoldPattern][lhs_], 
    rhs_] :> $boxFunction@SetDelayed[lhs, rhs]]


ClearAll[printDefs];
printDefs[f_Symbol] :=
  Scan[Composition[prn, FullCodeFormat], defBoxes[f]];
  
  
ClearAll[CodeFormatterPrint];  
CodeFormatterPrint[f_Symbol]  := 
	(
		CellPrint[Cell[ToString[f], "Subsubsection"]];
		printDefs[f]
	);


ClearAll[withSymbolDefContexts];
SetAttributes[withSymbolDefContexts, HoldAll];
withSymbolDefContexts[f_Symbol, code_] :=
  Block[{$ContextPath = 
     Join[DeleteCases[getDefContexts[f], "System`"], $ContextPath]},
   code];


ClearAll[spelunk];
spelunk[f_Symbol, boxFunction_: $boxFunction] :=
  Block[{$boxFunction = boxFunction},
   withSymbolDefContexts[f, 
    	CellPrint[Cell[ToString[f], "Subsubsection"]]; printDefs[f]]
   ];	


ClearAll[CodeFormatterSpelunk];
CodeFormatterSpelunk = spelunk;



ClearAll[codeFormatted];
SetAttributes[codeFormatted,HoldAll];
codeFormatted[code_]:=
	With[{formatted = Catch[FullCodeFormat @ MakeBoxes @ code, _]},
		prn[formatted] /; !MatchQ[formatted, {$Failed,_}]
	];
	
codeFormatted[code_]:=
	prn @ FullCodeFormat @ CodeFormatterMakeBoxes @ code;


ClearAll[CodeFormatted];
SetAttributes[CodeFormatted,HoldAll];

CodeFormatted /: (h:(Set|SetDelayed|UpSet|UpSetDelayed))[lhs_,CodeFormatted[rhs]]:=
	CodeFormatted[h[lhs,rhs]];
	
CodeFormatted /: (h:(TagSet|TagSetDelayed))[tag_,lhs_,CodeFormatted[rhs]]:=
	CodeFormatted[h[tag,lhs,rhs]];
	
CodeFormatted /: CompoundExpression[prev___,CodeFormatted[last_]]:=
	CodeFormatted[CompoundExpression[prev,last]];
	
CodeFormatted[code_]:= codeFormatted[code];



End[]

EndPackage[]


