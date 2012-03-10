##CodeFormatter.m

###Formatting Mathematica code 

`CodeFormatter.m` is a pretty-printer / code formatter for 
code written in the *Mathematica* language, which is also 
written in *Mathematica*. It works on the box level at the 
moment. The direct support for strings has not yet been 
implemented, but one can always convert code to boxes and 
use it then. Only a few boxes are currently supported, but 
those cover a vast majority of cases encountered for normal 
(not typeset) *Mathematica* code. The formatter is extensible 
and the support for more boxes will be added in the future.

###Installation

The installation procedure is standard, as for any *Mathematica*
package:

 - Download the package
 - Place it into one of the directories where *Mathematica* 
can find it, for example in a directory returned by evaluating
`FileNameJoin[$UserBaseDirectory,"Applications"]`
 - Call ``Needs["CodeFormatter`"]``

###How to use

There are currently two public functions, `FullCodeFormat` and 
`FullCodeFormatCompact`. Each of them accepts code in the box
form, as a single argument. You can use `MakeBoxes` to convert
code to boxes first, if you have it as an expression. What is 
returned is a formatted code, also in the box form. I recommend
using some helper function such as this:

    prn = CellPrint[Cell[BoxData[#], "Input"]] &

To apply to the resulting formatted code. As an example, you can 
try something like this:

    prn@FullCodeFormat@MakeBoxes@
     Module[{a, b}, a = 1; Block[{c, d}, c = a + 1; d = b + 2]; b]

or, as a more interesting example (I intentionally did not format 
this one here):

    prn@FullCodeFormat@MakeBoxes[
       SetAttributes[CleanUp, HoldAll]; 
       CleanUp[expr_, cleanup_] := 
      Module[{exprFn, result, abort = False, rethrow = True, seq}, 
        exprFn[] := expr;  result =  CheckAbort[ Catch[Catch[result 
        = exprFn[]; rethrow = False; result], _, seq[##1] &], abort 
        = True]; cleanup; If[abort, Abort[]]; If[rethrow, Throw[result 
       /. seq -> Sequence]]; result]]

###Further resources

The notebook coming with the package contains many more examples





