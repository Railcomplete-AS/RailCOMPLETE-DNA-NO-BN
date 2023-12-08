;=========================================================================================================================
; 
; DE-DB The good and the bad coding example.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-08-01 CLFEY Planned new release
;
;=========================================================================================================================

;------------------------------------------------------------------------------
; RAILCOMPLETE LISP CODING STYLE 
; CLFEY 2021-08-05
;------------------------------------------------------------------------------
;
; *** LISP STYLE ISSUES ***
;------------------------------------------------------------------------------
; LISP coding for 2D libraries within RC
; The following 'bad' code was an early sketch, it makes an excellent example for understanding "do'es and don'ts".
; 
;
; 
; STYLE MATTERS!
;------------------------------------------------------------------------------
; Take pride in writing such that you cannot tell who wrote the code afterwards.
; Don't leave a mess to your the guy who has to pick up your work later.
; Style matters for readability and maintainability.
; Style rules makes it easier to check for the reviewer.
; Tabs and indenting shall look the same in any editor.
; 
;
; 
; NAMING IDENTIFIERS
;------------------------------------------------------------------------------
; camelCase, PascalCase or UPPERCASE matters. It tells you immediately that this is a variable, a ServiceFunction or an OBJECT-SYMBOL-GENERATING-FUNCTION.
; camelCasedVariableName				variable
; PascalCasedFunctionName				Service function, i.e. one that you need, but it is not setting the blockName nor calling the CreateBlock... routines.	
; UPPERCASE-OBJECT-SYMBOL-FUNCTION 	Higher level function, i.e. one that contains a '(setq blockName _SIG_ "STZ-" "SCHUTZHALT")...' statement.
; _minorConstantsAreCamelCasedAndEnclosedInUnderscores_	if it is one of the many, many "minor" constants.
; - Constants shall have a unique, clear name of its role or action, such as _keepMirrorSource_ or _eraseMirrorSource_. AutoCAD is a big mess, mixing 'Y' and
; 'N' from command to command, so you can never tell what a 'yes' or a 'no' actually mean until you have studied the AutoCAD help documentation.
; _MAJOR_ Major constants such as _ADM_, _NOBN_, _DEDB_ are uppercased to make them "stick out" better.
; General (low-level) constants shall be declared in the .lsp file for general constants.
; Constants which are susceptible to being different for the different administrations belong in the appropriate section in the file for administration-specific
; constants:		'(cond (= _ADM_ _DEDB_) (setq _admSpecificConstant_ 999))
; 
; 
;
; TAB setting
;------------------------------------------------------------------------------
; You shall use 4 spaces per tab. Don't do anything else, or it messes up the readability of the code.
; Use tabs for indentation. Use spaces if you need to place things in a sketch/drawing.
; 
;
; 
; COMMENTS
;------------------------------------------------------------------------------
; At the end of a line of code:	(DoMyStuff)		; Do one or more TABs then one semicolon then one space, then your comment...	End with period '.' if writing a full sentence.
; On a separate line: 	
; 		; Indent the command to the same indentation as the lines of code it annotates. If it clarifies, end with a colon to hint at the following line(s) of code:
; 		(DoMyStuff)
; 
;
; 
; 
; THE 'SETQ' SECTION
; In general, no empty lines in setq. Comments are generally written after the key/value, using TABs + semicolon + comment.
; 
; No space between '(' and identifier in a function call: '( MyFunc)' is bad. '(MyFunc)' is good.
; Add space to the argument / local variables declaration in a function's head:  '(defun MyFunc (arg1/local1 local2)' is bad. But '(defun MyFunc ( arg1 / local1 local2 )' is good.
; These spaces improve readability.
;
;
;
; LOCAL VARIABLES
;------------------------------------------------------------------------------
; Cite all local variables after the '/' slash inside the ( arg1 arg2 ... argn / local1 local2 ... localn ) ; respect the spaces, for readability
; If you don't cite a variable, leaving it with a non-nil value, then it has become global and can mess up other routines using it in good faith.
; You can also 'kill' a variable by setting it 'nil' after use. Then LISP garbage collection will remove it shortly after use. 
; If you want a collection of cooperating routines to share variables, then you assign value (not making it local) in one routine and read them in another. 
; The last routine to terminate should 'clean up' and assign all such "group private variables' to nil, so no other routine will be affected afterwards.
; 
;
; 
; TOP-LEVEL-LISP-FUNCTION-NAMES-ARE-UPPERCASED
;------------------------------------------------------------------------------
; MAIN is the top level. MAIN calls one routine for each discipline (track, signaling etc), with uppercased function names. Each "discipline-routine" 
; in turn calls uppercased functions for each symbol (blockName) to creat, or each group of symbols. A typical blockName assignment is: 
;     '(setq blockName _SIG_ "STZ-" "SCHUTZHALT")...' or similar.
; The lower levels, the "service routines" called by the object-creation routines, are named in the style of "DEDB-PascalCaseName" when specific to 
; DEDB, and just using a PascalName (e.g. Draw... or Add...) without the administration's abbreviation first if we are dealing with a general routine
; useful for all administrations (DrawBox, DrawCircleAtPos etc.)
; 
; LISP is case insensitive. But for the sake of sticking to a C# coding style, all our added routines shall be PascalCased (every word, also the first,
; start with a capital letter). I.e.:  '(posTopLeft x y)' shall instead be '(PosTopLeft x y)' etc. Notepad++ can distinguish casing during searches.
;
; 
; 
; SKETCHES FOR 'DRAW' FUNCTIONS
;------------------------------------------------------------------------------
; Don't cite points that are not used,. Just put a plus sign '+' in corners
; Points are called 'p' or 'q':  p1 p2 p3 q1 q2 q3, p1a p1b etc
; Boxes are called 'x' and 'y':  x1 y1 x2 y2 (box 1 and box 2)
; Sides are called 's' : s1 (e.g. a triangle's side)
; Radii are called 'r':  "setq r 5.0"  or "setq r1 5.0"
; In general, number the "important surfaces first", the secondary ones later (like the short mast). If you call a subroutine, then don't repeat its points here.
; Number the points generally from bottom to top, each line from left to right. Get ideas from existing sketches.
; If you need a length such as ShortMastHeight, then show it in the sketched drawing.
; Make a function that returns a specific length, if you need it 2 or more times in various functions. Don't repeat basic information 'everywhere': 
; 
; 	(defun DEDB-GetBoardSupportHeight ( / tmp )
; 		(setq tmp 4.500)	; The value of the last thing that was evaluated will become the function's returned value
; 	)
; 
; To locate one of the available routines, look in the "_SRC\Main\00_DrawHelpers.lsp" file.
; 
; There is no (TopLeft) function. However, there are nine functions that may be used:
; (PosTL x y)	Top left
; (PosTC x y)	Top center
; ...
; (PosBR x y)	Bottom right
; 
; Each of them needs both the x and the y measure for the box to deduce where the Pos-es are.
; By default, a shape will be drawn centered around the _origin_ = (0,0) when drawn. However, many routines have a 'DrawXxxxAtPos' version as well.
; In general, all the 'DrawXxxx' routines need the target layer as its first argument. This argument is a data structure holding layer name plus more.
; 
; Tip: Draw all the symmetrical stuff centered about the origin first, then move it into place at the end before adding the short mast.
; Use Mirror, Move etc.
;
; You may also store an entire block as a "TMP" block and retrieve it again later. There is a function to delete such "garbage blocks" from the block table.
;
; If your DrawXxxxx routine uses the more basic AutoCAD functional level, using AutoCAD's '(command ...)' call, then your freedom is much greater BUT at 
; the risk of introducing hard errors. And portability to other, future, CAD systems will be harder to do. 
; If you decide to step down one level, then you must take care to use the (SetLayer ...) routine to switch to the correct layer before drawing anything.
;
;
;
; BLOCK NAME and DESCRIPTION in routines that create blocks (2D symbols)
;------------------------------------------------------------------------------
; Make sure that the correct discipline has been used: See file = "Administration specific constants.lsp"
; _COM_ = Allgemein
; _SUB_ = Tiefbau
; _TRK_ = Oberbau
; _SIG_ = Leit- und Sich.Tech.
; _BPN_ = Tafeln und Schilder
; (plus others)
; Then add a 3-letter combination (plus a dash) which makes the discipline + 3-letter abbreviation uniwue but still logical and understandable. Here, our
; challenge is that "Shutzhalt" and "Schachbretttafel" both start with "SCH", so Shutzhalt got the "SZH-" and Schachbrett-Tafel should for instance use "SBT-".
; After the 3-etter abbreviation and a '-' dash, a human-readable uppercased name shall follow. See rules below.
; 
; Strings in LISP are case sensistive. AutoCAD is bad when it comes to UTF-8 characters written from LISP.
; Style for Railcomplete identifiers: camelCase (first word starts with lowercase, all subsequent words start with uppercase).
; Special abbreviations such as '3D' will spell '3D' (not '3d'). If 'TopLeft' has been abbreviated to TL, then the function's name shall be 'PosTL' (not PosTl).
; 
; ** 'blockName' naming rules **
; Rule 1: A blockName is an UPPERCASE string such as (strcat _SIG_ "SZH-" "SCHUTZHALT")
; Rule 2: No special characters (use AE, UE, instead of Ä, Ü, ëtc):  "BRUECKE"
; Rule 3: Words or combined words are separated with dash '-' (not underscore) "ACHSZAEHLER-ANPASSUNG"
; Rule 4: No whitespace in block names (LISP cannot handle that)
; 
; ** 'description' naming rules **
; Rule 1: UPPERCASE
; Rule 2: No special characters (use AE, UE, instead of Ä, Ü, ëtc)
; Rule 3: Words are separated with dash '-' (not underscore)
; Rule 4: Use comma, period, colon, semicolon, space freely. No TAB or newline '\n' (RC routines does text wrapping for you). 
; Rule 5: The description shall closely repeat the block name, but in human-friendly text
; 
; 
; 
; ORGANIZING CODE
; Just one blank line between groups of code lines. No blank between subsequent lines which form a natural group.
; Don't call one of the "draw helper" routines inside another one. Call them one by one in sequence:
; Not this, because it is easy to get lost in the parentheses, it makes the human code review process hard: 
;    		(MoveUp (+ (HalfOf y1) (DEDB-GetBoardNormalSupportHeight)))
; But rather this, which is more readable and seldom leads to coding errors:
; 		(MoveUp (HalfOf y1))
; 		(MoveUp (DEDB-GetBoardNormalSupportHeight))
; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun BAD-EXAMPLE-DEDB-Schachbretttafel ( / blockName description x1 y1 x2 y2 )
	; DEDB: Schachbretttafel
	;	3----8----4 
	;   | 	 |	  |
	;   | x2 | x3 | 
	;   5----9----6
	;   | x4 | x5 |
	;   |    |    |
	;   1----10---2  
	;	     |	
	;		 |	
	;		 |
	;	   7-.-11
	(setq 
		blockName 	(strcat _SIG_ "STZ-" "Schachbretttafel")
		description (strcat 			 "Schachbretttafel")
		x1	4
		y1	6
		x2	2
		y2	3
		x3 	2
		y3	3
		x4	2
		y4	3
		x5	2
		y5	3
	)
	(DrawBox layDef_Zero x1 y1 layDef_BoardOrPole_Wipeout)
	(DrawBox layDef_Zero x2 y2 nil)
	(MoveUp (+ boardSupportHeight ( topLeft x1)))
	(DrawBox layDef_Zero x3 y3 nil)
	(MoveUp (+ boardSupportHeight (topRight x1)))
	(DrawBox layDef_Zero x4 y4 nil)
	(MoveUp (+ boardSupportHeight (bottomLeft x1)))
	(DrawBox layDef_Zero x5 y5 nil)
	(MoveUp (+ boardSupportHeight (bottomRight x1)))
	
	
	(DrawBox layDef_Zero x1 y1 layDef_BoardOrPole_Wipeout)
	(DrawBox layDef_Zero x2 y2 nil)
	(MoveUp (+ boardSupportHeight ( MoveToQuadrant 1)))
	(DrawBox layDef_Zero x5 y5 nil)
	(MoveUp (+ boardSupportHeight (MoveToQuadrant 3)))
	
	(DrawBox layDef_Zero x1 y1 layDef_BoardOrPole_Wipeout)
	(DrawBox layDef_Zero x2 y2 nil)
	(MoveUp (+ boardSupportHeight ( MirrorAboutXaxis)))

	
	(DEDB-DrawBoardMast45)
	(AddDescriptionBelowOrigin description 0) 
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun PERFECT-EXAMPLE-DEDB-SCHACHBRETTTAFEL ( / blockName description x1 y1 p5 p6 p7 p8 p9 p10 boardSupportHeight )
	; DEDB: Schachbretttafel
	;
	; 3----8----4
	; |    |    |
	; |    |10  |
	; 5---------6
	; |    |   	|
	; |   9|    |
	; 1----7----2
	;      |
	;      |
	; 	   |
	;    --.-- 
	(setq
		blockName 	(strcat _SIG_ "SBT-" "SCHACHBRETT-TAFEL")
		description (strcat 		     "SCHACHBRETT-TAFEL")
		x1   4
		y1   6
		p5 	(list -2.000  7.500)
		p6 	(list  2.000  4.500)
		p7 	(list -0.000  4.500)
		p8 	(list  2.000 10.500)
		p9 	(list -0.100  5.000)
		p10 (list  0.100  8.500)
		boardSupportHeight 4.5
	)
	(DrawBox layDef_Zero x1 y1 layDef_BoardOrPole_Wipeout)
	(MoveUp (+ boardSupportHeight (HalfOf y1)))
	(DrawLine layDef_Zero p5 p7)
	(DrawLine layDef_Zero p6 p8)
	(DrawHatchFromPoint _solidHatch_ p14 _angleZero_ _offsetZero_)
	(DrawHatchFromPoint _solidHatch_ p15 _angleZero_ _offsetZero_)
	
	(DEDB-DrawBoardMast45)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)
