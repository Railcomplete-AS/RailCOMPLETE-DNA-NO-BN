;=========================================================================================================================
;
; Main.lsp
; 
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
;
; Main LISP file for generating Bane NOR 2D symbol library for use with RailCOMPLETE.
;
; This file may well be called from a script, "GenerateSymbolLibrary.scr" run under AutoCAD from a batch file.
; See scripts and batchfiles under Github\RailCOMPLETE\Customization\_TOOLBOX...
;
; See also the DebugHelpers.lsp file contained in the 2D\_SRC folder.
;
; IMPORTANT NOTES on AutoLISP intricacies:
; 
; 1)
; Integer division such as (/ 3 2) returns 1, whereas (/ 3 2.0) and (/ 3.0 2) and (/ 3.0 2.0) return 1.5
;
; 2)
; Note: Unicode hex 'nnnn' is '\U+nnnn' in AutoCAD text - but in later versions (2019) of AutoCAD, non-English characters don't work at all. 
; Use globals defined in the global-constant file, with (eval (strcat "Skj" _oe_ "testykke")) where you need to use the combined string.
;
; We cannot use special national characters in folder names in a 'findfile' Visual LISP command.
; It also appears that Windows substitutes 'oe' for Scandinavian 'ø' in folder names without telling - so
; avoid 'oe' as well (in folder names). (Discovered 2020-04-06 by CLFEY on Norconsult PC.)
;
; 3)
; AutoLISP ignores overloaded commands when a dot "." precedes the command name, e.g. .MOVE
;
; 4)
; AutoLISP interprets the command and its parameters according to the US-EN locale when an underscore '_' precedes the command name or attribute name.
;
; 5) 
; Subcommand names such as "Justify" in the ATTDEF command will lead to an error, write only "J" (or better, "_J") and you're fine when writing LISP.
; This is NOT very consequently programmed in AutoLISP, since for instance '(command "._MOVE" "_ALL" "" "0,0" "0,1")' (move everything right by 1 unit) 
; accepts both the unique abbreviation "AL" ("_AL") and the full text "ALL" ("_ALL"). 
; But (command "._ATTDEF" "" "tagname" "prompt" "default_text" "Justify" "_MC" "0,0" 3.5 0) (middle center adjustment at (0,0) with 3.5 high letters, 
; 0 deg rotation) does NOT work. The ATTDEF command requires you to use ONLY the letters that are highlighted when using the command-line version 
; command, i.e. "J" ("_J") instead of "Justify" ("_Justify"). Same goes for '"ARC" "CE"...' which works, but '"ARC" "CEnter"...' does NOT work.
;
; 6)
;  LISP function names must contain at least one letter.

(vl-load-com) ; Load AutoCAD's Visual LISP environment (if not already loaded)

; Print a trace when running under VLIDE:
(setq majorStep 0)
(setq minorStep 0)
(setq microStep 0)

(defun step ( msg / )
	(setq majorStep (1+ majorStep))
	(setq minorStep 0)
	(setq microStep 0)
	(princ (strcat "--- Step " (itoa majorStep) " - " msg "\n")) (princ)
)
(defun subStep ( msg / )
	(setq minorStep (1+ minorStep))
	(setq microStep 0)
	(princ (strcat "--- Step " (itoa majorStep) "." (itoa minorStep) " - " msg "\n")) (princ)
)
(defun subSubStep ( msg / )
	(setq microStep (1+ microStep))
	(princ (strcat "--- Step " (itoa majorStep) "." (itoa minorStep) "." (itoa microStep) " - " msg "\n")) (princ)
)


(defun C:MAIN ( / )
	;
	; GLOBAL identifiers:
	;
	; paperScaleList	A list of paper drawing scales
	; nSchematicBlocks			Increments for each call to createSchematicBlockFromCurrentGraphics()
	; nScaledBlocks		Increments for each call to (createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)()
	;
	; The paperScaleList is used by routine (createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)().
	; Routine (createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)() tags geographical (scaled) symbol's names with 
	; "-1_nnn" in scale 1:nnn. Unscaled (schematic) symbols get no name suffix. Example list: ("250" "500")

	; TODO: 2020-08-06 CLFEY removeUnwantedStyles() doesn't work in batch mode - I gave up removing Norconsult stuff... 
	; (removeUnwantedStyles) ; Resolves and purges stuff which might be there due to your company's LISP automations for new DWG files

	; If debugging, use (setvar 'CMDECHO 1) to get maximum feedback. For silent and quicker operation, use (setvar 'CMDECHO 0). 
	; Quickest operation is when running inside VLIDE with 'CMDECHO == 0.
	
	(cond 
		((= calledFromVlide nil)
			(step "Called from batch file.")
		) ; Do nothing if called from batch file...
		(T 
			(step "Called from VLIDE debugger.")
			(command "._BCLOSE" "") ; Assume called from VLIDE debugger and exit BE if last debug run stranded in the Block Editor.
			(C:LDOFF) ; If debugging in a Norconsult environment, also turn OFF lee-mac.com Layer Director routines (see web on LM)
		)
	)
	
	; NB: The sequence matters in the following statements:
	(step "Setting AutoCAD parameters and layer '0' properties...")
	(setvar 'CMDECHO 0) ; maximum speed, minimum verbosity
	(setvar 'OSMODE 0) ; Otherwise LINE and other commands will produce bogus results, according to search on 'acad silent console mode'.
	(command 
		"._LAYER"
			"_Color" "white" "0"			; Set layer zero color to White
			"_New" "Defpoints"				; Create Defpoints layer, Yellow / No_plot
			"_Color" "Yellow" "Defpoints"
			"_Plot" "_NO" "Defpoints" ""
	)
	(step "Purge") (purgeAll)
	(step "Set CAD defaults") (setCadSystemDefaults)
	(step "Create ISO text style") (createIsoTextStyle)
	(step "Define global CAD constants") (defineGlobalCadSystemConstants)
	(step "Create standard RailCOMPLETE layers") (createStandardLayers)
	(step "Set default object properties to ByBlock") (setDefaultObjectPropertiesToByBlock)

	; Select drawing scales to be produced - Schematic will always be produced, scales must be specified in the paperScaleList global variable.
	;(setq paperScaleList '("250" ))
	(setq paperScaleList '("250" "500"))
	(setq tmp "")
	(foreach x paperScaleList (setq tmp (strcat tmp " 1:" x)))
	(step (strcat "Define available paper scales: "   tmp))
	(setq nSchematicBlocks 0) 	; Global: Increment for each new block created with createSchematicBlockFromCurrentGraphics and similar routines,
	(setq nScaledBlocks 0) 		; Global: Increment for each new block created with createGeoBlockInAllPaperScalesFromBlock and similar routines.

	(step "GENERATE-ANNOTATIONS") (C:GENERATE-ANNOTATIONS)
	; Trouble: The "BJELKEMAST" routine hangs... One click in the VLIDE 'F6' window and the code continues. Weird!
	(step "GENERATE-HIGH-VOLTAGE-SCALED-OBJECTS") (C:GENERATE-HIGH-VOLTAGE-SCALED-OBJECTS) ; High voltage objects except yokes, cantilevers and cantilever support brackets
	(step "GENERATE-HIGH-VOLTAGE-FIXED-SCALE-OBJECTS") (C:GENERATE-HIGH-VOLTAGE-FIXED-SCALE-OBJECTS) ; OCS yokes, cantilevers and cantilever support brackets, which only exist as 1:1 scale objects
	(step "GENERATE-THUMBNAILS") (C:GENERATE-THUMBNAILS) ; Thumbnail icons when creating objects which are not point objects (area, alignment, table etc)
	;;;;;;;;;;;; (C:GENERATE-SYMBOL-OVERVIEW-TABLE) ; Only for internal use - produce table showing all available 2D symbols.
	(step "GENERATE-COMMON-OBJECTS") (C:GENERATE-COMMON-OBJECTS) ; Felles
	(step "GENERATE-BOARDSANDPOLES-OBJECTS") (C:GENERATE-BOARDSANDPOLES-OBJECTS) ; Skilt og stolper (but not annotation objects which are scaled by RC with current CAD zoom level)
	(step "GENERATE-SUBSTRUCTURE-OBJECTS") (C:GENERATE-SUBSTRUCTURE-OBJECTS) ; Underbygning
	(step "GENERATE-SUPERSTRUCTURE-FIXED-SCALE-OBJECTS") (C:GENERATE-SUPERSTRUCTURE-FIXED-SCALE-OBJECTS) ; Superstructure (track and embankment) objects which follow the track's real geometry
	(step "GENERATE-SUPERSTRUCTURE-SCALED-OBJECTS") (C:GENERATE-SUPERSTRUCTURE-SCALED-OBJECTS) ; Superstructure (track and embankment) objects except switches and tongues
	(step "GENERATE-SIGNALING-OBJECTS") (C:GENERATE-SIGNALING-OBJECTS)
	(step "GENERATE-TELECOM-OBJECTS") (C:GENERATE-TELECOM-OBJECTS)
	(step "GENERATE-LOWVOLTAGE-OBJECTS") (C:GENERATE-LOWVOLTAGE-OBJECTS)
	
	; Store (as the name of two dummy blocks) the number of blocks generated (AutoCAD doesn't count them for you):
	(step "Storing the number of generated 2D schematic and scaled symbols as the names of two dummy blocks.")
	(command "._RECTANGLE" "-1,-1" "1,1" "") ; just add something to look at...
	(createSchematicBlockFromCurrentGraphics (strcat "___Number_of_Schematic_Blocks__" (rtos nSchematicBlocks _decimal_ 0)))
	(command "._CIRCLE" "0,0" 3.1416) ; just add something to look at...
	(createSchematicBlockFromCurrentGraphics (strcat "___Number_of_Geo_Blocks________" (rtos nScaledBlocks _decimal_ 0)))

	; *** The file name "2D.dwg" is expected by other batch files taking care of the results:
	(setq symbolLibraryFileName (strcat "2D_" (rtos (getvar "CDATE") 2 6) ".dwg"))
	(step (strcat "Saving resulting blocks to file '" symbolLibraryFileName "'..."))
	(SAVE-RESULT symbolLibraryFileName)
	(step "*** End of C:MAIN() - 2D library has been generated ***")
)



(defun SAVE-RESULT ( nn / fname )
	(if calledFromVlide
		(setq fname (strcat rootFolder "\\" nn))
		(setq fname (strcat (getvar "dwgprefix") nn))
	)
	(setvar 'FILEDIA 0) ; suppress window dialog when saving drawing
	(if (findfile fname)
		(command "._SAVEAS" "2018" fname "_YES") ; File exists already - accept to overwrite existing file
	;else
		(command "._SAVEAS" "2018" fname) ; File does not exist - just save it
	)
	(if calledFromVlide
		(step "Cannot close current file from this VLIDE script since the VLIDE depends on it. Close manually and exit AutoCAD.")
		(progn
			(step "Closing file...\n")
			(command "._CLOSE")
		)
	)
)
