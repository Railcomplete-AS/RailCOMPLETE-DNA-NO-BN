;=========================================================================================================================
;
; Main.lsp
; 
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
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
	; nSchematicBlocks		Increments for each call to routine 'createSchematicBlockFromCurrentGraphics'
	; nAnnotativeBlocks		Increments for each call to routine 
	; nMetricBlocks			Increments for each call to routine 
	;
	; TODO: 2020-08-06 CLFEY removeUnwantedStyles() doesn't work in batch mode - I gave up removing Norconsult stuff... 
	; (removeUnwantedStyles) ; Resolves and purges stuff which might be there due to your company's LISP automations for new DWG files

	; If debugging, use (setvar 'CMDECHO 1) to get maximum feedback. For silent and quicker operation, use (setvar 'CMDECHO 0). 
	; Quickest operation is when running inside VLIDE with 'CMDECHO == 0.
	
	(cond 
		(calledFromVlide
			; non-nil if called from AutoCAD's Visual Lisp Integrated Development Environment
			(step "Running under AutoCAD VLIDE...")
			(command "._BCLOSE" "") ; Assume called from VLIDE debugger and exit BE if last debug run stranded in the Block Editor.
			(C:LDOFF) ; If debugging in a Norconsult environment, also turn OFF lee-mac.com Layer Director routines (see web on LM)
		) 
		(calledFromBlade ; non-nil if called from BricsCAD Lisp Advanced Development Environment
			(step "Running under BricsCAD BLADE...")
			(command "._BCLOSE" "") ; Assume called from VLIDE debugger and exit BE if last debug run stranded in the Block Editor.
			(C:LDOFF) ; If debugging in a Norconsult environment, also turn OFF lee-mac.com Layer Director routines (see web on LM)
		) ; Do nothing if called from batch file...
		(T 
			(step "Called from Windows batch file, using a 'clean' AutoCAD or BricsCAD setup, no hidden automation started.")
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

	(setq nSchematicBlocks 0)
	(setq nAnnotativeBlocks 0)
	(setq nMetricBlocks 0)
	
	(step "GENERATE-ANNOTATIONS")				(C:GENERATE-ANNOTATIONS)
	(step "GENERATE-HIGH-VOLTAGE-OBJECTS")		(C:GENERATE-HIGH-VOLTAGE-OBJECTS)
	(step "GENERATE-THUMBNAILS")				(C:GENERATE-THUMBNAILS) ; Thumbnail icons when creating objects which are not point objects (area, alignment, table etc)
	(step "GENERATE-COMMON-OBJECTS")			(C:GENERATE-COMMON-OBJECTS)
	(step "GENERATE-BOARDSANDPOLES-OBJECTS")	(C:GENERATE-BOARDSANDPOLES-OBJECTS)
	(step "GENERATE-SUBSTRUCTURE-OBJECTS")		(C:GENERATE-SUBSTRUCTURE-OBJECTS)
	(step "GENERATE-SUPERSTRUCTURE-OBJECTS")	(C:GENERATE-SUPERSTRUCTURE-OBJECTS)
	(step "GENERATE-SIGNALING-OBJECTS")			(C:GENERATE-SIGNALING-OBJECTS)
	(step "GENERATE-TELECOM-OBJECTS")			(C:GENERATE-TELECOM-OBJECTS)
	(step "GENERATE-LOWVOLTAGE-OBJECTS")		(C:GENERATE-LOWVOLTAGE-OBJECTS)

	;;;; (C:GENERATE-SYMBOL-OVERVIEW-TABLE) ; Only for internal use - produce table showing all available 2D symbols.
	
	; Store (as the name of two dummy blocks) the number of blocks generated (AutoCAD doesn't count them for you):
	(step "Storing the number of generated 2D schematic and scaled symbols as the names of two dummy blocks.")
	(command "._RECTANGLE" "-1,-1" "1,1" "") ; just add something to look at...
	(createSchematicBlockFromCurrentGraphics (strcat "___Number_of_Schematic_Blocks__" (rtos nSchematicBlocks _decimal_ 0)))
	(command "._RECTANGLE" "-1,-1" "1,1" "") ; just add something to look at...
	(createSchematicBlockFromCurrentGraphics (strcat "___Number_of_Annotative_Blocks___" (rtos nAnnotativeBlocks _decimal_ 0)))
	(command "._RECTANGLE" "-1,-1" "1,1" "") ; just add something to look at...
	(createSchematicBlockFromCurrentGraphics (strcat "___Number_of_Metric_Blocks___" (rtos nMetricBlocks _decimal_ 0)))

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
