;=========================================================================================================================
;
; 99_Main.lsp
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
; Use globals defined in the global-constant file, with (eval (strcat "Skj" _OSLASH_ "testykke")) where you need to use the combined string.
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
; This is NOT very consequently programmed in AutoLISP, since for instance '(command "._MOVE" "_ALl" "" "0,0" "0,1")' (move everything right by 1 unit) 
; accepts both the unique abbreviation "AL" ("_AL") and the full text "ALL" (_selectAll_). 
; But (command _ATTDEF_ _ENTER_ "tagname" "prompt" "default_text" "Justify" _middleCenter_ "0,0" 3.5 0) (middle center adjustment at (0,0) with 3.5 high letters, 
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

(defun TraceLevel1 ( msg / )
	(setq majorStep (1+ majorStep))
	(setq minorStep 0)
	(setq microStep 0)
	(princ (strcat "--- Step " (itoa majorStep) " - " msg "\n")) (princ)
)
(defun TraceLevel2 ( msg / )
	(setq minorStep (1+ minorStep))
	(setq microStep 0)
	(princ (strcat "--- Step " (itoa majorStep) "." (itoa minorStep) " - " msg "\n")) (princ)
)
(defun TraceLevel3 ( msg / )
	(setq microStep (1+ microStep))
	(princ (strcat "--- Step " (itoa majorStep) "." (itoa minorStep) "." (itoa microStep) " - " msg "\n")) (princ)
)


(defun C:MAIN ( / )
	;
	; GLOBAL identifiers:
	;
	; nSchematicBlocks		Increments for each call to routine 'CreateSchematicBlockFromCurrentGraphics'
	; nAnnotativeBlocks		Increments for each call to routine 
	; nMetricBlocks			Increments for each call to routine 
	;
	; TODO: 2020-08-06 CLFEY RemoveUnwantedStyles() doesn't work in batch mode - I gave up removing Norconsult stuff... 
	; (RemoveUnwantedStyles) ; Resolves and purges stuff which might be there due to your company's LISP automations for new DWG files

	; If debugging, use (setvar 'CMDECHO 1) to get maximum feedback. For silent and quicker operation, use (setvar 'CMDECHO 0). 
	; Quickest operation is when running inside VLIDE with 'CMDECHO == 0.
	
	(cond 
		(calledFromVlide
			; non-nil if called from AutoCAD's Visual Lisp Integrated Development Environment
			(TraceLevel1 "Running under AutoCAD VLIDE...")
			(command _BCLOSE_ _ENTER_) ; Assume called from VLIDE debugger and exit BE if last debug run stranded in the Block Editor.
			(C:LDOFF) ; If debugging in a Norconsult environment, also turn OFF lee-mac.com Layer Director routines (see web on LM)
		) 
		(calledFromBlade ; non-nil if called from BricsCAD Lisp Advanced Development Environment
			(TraceLevel1 "Running under BricsCAD BLADE...")
			(command _BCLOSE_ _ENTER_) ; Assume called from VLIDE debugger and exit BE if last debug run stranded in the Block Editor.
			(C:LDOFF) ; If debugging in a Norconsult environment, also turn OFF lee-mac.com Layer Director routines (see web on LM)
		) ; Do nothing if called from batch file...
		(T 
			(TraceLevel1 "Called from Windows batch file, using a 'clean' AutoCAD or BricsCAD setup, no hidden automation started.")
		)
	)
	
	; NB: The sequence matters in the following statements:
	(TraceLevel1 "Setting AutoCAD parameters and layer '0' properties...")
	(setvar 'CMDECHO 0) ; maximum speed, minimum verbosity
	(setvar 'OSMODE 0) ; Otherwise LINE and other commands will produce bogus results, according to search on 'acad silent console mode'.
	(command 
		_LAYER_
			_colorizeLayer_		_colorWhite_ "0"
			_createNewLayer_	"Defpoints"
			_colorizeLayer_		_colorYellow_ "Defpoints"
			_plottability_		_isNotPlottable_ "Defpoints" _ENTER_
	)
	(TraceLevel1 "Purge")										(PurgeAll)
	(TraceLevel1 "Define global CAD constants")					(DefineGlobalCadSystemConstants)
	(TraceLevel1 "Set CAD defaults")							(SetCadSystemDefaults)
	(TraceLevel1 "Create ISO text style")						(CreateIsoTextStyle)
	(TraceLevel1 "Create standard RailCOMPLETE layers")			(CreateStandardLayers)
	(TraceLevel1 "Set default object properties to ByBlock")	(SetDefaultObjectPropertiesToByBlock)

	(setq nSchematicBlocks 0)
	(setq nAnnotativeBlocks 0)
	(setq nMetricBlocks 0)
	
	(TraceLevel1 "GENERATE-THUMBNAILS")							(C:GENERATE-THUMBNAILS)
	(TraceLevel1 "GENERATE-ANNOTATIONS")						(C:GENERATE-ANNOTATIONS)
;	(TraceLevel1 "GENERATE-COMMON-OBJECTS")						(C:GENERATE-COMMON-OBJECTS)
;	(TraceLevel1 "GENERATE-BOARDSANDPOLES-OBJECTS")				(C:GENERATE-BOARDSANDPOLES-OBJECTS)
;	(TraceLevel1 "GENERATE-SUBSTRUCTURE-OBJECTS")				(C:GENERATE-SUBSTRUCTURE-OBJECTS)
;	(TraceLevel1 "GENERATE-SUPERSTRUCTURE-OBJECTS")				(C:GENERATE-SUPERSTRUCTURE-OBJECTS)
;	(TraceLevel1 "GENERATE-HIGH-VOLTAGE-OBJECTS")				(C:GENERATE-HIGH-VOLTAGE-OBJECTS)
;	(TraceLevel1 "GENERATE-SIGNALING-OBJECTS")					(C:GENERATE-SIGNALING-OBJECTS)
;	(TraceLevel1 "GENERATE-TELECOM-OBJECTS")					(C:GENERATE-TELECOM-OBJECTS)
;	(TraceLevel1 "GENERATE-LOWVOLTAGE-OBJECTS")					(C:GENERATE-LOWVOLTAGE-OBJECTS)

	;;;; (C:GENERATE-SYMBOL-OVERVIEW-TABLE) ; Only for internal use - produce table showing all available 2D symbols.
	
	; Store (as the name of two dummy blocks) the number of blocks generated (AutoCAD doesn't count them for you):
	(TraceLevel1 "Storing the number of generated 2D schematic and scaled symbols as the names of two dummy blocks.")
	
	(DrawBox layDef_Zero 2 2 _noWipeout_) ; just add something to look at...
	(CreateSchematicBlockFromCurrentGraphics (strcat "___Number_of_Schematic_Blocks__" (rtos nSchematicBlocks _decimal_ 0)))
	(DrawBox layDef_Zero 3 3 _noWipeout_) ; just add something to look at...
	(CreateSchematicBlockFromCurrentGraphics (strcat "___Number_of_annotative_Blocks___" (rtos nAnnotativeBlocks _decimal_ 0)))
	(DrawBox layDef_Zero 4 4 _noWipeout_) ; just add something to look at...
	(CreateSchematicBlockFromCurrentGraphics (strcat "___Number_of_Metric_Blocks___" (rtos nMetricBlocks _decimal_ 0)))

	; *** The file name "2D.dwg" is expected by other batch files taking care of the results:
	(setq symbolLibraryFileName (strcat "2D_" (rtos (getvar "CDATE") 2 6) ".dwg"))
	(TraceLevel1 (strcat "Saving resulting blocks to file '" symbolLibraryFileName "'..."))
	(SAVE-RESULT symbolLibraryFileName)
	(TraceLevel1 "*** End of C:MAIN() - 2D library has been generated ***")
)



(defun SAVE-RESULT ( nn / fname tmpFiledia )
	(if calledFromVlide
		(setq fname (strcat rootFolder "\\" nn))
		(setq fname (strcat (getvar "dwgprefix") nn))
	)
	(setq tmpFiledia (getvar 'FILEDIA))
	(setvar 'FILEDIA 0) ; suppress window dialog when saving drawing
	(if (findfile fname)
		(command _SAVEAS_ "2018" fname _overwriteFile_) ; File exists already - accept to overwrite existing file
	;else
		(command _SAVEAS_ "2018" fname) ; File does not exist - just save it
	)
	(if calledFromVlide
		(TraceLevel1 "Cannot close current file from this VLIDE script since the VLIDE depends on it. Close manually and exit AutoCAD.")
	;else
		(progn
			(TraceLevel1 "Closing file...\n")
			(command _CLOSE_)
		)
	)
	(setvar 'FILEDIA tmpFiledia)
)
