;=========================================================================================================================
;
; 99_Main.lsp
; 
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
;
; Main LISP file for generating 2D symbol library for use with RailCOMPLETE.
;
; This 99_Main.lsp file is located in the 2D\Symbols folder.
; The remaining Lisp code is contained in folders 2D\_SRC\Utilities and folder 2D\Symbols.
; Fonts are found in _SRC_\Fonts.
; The (C:MAIN adm) routine in 99_Main.lsp shall be called after loading of all other Lisp files.
;
; Remember to set _CAD_ in your CAD system constants.lsp to select a target CAD system.
;
; A convenient way to use this file is to start your CAD program and run its integrated Lisp debugger.
; With AutoCAD, this amounts to using VLIDE.
; Open the command window (F6). Load the debugging boostrap file ('_2D Symbol Library bootstrap - NNNNN.lsp' or similarly named).
; Adjust folder names and save a similar DebugHelpers file to your own name.
; Then make all symbols by writing '(MkLib)' in the VLIDE debugger.
; Writing (hlp) will show a list of nice functions available in the boostrap Lisp file.
;
(vl-load-com) ; Load AutoCAD's Visual LISP environment (if not already loaded)

	
(defun C:MAIN ( / t0 t1 startTime endTime diffTime blocksPerSecond symbolLibraryFileName )
	; _LISPSYS_ 	; (global) 0=legacy VLIDE Autolisp development, 1=Visual Studio (VS) with Unicode character set, 2=VS with ASCII (MBCS)
	;				;  Note your CAD editor must be restarted each time you change the _LISPSYS_ variable.
	; 
	; GLOBAL identifiers:
	; -------------------
	; nSchematicBlocks		Increments for each call to routine 'CreateSchematicBlockFromCurrentGraphics'
	; nAnnotativeBlocks		Increments for each call to routine 
	; nMetricBlocks			Increments for each call to routine 
	; nTotalBlocks			Increments for each call to one of the three other routines
	;
	; TODO: 2020-08-06 CLFEY RemoveUnwantedStyles() doesn't work in batch mode - I gave up removing all Norconsult stuff... 
	; (RemoveUnwantedStyles) ; Resolves and purges stuff which might be there due to your company's LISP automations for new DWG files

	; If debugging, use (setvar 'CMDECHO 1) to get maximum feedback. For silent and quicker operation, use (setvar 'CMDECHO 0). 
	; Quickest operation is when running inside VLIDE with 'CMDECHO == 0.
	;
	(setq 
		t0			(getvar "CDATE")	; Integer = 1/100 seconds
		startTime	(strcat "*** STARTED     : " (TODAY t0) "_" (TIME t0))
	)
	(INITIALIZE)

	; Bane NOR
	(TraceLevel1 "10_GENERATE-THUMBNAILS")						(10_GENERATE-THUMBNAILS)
	(TraceLevel1 "11_GENERATE-ANNOTATIONS")						(11_GENERATE-ANNOTATIONS)
	(TraceLevel1 "20_GENERATE-COMMON-OBJECTS")					(20_GENERATE-COMMON-OBJECTS)
	(TraceLevel1 "21_GENERATE-BOARDSANDPOLES-OBJECTS")			(21_GENERATE-BOARDSANDPOLES-OBJECTS)
	(TraceLevel1 "22_GENERATE-CIVIL-WORKS-OBJECTS")				(22_GENERATE-CIVIL-WORKS-OBJECTS)
	(TraceLevel1 "23_GENERATE-TRACK-AND-EMBANKMENT-OBJECTS")	(23_GENERATE-TRACK-AND-EMBANKMENT-OBJECTS)
	(TraceLevel1 "24_GENERATE-HIGH-VOLTAGE-OBJECTS")			(24_GENERATE-HIGH-VOLTAGE-OBJECTS)
	(TraceLevel1 "25_GENERATE-LOWVOLTAGE-OBJECTS")				(25_GENERATE-LOWVOLTAGE-OBJECTS)
	(TraceLevel1 "26_GENERATE-SIGNALLING-OBJECTS")				(26_GENERATE-SIGNALLING-OBJECTS)
	(TraceLevel1 "27_GENERATE-TELECOM-OBJECTS")					(27_GENERATE-TELECOM-OBJECTS)

	;;;; (GENERATE-SYMBOL-OVERVIEW-TABLE) ; Only for internal use - produce table showing all available 2D symbols.
	
	; Store (as the name of dummy blocks) the number of blocks generated (AutoCAD doesn't count them for you):
	(TraceLevel1 "Storing the number of generated 2D schematic and scaled symbols as the names of two dummy blocks.")
	
	(DrawBox layDef_Zero 2 2 _noWipeout_) ; add dummy graphics...
	(CreateSchematicBlockFromCurrentGraphics (strcat "___Number_of_Schematic_Blocks__" (rtos nSchematicBlocks _decimal_ 0)))
	(DrawBox layDef_Zero 3 3 _noWipeout_) ; add dummy graphics...
	(CreateSchematicBlockFromCurrentGraphics (strcat "___Number_of_annotative_Blocks___" (rtos nAnnotativeBlocks _decimal_ 0)))
	(DrawBox layDef_Zero 4 4 _noWipeout_) ; add dummy graphics...
	(CreateSchematicBlockFromCurrentGraphics (strcat "___Number_of_Metric_Blocks___" (rtos nMetricBlocks _decimal_ 0)))
	
	(TraceLevel2 (strcat (rtos nSchematicBlocks _decimal_ 0) " schematic blocks created"))
	(TraceLevel2 (strcat (rtos nAnnotativeBlocks _decimal_ 0) " annotative blocks created"))
	(TraceLevel2 (strcat (rtos nMetricBlocks _decimal_ 0) " metric blocks created"))
	(TraceLevel2 (strcat (rtos nTotalBlocks _decimal_ 0) " TOTAL blocks created"))

	(setq 
		t1						(getvar "CDATE")	; Integer = 1/100 seconds
		endTime					(strcat "*** ENDED       : " (TODAY t1) "_" (TIME t1))
		diffTime				(strcat "*** TIME USED   : " (DIFFTIMESTRING t0 t1))
		blocksPerSecond			(strcat "*** PERFORMANCE : " (rtos (/ (float nTotalBlocks) (float (- (SECONDS t1) (SECONDS t0)))) _decimal_ 3) " blocks per second")
		symbolLibraryFileName	(strcat "NO-BN" "-" (TODAY t1) "_" (TIME t1) "-2D" ".dwg")
	)
	(TraceLevel1 (strcat "Saving resulting blocks to file '" symbolLibraryFileName "'..."))
	(SAVE-RESULT symbolLibraryFileName)

	(TraceLevel1 "\n*** End of (C:MAIN) - 2D library has been generated ***\n")
	(TraceLevel2 startTime)
	(TraceLevel2 endTime)
	(TraceLevel2 diffTime)
	(TraceLevel2 blocksPerSecond)
)



(defun INITIALIZE ( / )
	(ResetTraceLevels)
	(TraceLevel1 "============================================")
	(TraceLevel1 "Generating symbol library for NO-BN - Norway, Bane NOR")
	(TraceLevel1 "============================================")

	(TraceLevel1 "Define CAD system constants")					(DefineCadSystemConstants)
	(TraceLevel1 "Define administration specific constants")	(DefineAdministrationSpecificConstants)
	
	(cond 
		((= _CAD_ _ACAD_)
			; Info: VLIDE  = AutoCAD's Visual Lisp Integrated Development Environment, VS = Microsoft Visual Studio
			(setq _LISPSYS_ (getvar 'LISPSYS))
			(cond 
				((= _LISPSYS_ 0) (TraceLevel1 "Running under AutoCAD VLIDE, without full support for UNICODE UTF-8, only ASCII (MBCS)."))
				((= _LISPSYS_ 1) (TraceLevel1 "Running under Visual Studio with AutoLisp debugging extension, supporting UNICODE UTF-8."))
				((= _LISPSYS_ 2) (TraceLevel1 "Running under Visual Studio with AutoLisp debugging extension, using ASCII (MBCS) char set."))
			)
			(command _BCLOSE_ _ENTER_) ; Assume called from VLIDE debugger and exit BE if last debug run stranded in the Block Editor.
			
			(LDOFF) ; If debugging with Lee-Mac helpers, also turn OFF lee-mac.com Layer Director routines (see web on LM)

			; The global acad variable 'MEASUREMENT' must be set to 1 to use metric hatches.
			; 0 = Imperial; uses the hatch pattern file and linetype file designated by the ANSIHatch and ANSILinetype registry settings.
			; 1 = Metric; uses the hatch pattern file and linetype file designated by the ISOHatch and ISOLinetype registry settings.
			(setvar 'MEASUREMENT 1)

			; NB: The sequence matters in the following statements:
			(TraceLevel1 "Setting AutoCAD parameters and layer '0' properties...")
			(setvar 'CMDECHO 0) ; maximum speed, minimum verbosity
			(setvar 'OSMODE 0) ; Otherwise LINE and other commands will produce bogus results, according to research on 'acad silent console mode'.
			(command ; Special treatment for layer 0 in ACAD...
				_LAYER_
					_colorizeLayer_		_colorWhite_ "0"
					_createNewLayer_	"Defpoints"
					_colorizeLayer_		_colorYellow_ "Defpoints"
					_plottability_		_isNotPlottable_ "Defpoints" _ENTER_
			)
		) 
		((= _CAD_ _BCAD_)
			; Using BLADE - BricsCAD Lisp Advanced Development Environment
			(TraceLevel1 "Running under BricsCAD BLADE...")
			(command _BCLOSE_ _ENTER_) ; Assume called from VLIDE debugger and exit BE if last debug run stranded in the Block Editor.
			(LDOFF) ; If debugging in a Norconsult environment, also turn OFF lee-mac.com Layer Director routines (see web on LM)
			; TODO 2021-04-04 CLFEY Adaption to BricsCAD remains to be done.
		)
		(T 
			; Stop if called from batch file...
			(TraceLevel1 "Running from Windows batch file is not supported anymore. See legacy code.")
			(UndefinedFunctionWhichLeadsToLispCrashingHereOnPurpose)
		)
	)
	
	(TraceLevel1 "Purge")										(PurgeAll)
	(TraceLevel1 "Define CAD system constants")					(DefineCadSystemConstants)
	(TraceLevel1 "Set CAD defaults")							(SetCadSystemDefaults)
	(TraceLevel1 "Create ISO text style")						(CreateIsoTextStyle)
	(TraceLevel1 "Define administration specific constants")	(DefineAdministrationSpecificConstants)
	(TraceLevel1 "Create standard RailCOMPLETE layers")			(CreateStandardLayers)
	(TraceLevel1 "Set default object properties to ByBlock")	(SetDefaultObjectPropertiesToByBlock)

	; Reset counters for # of blocks created:
	(setq nTotalBlocks 0)
	(setq nSchematicBlocks 0)
	(setq nAnnotativeBlocks 0)
	(setq nMetricBlocks 0)
	
);INITIALIZE



(defun SAVE-RESULT ( nn / fname tmpFiledia )
	(setq fname (strcat rootFolder "\\..\\" nn))
	(setq tmpFiledia (getvar 'FILEDIA))
	(setvar 'FILEDIA 0) ; suppress window dialog when saving drawing
	(if (findfile fname)
		(command _SAVEAS_ "2018" fname _overwriteFile_) ; File exists already - accept to overwrite existing file
	;else
		(command _SAVEAS_ "2018" fname) ; File does not exist - just save it
	)
	(TraceLevel1 "Cannot close current file from this VLIDE script since the VLIDE depends on it. Close manually and exit AutoCAD.")
	(setvar 'FILEDIA tmpFiledia)
);SAVE-RESULT



(defun TODAY ( cdate / d yr mo day)
	(setq 
		d (rtos (if cdate cdate (getvar "CDATE")) 2 6)	;Use provided value or fetch from operating system
		yr (substr d 1 4)								;extract the year
        mo (substr d 5 2)								;extract the month
		day (substr d 7 2)								;extract the day
    )				
	; (strcat day "/" mo "/" yr)						;string 'em together in EN-US format
	;(strcat yr "-" mo "-" day )						;string 'em together in ISO format
	(strcat yr mo day )									;string 'em together in "RC" format
);TODAY



(defun TIME ( cdate / d hr m s)
    (setq 
		d (rtos (if cdate cdate (getvar "CDATE")) 2 6)	;Use provided value or fetch from operating system
		hr (substr d 10 2)								;extract the hour
		m (substr d 12 2)								;extract the minute
		s (substr d 14 2)								;extract the second
    )				
    ; (strcat hr ":" m ":" s)							;string 'em together in ISO format
	(repeat (- 2 (strlen s)) (setq s (strcat s "0")))
	(repeat (- 2 (strlen m)) (setq m (strcat m "0" m)))
	(repeat (- 2 (strlen hr)) (setq hr (strcat hr "0")))
	(strcat hr m s)										;string 'em together in "RC" format
);TIME



(defun SECONDS ( t0 / d yr mo day hr m s)
    (setq 
		; Ignore year, month, day (fails over midnight...)
		d (rtos t0 2 6)	;Use provided value or fetch from operating system
		hr (atoi (substr d 10 2))								;extract the hour
		m (atoi (substr d 12 2))								;extract the minute
		s (atoi (substr d 14 2))								;extract the second
    )				
	(+ (* hr 3600) (* m 60) s)
);SECONDS



(defun DIFFTIMESTRING ( t0 t1 / delta daysUsed hoursUsed minutesUsed secondsUsed )
	(defun mod (a b / )	(- a (* b (/ a b))))	; both a and b must be integers
	(defun div (a b / )	(/ a b))				; both a and b must be integers
	(setq
		delta 		(- (SECONDS t1) (SECONDS t0)) 	; seconds difference, as integer number
		secondsUsed	(mod delta 60)
		delta		(div delta 60)
		minutesUsed	(mod delta 60)
		delta		(div delta 60)
		hoursUsed	(mod delta 24)
	)
	(strcat (itoa hoursUsed) " hour(s), " (itoa minutesUsed) " minute(s) and " (itoa secondsUsed) " second(s)")
);DIFFTIMESTRING
