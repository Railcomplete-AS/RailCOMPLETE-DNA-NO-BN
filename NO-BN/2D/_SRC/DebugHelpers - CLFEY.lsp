;=========================================================================================================================
;
; DebugHelpers.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================
;
; Usage:  
;
; 1) Start AutoCAD
; 2) Write VLIDE
; 3) In the Visual LISP IDE, press Ctrl+Shift+L and select this file.
; 4) In VLIDE, write (mkAll)
;
; 5) Tip: Use Ctrl+Shift+C in VLIDE to switch between interactive debugging and command mode.
;
; Note: Adjust the 'rootFolder' folder name in the setq statement below to suit your own computer directory structure.
;


(princ (strcat "\n================================= DebugHelper.lsp =================================")) (prin1)
(vl-load-com) ; Load AutoCAD's Visual LISP environment (if not already loaded)
(setq rootFolder "c:\\users\\Claus Feyling\\documents\\github\\railcomplete-no-bn\\NO-BN\\2D\\_src")


(defun hlp ( / )
	(princ (strcat "\n=============================== DebugHelper - HLP ===============================")) (prin1)
	(princ (strcat "\n\n*** Root folder = " rootFolder " ***")) (prin1)
	(princ (strcat "\n\n" "VLIDE shortcuts:\n    TAB/Shift+TAB: Previous/next command\n    Ctrl+Shift+L: Load Lisp file\n    Ctrl+R: Reset from break loop\n    Ctrl+Shift+C: Toggle AutoCAD (Command) mode\n    F6: Console window")) (prin1)
	(princ (strcat "\n\n" "(mkAll)\n    Loads all 2D library project files, then runs MAIN routine.\n    Usage: (mkAll)")) (prin1)
	(princ (strcat "\n\n" "(ql)\n    Quickload - Loads all .lsp files in specified folder addressed from " rootFolder "\\symbols\\'.\n    Usage: (ql folderName)")) (prin1)
	(princ (strcat "\n\n" "(ldAll)\n    Defines the 'rootFolder' path, then loads all files contained in 2D library project.\n    Usage: (ldAll)")) (prin1)
	(princ (strcat "\n\n" "(ldFolder)\n    Loads all .lsp files in specified folder.\n    Usage: (ldFolder folderName)")) (prin1)
	(princ (strcat "\n\n" "(dir)\n    Lists all files and folders below 'rootFolder'.\n    Usage: (dir '<folderName>') or (dir '<folderName>'\\'<subFolderName>')")) (prin1)
	(princ (strcat "\n\n" "(init)\n    Initializes CAD system, sets constants and scale variables, creates standard layers.\n    Usage: (init)")) (prin1)
	(princ (strcat "\n\n" "(hlp)\n    Prints the help menu\n    Usage: (hlp)")) (prin1)
	(princ (strcat "\n\n===================================================================================")) (prin1)
)
(princ (strcat "\n\n" "hlp\n    Prints the help menu\n    Usage: (hlp)")) (prin1)



(defun *error* (s)
	; Error handler - Visual LISP BackTrace function - shows call stack upon error.
	; In VLIDE, you can access the same info by pressing Ctrl+Shift+T which brings up a callback trace window.
	; Press Ctrl+F9 to locate the source code for the error, where you can set a suitable breakpoint with F9
	; and add watches with Ctrl+W.
	(vl-bt)
	(princ)
)

;(defun *error* (errmsg)
;    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break,end"))
;		(princ (strcat "\nError: " errmsg))
;    )
;)


; For debugging purposes:
(defun C:I2M ( / m in string) ;inches to meters
	(setq in (GETDIST "\nValue in Inches: ")) ; A prompt appears (either as tooltip in modelspace, or in the console line).
	(setq m (* in 0.0254))
	(alert (strcat (rtos in 2 1) " inches is " (rtos m 2 3) " meters.")) ; A box pops up with an "OK" button. Control returns when the user clicks 'OK'.
)



; For debugging purposes:
(defun trc (s / )
	(if counter 
		(setq counter (+ 1 counter))
		(setq counter 1)
	)
	(princ (strcat "--> " (rtos counter) " - " (if (= s nil) "" s) "\n"))
)
; Usage example:
;	(setq counter 1)
;	(trc "BETONG-TELE-OG-FORDELINGSSKAP")


(defun mkAll ( / )
	; (Re-)load everything, then run MAIN
	(ldAll)
	(princ (strcat "\n\n\n==> Running MAIN() routine...\n")) (prin1)
	(setq calledFromVlide T)	; T (true) if used under AutoCAD VLIDE debugger, otherwise set to nil
	(setq calledFromBlade nil)	; T (true) if used under BricsCAD BLADE debugger, otherwise set to nil
	(C:MAIN)
	(princ (strcat "Done.\n")) (prin1)
)


(defun ldAll ( / )
	(setvar 'SECURELOAD 0) ; Suppress AutoCAD's asking for every Lisp file loading whether it is a trusted file.
	(princ (strcat "\n\n================= DEBUGHELPER - ldAll ================= \n")) (prin1)
	(ldFolder (strcat rootFolder "\\" "Fonts"))
	(ldFolder (strcat rootFolder "\\" "Utilities"))
	(ldFolder (strcat rootFolder "\\" "Symbols"))
	(princ (strcat "============================================================= \n")) (prin1)
)



(defun ldFolder ( folderName / x ) ; 'Manual' lambda function...
	(princ (strcat "Loading all files in folder " folderName "...\n"))
	(if (findfile folderName)
		(progn
			;(princ "\n") 
			;(princ (vl-directory-files (findfile folderName)))
			;(princ "\n")
			(foreach x (vl-directory-files (findfile folderName))
				(if (wcmatch (strcase x) "*.LSP")
					(progn 
						(princ (strcat "    Loading " x "...\n"))
						(load (findfile (strcat folderName "\\" x)))
						;(princ (strcat "done\n"))
					)
				)
			)
			(princ (strcat "Done\n"))
		)
	;else
		(progn
			(print " error: No .LSP files found - load failed.\n") 
			(prin1)
		)
	)
)

(defun ql ( folderName / ) (ldFolder (strcat rootFolder "\\symbols\\" folderName)))


(defun dir ( folderName / )
	(setq folderName (strcat rootFolder "\\" folderName))
	(foreach x (vl-directory-files (findfile folderName))
		(princ (strcat x "\n"))
	)
	(prin1)
)



(defun init ( / )
	(ldAll)
	(setvar 'CMDECHO 0)
	(setvar 'OSMODE 0) ; Otherwise LINE and other commands will produce bogus results, according to search on 'acad silent console mode'.
	(command 
		_LAYER_
			_colorizeLayer_ _colorWhite_ "0"			; Set layer zero color to white (special CAD layer name: Cannot be deleted / purged)
			_createNewLayer_ "Defpoints"				; then create Defpoints layer (special CAD layer name: auto-exempt during plotting to PDF or paper)
			_colorizeLayer_ _colorWhite_ "Defpoints"
			_plottability_ _isNotPlottable_ "Defpoints" _ENTER_
	)
	(purgeAll)
	(setCadSystemDefaults)
	(createIsoTextStyle)
	(defineGlobalCadSystemConstants)
	(createStandardLayers)
	(setDefaultObjectPropertiesToByBlock)

	(setq nSchematicBlocks 0)
	(setq nAnnotativeBlocks 0)
	(setq nMetricBlocks 0)
)
