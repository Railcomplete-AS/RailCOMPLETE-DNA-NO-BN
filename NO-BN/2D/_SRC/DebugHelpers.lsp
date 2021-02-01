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
	(princ (strcat "\n\n" "VLIDE shortcuts:\n    Ctrl+Shift+L: Load Lisp file\n    Ctrl+Shift+C: AutoCAD (Command) mode\n    F6: Console window")) (prin1)
	(princ (strcat "\n\n" "(mkAll)\n    Loads all 2D library project files, then runs MAIN routine.\n    Usage: (mkAll)")) (prin1)
	(princ (strcat "\n\n" "(ql)\n    Quickload - Loads all .lsp files in specified folder addressed from " rootFolder "\\symbols\\'.\n    Usage: (ql folderName)")) (prin1)
	(princ (strcat "\n\n" "(loadAll)\n    Defines the 'rootFolder' path, then loads all files contained in 2D library project.\n    Usage: (loadAll)")) (prin1)
	(princ (strcat "\n\n" "(loadFolder2)\n    Loads all .lsp files in specified folder.\n    Usage: (loadfolder2 folderName)")) (prin1)
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
	; and add wathces with Ctrl+W.
	(vl-bt)
	(princ)
)

;(defun *error* (errmsg)
;    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break,end"))
;		(princ (strcat "\nError: " errmsg))
;    )
;)



(defun mkAll ( / )
	; (Re-)load everything, then run MAIN
	(loadAll)
	(princ (strcat "\n\n\n==> Running MAIN() routine...\n")) (prin1)
	(setq calledFromVlide T)	; T (true) if used under AutoCAD VLIDE debugger, otherwise set to nil
	(setq calledFromBlade nil)	; T (true) if used under BricsCAD BLADE debugger, otherwise set to nil
	(C:MAIN)
	(princ (strcat "Done.\n")) (prin1)
)


(defun loadAll ( / )
	(setvar 'SECURELOAD 0) ; Suppress AutoCAD's asking for every Lisp file loading whether it is a trusted file.
	(princ (strcat "\n\n================= DEBUGHELPER - LOADALL ================= \n")) (prin1)
	(loadFolder2 (strcat rootFolder "\\" "Fonts"))
	(loadFolder2 (strcat rootFolder "\\" "Utilities"))
	(loadFolder2 (strcat rootFolder "\\" "Symbols"))
	(princ (strcat "============================================================= \n")) (prin1)
)



(defun loadFolder2 ( folderName / x ) ; 'Manual' lambda function...
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

(defun ql ( folderName / ) (loadFolder2 (strcat rootFolder "\\symbols\\" folderName)))


(defun dir ( folderName / )
	(setq folderName (strcat rootFolder "\\" folderName))
	(foreach x (vl-directory-files (findfile folderName))
		(princ (strcat x "\n"))
	)
	(prin1)
)



(defun init ( / )
	(setvar 'CMDECHO 0)
	(setvar 'OSMODE 0) ; Otherwise LINE and other commands will produce bogus results, according to search on 'acad silent console mode'.
	(command 
		"._LAYER"
			"_Color" "white" "0"			; Set layer zero color to white
			"_New" "Defpoints"				; Create Defpoints layer, white / No_plot
			"_Color" "white" "Defpoints"
			"_Plot" "_NO" "Defpoints" ""
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

;  (loadAll)
