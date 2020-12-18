;=========================================================================================================================
;
; DebugHelpers.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
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
(setq rootFolder "c:\\users\\clfey\\github\\railcomplete\\customization\\NO-BN\\2D\\_src")



(defun hlp ( / )
	(princ (strcat "\n=============================== DebugHelper - HLP ===============================")) (prin1)
	(princ (strcat "\n\nRoot folder = " rootFolder)) (prin1)
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
	(C:MAIN)
	(princ (strcat "Done.\n")) (prin1)
)


(defun loadAll ( / )
	; Adjust the following rootFolder directory;
	(princ (strcat "\n\n================= DEBUGHELPER - LOADALL ================= \n")) (prin1)
	(loadFolder2 (strcat rootFolder "\\" "Fonts"))
	(loadFolder2 (strcat rootFolder "\\" "Utilities"))
	(loadFolder2 (strcat rootFolder "\\" "Symbols"))
	(loadFolder2 (strcat rootFolder "\\" "Symbols\\Thumbnails"))
	(loadFolder2 (strcat rootFolder "\\" "Symbols\\Annotations"))
	(loadFolder2 (strcat rootFolder "\\" "Symbols\\Common"))
		;(loadFolder2 (strcat rootFolder "\\" "Symbols\\BoardsAndPoles"))
		;(loadFolder2 (strcat rootFolder "\\" "Symbols\\Substructure"))
		;(loadFolder2 (strcat rootFolder "\\" "Symbols\\Superstructure"))
		;(loadFolder2 (strcat rootFolder "\\" "Symbols\\HighVoltage"))
		;(loadFolder2 (strcat rootFolder "\\" "Symbols\\LowVoltage"))
	(loadFolder2 (strcat rootFolder "\\" "Symbols\\Signaling"))
		;(loadFolder2 (strcat rootFolder "\\" "Symbols\\Telecom"))
(cond (nil
));cond nil
	(princ (strcat "============================================================= \n")) (prin1)
)



(defun loadFolder2 ( folderName / x ) ; 'Manual' lambda function...
	(princ (strcat "Loading all files in folder " folderName "..."))
	(if (findfile folderName)
		(progn
			(foreach x (vl-directory-files (findfile folderName))
				(if (wcmatch (strcase x) "*.LSP")
					;(princ (strcat x "\n"))
					(load (strcat (findfile folderName) "\\" x))
				)
			)
			(princ (strcat "Done\n"))
		)
	;else
		(progn
			(print "; error: No .LSP files found - load failed.\n") 
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

	; Select drawing scales to be produced - Schematic will always be produced, scales must be specified in the paperScaleList global variable.
	;(setq paperScaleList '("250" "500" "1000"))
	(setq paperScaleList '("250" ))
	(setq nSchematicBlocks 0) 	; Global: Increment for each new block created with createSchematicBlockFromCurrentGraphics and similar routines,
	(setq nScaledBlocks 0) 		; Global: Increment for each new block created with createGeoBlockInAllPaperScalesFromBlock and similar routines.
	; Show...:
	paperScaleList
)
