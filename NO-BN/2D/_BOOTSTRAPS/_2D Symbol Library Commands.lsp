;=========================================================================================================================
;
; _2D Symbol Library Commands.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY Merged 2D symbol generation into specific DNA repository. LISP code was adapted as needed.
;
;=========================================================================================================================
;
; Usage:  
;
; 1) Start AutoCAD
; 2) Write _VLIDE
; 3) In the Visual LISP IDE, press Ctrl+Shift+L and select one of the personal bootstrap files, such as 
;    "_2D Symbol Library bootstrap - CLFEY.lsp" which loads and calls the library commands file.
; 4) In VLIDE, write (mk) to start building the 2D library.
; 5) Tip: Use Ctrl+Shift+C in VLIDE to switch between interactive debugging and command mode.
;
; Note: Adjust the 'rootFolder' folder name in the personal boostrap file to suit your own computer directory structure.
;

(defun Hlp ( / )
	(princ (strcat "\n=============================== 2D Symbol Library Commands - Hlp ===============================")) (prin1)
	(princ (strcat "\n\n*** rootFolder = " rootFolder " ***")) (prin1)
	(princ (strcat "\n\n" "VLIDE shortcuts:\n    TAB/Shift+TAB: Previous/next command\n    Ctrl+Shift+L: Load Lisp file\n    Ctrl+R: Reset from break loop\n    Ctrl+Shift+C: Toggle AutoCAD (Command) mode\n    F6: Console window")) (prin1)
	(princ (strcat "\n\n" "(MkLib)\n    Loads all 2D library project files, then runs C:MAIN routine with adm=XX-YY.\n    Usage: (MkLib)")) (prin1)
	(princ (strcat "\n\n" "(Init)\n    Initializes CAD system, sets constants and scale variables, creates standard layers.\n    Usage: (Init \"XX-YY\")")) (prin1)
	(princ (strcat "\n\n" "(LdAll))\n    Defines the 'rootFolder' path, then loads all files contained in 2D library project.\n    Usage: (LdAll)")) (prin1)
	(princ (strcat "\n\n" "(LdF f)\n    Loads all .lsp files in specified folder under rootFolder.\n    Usage: (Ldf \"main\")")) (prin1)
	(princ (strcat "\n\n" "(LdSub f)\n    Loads all .lsp files in specified subfolder of " rootFolder "\\main\\'.\n    Usage: (LdSub \"signaling\")")) (prin1)
	(princ (strcat "\n\n" "(Ls f)\n    Lists all files and folders below 'rootFolder'.\n    Usage: (Ls '<folderName>') or (LS '<folderName>'\\'<subFolderName>')")) (prin1)
	(princ (strcat "\n\n" "(Hlp)\n    Prints the help menu\n    Usage: (Hlp)")) (prin1)
	(princ (strcat "\n\n===================================================================================")) (prin1)
	(princ (strcat "\n\n" "Hlp\n    Prints the help menu\n    Usage: (Hlp)")) (prin1)
  (prin1)
)

(hlp)


(defun MkLib ( / )
	(LdAll)
	(princ "\n\n\n==> Running (C:MAIN)...\n") (prin1)
	; (setq calledFromVlide T)	; T (true) if used under AutoCAD VLIDE debugger, otherwise set to nil
	; (setq calledFromBlade nil)	; T (true) if used under BricsCAD BLADE debugger, otherwise set to nil
	(C:MAIN)
	(princ (strcat "Done.\n")) (prin1)
)



; Initialize (so you can test just one small piece, with all the global constants etc already defined)
(defun Init ( / )
	(LdAll)
	(INITIALIZE)
)



(defun LdAll ( / )
	(setvar 'SECURELOAD 0) ; Suppress AutoCAD's asking for every Lisp file loading whether it is a trusted file.
	(princ (strcat "\n\n================= DEBUGHELPER - LdAll ================= \n")) (prin1)
	(LdF "Fonts")
	(LdF "Utilities")
	(LdF "Main")
	(princ (strcat "============================================================= \n")) (prin1)
)



(defun LdF ( fn / x ) 
	; Coded as 'manual' lambda function:
	(setq folderName (strcat rootFolder "\\" fn))
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



; Load subfolder (under folder 'Main')
(defun LdSub ( fn / ) 
	(LdF (strcat "Main\\" fn))
)



; LiSt files
(defun Ls ( folderName / )
	(setq folderName (strcat rootFolder "\\" folderName))
	(foreach x (vl-directory-files (findfile folderName))
		(princ (strcat x "\n"))
	)
	(prin1)
)



(defun *error* (s)
	; Error handler - Visual LISP BackTrace function - shows call stack upon error.
	; In VLIDE, you can access the same info by pressing Ctrl+Shift+T which brings up a callback trace window.
	; Press Ctrl+F9 to locate the source code for the error, where you can set a suitable breakpoint with F9
	; and add watches with Ctrl+W.
	(vl-bt)
	(princ)
)



; For debugging purposes - or just use this code as your Lisp template:
(defun C:I2M ( / m in string) ;inches to meters
	(setq in (GETDIST "\nValue in Inches: ")) ; A prompt appears (either as tooltip in modelspace, or in the console line).
	(setq m (* in 0.0254))
	(alert (strcat (rtos in 2 1) " inches is " (rtos m 2 3) " meters.")) ; A box pops up with an "OK" button. Control returns when the user clicks 'OK'.
)
