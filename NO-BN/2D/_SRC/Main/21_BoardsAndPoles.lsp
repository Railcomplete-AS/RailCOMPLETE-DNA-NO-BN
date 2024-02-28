;=========================================================================================================================
;
; 21_BoardsAndPoles.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Boards and poles objects top-level LISP routine

; to many
(setq f (strcat rootFolder "\\Main\\BoardsAndPoles"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)

(defun 21_GENERATE-BOARDSANDPOLES-OBJECTS ( / #symbols blockNames descriptions )
	(SetCadSystemDefaults)

	; Implemented for all administrations:
	(TraceLevel3 "BOARD-DISTRESS-CALL-TELEPHONE")		(BOARD-DISTRESS-CALL-TELEPHONE)
	(TraceLevel2 "BOARD-EVACUATION-DISTANCE")		 	(BOARD-EVACUATION-DISTANCE)
	
	; Specific to this administration:
	(TraceLevel2 "GetBoardAndPoleNames...")
	(setq
		blockNames (GetBoardAndPoleNames)	; Function with list of all boards to be added - filename must be the same as the function name.
		#symbols (length blockNames)
	)
	(setq n 0)
	(repeat #symbols
		(TraceLevel2 (nth n blocknames))
		(setq descriptions (append descriptions (list (eval (list (read (nth n blocknames))))))) ; Call function, create object, return description
		(setq n (1+ n))
	)
	; 2020-07-26 deprecated code, must be fixed if it is to be used:
	; (TABLE #symbols blockNames descriptions)
	; (command "._SAVEAS" (strcat (findfile "JBV") "\\" "Signs.dwg") _overwriteFile_)
)
