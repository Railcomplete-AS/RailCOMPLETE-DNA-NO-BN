;=========================================================================================================================
;
; BoardsAndPoles.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Boards and poles objects top-level LISP routine

(loadFolder (findfile "BoardsAndPoles"))

(defun C:GENERATE-BOARDSANDPOLES-OBJECTS ( / 
		#symbols
		blockNames
		descriptions
	)
	(setCadSystemDefaults)
	
	(setq
		blockNames (getBoardAndPoleNames)	; Funksjon med liste over alle skilt som skal lages - filnavn må være samme som funksjonens navn.
		#symbols (length blockNames)
	)
	(setq n 0)
	(repeat #symbols
		(subStep (nth n blocknames))
		(setq descriptions (append descriptions (list (eval (list (read (nth n blocknames))))))) ; Call function
		; Save block to separate file:
		; (command "-wblock" (strcat dwgpath (nth n blocknames) ".dwg") "_YES" (nth n blocknames))
		(setq n (1+ n))
	)
	; 2020-07-26 deprecated code, must be fixed if it is to be used:
	; (TABLE #symbols blockNames descriptions)
	; (command "._SAVE" (strcat (findfile "JBV") "\\" "Signs.dwg") "_YES")
)
