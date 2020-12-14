;
; Skiltobjekter.lsp
;
(loadFolder (findfile "Skilt"))

(defun C:GENERATE-BOARDS-AND-POLES-OBJECTS ( / 
		systemVars
		userSettings
		tempSettings
		symbols
		blockNames
		descriptions
	)
	(setCadSystemDefaults)
	(newLayer "JBTSI__SKILT_WIPEOUT" "white" "Wipeout for skilt")
	(setq
		blockNames (getFunctionNames)	; Funksjon med liste over alle skilt som skal lages - filnavn er samme som funksjon.
		#symbols (length blockNames)
	)
	(setq n 0)
	(repeat #symbols
		(setq descriptions (append descriptions (list (eval (list (read (nth n blocknames)))))))
		; Save block to separate file:
		; (command "-wblock" (strcat dwgpath (nth n blocknames) ".dwg") "y" (nth n blocknames))
		(setq n (1+ n))
	)
	; (TABLE #symbols blockNames descriptions)
	; (command "._SAVE" (strcat (findfile "JBV") "\\" "Signs.dwg") "Y")
)