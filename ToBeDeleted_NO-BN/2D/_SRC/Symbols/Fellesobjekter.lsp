;
; Fellesobjekter.lsp
;
(loadFolder (findfile "Felles"))

(defun C:GENERATE-COMMON-OBJECTS ()
	(setCadSystemDefaults)
	(C:CONTROLLER)
	(C:LABEL)
	(C:MARKER)
	(C:SECTION)
	(C:WATCH)
)

