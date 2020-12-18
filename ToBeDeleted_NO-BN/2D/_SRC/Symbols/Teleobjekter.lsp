;
; Teleobjekter.lsp
;
(loadFolder (findfile "Tele")) 

(defun C:GENERATE-TELECOM-OBJECTS (/)
	(setCadSystemDefaults)
	(C:TELERACK)
	(C:TELEFON) 
)