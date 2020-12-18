;
; Fiktivt punkt.lsp
;
(defun C:FIKTIVT-PUNKT ()
	(FIKTIVT-PUNKT)
	(VIAPUNKT)
)



(defun FIKTIVT-PUNKT (/ blockName ang len1 width)
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-FIKTIVT-PUNKT"
		ang 130
		len1 (/ 3.0 2.0)
		width (/ 0.416 2.0)
	)
	(command 
		"._PLINE"
			"0,0" 
			(strcat (rtos len1) "<" (rtos ang))
			(strcat "@" (rtos width) "<" (rtos (+ ang 90)))
			(strcat "-" (rtos (/ width (sin (D->R ang)))) "," "0")
			""
		"._MIRROR" "L" "" "0,0" "1,0" "N" 
	)
	(newBlock blockName)
)



(defun VIAPUNKT (/ blockName len1 width)
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-VIAPUNKT"
		len1 (/ 3.0 2.0)
		width (/ len1 2.0)
	)
	(command
		"._POLYGON" 3 (list 0 0) "Inscribed" len1
		"._POLYGON" 3 (list 0 0) "Inscribed" (- len1 width)
		"._ROTATE" "ALL" "" (list 0 0) (- 90)
		"._MOVE" "ALL" "" (list 0 0) (list (- len1) 0)
	)
	(newBlock blockName)
)

