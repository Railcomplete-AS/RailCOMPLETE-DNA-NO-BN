;
; Akselteller-sopp.lsp
;
(defun C:AKSELTELLER-SOPP (/ blockName Size )
	(setq
		blockName "NO-BN-2D-JBTSI-TOGDETEKSJON-TELLEPUNKT-SOPP"
		scale	1.0
	)
	(command
		"._PLINE" (list 0.0 (- 0.06)) (list (- 1.0) (- 0.06)) (list (- 1.0) 0.06) (list 0.0 0.06) ""
		"._PLINE" (list (- 0.06) 0.06) (list (- 0.06) 1.0) (list 0.0 1.0) ""
		"._PLINE" (list (- 0.06) 1.0) (list (- 0.85) 1.0) (list (- 0.85) 1.1) (list 0 1.1) ""
		"._PLINE" (list (- 0.75) 1.1) (list (- 0.7) 2.0) (list (- 0.6) 2.15) (list 0 2.15) ""
		"._MIRROR" "ALL" "" (list 0.0 0.0) (list 0.0 1.0) "NO"
		"._SCALE" "ALL" "" "0,0" scale
	)
	(addText "Az" (list 0 1.625) 0.5 0 "ISO" "MC")
	(newBlock blockName)
)