;
; Avsporingsindikator.lsp
;
(defun C:AVSPORINGSINDIKATOR ( )
	(AVSPORINGSINDIKATOR)
)

(defun AVSPORINGSINDIKATOR (/ x dx)
	(setq
		blockName "NO-BN-2D-JBTSI-DIVERSE-AVSPORINGSINDIKATOR"
		x 0.6
        dx 0.05
	)
	(command
		"._RECTANG" (list (- x) (- x)) (list x x)
		"._RECTANG" (list (- dx) (- x)) (list dx x)
	)
	(drawHatch 0.01)
	(newBlock blockName)
	blockName
)
