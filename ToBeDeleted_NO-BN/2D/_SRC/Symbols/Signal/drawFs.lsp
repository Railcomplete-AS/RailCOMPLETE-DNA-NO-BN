;
; drawFs.lsp
;
(defun drawFs (pole / r lowerPos upperPos)
	(setq
		r (getSradius)
		lowerPos (list (- pole (* 3 r)) (- r))
		upperPos (list (- pole r) (- r))
	)
	(drawLantern lowerPos)
	(drawLantern upperPos)
)