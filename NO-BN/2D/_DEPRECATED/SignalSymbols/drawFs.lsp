;
; drawFs.lsp
;
; 2020-08-02 CLFEY Deprecated code, is now re-written and included in main signal LISP routines.
;
(defun drawFs (pole / r lowerPos upperPos)
	(setq
		r (getLargeLanternRadius)
		lowerPos (list (- pole (* 3 r)) (- r))
		upperPos (list (- pole r) (- r))
	)
	(drawHsLantern lowerPos)
	(drawHsLantern upperPos)
)