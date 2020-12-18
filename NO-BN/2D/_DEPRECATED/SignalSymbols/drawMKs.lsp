;
; drawMKs.lsp
;
; 2020-08-02 CLFEY Deprecated code, is now re-written and included in main signal LISP routines.
;
(defun drawMKs (pole / r MKsradius topFKs lanternMKs)
	(setq
		r (getLargeLanternRadius)
		MKsradius (getMediumLanternRadius)
		topFKs (list (- pole (* 4 r)) (- r))
		lanternMKs (list (- pole r) MKsradius)
	)
	(command "._CIRCLE" lanternMKs MKsradius)
	(princ)
)