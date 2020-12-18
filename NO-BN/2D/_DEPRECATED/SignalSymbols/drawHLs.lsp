;
; drawHls.lsp - Hovedlinjesignal (linjesignal)
;
; Bokstaven(e) på signalet settes inn som TextAttribute i ObjectDefinition i DNA.
;
; 2020-08-02 CLFEY Deprecated code, is now re-written and included in main signal LISP routines.
;
(defun drawHLs (pole)
	; Version without signal FKs at the same time:
	(setq
		r (getLargeLanternRadius)
    	x (- pole (* 6 r))
		y (- (* 2 r))
		pt1 (list x y)
		pt2 (list (+ x (* 2 r)) (+ y (* 2 r)))
		pt3 (list (+ x r) (+ y r))
	)
	(command "._RECTANGLE" pt1 pt2)
	(addAtt "HLS" "Hovedlinjesignal" "A/D" pt3 0.7 -90 "iso" "_MC" _lockPosition_)
	(princ)
)

(defun drawHLs2 (pole)
	; Version with signal FKs at the same time:
	(setq
		r (getLargeLanternRadius)
    	x (- pole (* 2 r))
		y 0
		pt1 (list x y)
		pt2 (list (+ x (* 2 r)) (* 2 r))
		pt3 (list (+ x r) (+ y r))
	)
	(command "._RECTANGLE" pt1 pt2)
	(addAtt "HLS" "Hovedlinjesignal" "A/D" pt3 0.7 -90 "iso" "_MC" _lockPosition_)
	(princ)
)