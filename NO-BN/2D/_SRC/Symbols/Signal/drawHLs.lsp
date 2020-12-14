;
; drawHls.lsp - Hovedlinjesignal
;
; Bokstaven(e) på signalet settes inn som TextAttribute i ObjectDefinition i DNA.
;
(defun drawHLs (pole)
	(setq
		r (getSradius)
    	x (- pole (* 6 r))
		y (- (* 2 r))
		pt1 (list x y)
		pt2 (list (+ x (* 2 r)) (+ y (* 2 r)))
		pt3 (list (+ x r) (+ y r))
	)
	(command "._RECTANG" pt1 pt2)
	(addAtt "HLS" "Hovedlinjesignal" "A/D" pt3 0.7 -90 "iso" "MC" 16)
	(princ)
)

(defun drawHLs2 (pole)
	(setq
		r (getSradius)
    	x (- pole (* 2 r))
		y 0
		pt1 (list x y)
		pt2 (list (+ x (* 2 r)) (* 2 r))
		pt3 (list (+ x r) (+ y r))
	)
	(command "._RECTANG" pt1 pt2)
	(addAtt "HLS" "Hovedlinjesignal" "A/D" pt3 0.7 -90 "iso" "MC" 16)
	(princ)
)