;
; drawS68E.lsp
;
(defun drawS68E (pole)
	(setq
		sradius (getSradius)
    	x (- pole (* 6 sradius))
		y (- (* 2 sradius))
		pt1 (list x y)
		pt2 (list (+ x (* 2 sradius)) (+ y (* 2 sradius)))
		pt3 (list (+ x sradius) (+ y sradius))
	)
	(command "._RECTANG" pt1 pt2)
	(addAtt "S68E" "Lysende hastighetssignal" 5 pt3 1.5 -90 "iso" "MC" 16)
	(princ)
)

(defun drawS68E2 (pole)
	(setq
		sradius (getSradius)
    	x (- pole (* 2 sradius))
		y 0
		pt1 (list x y)
		pt2 (list (+ x (* 2 sradius)) (* 2 sradius))
		pt3 (list (+ x sradius) (+ y sradius))
	)
	(command "._RECTANG" pt1 pt2)
	(addAtt "S68E" "Lysende hastighetssignal" 5 pt3 1.5 -90 "iso" "MC" 16)
	(princ)
)