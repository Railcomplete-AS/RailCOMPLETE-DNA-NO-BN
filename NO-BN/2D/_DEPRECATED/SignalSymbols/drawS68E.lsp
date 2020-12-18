;
; drawS68E.lsp
;
; 2020-08-02 CLFEY Deprecated code, is now re-written and included in main signal LISP routines.
; New name: 'LHs' instead of 'S68E'
;
(defun drawS68E (pole)
	(setq
		sradius (getLargeLanternRadius)
    	x (- pole (* 6 sradius))
		y (- (* 2 sradius))
		pt1 (list x y)
		pt2 (list (+ x (* 2 sradius)) (+ y (* 2 sradius)))
		pt3 (list (+ x sradius) (+ y sradius))
	)
	(command "._RECTANGLE" pt1 pt2)
	(addAtt "S68E" "Lysende hastighetssignal" 5 pt3 1.5 -90 "iso" "_MC" _lockPosition_)
	(princ)
)

(defun drawS68E2 (pole)
	(setq
		sradius (getLargeLanternRadius)
    	x (- pole (* 2 sradius))
		y 0
		pt1 (list x y)
		pt2 (list (+ x (* 2 sradius)) (* 2 sradius))
		pt3 (list (+ x sradius) (+ y sradius))
	)
	(command "._RECTANGLE" pt1 pt2)
	(addAtt "S68E" "Lysende hastighetssignal" 5 pt3 1.5 -90 "iso" "_MC" _lockPosition_)
	(princ)
)