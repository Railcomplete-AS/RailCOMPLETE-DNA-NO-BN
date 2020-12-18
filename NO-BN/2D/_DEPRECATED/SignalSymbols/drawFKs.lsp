;
; drawFKs.lsp
;
; 2020-08-02 CLFEY Deprecated code, is now re-written and included in main signal LISP routines.
;
(defun drawFKs (mainPole F / FKsRadius FKsSide FKsDist X Y pt1 pt2)
	(if (= F 1)
		(setq 
			r (getLargeLanternRadius)
			topFKs (list (- mainPole (* 4 r)) (- r))
		) 
	;else
		(setq
			r (getLargeLanternRadius)
			topFKs (list mainPole (- r))
		)
	);endif
	(setq
		FKsRadius 0.2
		FKsSide 2.0
		FKsDist 0.67
		X (- (car topFKs) (/ FKsSide 2))
		Y (car (cdr topFKs))
		pt1 (list (+ X (/ FKsSide 2)) (+ Y (/ FKsSide 2)))
		pt2 (list (- X (/ FKsSide 2)) (- Y (/ FKsSide 2)))
	)
	(bulbFKs (list X Y) 0.2)
	(bulbFKs (list (+ X FKsDist) Y) 0.2)
	(bulbFKs (list (- X FKsDist) Y) 0.2)
	(bulbFKs (list X (+ Y FKsDist)) 0.2)
	(bulbFKs (list X (- Y FKsDist)) 0.2)
	(command "._RECTANGLE" pt1 pt2)
	(princ)
)

(defun bulbFKs (bulbPos FKsRadius)
	(command "._CIRCLE" bulbPos FKsRadius)
	(princ)
)