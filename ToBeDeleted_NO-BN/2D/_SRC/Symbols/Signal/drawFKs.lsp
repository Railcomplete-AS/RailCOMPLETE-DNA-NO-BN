;
; drawFKs.lsp
;
(defun drawFKs (topOfPole F / FKsradius FKsSide FKsDist X Y pt1 pt2)
	(if (= F 1)
		(setq 
			r (getSradius)
			topFKs (list (- topOfPole (* 4 r)) (- r))
		) 
	;else
		(setq
			r (getSradius)
			topFKs (list topOfPole (- r))
		)
	);endif
	(setq
		FKsradius 0.2
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
	(command "._RECTANG" pt1 pt2)
	(princ)
)

(defun bulbFKs (bulbPos FKsradius)
	(command "._CIRCLE" bulbPos FKsradius)
	(princ)
)