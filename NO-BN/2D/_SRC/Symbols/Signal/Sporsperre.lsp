;
; Sporsperre.lsp
;
(defun C:SPORSPERRE ()
	;create blocks
	(setq quadrant 1)
	(newLayer "JBTSI__SA_SSP_TEKST" "white" "Signal - visning av sporsperre tekst")
	(newLayer "JBTSI__SA_SSP_WIPEOUT" "white" "Wipeout for sporsperrer")
	(repeat 4
		(sporSperre "ENKEL" quadrant)
		(sporSperre "DOBBEL" quadrant)
		(setq quadrant (+ 1 quadrant))
	)
)



(defun sporSperre (spec quadrant / x y a1 a2 ang pt)
	(setq
		blockName "NO-BN-2D-JBTOB-SPOROBJEKT-SPORSPERRE"
		x (/ 3 2.0)
		y (/ 1.5 2)
		a1 0.5
		a2 0.70
		ang (atan (/ y x))
		pt (list (+ x (* a1 (cos ang))) (+ y (* a1 (sin ang))))
		;h (sqrt (+ (* (* 2 x) (* 2 x)) (* (* 2 y) (* 2 y))))
	)
	(if (= spec "ENKEL")
		(progn
			(setq blockName (strcat blockName "-" spec "-" (rtos quadrant 2 0)))
			(command 
				"._LAYER" "SET" "0" ""
				"._RECTANG" (list x y) (list (- x) (- y))
				"._LAYER" "SET" "JBTSI__SA_SSP_WIPEOUT" ""
				"._WIPEOUT" "Polyline" "L" "" "Yes"
				"._LAYER" "SET" "0" ""
				"._LINE" (list (- x) (- y)) pt ""
				"._PLINE"
					(polar pt (+ (/ pi 2) ang) (/ a2 2))
					(polar pt (+ (/ pi 2) ang) (/ a2 -2))
					(polar pt ang a2)
					"CLOSE"
			)
			(drawHatch 0.01)
			(command "._MOVE" "ALL" "" (list 0 y) "0,0")
			(command "._LAYER" "Set" "JBTSI__SA_SSP_TEKST" "")
			(command "._COLOR" "ByBlock" "")
			(mirrorSelection quadrant "ALL")
			(newBlock blockName)
			blockName
		)
	;else "DOBBEL"
		(progn
			(setq 
				blockName (strcat blockName "-" spec "-" (rtos quadrant 2 0))
				ang (atan (/ (* 2 y) x))
				pt (list (* a1 (cos ang)) (+ y (* a1 (sin ang))))
			)
			(command
				"._LAYER" "SET" "0" ""
				"._RECTANG" (list x y) (list (- x) (- y))
				"._LAYER" "SET" "JBTSI__SA_SSP_WIPEOUT" ""
				"._WIPEOUT" "Polyline" "L" "" "Yes"
				"._LAYER" "SET" "0" ""
				"._LINE" (list (- x) (- y)) pt ""
				"._PLINE" 
					(polar pt (+ (/ pi 2) ang) (/ a2 2))
					(polar pt (+ (/ pi 2) ang) (/ a2 -2))
					(polar pt ang a2)
					"CLOSE"
			)
			(drawHatch 0.01)
			(setq 
				pt (list 
					(+ x (* a1 (cos ang))) 
					(+ y (* a1 (sin ang))))
			)
			(command
				"._LINE" (list 0 (- y)) pt ""
				"._PLINE" 
					(polar pt (+ (/ pi 2) ang) (/ a2 2))
					(polar pt (+ (/ pi 2) ang) (/ a2 -2))
					(polar pt ang a2)
					"CLOSE"
			)
			(drawHatch 0.01)
			(command 
				"._LAYER" "Set" "JBTSI__SA_SSP_TEKST" ""
				"._COLOR" "ByBlock"
				"._ROTATE" "ALL" "" "0,0" 90
				"._MOVE" "ALL" "" (list 0 y) "0,0"
				"._MIRROR" "ALL" "" "0,0" "0,1" "Y"
			)
			(mirrorSelection quadrant "ALL")
			(newBlock blockName)
			blockName
		)
	);endif ENKEL / DOBBEL
)

