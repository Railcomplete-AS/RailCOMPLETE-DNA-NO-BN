(defun C:JORDING ()
	(JORDPOTENSIAL)
	(JORDINGSSKINNE)
)
; Generelt jordsymbol - kan benyttes som jordpotensial-markør, eller som kråkefotsymbol osv.
(defun JORDPOTENSIAL (/ blockName)
	(setq
		blockName "NO-BN-2D-JBTKL-JORDING-JORDPOTENSIAL"
	)
	(command
		;"._CIRCLE" (list 0 0) 0.45
		"._LINE" "0,10" "0,25" ""
		"._LINE" "-10,10" "10,10" ""
		"._LINE" "-6,5" "6,5" ""
		"._LINE" "-3,0" "3,0" ""
		"._MOVE" "ALL" "" "0,25" "0,0"
		"._SCALE" "ALL" "" "0,0" "0.10"
	)
	(newBlock blockName)
	blockName
)


; For montasje under datagulv i bygninger, på vegg, i trekkekum med mer. 
; TODO - lage flere varianter, modeller faktisk utseende med flerer gjengede hull for skruforbindere / kabelsko.
(defun JORDINGSSKINNE (/ x y circDist r blockName)
	(setq
		blockName "NO-BN-2D-JBTKL-JORDING-JORDINGSSKINNE"
		x (/ 3.0 2.0)
		y (/ 1.0 2.0)
		circDist (/ 3.0 5.0)
		r 0.15
		scale 0.5
	)
	(command
		"._RECTANGLE" (list (- x) (- y)) (list x y)
		"._CIRCLE" (list (+ (- x) circDist) 0) r
		"._CIRCLE" (list (+ (- x) (* 2 circDist)) 0) r
		"._CIRCLE" (list (+ (- x) (* 3 circDist)) 0) r
		"._CIRCLE" (list (+ (- x) (* 4 circDist)) 0) r
		"._SCALE" "ALL" "" "0,0" scale
	)
	(newBlock blockName)
	blockName
)