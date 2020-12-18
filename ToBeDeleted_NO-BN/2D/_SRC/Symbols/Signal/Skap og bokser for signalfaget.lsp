;
; Skap og bokser for signalfaget.lsp
;
(defun C:SKAP-OG-BOKSER-FOR-SIGNALFAGET ()
	(newLayer "JBTSI__KOLLISJONSKONTROLL" 62 "")
	(APPARATSKAP-LITE "VHENGSLET")
	(APPARATSKAP-LITE "HHENGSLET")
	(APPARATSKAP-STORT "VHENGSLET")
	(APPARATSKAP-STORT "HHENGSLET")
	(APPARATSKAP-STORT "DOBBEL")
	(KABELBOKS)
	(NOEKKELSKAP "VHENGSLET")
	(NOEKKELSKAP "HHENGSLET")
	(S-LAAS)
	(SVEIVSKAP "VHENGSLET")
	(SVEIVSKAP "HHENGSLET")
)



(defun APPARATSKAP-LITE (door / x y attreq)
    (setq 
		x 1.0
		y 1.0
		blockName (strcat "NO-BN-2D-JBTSI-SKAP-APPARATSKAP-LITE-" door)
	)
	(command
		"._LAYER" "Set" "0" ""
		"._RECTANG" "0,0" (list x y)
		"._LINE" "0,0" (list x y) ""
		"._LINE" (list x 0) (list 0 y) ""
		"._MOVE" "ALL" "" "Displacement" (list (/ x -2) (- y))
	)
	(command
		"._LAYER" "Set" "JBTSI__KOLLISJONSKONTROLL" ""
		"._COLOR" "ByLayer"
	)
	(if (= door "VHENGSLET")
		(command 
			"._LINE"
				(list  (/ x 2) 0)
				(list (- (/ x 2) (* x (cos (/ 3.1415 4)))) (* x (sin (/ 3.1415 4))))
				""
			"._ARC" (list (/ x -2) 0) "C" (list (/ x 2) 0) "A" -180
		) 
	;else
		(command
			"._LINE"
				(list  (/ x -2) 0)
				(list (+ (* x (cos (/ 3.1415 4)))  (/ x -2)) (* x (sin (/ 3.1415 4))))
				""
			"._ARC" (list (/ x 2) 0) "C" (list (/ x -2) 0) "A" 180
		)
    )
	(command
		"._MOVE" "ALL" "" "0,0" (list 0 y)
		"._ROTATE" "ALL" "" (list 0 0) 180
	)
	(newBlock blockName)
	blockName
)



(defun APPARATSKAP-STORT (door / x y x2 y2 attreq)
    (setq 
		x 1.2
		y 0.5
		blockName (strcat "NO-BN-2D-JBTSI-SKAP-APPARATSKAP-STORT-" door)
	)
	(command 
		"._LAYER" "Set" "0" ""
		"._RECTANG" "0,0" (list x y)
		"._LINE" "0,0" (list x y) ""
		"._LINE" (list x 0) (list 0 y) ""
		"._MOVE" "ALL" "" "Displacement" (list (/ x -2) (- y))
	)
	(command 
		"._LAYER" "Set" "JBTSI__KOLLISJONSKONTROLL" ""
		"._COLOR" "ByLayer"
	)
	(if (= door "VHENGSLET")
		(command 
			"._LINE" 
				(list  (/ x 2) 0)
				(list (- (/ x 2) (* x (cos (/ 3.1415 4)))) (* x (sin (/ 3.1415 4))))
				""
			"._ARC" (list (/ x -2) 0) "C" (list (/ x 2) 0) "A" -180
		)
    )
	(if (= door "HHENGSLET")
		(command 
			"._LINE" 
				(list  (/ x -2) 0)
				(list (+ (* x (cos (/ 3.1415 4)))  (/ x -2)) (* x (sin (/ 3.1415 4))))
				""
			"._ARC" (list (/ x 2) 0) "C" (list (/ x -2) 0) "A" 180
		)
    )
	(if (= door "DOBBEL")
		(command 
			"._LINE" 
				(list  (/ x 2) 0)
				(list (- (/ x 2) (* (/ x 2) (cos (/ 3.1415 4)))) (* (/ x 2) (sin (/ 3.1415 4))))
				""
			"._ARC" (list 0 0) "C" (list (/ x 2) 0) "A" -180 
			"._LINE"
				(list  (/ x -2) 0)
				(list (+ (* (/ x 2) (cos (/ 3.1415 4)))  (/ x -2)) (* (/ x 2) (sin (/ 3.1415 4))))
				""
			"._ARC" (list 0 0) "C" (list (/ x -2) 0) "A" 180
		)
    )
	(command
		"._LAYER" "Set" "0" ""
		"._MOVE" "ALL" "" "0,0" (list 0 (/ y 2))
		"._ROTATE" "ALL" "" (list 0 0) 180
	)
	(newBlock blockName)
	blockName
)



(defun KABELBOKS (/ blockName s1 s2)
	(setq
		blockName "NO-BN-2D-JBTSI-SKAP-KABELBOKS"
		s1 (/ 0.2 2) ; Actual measures are approx 0.2 wide
		s2 s1
		scale 3 ; Make it more visible in a 2D drawing
	)
	(command
		"._RECTANGLE" (list (- s1) 0) (list s1 (- s2))
		"._LINE" (list (- s1) 0) (list s1 (- s2)) ""
		"._MIRROR" "L" "" "0,0" "0,1" "N"
		"._SCALE" "ALL" "" "0,0" scale
	)
	(newBlock blockName)
	blockName
)



(defun NOEKKELSKAP ( door / blockName x y letterscale pt1 pt2 pt3 pt4)
	(setq
		blockName (strcat "NO-BN-2D-JBTSI-SKAP-NOEKKELSKAP-" door)
		x 1.0
		y 1.0
		letterscale 0.5
		pt1 (list (* letterscale (/ x -2)) (* (- 1 letterscale) (/ y 2)))
		pt2 (list (* letterscale (/ x -2)) (* (+ 1 letterscale) (/ y 2)))
		pt3 (list (* letterscale (/ x 2)) (* (- 1 letterscale) (/ y 2)))
		pt4 (list (* letterscale (/ x 2)) (* (+ 1 letterscale) (/ y 2)))
	)
	(command "._LAYER" "Set" "0" "")
	(command 
		"._RECTANG"
			(list  (/ x -2) 0)
			(list (/ x 2) y)
	)
	; Draw 'N' in rectangle:
	(command 
		"._PLINE" pt1 pt2 pt3 pt4 ""
	)
	(command
		"._LAYER" "Set" "JBTSI__KOLLISJONSKONTROLL" ""
	   "._COLOR" "ByLayer"
	)
	(if (= door "VHENGSLET")
		(command 
			"._LINE"
				(list  (/ x 2) y)
				(list (- (/ x 2) (* x (cos (/ 3.1416 4)))) (+ y (* y (sin (/ 3.1416 4)))))
				""
			"._ARC" (list (/ x -2) y) "C" (list (/ x 2) y) "A" -180
		)
	;else
		(command 
			"._LINE"
				(list  (/ x -2) y)
				(list (+ (* x (cos (/ 3.1415 4)))  (/ x -2)) (+ y (* y (sin (/ 3.1415 4)))))
				""
			"._ARC" (list (/ x 2) y) "C" (list (/ x -2) y) "A" 180
	   )
	)
	(command 
		"._LAYER" "Set" "0" ""
		"._ROTATE" "ALL" "" (list 0 0) 180
	)
	(newBlock blockName)
	blockName
)



(defun S-LAAS (/ blockName)
	(setq blockName "NO-BN-2D-JBTSI-SAMLELAAS")
	(command
		"._LINE" "-4.5,0" "4.5,0" ""
		"._LINE" "-3,0" "-3,2" "3,2" "3,0" ""
		"._LINE" "-3,1" "-2,0" ""
		"._LINE" "-3,2" "-1,0" ""
		"._LINE" "-2,2" "0,0" ""
		"._LINE" "-1,2" "1,0" ""
		"._LINE" "0,2" "2,0" ""
		"._LINE" "1,2" "3,0" ""
		"._LINE" "2,2" "3,1" "")
  	(newBlock blockName)
	blockName
)



(defun SVEIVSKAP (door / blockName x y crank1 crank2 crank3 crank4)
    (setq 
		blockName (strcat "NO-BN-2D-JBTSI-SKAP-SVEIVSKAP-" door)
		x 1.0
		y 1.0
		crank1 (list 0.2 0.6)
		crank2 (list 0.6 0.6)
		crank3 (list 0.6 0.4)
		crank4 (list 0.8 0.4)
	)
	(command "._LAYER" "Set" "0" "")
	(command 
		"._RECTANG" "0,0" (list x y)
	)
	; Draw 'crank handle' in rectangle:
	(command 
		"._PLINE" crank1 crank2 crank3 crank4 ""
	)
	(command 
		"._MOVE" "ALL" "" "Displacement" (list (/ x -2) (- y))
	)
	(command "._LAYER" "Set" "JBTSI__KOLLISJONSKONTROLL" "")
	(command "._COLOR" "ByLayer")
	(if (= door "VHENGSLET")
		(command 
			"._LINE"
				(list  (/ x 2) 0)
				(list (- (/ x 2) (* x (cos (/ 3.1415 4)))) (* x (sin (/ 3.1415 4))))
				""
			"._ARC" (list (/ x -2) 0) "C" (list (/ x 2) 0) "A" -180
		)
	 ;else
		(command 
			"._LINE"
				(list  (/ x -2) 0)
				(list (+ (* x (cos (/ 3.1415 4)))  (/ x -2)) (* x (sin (/ 3.1415 4))))
				""
			"._ARC" (list (/ x 2) 0) "C" (list (/ x -2) 0) "A" 180
		)
	)
	(command
		"._MOVE" "ALL" "" "0,0" (list 0 y)
		"._ROTATE" "ALL" "" (list 0 0) 180
	)
	(newBlock blockName)
	blockName
)

