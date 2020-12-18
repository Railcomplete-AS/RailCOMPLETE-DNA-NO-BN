
(defun Roemningsavstand_H (/ width height)
	(setq blockName "NO-BN-2D-JBTSI-SKILT-TREDJEPERSON-ROEMNINGSVEI-H"
		  description "Roemningsavstand mot hoeyre"
		  width 10.0
		  height 7.0
	)
	(command
		"._RECTANG" (list 0 0) (list width height)
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
	)
	(addText "RØMNINGSVEI" (list (/ width 2) (- height 1.0)) 1.0 0 "iso" "MC")
	(addAtt "HDIST" "Hdist" "38" (list (/ width 2) 1.5) 1.5 0 "iso" "MC" 16)
	(DrawArrow (list 6.61 4.0) 0)
	(command "._MOVE" "ALL" "" "0,0" (list (- (/ width 2)) 0))
	(command "._SCALE" "ALL" "" "0,0" "0.3")
	(newBlock blockName)
	blockName
)



(defun Roemningsavstand_V (/ width height)
	(setq 
		blockName "NO-BN-2D-JBTSI-SKILT-TREDJEPERSON-ROEMNINGSVEI-V"
		description "Roemningsavstand mot venstre"
		width 10.0
		height 7.0
	)
	(command 
		"._RECTANG" (list 0 0) (list width height)
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
	)
	(addText "RØMNINGSVEI" (list (/ width 2) (- height 1.0)) 1.0 0 "iso" "MC")
	(addAtt "VDIST" "Vdist" "38" (list (/ width 2) 1.5) 1.5 0 "iso" "MC" 16)
	(DrawArrow (list 3.37 4.0) 180)
	(command "._MOVE" "ALL" "" "0,0" (list (- (/ width 2)) 0))
	(command "._SCALE" "ALL" "" "0,0" "0.3")
	(newBlock blockName)
	blockName
)



(defun Roemningsavstand_VH (/ width height)
	(setq 
		blockName "NO-BN-2D-JBTSI-SKILT-TREDJEPERSON-ROEMNINGSVEI-VH"
		description "Roemningsavstand mot venstre og hoeyre"
		width 10.0
		height 7.0
	)
	(command
		"._RECTANG" (list 0 0) (list width height)
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
	)
	(addText "RØMNINGSVEI" (list (/ width 2) (- height 1.0)) 1.0 0 "iso" "MC")
	(addAtt "VDIST" "Vdist" "38" (list 2.5 1.5) 1.5 0 "iso" "MC" 16)
	(addAtt "HDIST" "Hdist" "38" (list 7.5 1.5) 1.5 0 "iso" "MC" 16)
	(DrawArrow (list 9.25 4.0) 0)
	(DrawArrow (list 0.75 4.0) 180)
	(command "._MOVE" "ALL" "" "0,0" (list (- (/ width 2)) 0))
	(command "._SCALE" "ALL" "" "0,0" "0.3")
	(newBlock blockName)
	blockName
)



(defun DrawArrow (tip dir / p1 p2 p3 p4 q1 q2 q3 q4)
	(setq 
		p1 (polar tip (D->R 135) 1.067)
		p2 (polar p1 (D->R 180) 0.75)
		p3 (polar p2 (D->R 315) 0.7071)
		p4 (polar p3 (D->R 180) 2.25)
		q1 (polar tip (D->R 225) 1.067)
		q2 (polar q1 (D->R 180) 0.75)
		q3 (polar q2 (D->R 45) 0.7071)
		q4 (polar q3 (D->R 180) 2.25)
	)
	(command "._PLINE" tip p1 p2 p3 p4 q4 q3 q2 q1 tip "")
	(command "._ROTATE" "LAST" "" tip dir)
)



(defun c:DrawRoemningsAvstandsSkilt ()
	(Roemningsavstand_V)
	(Roemningsavstand_H)
	(Roemningsavstand_VH)
)