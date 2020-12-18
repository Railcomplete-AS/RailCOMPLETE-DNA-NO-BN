(defun 67A (/ blockName description side offset)
  (setq	blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-67A-ORIENTERINGSIGNAL"
		description "Signal 67A Orienteringsignal"
	side 2.25
	offset 0.1406
  )
  (draw67A side offset)
  (drawHatchSelectPoint 0.01 (list 0 offset) 45 0.01)
  (newBlock blockName)
  description
)

(defun draw67A (side offset)
  (command "._RECTANG"
	   "0,0"
	   (list side side)
	   "._OFFSET"
	   offset
	   "0,0"			;sel rectangle
	   (list (/ side 2) (/ side 2))	;side to offset to
	   ""
  	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (command "._ROTATE" "ALL" "" "0,0" "45")
)

(defun 67B (/ blockName	description side offset cos45 pt1 pt2 inner)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-67B-ORIENTERINGSIGNAL-PLANOVERGANG"
		description "Signal 67B Orienteringsignal for planovergang"
    side 2.25
	offset 0.1406
	cos45 (cos (D->R 45))
	pt1 (list 0 (/ offset cos45))
	pt2 (list 0 (- (/ side cos45) (/ offset cos45)))
	inner (- (- side (* 2 offset)) (* offset cos45))
	)
  (command "._RECTANG"
	   "0,0"
	   (list side side)
	   "._ROTATE" "ALL" "" "0,0" "45"
	   "._PLINE"
	   pt1
	   (polar pt1 (D->R 45) inner)
	   (polar pt1 (D->R 135) inner)
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   pt2
	   (polar pt2 (D->R (- 45)) inner)
	   (polar pt2 (D->R (- 135)) inner)
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
  )
  
  (drawHatchSelectPoint 0.01 (list 0 offset) 45 0.01)
  (newBlock blockName)
  description
)

(defun 67C (/ blockName	description side offset cos45 pt1 pt2 inner)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-67C-ORIENTERINGSIGNAL-HOLDEPLASS"
		description "Signal 67C Orienteringsignal for holdeplass"
	side 2.25
	offset 0.1406
	cos45 (cos (D->R 45))
	pt1 (list 0 (/ offset cos45))
	pt2 (list 0 (- (/ side cos45) (/ offset cos45)))
	inner (- (- side (* 2 offset)) (* offset cos45))
	)
  (command "._RECTANG"
	   "0,0"
	   (list side side)
	   "._ROTATE" "ALL" "" "0,0" "45"
	   "._PLINE"
	   pt1
	   (polar pt1 (D->R 45) inner)
	   (polar pt1 (D->R 135) inner)
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   pt2
	   (polar pt2 (D->R (- 45)) inner)
	   (polar pt2 (D->R (- 135)) inner)
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
  )
  (drawHatchSelectPoint 0.01  (list 0 offset) 45 0.01)
  (command "._ROTATE" "ALL" ""
	   (list 0 (* side cos45))
	   "90"
	   )
  (newBlock blockName)
  description
)

(defun 67D (/ blockName description side offset cos45 pt1 pt2 shift inner45 inner0)
  (setq	blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-67D-ORIENTERINGSIGNAL-PLANOVERGANG-HOLDEPLASS"
		description "Signal 67D Orienteringsignal for planovergang og holdeplass"
	side	  2.25
	offset	  0.1406
	cos45	  (cos (D->R 45))
	pt1	  (list 0 (/ offset cos45))
	pt2	  (list 0 (- (/ side cos45) (/ offset cos45)))
	shift	  (* offset cos45)
	inner45	  (rtos (- (- side (* 2 offset)) (* 2 shift)) 2 10)
	inner0	  (rtos (* (- side (* 2 offset) (* 2 shift)) cos45) 2 10)
  )
  (command "._RECTANG"
	   "0,0"
	   (list side side)
	   "._ROTATE"
	   "ALL"
	   ""
	   "0,0"
	   "45"
					;Bottom Right:
	   "._PLINE"
	   (polar pt1 (D->R 45) shift)
	   (strcat "@" inner45 "<" "45")
	   (strcat "@" inner0 "<" "180")
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
					;Top Right:
	   "._PLINE"
	   (polar pt2 (D->R (- 45)) shift)
	   (strcat "@" inner45 "<" "-45")
	   (strcat "@" inner0 "<" "180")
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
					;Top Left:
	   "._PLINE"
	   (polar pt2 (D->R (- 135)) shift)
	   (strcat "@" inner45 "<" "-135")
	   (strcat "@" inner0 "<" "0")
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
					;Bottom Left:
	   "._PLINE"
	   (polar pt1 (D->R 135) shift)
	   (strcat "@" inner45 "<" "135")
	   (strcat "@" inner0 "<" "0")
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
  )
  (drawHatchSelectPoint "0.01" (list 0 offset) 0 0.01)
  (newBlock blockName)
  description
)

(defun 67E (/ blockName description  x y)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-67E-ORIENTERINGSIGNAL-DAGTID"
		description "Signal 67E Orienteringsignal dagtid"
	x 2.25
	y 1.50
	)
  (command "._RECTANG" "0,0" (list x y)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._LINE" "0,0" (list x y) ""
	 )
  (drawHatchSelectPoint "0.01" (list (/ x 2) (/ y 3)) 0 0.01)
  (command "._MOVE" "ALL" "" "DISPLACEMENT"
	   (strcat (rtos (- (/ x 2)) 1 3) ",0")
	   )
  (newBlock blockName)
  description
)