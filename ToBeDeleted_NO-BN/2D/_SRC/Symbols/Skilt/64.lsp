(defun 64A (/ blockName description  x y xs ys textpt)
  (setq	blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-64A-GRENSESTOLPE"
		description "Signal 64A Grensestolpe"
	x 0.5
	y 0.3
	xs (rtos x 1 2)
	ys (rtos y 1 2)
	textpt (list 0.2 0.5)
  )
  (command "._PLINE" ;;1
	   (list 0 x)
	   (list 0 0)
	   (list y 0)
	   (list y x)
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list y x)
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.01)
  (command "._PLINE" ;;2
	   (list y (* 2 x))
	   (strcat "@" "0," xs)
	   (list 0 (* 3 x))
	   (list 0 (* 2 x))
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""	
	   "._PLINE"
	   (list y (* 3 x))
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.01)
  (command "._PLINE" ;;3
	   (list y (* 4 x))
	   (strcat "@" "0," xs)
	   (list 0 (* 5 x))
	   (list 0 (* 4 x))
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list  y (* 5 x))
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.01)
  (command "._PLINE" ;;4
	   (list y (* 6 x))
	   (strcat "@" "0," xs)
	   (list 0 (* 7 x))
	   (list 0 (* 6 x))
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list  y (* 7 x))
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.01)
  (command "._PLINE" ;;5
	   (list y (* 8 x))
	   (strcat "@" "0," xs)
	   (list 0 (* 9 x))
	   (list 0 (* 8 x))
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list  y (* 9 x))
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.01)
  (command "._MOVE" "ALL" "" "DISPLACEMENT" "-0.15,0")
  (addAtt "LeftUp" "LeftUp" "Svart/Hvit" "-0.9,2.5" 0.9 90 "iso" "MC" 16)
  (addAtt "RightUp" "RightUp" "Svart/Hvit" "0.9,2.5" 0.9 -90 "iso" "MC" 16)
  (newBlock blockName)
  description
)

(defun 64B (/ blockName description  x y xs ys textpt)
  (setq	blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-64B-SEKSJONERINGSSTOLPE"
		description "Signal 64B Seksjoneringsstolpe"
	x 1.25
	y 0.3
	xs (rtos x 1 2)
	ys (rtos y 1 2)
	textpt (list 0.2 0.5)
  )
  (command "._PLINE" ;;1
	   (list 0 x)
	   (list 0 0)
	   (list y 0)
	   (list y x)
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list y x)
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.01)
  (command "._PLINE" ;;2
	   (list y (* 2 x))
	   (strcat "@" "0," xs)
	   (list 0 (* 3 x))
	   (list 0 (* 2 x))
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list y (* 3 x))
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.01)
  (command "._MOVE" "ALL" "" "DISPLACEMENT" "-0.15,0")
  (addAtt "LeftUp" "LeftUp" "Rød/Hvit" "-0.9,2.5" 0.9 90 "iso" "MC" 16)
  (addAtt "RightUp" "RightUp" "Rød/Hvit" "0.9,2.5" 0.9 -90 "iso" "MC" 16)
  (newBlock blockName)
  description
)

(defun 64C (/ blockName description  x y xs ys textpt)
  (setq	blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-64C-RASVARSLINGSSTOLPE"
	description "Signal 64C Rasvarslingsstolpe";feilmerket i T_Stema_5102495_sy...TRV_500
	x 0.5
	y 0.3
	xs (rtos x 1 2)
	ys (rtos y 1 2)
	textpt (list 0.2 0.5)
  )
  ;Gul
  (command "._PLINE" ;;1
	   (list 0 x)
	   (list 0 0)
	   (list y 0)
	   (list y x)
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list y x)
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.075)
  ;Gul
  (command "._PLINE" ;;2
	   (list y (* 2 x))
	   (strcat "@" "0," xs)
	   (list 0 (* 3 x))
	   (list 0 (* 2 x))
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list y (* 3 x))
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.075)
  (command "._PLINE" ;;3
	   (list y (* 4 x))
	   (strcat "@" "0," xs)
	   (list 0 (* 5 x))
	   (list 0 (* 4 x))
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list  y (* 5 x))
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.09)
  (command "._PLINE" ;;4
	   (list y (* 6 x))
	   (strcat "@" "0," xs)
	   (list 0 (* 7 x))
	   (list 0 (* 6 x))
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list  y (* 7 x))
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.09)
  (command "._PLINE" ;;5
	   (list y (* 8 x))
	   (strcat "@" "0," xs)
	   (list 0 (* 9 x))
	   (list 0 (* 8 x))
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list  y (* 9 x))
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.09)
  (command "._MOVE" "ALL" "" "DISPLACEMENT" "-0.15,0")
  (addAtt "LeftUp" "LeftUp" "Gul/Hvit" "-0.9,2.5" 0.9 90 "iso" "MC" 16)
  (addAtt "RightUp" "RightUp" "Gul/Hvit" "0.9,2.5" 0.9 -90 "iso" "MC" 16)
  (newBlock blockName)
  description
)

(defun 64D (/ blockName description  x y xs ys textpt)
  (setq	blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-64D-BREMSESTOLPE"
	description "Signal 64D Bremsestolpe"
	x 1.25
	y 0.3
	xs (rtos x 1 2)
	ys (rtos y 1 2)
	textpt (list 0.2 0.5)
  )
  ;svart
  (command "._PLINE"
	   (list 0 0)
	   (strcat "@" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@-" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.01)
  ;gul
  (command "._PLINE"
	   (list 0 x)
	   (strcat "@" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@-" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.11)
  ;svart
  (command "._PLINE"
	   (list 0 (* 2 x))
	   (strcat "@" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@-" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.01)
  ;gul

  (command "._PLINE"
	   (list 0 (* 3 x))
	   (strcat "@" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@-" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.1)
  (command "._MOVE" "ALL" "" "DISPLACEMENT" "-0.15,0")
  (addAtt "LeftUp" "LeftUp" "Gul/Svart" "-0.9,2.5" 0.9 90 "iso" "MC" 16)
  (addAtt "RightUp" "RightUp" "Gul/Svart" "0.9,2.5" 0.9 -90 "iso" "MC" 16)
  (newBlock blockName)  
  description
)

(defun 64E (/ blockName description  x y xs ys textpt)
  (setq	blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-64E-TEKNISKSTOLPE"
		description "Signal 64E Teknisk stolpe"
	x 0.5
	y 0.3
	xs (rtos x 1 2)
	ys (rtos y 1 2)
	textpt (list 0.2 0.5)
  )
  (command "._PLINE" ;;1
	   (list 0 x)
	   (list 0 0)
	   (list y 0)
	   (list y x)
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list y x)
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.03)
  (command "._PLINE" ;;2
	   (list y (* 2 x))
	   (strcat "@" "0," xs)
	   (list 0 (* 3 x))
	   (list 0 (* 2 x))
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list y (* 3 x))
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.03)
  (command "._PLINE" ;;3
	   (list y (* 4 x))
	   (strcat "@" "0," xs)
	   (list 0 (* 5 x))
	   (list 0 (* 4 x))
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list  y (* 5 x))
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.03)
  (command "._PLINE" ;;4
	   (list y (* 6 x))
	   (strcat "@" "0," xs)
	   (list 0 (* 7 x))
	   (list 0 (* 6 x))
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list  y (* 7 x))
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.03)
  (command "._PLINE" ;;5
	   (list y (* 8 x))
	   (strcat "@" "0," xs)
	   (list 0 (* 9 x))
	   (list 0 (* 8 x))
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list  y (* 9 x))
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.03)
  (command "._MOVE" "ALL" "" "DISPLACEMENT" "-0.15,0")
  (addAtt "LeftUp" "LeftUp" "Blå/Hvit" "-0.9,2.5" 0.9 90 "iso" "MC" 16)
  (addAtt "RightUp" "RightUp" "Blå/Hvit" "0.9,2.5" 0.9 -90 "iso" "MC" 16)
  (newBlock blockName)
  description
)

(defun 64F (/ blockName description  x y xs ys textpt)
  (setq	blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-64F-DVERGSIGNALSTOLPE"
		description "Signal 64F Dvergsignalstolpe"
	x 0.25
	y 0.3
	xs (rtos x 1 2)
	ys (rtos y 1 2)
	textpt (list 0.2 0.5)
  )

  (command "._PLINE" ;;1
	   (list y 0)
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
	   )
  (drawHatch 0.01)
  (command "._PLINE" ;;2
	   (list y (* 1 x))
	   (strcat "@" "0," xs)
	   (list 0 (* 2 x))
	   (list 0 (* 1 x))
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list y (* 2 x))
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.01)
  (command "._PLINE" ;;3
	   (list y (* 3 x))
	   (strcat "@" "0," xs)
	   (list 0 (* 4 x))
	   (list 0 (* 3 x))
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE"
	   (list  y (* 4 x))
	   (strcat "@-" ys ",0")
	   (strcat "@" "0," xs)
	   (strcat "@" ys ",0")
	   "ClOSE"
  )
  (drawHatch 0.01)
  (command "._MOVE" "ALL" "" "DISPLACEMENT" "-0.15,0")
  (addAtt "LeftUp" "LeftUp" "Svart/Hvit" "-0.9,0.625" 0.9 90 "iso" "MC" 16)
  (addAtt "RightUp" "RightUp" "Svart/Hvit" "0.9,0.625" 0.9 -90 "iso" "MC" 16)
  (newBlock blockName)
  description
)