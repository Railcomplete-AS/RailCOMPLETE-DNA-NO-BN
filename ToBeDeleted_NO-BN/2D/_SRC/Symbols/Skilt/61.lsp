(defun 61A (/ blockName description dim1 dim2 dim3 dim4 x1 x2 pt1 pt2 pt11 pt12 pt13 pt14 pt15 pt16)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-61A-AVSTAND1"
		description "Signal 61A Avstandsskilt 1"
    	dim1 1.50 	;width
	dim2 1.50 	;height1
	dim3 (/ 5.0 6.0) 	;height2
	dim4 0.50 	;separation
	x1 (/ dim1 2)
	x2 (/ dim1 (- 2))

	pt1 (list x1 0)
	pt2 (list x2 0)

	pt11 (list x2 (+ dim2 (* 4 dim4)))
	pt12 (list x1 (+ dim3 (* 4 dim4)))

	pt13 (list x1 (+ dim3 (* 5 dim4)))
	pt14 (list x2 (+ dim2 (* 5 dim4)))
	pt15 (list x2 (+ dim2 dim4 (* 5 dim4)))
	pt16 (list x1 (+ dim3 dim4 (/ 2.0 3) (* 5 dim4)))
  )
  (command "._PLINE" pt1 pt2 pt11 pt12 "CLOSE")
  (drawHatch 0.01)
  (command "._PLINE" pt13 pt14 pt15 pt16 pt13 "")
  (command "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (drawHatch 0.01)
  (command "._PLINE" pt12 pt13 pt14 pt11 "CLOSE")
  (command "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (newBlock blockName)
  description
)

(defun 61B (/ blockName description dim1 dim2 dim3 dim4 x1 x2 pt1 pt2 pt7 pt8 pt9 pt10 pt11 pt12 pt13 pt14 pt15 pt16)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-61B-AVSTAND2"
		description "Signal 61B Avstandsskilt 2"
    	dim1 1.50 	;width
	dim2 1.50 	;height1
	dim3 (/ 5.0 6.0) 	;height2
	dim4 0.50 	;separation
	x1 (/ dim1 2)
	x2 (/ dim1 (- 2))

	pt1 (list x1 0)
	pt2 (list x2 0)

	pt7 (list x2 (+ dim2 (* 2 dim4)))
	pt8 (list x1 (+ dim3 (* 2 dim4)))

	pt9  (list x1 (+ dim3 (* 3 dim4)))
	pt10 (list x2 (+ dim2 (* 3 dim4)))
	pt11 (list x2 (+ dim2 (* 4 dim4)))
	pt12 (list x1 (+ dim3 (* 4 dim4)))

	pt13 (list x1 (+ dim3 (* 5 dim4)))
	pt14 (list x2 (+ dim2 (* 5 dim4)))
	pt15 (list x2 (+ dim2 dim4 (* 5 dim4)))
	pt16 (list x1 (+ dim3 dim4 (/ 2.0 3) (* 5 dim4)))
  )
  (command "._PLINE" pt1 pt2 pt7 pt8 pt1 "")
  (drawHatch 0.01)
  (command "._PLINE" pt9 pt10 pt11 pt12 pt9 "")
  (drawHatch 0.01)
  (command "._PLINE" pt13 pt14 pt15 pt16 pt13 "")
  (drawHatch 0.01)
  (command "._PLINE" pt8 pt9 pt10 pt7 "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE" pt12 pt13 pt14 pt11 "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")

  (newBlock blockName)
  description
)

(defun 61C (/ blockName description dim1 dim2 dim3 dim4 x1 x2 pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9 pt10 pt11 pt12 pt13 pt14 pt15 pt16)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-61C-AVSTAND3"
		description "Signal 61C Avstandsskilt 3"
    	dim1 1.50 	;width
	dim2 1.50 	;height1
	dim3 (/ 5.0 6.0) 	;height2
	dim4 0.50 	;separation
	x1 (/ dim1 2)
	x2 (/ dim1 (- 2))

	pt1 (list x1 0)
	pt2 (list x2 0)
	pt3 (list x2 dim2)
	pt4 (list x1 dim3)

	pt5 (list x1 (+ dim3 dim4))
	pt6 (list x2 (+ dim2 dim4))
	pt7 (list x2 (+ dim2 (* 2 dim4)))
	pt8 (list x1 (+ dim3 (* 2 dim4)))

	pt9  (list x1 (+ dim3 (* 3 dim4)))
	pt10 (list x2 (+ dim2 (* 3 dim4)))
	pt11 (list x2 (+ dim2 (* 4 dim4)))
	pt12 (list x1 (+ dim3 (* 4 dim4)))

	pt13 (list x1 (+ dim3 (* 5 dim4)))
	pt14 (list x2 (+ dim2 (* 5 dim4)))
	pt15 (list x2 (+ dim2 dim4 (* 5 dim4)))
	pt16 (list x1 (+ dim3 dim4 (/ 2.0 3) (* 5 dim4)))
  )
  (command "._PLINE" pt1 pt2 pt3 pt4 pt1 "")
  (drawHatch 0.01)
  (command "._PLINE" pt5 pt6 pt7 pt8 pt5 "")
  (drawHatch 0.01)
  (command "._PLINE" pt9 pt10 pt11 pt12 pt9 "")
  (drawHatch 0.01)
  (command "._PLINE" pt13 pt14 pt15 pt16 pt13 "")
  (drawHatch 0.01)
  (command "._PLINE" pt4 pt5 pt6 pt3 "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE" pt8 pt9 pt10 pt7 "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._PLINE" pt12 pt13 pt14 pt11 "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   )
  (newBlock blockName)
  description
)