(defun 69A (/ blockName description y1 y2 offset)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-69A-MIDLERTIDIG-NEDSATT"
	description "Signal 69A Midlertidig nedsatt hastighet"
	y1 0.750
	y2 2.250
	offset 0.1414
	)
  (draw69A y1 y2 offset)
  (addAtt "HAST_10" "Vent-hastighet (10-ere)" 4 ;endret prompt ift TRV_500
	  "0,1.8957" 1.25 0.0 "iso" "MC" 16)
  (newBlock blockName)
  description
)

(defun draw69A (y1 y2 offset)
  (command "._PLINE"
	   "0,0"
	   (list y1 y1)
	   (strcat "@0," (rtos y2 2 4))
	   ""
	   "._OFFSET" offset "0,0" "0,1" ""
	   "._BREAK" "L"
	   (list 0 (/ offset (cos (D->R 45))))
	   (list (- (/ offset (cos (D->R 45)))) 0)
	   "._MIRROR" "ALL" "" "0,0" "0,1" "N"
	   "LINE"
	   (list y1 (+ y2 y1))
	   (list (- y1) (+ y2 y1))
	   ""
	   "._PLINE" "0,0.2" (list 0.6086 0.8086) (list 0.6086 3.0) (list (- 0.6086) 3.0) (list (- 0.6086) 0.8086) "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (drawHatchSelectPoint "0.01" (list 0 (/ offset 2)) 0 0.01)
)

(defun 69B (/ blockName description y1 y2 offset)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-69B-MIDLERTIDIG-OPPHOERER"
	description "Signal 69B Midlertidig hastighet opphører"
	y1 0.750
	y2 2.250
	offset 0.1414
	)
  (draw69A y1 y2 offset)
  (command "._ROTATE" "ALL" "" (list 0 (/ (+ y1 y2) 2)) 180)
  (addAtt "HAST_10" "Kjør-hastighet (10-ere)" 9 ;endret prompt ift TRV_500
	  "0,1.1043" 1.25 0.0 "iso" "MC" 16)
  (newBlock blockName)
  description
)