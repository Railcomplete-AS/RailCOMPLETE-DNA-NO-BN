(defun 63A (/ blockName description  x1 x2 y)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-63A-FALLVISER"
		description "Signal 63A Fallviser"
	x1 2.25
	x2 3.125
	y 1.75)
  (command "._PLINE"
	(list 0 0)
	(list 0 y)
	(list x2 (- y x2))
	(list x1 (- x1))
	"CLOSE"
  "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (addAtt "GRADIENT" "Fall" 7 ;FALL
	  "1.5625,-0.6875" 0.90 315 "iso" "MC" 16)
  (newBlock blockName)
  description
)
(defun 63B (/ blockName description x1 x2 y)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-63B-STIGNINGSVISER"
		description "Signal 63B Stigningsviser"
	x1 2.25
	x2 3.125
	y 1.75)
   (command "._PLINE"
	(list 0 0)
	(list 0 y)
	(list x1 (+ y x1))
	(list x2 x2)
	"CLOSE"
	    "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (addAtt "GRADIENT" "Stigning" 11 ;STIGNING
	  "1.4,2.3" 0.90 44.6 "iso" "MC" 16)
  (newBlock blockName)
  description
)