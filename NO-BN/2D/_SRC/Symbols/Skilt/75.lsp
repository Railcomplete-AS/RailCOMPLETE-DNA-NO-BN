(defun 75A-1 (/ blockName description dim1 dim2)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-75A-1-KILOMETER-NY"
		description "Signal 75A-1 Kilometerskilt ny type"
	dim1 3.0
	dim2 2.25
	)
  (command "._RECTANG" (list (- (/ dim2 2)) 0) (list (/ dim2 2) dim1)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (addAtt "HEL_KM" "Hel km" "454" "0,2.2500" 0.9000 0 "iso" "MC" 16)
  (addAtt "HALV_KM" "Halv km (0 eller 5)" "5" "0,0.7500" 0.9000 0 "iso" "MC" 16)
  (newBlock blockName)
  description
)

(defun 75A-2 (/ blockName description dim1 dim2)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-75A-2-KILOMETER-GAMMEL"
		description "Signal 75A-2 Kilometerskilt gammel type"
	dim1 1.5
	dim2 2.4375
	)
  (command "._RECTANG" (list (- (/ dim2 2)) 0) (list (/ dim2 2) dim1)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (addAtt "HEL_KM" "Hel km" "454" "0,0.75" 0.9000 0 "iso" "MC" 16)
  (newBlock blockName)
  description
)

(defun 75B (/ blockName description x y1 y2)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-75B-KILOMETER-TUNNEL"
		description "Signal 75B Kilometerskilt for tunneler"
	x 1.25
	y1 1.50
	y2 4.25
	)
  (command "._RECTANG" (list (- (/ x 2.0)) 0) (list (/ x 2.0) (+ y1 y2))
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	"._LINE" (list (- (/ x 2.0)) y1) (list (/ x 2.0) y1) ""
  )
  (addAtt "HUNDRE_KM" "100 km" "4" (list 0 (+ y1 (* 5.0 (/ y2 6.0)))) 0.9000 0 "iso" "MC" 16)
  (addAtt "TI_KM" "10 km" "5" (list 0 (+ y1 (* 3.0 (/ y2 6.0)))) 0.9000 0 "iso" "MC" 16)
  (addAtt "EN_KM" "1 km" "4" (list 0 (+ y1 (/ y2 6.0))) 0.9000 0 "iso" "MC" 16)
  (addAtt "HALV_KM" "0 eller 5 (hundre meter)" "0" (list 0 (/ y1 2.0)) 0.9000 0 "iso" "MC" 16)
  (newBlock blockName)
  description
)

(defun 75C-1 (/ blockName description)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-VEDLIKEHOLD-SIGNAL-75C-1-HEV-HSIDE"
		description "Signal 75C-1 Hev sporrenser høyremontert")
  (draw75C-2)
  (addText "Hev" "1.125,0.375" 0.5 0.0 "ISO" "Middle")
  (command "._MIRROR" "ALL" "" "0,0" "0,1" "YES")
  (newBlock blockName)
  description
)
(defun 75C-2 ()
  (setq blockName "NO-BN-2D-JBTSI-SKILT-VEDLIKEHOLD-SIGNAL-75C-2-HEV-VSIDE"
		description "Signal 75C-2 Hev sporrenser venstremontert")
  (draw75C-2)
  (addText "Hev" "1.125,0.375" 0.5 0.0 "ISO" "Middle")
  (newBlock blockName)
  description
)


(defun 75D-1 (/ blockName description)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-VEDLIKEHOLD-SIGNAL-75D-1-SENK-HSIDE"
		description "Signal 75D-1 Senk sporrenser høyremontert")
  (draw75C-2)
  (command "._ROTATE" "ALL" "" "0,0" "180"
	   "._MOVE" "ALL" "" "DISPLACEMENT"
	   (list 0 0.75)
	   )
  (addText "Senk" "-1.125,0.375" 0.5 0.0 "ISO" "Middle")
  (newBlock blockName)
  description
)
(defun 75D-2 (/ blockName description)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-VEDLIKEHOLD-SIGNAL-75D-2-SENK-VSIDE"
		description "Signal 75D-2 Senk sporrenser venstremontert")
  (draw75C-2)
  (addText "Senk" "1.125,0.375" 0.5 0.0 "ISO" "Middle")
  (command "._MIRROR" "ALL" "" "0,0" "1,0" "YES"
	   "._MOVE" "ALL" "" "DISPLACEMENT"
	   (list 0 0.75)
	   )
  (newBlock blockName)
  description
)

(defun draw75C-2 (/ x y1 y2 l)
  (setq x  2.25
	y1 0.75
	y2  "0.5"
	)
  (command "._PLINE"
	   (list 0 0)
	   (list x 0)
	   (list x y1)
	   (strcat "@-" y2 "," y2)
	   (strcat "@-" y2 ",-" y2)
	   (list 0 y1)
	   "CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
)