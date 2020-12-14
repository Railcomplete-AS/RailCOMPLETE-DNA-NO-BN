(defun 101-1 (/ blockName description x y)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-101-1-ID-3-LINJER"
		description "Signal 101-1 Identifikasjonsskilt 3 linjer"
	x 8.0
	y 4.5
	)
  (command "._RECTANG" (list (- (/ x 2)) 0) (list (/ x 2) y)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  ;top
  (addAtt "ID_BOKSTAV" "ID-skilt øvre linje (bokstav)" "M" "0,3.7500" 1.25 0 "iso" "MC" 16)
  (addAtt "ID_NUMMER" "ID-skilt midtre linje (signalnummer)" "5894" "0,2.2500" 1.25 0 "iso" "MC" 16)
  (addAtt "ID_STED" "ID-skilt nedre line (stedsforkortelse)" "XYZ" "0,0.750" 1.25 0 "iso" "MC" 16)
  (newBlock blockName)
  description
)

(defun 101-2 (/ blockName description x y)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-101-2-ID-2-LINJER"
		description "Signal 101-2 Identifikasjonsskilt 2 linjer"
	x 8.0
	y 3.0
	)
  (command "._RECTANG" (list (- (/ x 2)) 0) (list (/ x 2) y)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  ;top
  (addAtt "ID_BOKSTAV_OG_NR" "ID-skilt øvre linje (Evt. bokstav, signalnummer)" "M 5894" "0,2.25" 1.25 0 "iso" "MC" 16)
  (addAtt "ID_STED" "ID-sklt nedre line stedsforkortelse)" "XYZ" "0,0.750" 1.25 0 "iso" "MC" 16)
  (newBlock blockName)
  description
)

(defun 101-3 (/ blockName description radius)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-101-3-ID-RUNDT"
		description "Signal 101-3 Identifikasjonsskilt rundt"
	radius 3.0
	)
  (command "._CIRCLE" (list 0 radius) radius
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._POLYGON" "120" (list 0 radius) "I" radius
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  
  ;top
  (addAtt "ID_BOKSTAV" "ID-skilt øvre linje (bokstav)" 	"M" "0,5.1000" 1.25 0 "iso" "MC" 16)
  (addAtt "ID_NUMMER" "ID-skilt midtre linje (signalnummer)"  "5894" "0,3.3000" 1.25 0 "iso" "MC" 16)
  (addAtt "ID_STED" "ID-sklt nedre line stedsforkortelse)" 	"XYZ" "0,1.50" 1.25 0 "iso" "MC" 16)
  (newBlock blockName)
  description
)

(defun 101-4-NY (/ blockName description x y)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-101-6-ID-MINDRE-SIGNAL-300x100"
		description "Signal 101-6 Identifikasjonsskilt mindre signal stort"
		x 3.0
		y 1.0)
  (command "._RECTANGLE" (list 0 0) (list x y)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  
  ;top
  (addAtt "ID_MINDRE_SIGNAL" "ID-skilt mindre signal signalnummer" "R1234" (list (/ x 2.0) (/ y 2.0)) 0.75 0 "iso" "MC" 16)
  (command "._MOVE" "ALL" "" "Displacement" (list (- (/ x 2)) 0))
  (newBlock blockName)
  description
)

(defun 101-4-GAMMEL (/ blockName description x y)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-101-5-ID-MINDRE-SIGNAL-180x100"
		description "Signal 101-5 Identifikasjonsskilt mindre signal lte"
		x (/ 9.0 5.0)
		y 1.0)
  (command "._RECTANGLE" (list 0 0) (list x y)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  
  ;top
  (addAtt "ID_MINDRE_SIGNAL" "ID-skilt mindre signal signalnummer" "R12" (list (/ x 2.0) (/ y 2.0)) 0.75 0 "iso" "MC" 16)
  (command "._MOVE" "ALL" "" "Displacement" (list (- (/ x 2.0)) 0))
  (newBlock blockName)
  description
)

(defun 101-7 (/ blockName description y1 y2 offset)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-101-7-ID-MINDRE-SIGNAL-200x300"
		description "Signal W Planovergang"
	y1 1.0
	y2 1.50
	)
  (command "._RECTANG"
	   (list (/ y1 (- 2)) 0)
	   (list (/ y1 2) y2)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (addAtt "ID_MINDRE_SIGNAL" "ID-skilt mindre signal signalnummer" "W" (list 0 (/ y2 2)) 0.7 0 "iso" "MC" 16)
  (newBlock blockName)
  description
)

(defun 101-8 (/ blockName description y1 y2 offset)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-101-8-ID-MINDRE-SIGNAL-300x200"
		description "Signal Z Skiftesignal"
	y1 1.50
	y2 1.0
	)
  (command "._RECTANG"
	   (list (/ y1 (- 2)) 0)
	   (list (/ y1 2) y2)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (addAtt "ID_MINDRE_SIGNAL" "ID-skilt mindre signal signalnummer" "Z" (list 0 (/ y2 2)) 0.7 0 "iso" "MC" 16)
  (newBlock blockName)
  description
)

(defun 101-9 (/ blockName description y1 y2 offset)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-101-9-ID-ERTMS-400x200"
		description "2-linjer ID ERTMS"
	y1 8.0
	y2 3.0
	)
  (command "._RECTANG"
	   (list (/ y1 (- 2)) 0)
	   (list (/ y1 2) y2)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (addAtt "ID_BOKSTAV_OG_NR" "ID-skilt øvre linje (Evt. bokstav, signalnummer)" "M 5894" (list 0 (/ (* 3 y2) 4)) 1.25 0 "iso" "MC" 16)
  (addAtt "ID_STED" "ID-skilt nedre line (stedsforkortelse)" "XYZ" (list 0 (/ y2 4)) 1.25 0 "iso" "MC" 16)
  (newBlock blockName)
  description
)

(defun 101-10 (/ blockName description y1 y2 offset)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-101-10-ID-ERTMS-700x350"
		description "2-linjer ID ERTMS"
	y1 8.0
	y2 4.5
	)
  (command "._RECTANG"
	   (list (/ y1 (- 2)) 0)
	   (list (/ y1 2) y2)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (addAtt "ID_BOKSTAV" "ID-skilt øvre linje (bokstav)" "M" (list 0 (/ (* 5 y2) 6)) 1.25 0 "iso" "MC" 16)
  (addAtt "ID_NUMMER" "ID-skilt midtre linje (signalnummer)" "5894" (list 0 (/ (* 3 y2) 6)) 1.25 0 "iso" "MC" 16)
  (addAtt "ID_STED" "ID-skilt nedre line (stedsforkortelse)" "XYZ" (list 0 (/ y2 6)) 1.25 0 "iso" "MC" 16)
  (newBlock blockName)
  description
)