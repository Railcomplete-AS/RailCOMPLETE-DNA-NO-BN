(defun 74-1 (/ blockname description dim1 dim2)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-74-1-TOGLENGDE1"
		description "Signal 74-1 Toglengdeskilt Alt 1"
	dim1 1.5
	dim2 3.0
	)
  (command "._RECTANG" (list (- (/ dim2 2)) 0) (list (/ dim2 2) dim1)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (addAtt "TOGLENGDE" "Toglengde" "220m" "0,0.75" 0.9000 0 "iso" "MC" 16)
  (newBlock blockName)
  description
)

(defun 74-2 (/ blockName description dim1 dim2)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-74-2-TOGLENGDE2"
		description "Signal 74-2 Toglengdeskilt Alt 2"
	dim1 5.0
	dim2 0.75
	)
  (command "._RECTANG" (list (- (/ dim2 2)) 0) (list (/ dim2 2) dim1)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (addAtt "HUNDRE_M" "100 m" "2" "0,4.3750" 0.9000 0 "iso" "MC" 16)
  (addAtt "TI_M" 	"10 m" "2" "0,3.1250" 0.9000 0 "iso" "MC" 16)
  (addAtt "EN_M" 	 "1 m" "0" "0,1.8750" 0.9000 0 "iso" "MC" 16)
  (addText		       "m" "0,0.6250" 0.9000 0 "iso" "MC")  
  (newBlock blockName)
  description
)
