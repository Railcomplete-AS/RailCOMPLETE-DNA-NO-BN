(defun Skilt_Spornummer (/ blockName description x y)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SPORNUMMER"
		description "Skilt Spornummer"
	x 3.0
	y 2.25
	)
  (command "._RECTANG" (list (- (/ x 2)) 0) (list (/ x 2) y)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")

  (addAtt "SPORNUMMER" "Spornummer" "38" "0,1.125" 1.25 0 "iso" "MC" 16)
  (newBlock blockName)
  description
)