(defun Skilt_Underskilt_meterangivelse (/ blockName description dim1 dim2)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-UNDERSKILT-METERANGIVELSE"
		description "Skilt Underskilt meterangivelse"
	dim1 1.5
	dim2 3.0
	)
  (command "._RECTANG" (list (- (/ dim2 2)) 0) (list (/ dim2 2) dim1)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (addAtt "AVSTAND" "Avstand (m):" "300m" "0,0.75" 0.9000 0 "iso" "MC" 16)
  (newBlock blockName)
  description
)