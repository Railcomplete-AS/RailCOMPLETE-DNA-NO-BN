(defun Skilt_Sidespor (/ blockname description x y)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIDESPOR"
	description "Skilt Sidespor"
	x 8.0
	y 4.5
	)
	(command "._RECTANG" (list (- (/ x 2)) 0) (list (/ x 2) y)
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" "")
	(addAtt "SSP_NAVN" "Sidespor navn (store bokstaver):" "RIEBER OG SØNN" "0,3.75" 0.7000 0 "iso" "MC" 16)
	(addText "sidespor" "0,3.0" 0.5000 0 "iso" "MC")
	(addAtt "SSP_KM" "Kilometer (xx,xxx):" "Km, 132,72" "0,2.25" 0.7000 0 "iso" "MC" 16)
	(addText "mellom stasjonene" "0,1.5" 0.5000 0 "iso" "MC")
	(addAtt "SSP_STASJONER" "Mellom stasjon <xxx> og <yyy>" "Berg og Halden" "0,0.75" 0.7000 0 "iso" "MC" 16)
	(newBlock blockName)
	description
)