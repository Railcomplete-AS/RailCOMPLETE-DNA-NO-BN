;
; Skilt_Kjedebrudd.lsp
;
(defun Skilt_Kjedebrudd (/ blockName description x y)
	(setq 
		blockName "NO-BN-2D-JBTSI-SKILT-VEDLIKEHOLD-SIGNAL-KJEDEBRUDD"
		description "Skilt Kjedebrudd"
		x1 8.0
		y1 5.5
		x2 4.0
		y2 2.5
		y 1.5
	)
	(command
		"._RECTANG"
			(list (- (/ x1 2)) 0)
			(strcat "@" (rtos x1) "," (rtos y1))
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
		"._RECTANG"
			(list (- (/ x1 2)) y)
			(strcat "@" (rtos x2) "," (rtos y2))
		"._RECTANG"
			(list 0 y)
			(strcat "@" (rtos x2) "," (rtos y2))
	)
	(addText "KJEDEBRUDD" "0,4.75" 0.90 0 "iso" "MC")
	(addAtt "FRA_KM" "Fra km:" "15" "-2.0,3.375" 0.90 0 "iso" "MC" 16)
	(addAtt "FRA_M" "Fra meter:" "422" "-2.0,2.1250" 0.90 0 "iso" "MC" 16)
	(addAtt "TIL_KM" "Til km:" "15" "2.0,3.375" 0.90 0 "iso" "MC" 16)
	(addAtt "TIL_M" "Til meter:" "450" "2.0,2.1250" 0.90 0 "iso" "MC" 16)
	(addAtt "SPRANG" "Sprang:" "+28m" "0,0.7500" 0.90 0 "iso" "MC" 16)
	(newBlock blockName)
	description
)