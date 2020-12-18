;
; 70.lsp
;
(defun 70A (/ blockName description y1 y2 offset)
	(setq 
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-70-PLANOVERGANGSSKILT"
		description "Signal 70 Planovergang"
		y1 1.50
		y2 2.250
	)
	(command 
		"._RECTANG"
	   (list (/ y1 (- 2)) 0)
	   (list (/ y1 2) y2)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	)
	(addText "V" "0,1.1250" 1.750 0 "iso" "MC")
	(newBlock blockName)
	description
)