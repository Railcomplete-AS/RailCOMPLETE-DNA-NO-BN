;
; Skilt_1000V_togvarmeanlegg.lsp
;
(defun Skilt_1000V_togvarmeanlegg (/ blockName description x y)
	(setq
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-1000V"
		description "Skilt 1000V togvarmeanlegg"
		x 4.0
		y 1.5
	)
	(command
		"._RECTANG" (list (- (/ x 2)) 0) (list (/ x 2) y)
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
	)
	(addText "1000 V" "0,0.75" 0.90 0 "iso" "MC")
	(newBlock blockName)
	description
)

