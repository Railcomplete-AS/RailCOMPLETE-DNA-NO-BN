;
; Skilt_StoppSeOgLytt.lsp
;
(defun Skilt_StoppSeOgLytt ( / blockName description x y
	)
  (setq 
	blockName "NO-BN-2D-JBTSI-SKILT-TREDJEPERSON-STOPPSEOGLYTT"
		description "Skilt Stopp se og lytt etter tog"
		x 4.0
		y 1.9
	)
	(command
		"._RECTANG" "0,0" (list x y)
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
	)
	(addText "STOPP" "2,1.2356" 0.90 0 "iso" "MC")
	(addText "Se og lytt etter tog" "0.2643,0.3" 0.30 0 "iso" "Left")
	(command "._MOVE" "ALL" "" "Displacement" (list (- (/ x 2)) 8))
	(command "._LINE" "0,0" "0,8" "")
	(newBlock blockName)
	description
)