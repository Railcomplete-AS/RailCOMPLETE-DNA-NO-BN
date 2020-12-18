;
; 73.lsp
;
(defun 73A (/ blockName description)
	(setq
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-73-RASVARSEL"
		description "Signal 73 Rasvarslingsskilt"
	)
	(draw65G-1)
	(addText "R" "0,1.125" 1.125 0 "iso" "MC")
	(newBlock blockName)
	description
)