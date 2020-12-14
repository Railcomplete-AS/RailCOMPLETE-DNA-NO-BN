;
; Signal manglende symbol.lsp
;
(defun C:SIGNAL-MANGLENDE-SYMBOL (/ blockName pole)
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-MANGLENDE-SYMBOL"
		pole 6
	)
	(addText "MANGLER SYMBOL" (list 0 6) 0.5 0.0 "iso" "BC") 
    (command "._ROTATE" "ALL" "" "0,0" -90)
    (drawPole 0 pole)
    (drawBase)
    (command "._ROTATE" "ALL" "" "0,0" 90)
	(newBlock blockName)
)