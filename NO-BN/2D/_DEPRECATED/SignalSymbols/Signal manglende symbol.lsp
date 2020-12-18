;
; Signal manglende symbol.lsp
;
; 2020-08-02 CLFEY Deprecated code, is now included in main signal LISP routines.
;
(defun C:SIGNAL-MANGLENDE-SYMBOL ( / blockName pole)
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-MANGLENDE-SYMBOL"
		pole 6
	)
	(addText "MANGLER SYMBOL" (list 0 6) 0.5 0.0 "iso" "_BC") 
    (command "._ROTATE" "_ALL" "" "0,0" "-90")
    (drawLyingPole 0 pole)
    (drawLyingHsBase)
    (command "._ROTATE" "_ALL" "" "0,0" 90)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)
