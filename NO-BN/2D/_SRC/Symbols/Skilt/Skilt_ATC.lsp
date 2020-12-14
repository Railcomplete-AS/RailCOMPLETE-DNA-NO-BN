;
; Skilt_ATC.lsp
;
(defun Skilt_ATC_utkoplet (/ blockName description x y)
	(setq 
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-ATC-UTKOPLET"
		description "Skilt ATC utkoplet"
		x 3.0
		y 2.25
	)
	(command 
		"._RECTANG" (list (- (/ x 2)) 0) (list (/ x 2) y)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	  )
	(addText "ATC" "0,1.5" 0.9000 0 "iso" "MC")
	(addText "utkoplet" "0,0.75" 0.5000 0 "iso" "MC")
	(newBlock blockName)
	description
)

(defun Skilt_ATC_innkoplet (/ blockName description x y)
	(setq
		blockName  "NO-BN-2D-JBTSI-SKILT-KJOERENDE-ATC-INNKOPLET"
		description "Skilt ATC innkoplet"
		x 3.0
		y 2.25
	)
	(command 
		"._RECTANG" (list (- (/ x 2)) 0) (list (/ x 2) y)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	)
	(addText "ATC" "0,1.5" 0.9000 0 "iso" "MC")
	(addText "innkoplet" "0,0.75" 0.5000 0 "iso" "MC")
	(newBlock blockName)
	description
)