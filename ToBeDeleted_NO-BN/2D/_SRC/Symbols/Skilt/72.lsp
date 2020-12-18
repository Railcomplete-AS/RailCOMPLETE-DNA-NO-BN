;
; 72.lsp
;
(defun 72A (/ blockName description y1 y2 offset)
	(setq 
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-72A-STARTFJS"
		description "Signal 72A FJS begynner"
		y1 2.250
		y2 1.50
	)
	(command 
		"._RECTANG"
	   (list (/ y1 (- 2)) 0)
	   (list (/ y1 2) y2)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	)
	(addText  "FJS" "0,0.7500" 0.9 0 "iso" "MC")
	(newBlock blockName)
	description
)



(defun 72B (/ blockName description dim1 dim2 pt1 pt2 offset ang xoff yoff)
	(setq
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-72B-SLUTTFJS"
		description "Signal 72B FJS slutter"
    	dim1 1.50
		dim2 2.250
		pt1 (list (- (/ dim2 2)) 0.0)
		pt2 (list (/ dim2 2) dim1)
		offset 0.25
		ang (atan dim1 dim2)
		xoff (/ offset (sin ang))
		yoff (* xoff (tan ang))
	)
	(command 
		"._RECTANG" pt1 pt2
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._LINE" pt1 pt2 ""
	   "._LINE"
	   (polar pt1 0 xoff)
	   (polar pt2 (D->R 270) yoff) ""
	   "._LINE"
	   (polar pt1 (D->R 90) yoff)
	   (polar pt2 (D->R 180) xoff) ""
	)
	(addText "FJS" (list 0 (/ dim1 2)) 0.90 0.0 "iso" "MC")
	(newBlock blockName)
	description
)