(defun Skilt_Bevegelig_kryss (/ blockName description x y)
	(setq blockName "NO-BN-2D-JBTSI-SKILT-VEDLIKEHOLD-BEVEGELIG-KRYSS"
			description "Skilt Bevegelig Kryss"
		x 6.0
		y 3.0
		offset 0.5
		ang (atan y x)
		xOffset (/ offset (sin ang))
		yOffset (* xOffset (tan ang))
	)
	(command  
		"._RECTANGLE" (list (- (/ x 2)) 0) (list (/ x 2) y)
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
		"._LINE"
			(list (- (/ xOffset 2) (/ x 2)) 0)
			(list 0 (- (/ y 2) (/ yOffset 2)))
			""
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
		"._MIRROR" "L" "" (list 0 (/ y 2)) (list 1 (/ y 2)) "NO"
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
		"._LINE" 
			(list (/ x (- 2)) (/ yOffset 2))
			(list (/ xOffset (- 2)) (/ y 2)) 
			""
	   "._MIRROR" "L" "" "0,0" "0,1" "NO"
	   "._MIRROR" "L" "" (list 0 (/ y 2)) (list 1 (/ y 2)) "NO"
  	   "._MIRROR" "L" "" "0,0" "0,1" "NO"
	   )
	(addText "Bevegelig" "0,2.2500" 0.90 0 "iso" "MC")
	(addText "kryss" "0,1.0" 0.90 0 "iso" "MC")
	(newBlock blockName)
	description
)