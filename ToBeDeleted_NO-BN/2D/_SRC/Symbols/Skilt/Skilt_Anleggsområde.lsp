(defun Skilt_Anleggsomraade_begynner (/ blockName description x y)
	(setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-ANLEGGSOMRAADE-BEGYNNER"
		description "Skilt Anleggsområde begynner"
		x 6.0
		y 3.0
	)
	(command
		"._RECTANG" (list (- (/ x 2)) 0) (list (/ x 2) y)
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
	)
	(addText "Anleggs-" "0,2.25" 0.90 0 "iso" "MC")
	(addText "område"   "0,1.00" 0.90 0 "iso" "MC")
	(newBlock blockName)
	description
)

(defun Skilt_Anleggsomraade_slutter (/ blockName description x y pt1 pt2 offset ang yoff xoff)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-ANLEGGSOMRAADE-SLUTTER"
		description "Skilt Anleggsområde slutter"
		x 6.0
		y 3.0
		pt1 (list (- (/ x 2)) 0.0)
		pt2 (list (/ x 2) y)
		offset 0.25
		ang (atan y x)
		yoff (/ offset (sin ang))
		xoff (* yoff (tan ang))
	)
	(command "._RECTANG" pt1 pt2
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
		"._LINE" pt1 pt2 ""
		"._LINE"
		(polar pt1 0 yoff)
		(polar pt2 (D->R 270) xoff) ""
		"._LINE"
		(polar pt1 (D->R 90) xoff)
		(polar pt2 (D->R 180) yoff) ""
	)
	(addText "Anleggs-" "0,2.25" 0.90 0 "iso" "MC")
	(addText "område"   "0,1.00" 0.90 0 "iso" "MC")
	(newBlock blockName)
	description
)