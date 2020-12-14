;
; Skilt_Noedtelefon.lsp
;
(defun Skilt_Noedtelefon (/ blockName description side1 side2 diff radius)
	(setq
		blockName "NO-BN-2D-JBTSI-SKILT-TREDJEPERSON-NOEDTELEFON"
		description "Skilt Nødtelefon"
		side1 2.25
		side2 "1.65"
		diff (/ (- side1 (atof side2)) 4)
		radius (rtos diff)
	)
	(command
		"._RECTANG" (list (- (/ side1 2)) 0) (list (/ side1 2) side1)
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
		"._PLINE"
		(list (- (* 2 diff) (/ side1 2) ) diff)
		(strcat "@" side2 ",0")

		"ARC" "DIRECTION" ""
		(strcat "@" radius "," radius) ;endp arc
		"LINE"
		(strcat "@0," side2)
	   
		"ARC" "DIRECTION" ""
		(strcat "@-" radius "," radius)
		"LINE"
		(strcat "@-" side2 ",0")

		"ARC" "DIRECTION" ""
		(strcat "@-" radius ",-" radius)
		"LINE"
		(strcat "@0,-" side2)

		"ARC" "DIRECTION" ""
		(strcat "@" radius ",-" radius)
		""
	)

	(drawArc "0,0.9"    1.0 42.3681 137.6319)
	(drawArc "-0.5,1.5" 0.25 162.8086 237.5002)
	(drawArc "-0.5,1.5" 0.25 (- 354.7138 360.0) 23.5782)
	(drawArc "0.5,1.5" 0.25 156.4218 185.2862)
	(drawArc "0.5,1.5" 0.25 (- 302.4998 360.0) 17.1914)
	(command
		"._LINE"
			(list -0.2511 1.4770)
			(list -0.6343 1.2892)
			""
			"._LINE"
			(list 0.2511 1.4770)
			(list 0.6343 1.2892)
			""
			"._LINE"
			(list -0.2709 1.6000)
			(list 0.2709 1.6000)
			""
	)
	(addText "SOS" "0,0.75" 0.70 0 "iso" "MC")
	(newBlock blockName)
	description
)



(defun drawArc (center rad ang1 ang2)
	(command
		"._ARC" "C"
	   center
	   (strcat "@" (rtos rad) "<" (rtos ang1)) ;first point
	   "ANGLE" (- ang2 ang1)
	)
)

