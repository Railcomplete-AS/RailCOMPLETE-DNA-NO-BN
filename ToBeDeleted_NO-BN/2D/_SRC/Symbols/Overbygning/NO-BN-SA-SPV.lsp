;
; NO-BN-SA-SPV.lsp
;
; Bane NOR, Signal, sporvekselsymboler.
; Skravert sporvekselsymbol betyr ulike varianter av kontroll og styring. Se TRV.
; Merk: Det er innført en fylt ring i teoretisk kryss - den betyr "motorisert omlegging"
; som et alternativ til håndstilt med muskelkraft (for en ikke sentralstilt sporveksel).
;
(defun NO-BN-SA-SPV (quadrant Drawing_Number 
	/	blockName
		switchParameters
		crossType A B R x railProfile ang radius 
		str i hatchVariant
	)
	(setq
		switchParameters	(getSwitchParameters Drawing_Number)
		crossType	(cadr (assoc "Skinnekryss" switchParameters)) ; Movable point frog or fixed
		A			(/ (cadr (assoc "A" switchParameters)) 1000.0)
		B			(/ (cadr (assoc "B" switchParameters)) 1000.0)
		R 			(cadr (assoc "R" switchParameters))
		x			(cadr (assoc "x" switchParameters))
		railProfile	(cadr (assoc "Skinneprofil" switchParameters))
		ang			(R->D (atan (/ 1.0 x))) ; sporvekselsymbol
		radius		0.75
	)
	(setq str "")
	(if (< x 10)
		(setq str "0")
	)
	(setq i 0)
	(repeat 3
		(cond 
			((= i 0) (setq hatchVariant "FYLT")) ; signfies central control, interlocked switch
			((= i 1) (setq hatchVariant "LETT")) ; signifies key-locked control in right / left / both position(s)
			((= i 2) (setq hatchVariant "TOM")) ; signifies that the point's tongues position is neither controlled or detected
		)
		(setq blockName (strcat "NO-BN-2D-JBTSI-CONNECTION-SWITCH-" str (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" crossType "-" hatchVariant "-" (rtos quadrant 2 0)))
		;Ring i teoretisk kryss:
		(command
			"._LAYER" "SET" "0" ""
			"._COLOR" "ByBlock"
			"._LINE" "0,0" (list (- A radius) 0) ""
			"._CIRCLE" (list A 0) radius
		)
		;Sporvekselsymbol - signalfaget:
		(command 
			"._PLINE"
				(list (+ A radius) 0)
				"Arc" "CE" (list A 0) "Angle" ang
				"Line" (strcat "@" (rtos (- B radius)) "<" (rtos ang))
				"Arc" "CE" (list A 0) "Angle" (- ang)
				"Line"
				"Close"
		)
		(cond ; Careful! Do not set comment on a single line here, this would deselect the previous drawing entity, on which the next drawHatch command relies!
			((= hatchVariant "FYLT") (drawHatch 0.06)) ;Skravering av sporvekselsymbol - fylt = detektert i drivmaskin eller sensor
			((= hatchVariant "LETT") (drawHatch 0.3)) ;Skravering av sporvekselsymbol - lett = detektert indirekte med nøkkel i kontrollås 
		)   
		; Wipeout under sirkelen som markerer teoretisk kryss (vi skal kunne skjule sporlinjen som ligger under):
		(drawWipedOutCircle Drawing_Number) 
		;Mirror 0x, 1x eller 2x til korrekt kvadrant:
		(mirrorSelection quadrant "ALL") ; TODO 2019-07-26 CLFEY - Hva benyttes 'rev' til? Koden var slik: (setq rev (mirrorSelection quadrant "ALL"))
		(newBlock blockName)
		blockName
		(setq i (+ 1 i))
	);repeat
	(command
		"._LAYER" "Set" "0" ""
		"._COLOR" "ByBlock"
	)
)



; Wipeout under the circle at theoretical crossing in switch - a legacy way of drawing Bane NOR switch symbols for signaling
(defun drawWipedOutCircle (Drawing_Number / switchParameters A radius)
	(setq
		switchParameters (getSwitchParameters Drawing_Number)
		A		  (/ (cadr (assoc "A" switchParameters)) 1000.0)
		radius 	  0.75
	)
	(command
		"._LAYER" "Set" "JBTFE__SPV_WIPEOUT" ""
		"._COLOR" "ByBlock"
		"._POLYGON" 16 (list A 0) "Inscribed" radius
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._DRAWORDER" "L" "" "Above" "ALL" ""
		"._LAYER" "Set" "0" ""
		"._COLOR" "ByLayer"
	)
)



(defun NO-BN-SA-SPV-KONTROLL (/ blockName sides side i)
	; Kontrollåst pos H/V - vis firkant for låsesymbolet på H side, V side eller begge sider
	(setq sides (list "V" "H" "HV"))
	(setq i 0)
	(repeat 3
		(setq side (nth i sides))
		(setq blockName (strcat "NO-BN-2D-JBTSI-CONNECTION-SWITCH-KONTROLL-" side))
		(command
			"._LAYER" "Set" "0" ""
			"._COLOR" "ByBlock"
		)
		(if (or (= side "V") (= side "HV"))
			(progn
				(command "._RECTANG" (list 1.85 -0.75) (list 4.85 -1.75))
			)
		)
		(if (or (= side "H") (= side "HV"))
			(progn
			(command "._RECTANG" (list 1.85 0.75) (list 4.85 1.75))
			)
		)
		(newBlock blockName)
		(setq i (+ 1 i))
	)
	(command
		"._LAYER" "Set" "0" ""
		"._COLOR" "ByBlock"
	)
	blockName
)

