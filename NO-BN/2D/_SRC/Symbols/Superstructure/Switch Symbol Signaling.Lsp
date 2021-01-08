;=========================================================================================================================
;
; Switch Symbol Signaling.Lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
; See Bane NOR, TRV, Signal, sporvekselsymboler

; Signaling discipline's graphics for inclusion in turnout symbols

;-------------------------------------------------------------------------------------------------------------------------
;
; Guide to understanding the RailCOMPLETE switch object symbols
; =============================================================
;
; The switch symbols is hatched with the following meaning:
;
; Empty (no hatch)	Manually operated switch (either hand-thrown or thrown electrically but non-interlocked).
; Filled hatch		Centrally controlled from interlocking.
; Sparse hatch		Manually operated but mechanically locked with electrically supervised key, or electrically clamp-locked and supervised.
; 
; A circle marks the theoretical crossing of the switch leg's rear end tangent extensions:
;
; Filled circle*	Push-button electrically locally operated (German: Elektrisches Ortsgestellte Weiche - EOW).
; Empty circle		Hand-thrown (manually), or centrally controlled and interlocked point machine.
; 					*): This symbolism has been inrtoduced by RailCOMPLETE and is not regulated by Bane NOR.
;
;-------------------------------------------------------------------------------------------------------------------------

(defun SWITCH-SYMBOL-SIGNALING ( quadrant Drawing_Number 
	/	blockName
		switchParameters
		crossType A B R x railProfile ang radius 
		str i hatchVariant
	)
	(setq
		switchParameters (getSwitchParameters Drawing_Number)
		crossType	(cadr (assoc "SwitchCrossing" switchParameters)) ; Movable point frog or fixed
		A			(/ (cadr (assoc "A" switchParameters)) 1000.0)
		B			(/ (cadr (assoc "B" switchParameters)) 1000.0)
		R 			(cadr (assoc "R" switchParameters))
		x			(cadr (assoc "x" switchParameters))
		railProfile	(cadr (assoc "RailProfile" switchParameters))
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
			((= i 0) (setq hatchVariant "FILLED")) ; signfies central control, interlocked switch
			((= i 1) (setq hatchVariant "SPARSE")) ; signifies key-locked control in right / left / both position(s)
			((= i 2) (setq hatchVariant _noHatch_)) ; signifies that the point's tongues position is neither controlled or detected
		)
		(setq blockName (strcat "NO-BN-2D-JBTSI-CONNECTOR-SWITCH-" str (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" crossType "-" hatchVariant "-" (rtos quadrant 2 0)))
		; A circle marks the theoretical crossing (where the two end-of-switch tangent extensions meet);
		(setLayer layer_Zero)

		(command
			"._LINE" "0,0" (list (- A radius) 0) ""
			"._CIRCLE" (list A 0) radius
		)
		;Switch symbol - from the Signaling discipline's viewpoint:
		(command 
			"._PLINE"
				(list (+ A radius) 0)
				"_ARC" "_CE" (list A 0) "Angle" ang
				"Line" (strcat "@" (rtos (- B radius)) "<" (rtos ang))
				"_ARC" "_CE" (list A 0) "Angle" (- ang)
				"Line"
				"_CLOSE"
		)
		(cond ; Careful! Do not set comment on a single line here, this would deselect the previous drawing entity, on which the next (drawHatch command relies!
			((= hatchVariant "FILLED") (drawHatch _filledHatch_))	; Tongue positions detected by external sensor or internally in point machine
			((= hatchVariant "SPARSE") (drawHatch _sparseHatch_)) 	; Tongue positions detected indirectly through a mechanical lock and electrically supervised key
		)   
		; Wipeout under sirkelen som markerer teoretisk kryss (vi skal kunne skjule sporlinjen som ligger under):
		(drawWipedOutCircle Drawing_Number) 
		;Mirror 0x, 1x eller 2x til korrekt kvadrant:
		(moveToQuadrant quadrant "_ALL") ; TODO: 2019-07-26 CLFEY - Hva benyttes 'rev' til? Koden var slik: (setq rev (moveToQuadrant quadrant "_ALL"))
		(createSchematicBlockFromCurrentGraphics blockName)
		blockName
		(setq i (+ 1 i))
	);repeat
)



(defun drawWipedOutCircle (Drawing_Number / switchParameters A radius)
	; Wipeout under the circle at theoretical crossing in switch - a way of drawing Bane NOR switch symbols for signaling.
	(setq
		switchParameters (getSwitchParameters Drawing_Number)
		A		  (/ (cadr (assoc "A" switchParameters)) 1000.0)
		radius 	  0.75
	)
	(setLayer layer_Turnout_Wipeout)
	(command
		"._POLYGON" 16 (list A 0) "_INSCRIBED" radius
		"._WIPEOUT" "_POLYLINE" "_LAST" "" "_YES"
		"._DRAWORDER" "_LAST" "" "_Above" "_ALL" ""
	)
)



(defun SWITCH-SYMBOL-SIGNALING-CONTROL-METHOD ( / blockName sides side i )
	; Kontrollåst pos H/V - vis firkant for låsesymbolet på H side, V side eller begge sider
	(setq sides (list "V" "H" "HV"))
	(setq i 0)
	(repeat 3
		(setq side (nth i sides))
		(setq blockName (strcat "NO-BN-2D-JBTSI-CONNECTOR-SWITCH-KONTROLL-" side))
		(setLayer layer_Zero)
		(if (or (= side "V") (= side "HV"))
			(progn
				(command "._RECTANGLE" (list 1.85 -0.75) (list 4.85 -1.50))
			)
		)
		(if (or (= side "H") (= side "HV"))
			(progn
			(command "._RECTANGLE" (list 1.85 0.75) (list 4.85 1.50))
			)
		)
		(createSchematicBlockFromCurrentGraphics blockName)
		(setq i (+ 1 i))
	)
	blockName
)

