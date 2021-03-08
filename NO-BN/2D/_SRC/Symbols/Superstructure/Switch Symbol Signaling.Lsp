;=========================================================================================================================
;
; Switch Symbol Signaling.Lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
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

; TODO 2021-01-25 CLFEY: Include all NO-BN schematic signaling sysmbols for switches and crossings
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
	(setq str _ENTER_)
	(if (< x 10)
		(setq str "0")
	)
	(setq i 0)
	(repeat 3
		(cond 
			((= i 0) (setq hatchVariant "FILLED")) ; signfies central control, interlocked switch
			((= i 1) (setq hatchVariant "SPARSE")) ; signifies key-locked control in right / left / both position(s)
			((= i 2) (setq hatchVariant "EMPTY")) ; signifies that the point's tongues position is neither controlled nor detected
		)
		(setq 
			blockName (strcat "NO-BN-2D-JBTSI-CONNECTOR-SWITCH-" str (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" crossType "-" hatchVariant "-" (rtos quadrant 2 0))
			description	(strcat "SPORVEKSEL / SIGNAL-SYMBOL, "   str (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" crossType "-" hatchVariant "-" (rtos quadrant 2 0))
		)
		; A circle marks the theoretical crossing (where the two end-of-switch tangent extensions meet);
		(setLayer layDef_Zero)

		(command
			_LINE_ _origo_ (list (- A radius) 0) _ENTER_
			_CIRCLE_ (list A 0) radius
		)
		; Switch symbol - from the Signaling discipline's viewpoint:
		(command 
			_POLYLINE_
				(list (+ A radius) 0)
				_setPolylineArcMode_
				_setPolylineArcCenter_ (list A 0) 
				_setPolylineArcAngle_ ang
				_setPolylineLineMode_
				(strcat "@" (rtos (- B radius)) "<" (rtos ang))
				_setPolylineArcMode_
				_setPolylineArcCenter_ (list A 0)
				_setPolylineArcAngle_ (- ang)
				_setPolylineLineMode_
				_closedPolyline_
		)
		(cond ; Careful! Do not set comment on a single line here, this would deselect the previous drawing entity, on which the next (drawHatch command relies!
			((= hatchVariant "FILLED") (drawHatch _solidHatch_))	; Tongue positions detected by external sensor or internally in point machine
			((= hatchVariant "SPARSE") (drawHatch _sparseHatch_)) 	; Tongue positions detected indirectly through a mechanical lock and electrically supervised key
		)   
		; Wipeout under sirkelen som markerer teoretisk kryss (vi skal kunne skjule sporlinjen som ligger under):
		(drawWipedOutCircle Drawing_Number) 
		; Mirror 0x, 1x eller 2x til korrekt kvadrant:
		(moveToQuadrant quadrant _selectAll_)
		(addDescriptionBelowOrigo description 1.0)
		(createMetricBlockFromCurrentGraphics blockName)
		(setq i (+ 1 i))
	);repeat
)



(defun drawWipedOutCircle (Drawing_Number / switchParameters A r )
	; Wipeout under the circle at theoretical crossing in switch - a way of drawing Bane NOR switch symbols for signaling.
	(setq
		switchParameters (getSwitchParameters Drawing_Number)
		A	(/ (cadr (assoc "A" switchParameters)) 1000.0)
		r	0.75
	)
	(command _POLYGON_ 16 (list A 0) _inscribed_ r)
	(addWipeoutToLastClosedPolyline layDef_Turnout_Wipeout _eraseWipeoutSource_)
)



(defun SWITCH-SYMBOL-SIGNALING-KEYLOCKED ( / blockName sides side i )
	; Key locked tongues in position LEFT, RIGHT or BOTH - show a rectangle symoblizing the lock(s)
	(setq sides (list "LEFT" "RIGHT" "BOTH"))
	(setq i 0)
	(repeat 3
		(setq side (nth i sides))
		(setq blockName (strcat "NO-BN-2D-JBTSI-CONNECTOR-SWITCH-KEYLOCKED-" side))
		(setLayer layDef_Zero)
		(if (or (= side "LEFT") (= side "BOTH"))
			(progn
				(command _RECTANGLE_ (list 1.85 -0.75) (list 4.85 -1.50))
			)
		)
		(if (or (= side "LEFT") (= side "BOTH"))
			(progn
			(command _RECTANGLE_ (list 1.85 0.75) (list 4.85 1.50))
			)
		)
		(createMetricBlockFromCurrentGraphics blockName)
		(setq i (+ 1 i))
	)
	blockName
)

