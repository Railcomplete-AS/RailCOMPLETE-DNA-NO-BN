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
; 					*): This symbolism has been introduced by RailCOMPLETE and is not regulated by Bane NOR.
;
;-------------------------------------------------------------------------------------------------------------------------

; TODO 2021-01-25 CLFEY: Include all NO-BN schematic signaling sysmbols for switches and crossings



(defun SWITCH-SYMBOL-SIGNALING-THROWING-METHOD (drawingNumber / blockName switchParameters A r )
	; Generate symbol for filled circle at theoretical crossing in switch - used to signify "motorized hand-thrown with push-button operation"
	; German: "Elektrisch Ortsgestellte Weichen", EOW.
	(setq
		switchParameters 	(getSwitchParameters drawingNumber)
		A	  				(/ (cadr (assoc "A" switchParameters)) 1000.0)
		r 	  				0.75
	)
	(setq blockName (strcat "NO-BN-2D-JBTSI-SWITCH-THROWING-METHOD" "-" (rtos A 2 3)))
	(SetLayerAndObjectColor layDef_Zero "_ByBlock")
	(command _CIRCLE_ (list A 0) r)
	(DrawHatch _solidHatch_)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun SWITCH-SYMBOL-SIGNALING ( quadrant drawingNumber 
	/	blockName
		switchParameters
		switchDiamondType A B R x railProfile ang r 
		str i variation
	)
	(setq
		switchParameters (getSwitchParameters drawingNumber)
		switchDiamondType	(cadr (assoc "SwitchCrossing" switchParameters)) ; Movable point frog or fixed
		A					(/ (cadr (assoc "A" switchParameters)) 1000.0)
		B					(/ (cadr (assoc "B" switchParameters)) 1000.0)
		R 					(cadr (assoc "R" switchParameters))
		x					(cadr (assoc "x" switchParameters))
		railProfile			(cadr (assoc "RailProfile" switchParameters))
		ang					(R->D (atan (/ 1.0 x))) ; sporvekselsymbol
		r					0.75
	)
	(setq str _ENTER_)
	(if (< x 10)
		(setq str "0") ; Pad with leading zeros
	)
	(foreach variation '("REMOTE_CONTROLLED" "KEYLOCKED" "NOT_INTERLOCKED")
		(setq 
			blockName (strcat "NO-BN-2D-JBTSI-CONNECTOR-SWITCH-" str (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" switchDiamondType "-" variation "-" (rtos quadrant 2 0))
			description	(strcat "SPORVEKSEL / SIGNAL-SYMBOL, "   str (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" switchDiamondType "-" variation "-" (rtos quadrant 2 0))
		)
		; A circle marks the theoretical crossing (where the two end-of-switch tangent extensions meet);
		(SetLayer layDef_Zero)

		(command
			_LINE_ _origo_ (list (- A r) 0) _ENTER_
			_CIRCLE_ (list A 0) r
		)
		; Switch symbol - from the Signaling discipline's viewpoint:
		(command 
			_POLYLINE_
				(list (+ A r) 0)
				_setPolylineArcMode_
				_setPolylineArcCenter_ (list A 0) 
				_setPolylineArcAngle_ ang
				_setPolylineLineMode_
				(strcat "@" (rtos (- B r)) "<" (rtos ang))
				_setPolylineArcMode_
				_setPolylineArcCenter_ (list A 0)
				_setPolylineArcAngle_ (- ang)
				_setPolylineLineMode_
				_closedPolyline_
		)
		(cond ; Careful! Do not set comment on a single line here, this would deselect the previous drawing entity, on which the next (DrawHatch command relies!
			((= variation "REMOTE_CONTROLLED") (DrawHatch _solidHatch_))	; Tongue positions detected by external sensor or internally in point machine
			((= variation "KEYLOCKED") (DrawHatch _sparseHatch_)) 	; Tongue positions detected indirectly through a mechanical lock and electrically supervised key
		)
		; Wipeout under sirkelen som markerer teoretisk kryss (vi skal kunne skjule sporlinjen som ligger under):
		(DrawWipedOutCircle drawingNumber) 
		; Mirror 0x, 1x eller 2x til korrekt kvadrant:
		(MoveToQuadrant quadrant _selectAll_)
		(AddDescriptionBelowOrigo description _one_)
		(CreateMetricBlockFromCurrentGraphics blockName)
	)
)



(defun DrawWipedOutCircle (drawingNumber / switchParameters A r )
	; Wipeout under the circle at theoretical crossing in switch - a way of drawing Bane NOR switch symbols for signaling.
	(setq
		switchParameters (getSwitchParameters drawingNumber)
		A	(/ (cadr (assoc "A" switchParameters)) 1000.0)
		r	0.75
	)
	(command _POLYGON_ 16 (list A 0) _inscribedPolygon_ r)
	(AddWipeoutToLastClosedPolyline layDef_Turnout_Wipeout _eraseWipeoutSource_)
)



(defun SWITCH-SYMBOL-SIGNALING-KEYLOCKED ( / blockName variation )
	; Key locked tongues in position LEFT, RIGHT or BOTH - show a rectangle symoblizing the lock(s)
	(foreach variation '("LEFT" "RIGHT" "BOTH")
		(setq blockName (strcat "NO-BN-2D-JBTSI-CONNECTOR-SWITCH-KEYLOCKED-" variation))
		(SetLayer layDef_Zero)
		(if (or (= variation "LEFT") (= variation "BOTH"))
			(progn
				(command _RECTANGLE_ (list 1.85 -0.75) (list 4.85 -1.50))
			)
		)
		(if (or (= variation "LEFT") (= variation "BOTH"))
			(progn
			(command _RECTANGLE_ (list 1.85 0.75) (list 4.85 1.50))
			)
		)
		(CreateMetricBlockFromCurrentGraphics blockName)
	)
)
