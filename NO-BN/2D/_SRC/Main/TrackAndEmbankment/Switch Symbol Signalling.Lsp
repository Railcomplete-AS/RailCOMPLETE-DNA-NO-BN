;=========================================================================================================================
;
; Switch Symbol Signalling.Lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See Bane NOR, TRV, Signal, sporvekselsymboler

; Signalling discipline's graphics for inclusion in switch symbols

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
; 					*): This symbolism has been introduced by RailCOMPLETE.
;
;-------------------------------------------------------------------------------------------------------------------------

; TODO 2021-01-25 CLFEY: Include all NO-BN schematic signaling symbols for switches and crossings



(defun SWITCH-SYMBOL-SIGNALLING-KEYLOCKED ( blockName1 / blockName variation )
	; Key locked tongues in position LEFT, RIGHT or BOTH - show a rectangle symoblizing the lock(s)
	(foreach variation '("LEFT" "RIGHT" "BOTH")
		(setq
			blockName (strcat blockName1 "-KEYLOCKED-" variation)
		)

		(SetLayer layDef_Zero)
		(if (or (= variation "LEFT") (= variation "BOTH"))
			(progn
				(command _RECTANGLE_ (list 1.85 -0.75) (list 4.85 -1.50))
			)
		)
		(if (or (= variation "RIGHT") (= variation "BOTH"))
			(progn
			(command _RECTANGLE_ (list 1.85 0.75) (list 4.85 1.50))
			)
		)
		(CreateMetricBlockFromCurrentGraphics blockName)
	)
)



(defun SWITCH-SYMBOL-SIGNALLING-CIRCLE-AT-TANGENT-INTERSECTION ( drawingNumber blockName1 / blockName switchParameters A radius )
	; Generate symbol for filled circle at theoretical crossing in switch - used to signify "motorized hand-thrown with push-button operation"
	; German: "Elektrisch Ortsgestellte Weichen", EOW.
	(setq
		switchParameters 	(getSwitchParameters drawingNumber)
		A	  				(/ (cadr (assoc "A" switchParameters)) 1000.0)
		radius				0.75
	)
	(setq 
		blockName (strcat blockName1 "-CIRCLE-AT-TANGENT-INTERSECTION-" (rtos A 2 3))
	)
	(SetLayer layDef_Zero)
	(command _CIRCLE_ (list A 0) radius)
	(DrawHatch _solidHatch_)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun SWITCH-SYMBOL-SIGNALLING ( quadrant drawingNumber blockName1 description1
	/	blockName
		description
		switchParameters
		switchDiamondType  A B C D E F L R x railProfile ang
		zeroPad 
		variation
	)
	(setq
		switchParameters (getSwitchParameters drawingNumber)
		switchDiamondType	(cadr (assoc "SwitchDiamondType" switchParameters)) ; Movable point frog or fixed
		A					(/ (cadr (assoc "A" switchParameters)) 1000.0)
		B					(/ (cadr (assoc "B" switchParameters)) 1000.0)
		R 					(cadr (assoc "R" switchParameters))
		x					(cadr (assoc "x" switchParameters))
		railProfile			(cadr (assoc "RailProfile" switchParameters))
		ang					(R->D (atan (/ 1.0 x))) ; sporvekselsymbol
		radius				0.75
	)
	(if (< x 10)
		(setq zeroPad "0") ; Pad with leading zeros
	;else
		(setq zeroPad _emptyString_)
	)
	(foreach variation '("REMOTE_CONTROLLED" "KEYLOCKED" "NOT_INTERLOCKED")
		(setq
			blockName	(strcat blockName1 "-"    zeroPad (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" switchDiamondType "-" variation "-" (rtos quadrant 2 0))
			description	(strcat description1 ", " zeroPad (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" switchDiamondType "-" variation "-" (rtos quadrant 2 0))
		)
		; A circle marks the theoretical crossing (where the two end-of-switch tangent extensions meet);
		(SetLayer layDef_Zero)

		(command
			_LINE_ _origin_ (list (- A radius) 0) _ENTER_
			_CIRCLE_ (list A 0) radius
		)
		; Switch symbol - from the Signalling discipline's viewpoint:
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
		(cond ; Careful! Do not set comment on a single line here, this would deselect the previous drawing entity, on which the next (DrawHatch command relies!
			((= variation "REMOTE_CONTROLLED") (DrawHatch _solidHatch_))	; Tongue positions detected by external sensor or internally in point machine
			((= variation "KEYLOCKED") (DrawHatch _sparseHatch_)) 	; Tongue positions detected indirectly through a mechanical lock and electrically supervised key
		)
		; Wipeout under the circle marking the theoretical cross (we must be able to hide the track line underneath):
		(DrawWipedOutCircle drawingNumber) 
		; Mirror 0x, 1x or 2x to correct quadrant:
		(MoveToQuadrant quadrant _selectAll_)
		(AddDescriptionBelowOrigin description _three_)
		(CreateMetricBlockFromCurrentGraphics blockName)
	)
)



(defun FRSR-SWITCH-SYMBOL-SIGNALLING ( quadrant blockName1 description1
	/	blockName
		description
		variation
	)
	;
	;           /
	;    JP   /  
	;  -----/-------------
	;       +---------+
	;       |         |			Train is directed to the right when command device is in its '+' normal position
	;       +---------+
	;
	(foreach variation '("REMOTE_CONTROLLED" "KEYLOCKED" "NOT_INTERLOCKED")
		(setq
			blockName	(strcat blockName1 "-"    zeroPad (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" switchDiamondType "-" variation "-" (rtos quadrant 2 0))
			description	(strcat description1 ", " zeroPad (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" switchDiamondType "-" variation "-" (rtos quadrant 2 0))
		)
		; Switch symbol - from the Signalling discipline's viewpoint:
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
		(cond ; Careful! Do not set comment on a single line here, this would deselect the previous drawing entity, on which the next (DrawHatch command relies!
			((= variation "REMOTE_CONTROLLED") (DrawHatch _solidHatch_))	; Tongue positions detected by external sensor or internally in point machine
			((= variation "KEYLOCKED") (DrawHatch _sparseHatch_)) 	; Tongue positions detected indirectly through a mechanical lock and electrically supervised key
		)
		; Wipeout under the circle marking the theoretical cross (we must be able to hide the track line underneath):
		(DrawWipedOutCircle drawingNumber) 
		; Mirror 0x, 1x or 2x to correct quadrant:
		(MoveToQuadrant quadrant _selectAll_)
		(AddDescriptionBelowOrigin description _three_)
		(CreateMetricBlockFromCurrentGraphics blockName)
	)
)


; Draw helpers
(defun DrawWipedOutCircle (drawingNumber / switchParameters A radius )
	; Wipeout under the circle at theoretical crossing in switch - a way of drawing Bane NOR switch symbols for signaling.
	(setq
		switchParameters (getSwitchParameters drawingNumber)
		A	(/ (cadr (assoc "A" switchParameters)) 1000.0)
		radius	0.75
	)
	(command _POLYGON_ 16 (list A 0) _inscribedPolygon_ radius)
	(AddWipeoutToLastClosedPolyline layDef_Switch_SigCircle_Wipeout _eraseWipeoutSource_)
)
