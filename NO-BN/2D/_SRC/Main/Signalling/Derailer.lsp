;=========================================================================================================================
;
; Derailer.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Derailer

(defun DERAILER ( / singleOrDouble activeOrInactive nSignals operation quadrant )

	(SetCadSystemDefaults)  
	(foreach singleOrDouble (list _single_ _double_)
		(foreach activeOrInactive (list _inactive_ _active_)
			(TraceLevel3 (strcat "DERAILER " singleOrDouble " / " activeOrInactive))
			(foreach quadrant '(1 2 3 4) (NOBN-DERAILER singleOrDouble activeOrInactive quadrant))
		)
	)
)



(defun NOBN-DERAILER ( singleOrDouble activeOrInactive quadrant / blockName description singleOrDoubleText activeOrInactiveText x y a1 a2 ang p1 )
	;                  ___p1
	;                    / | Arrow head
	;       +-----.----/+
	;  -----|  ///////  |------	Active derailer (with one or two arrows) - to be placed on top of left rail (as shown, running towards right)
	;       +/----------+
	;       
	;       +-----------+
	;       |           |		Inactive derailer (no arrow!)
	;       +-----------+
	;  -----------.-----------
	;
	;  The derailer symbol does not include the integrated derailer signal.
	;
	(setq
		blockName				(strcat _SIG_ "SSP-"	"SPORSPERRE"	)
		description				(strcat 				"SPORSPERRE"	)
		singleOrDoubleText		(cond ((= singleOrDouble _single_) "ENKEL") ((= singleOrDouble _double_) "DOBBEL"))
		activeOrInactiveText	(cond ((= activeOrInactive _active_) "AVSPORENDE") ((= activeOrInactive _inactive_) "INAKTIV"))
		blockName				(strcat blockName "-" singleOrDoubleText "-" activeOrInactiveText "-" (rtos quadrant 2 0))
		description				(strcat description ", " singleOrDoubleText ", " activeOrInactiveText)
		x 6.0			; Box
		y 3.0
		a1 2.0		; Extension line from diagonal
		a2 1.5		; Width of arrow head
		ang nil 	; 
		pt nil  	; 
	)
	; Centered basic box with wipeout
	(DrawBox layDef_Zero x y layDef_Derailer_Wipeout)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(SetLayer layDef_Zero)
	(cond 
		((= singleOrDouble _single_)
			(cond
				((= activeOrInactive _inactive_) ; Inactive (OFF) - box with wipeout moved away from track, no arrow
					(MoveUp y)
				)
				((= activeOrInactive _active_) ; Active(ON) -  box is centered on track, with wipeout and arrow
					(setq
						ang (atan (/ y x)) ; The diagonal's angle
						pt (list (+ (HalfOf x) (* a1 (cos ang))) (+ (HalfOf y) (* a1 (sin ang)))) ; Tip of the arrow head
					)
					(command ; Add one arrow
						_LINE_ (PointBL x y) pt _ENTER_					; Arrow line (diagonal plus extension), pointing at quadrant I
						_POLYLINE_										; Arrow head
							(polar pt (+ (HalfOf pi) ang) (/ a2  2))			
							(polar pt (+ (HalfOf pi) ang) (/ a2 -2))
							(polar pt ang a2)
							_closedPolyline_
					)
					(DrawHatch _denseHatch_)										; Filled arrow head
				)
			)
		)
		((= singleOrDouble _double_)
			(cond
				((= activeOrInactive _inactive_) ; Inactive (OFF) - box with wipeout moved away from track, no arrow
					(MoveUp y)
				)
				((= activeOrInactive _active_) ; Active(ON) -  box is centered on track, with wipeout and arrow
					(setq 
						ang (atan (/ (* 2 y) x)) ; Steeper angle, to make space for two arrows
						pt (list (+ 0 (* a1 (cos ang))) (+ (HalfOf y) (* a1 (sin ang)))) ; Tip of first arrow
					)
					(command ; First arrow of two
						_LINE_ (PointBL x y) pt _ENTER_
						_POLYLINE_ ; First arrow
					(polar pt (+ (HalfOf pi) ang) (/ a2 2))
							(polar pt (+ (HalfOf pi) ang) (/ a2 -2))
							(polar pt ang a2)
							_closedPolyline_
					)
					(DrawHatch _denseHatch_)
					(setq pt (list (+ (HalfOf x) (* a1 (cos ang))) (+ (HalfOf y) (* a1 (sin ang))))) ; Tip of second arrow
					(command ; Second arrow
						_LINE_ (list 0 (/ y -2)) pt _ENTER_
						_POLYLINE_ 
							(polar pt (+ (HalfOf pi) ang) (/ a2 2))
							(polar pt (+ (HalfOf pi) ang) (/ a2 -2))
							(polar pt ang a2)
							_closedPolyline_
					)
					(DrawHatch _denseHatch_)
				)
			)
		)
	)
	; Position the derailer in the specified quadrant, add text, then create blocks
	(MoveToQuadrant quadrant _selectAll_); Move to specified quadrant
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
