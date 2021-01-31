;=========================================================================================================================
;
; Derailer.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Derailer

(defun C:DERAILER ( / variation state quadrant)
	(setCadSystemDefaults)  
	(foreach variation (list _single_ _double_)
		(foreach state (list _inactive_ _active_)
			(foreach quadrant '(1 2 3 4)
				(DERAILER variation state quadrant)
			)
		)
	)
)



(defun DERAILER ( variation state quadrant / variationText stateText blockName description x y a1 a2 ang pt )
	(setq
		variationText	(cond ((= variation _single_) "ENKEL") (T "DOBBEL"))
		stateText		(cond ((= state _inactive_) "AVLAGT") (T "PAALAGT"))
		blockName 	(strcat "NO-BN-2D-JBTOB-SPOROBJEKT-SPORSPERRE" "-" variationText "-" stateText "-" (rtos quadrant 2 0))
		description	(strcat variationText " SPORSPERRE, " stateText)
		x 6.0		; Box 6x3 with diagonal, and extension as arrow
		y 3.0
		a1 2.0		; Extension line from diagonal
		a2 1.5		; Width of arrow head
		ang nil 	; see below
		pt nil  	; see below
	)
	; Centered basic box with wipeout
	(drawBox layer_Zero x y layer_Derailer_Wipeout)
	(addDescriptionBelowOrigo description (halfOf y))
	(setLayer layer_Zero)
	(cond 
		((= variation _single_)
			(cond
				((= state _inactive_) ; Inactive (OFF) - box with wipeout moved away from track, no arrow
					(moveUp y)
				)
				((= state _active_) ; Active(ON) -  box is centered on track, with wipeout and arrow
					(setq
						ang (atan (/ y x)) ; The diagonal's angle
						pt (list (+ (/ x 2) (* a1 (cos ang))) (+ (/ y 2) (* a1 (sin ang)))) ; Tip of the arrow head
					)
					(command ; Add one arrow
						"._LINE" (list (- (/ x 2)) (- (/ y 2))) pt ""		; Arrow line (diagonal plus extension), pointing at quadrant I
						"._PLINE"											; Arrow head
							(polar pt (+ (/ pi 2) ang) (/ a2 2))			
							(polar pt (+ (/ pi 2) ang) (/ a2 -2))
							(polar pt ang a2)
							"_CLOSE"
					)
					(drawHatch _denseHatch_)										; Filled arrow head
				)
			)
		)
		((= variation _double_)
			(cond
				((= state _inactive_) ; Inactive (OFF) - box with wipeout moved away from track, no arrow
					(moveUp y)
				)
				((= state _active_) ; Active(ON) -  box is centered on track, with wipeout and arrow
					(setq 
						ang (atan (/ (* 2 y) x)) ; Steeper angle, to make space for two arrows
						pt (list (+ 0 (* a1 (cos ang))) (+ (/ y 2) (* a1 (sin ang)))) ; Tip of first arrow
					)
					(command ; First arrow of two
						"._LINE" (list (- (/ x 2)) (- (/ y 2))) pt ""
						"._PLINE" ; First arrow
							(polar pt (+ (/ pi 2) ang) (/ a2 2))
							(polar pt (+ (/ pi 2) ang) (/ a2 -2))
							(polar pt ang a2)
							"_CLOSE"
					)
					(drawHatch _denseHatch_)
					(setq pt (list (+ (/ x 2) (* a1 (cos ang))) (+ (/ y 2) (* a1 (sin ang))))) ; Tip of second arrow
					(command ; Second arrow
						"._LINE" (list 0 (- (/ y 2))) pt ""
						"._PLINE" 
							(polar pt (+ (/ pi 2) ang) (/ a2 2))
							(polar pt (+ (/ pi 2) ang) (/ a2 -2))
							(polar pt ang a2)
							"_CLOSE"
					)
					(drawHatch _denseHatch_)
				)
			)
		)
	)
	; Position the derailer in the specified quadrant, add text, then create blocks
	(moveToQuadrant quadrant "_ALL"); Move to specified quadrant
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

