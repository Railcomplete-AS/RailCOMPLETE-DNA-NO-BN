;=========================================================================================================================
;
; ANYADM Derailer.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Derailer

(defun ANYADM-DERAILER ( / singleOrDouble activeOrInactive nSignals operation quadrant )

	(SetCadSystemDefaults)  
	(foreach singleOrDouble (list _single_ _double_)
		(foreach activeOrInactive (list _inactive_ _active_)
			(TraceLevel3 (strcat "DERAILER " singleOrDouble " / " activeOrInactive))
			(cond 
				((= _ADM_ _XXGL_)
				)
				((= _ADM_ _NOBN_)
					(foreach quadrant '(1 2 3 4) (NOBN-DERAILER singleOrDouble activeOrInactive quadrant))
				)
				((= _ADM_ _FRSR_)
				)
				((= _ADM_ _DEDB_)
					(foreach nSignals '(0 1 2)
						(foreach operation (list _manual_ _machine_)
							(foreach quadrant '(1 2 3 4) (DEDB-DERAILER singleOrDouble activeOrInactive nSignals operation quadrant))
						)
					)
				)
			);cond
		)
	)
)



(defun XXGL-DERAILER ( singleOrDouble activeOrInactive quadrant / blockName description )
	(setq 
		blockName	(strcat _SIG_ "DER-"	"DERAILER"	)
		description	(strcat 				"DERAILER"	)
	)
	; TODO Make symbol
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
						_LINE_ (PosBL x y) pt _ENTER_					; Arrow line (diagonal plus extension), pointing at quadrant I
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
						_LINE_ (PosBL x y) pt _ENTER_
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



(defun FRSR-DERAILER ( singleOrDouble activeOrInactive quadrant / blockName description )
	(setq 
		blockName 	(strcat _SIG_ "DER-"	"DERAILER"		)
		description	(strcat 				"DERAILLEUR"	)
	)
	; TODO Make symbol
)



(defun DEDB-DERAILER ( singleOrDouble activeOrInactive nSignals operation quadrant / 
		blockName description
		singleOrDoubleText activeOrInactiveText signalText operationText
		x y p1 p2 p3 p4 p5 p6 p7 p8 r
	)
	; Derailer - Gleissperre. SHos signal Sh0 (stop) or Sh1 (proceed allowed).
	;
	; Draw at origin, add hatch and arrow and signal, then move it:
	;    
	;     ___   
	;    / | \  (Second signal, if signals for both driving directions are needed)
	;   (  |  )----+
	;    \_|_/     |   --+			Arrow head. Points 1..4 form an optional integrated derailer "open" signal.
	;           +--+--+ /|
	;           |     |/  
	;           |  .  /				Active, one arrow, to be placed on top of left rail (as shown, running towards right)
	;           |    /|
	;           | 7 / |				Points 1..6 form an integrated derailer "closed" signal.
	;      -----|  /  |------		Active derailer, with one (centered) or two arrows (6,7)
	;           | / 8 |        		p8 is also the hatch seed point.
	;           |/    |
	;           /     |
	;          /|     |
	;         / +--1--+   ___
	;              |     / 5 \ 
	;              2----3  4  )		Circle with vertical bar | = Signal Sh0 "Halt! Fahrverbot."
	;                    \_6_/
	;    
	;   ___           
	;  /   \  Open:
	; (6 4 5)          --+
	;  \_3_/            /|			Arrow head. Points 1..6 form an integrated derailer "open" signal. 
	;    |  +----------/+       	Circle with horizontal bar -- = Sh1 "Fahrverbot aufgehoben."
	;    2--1   7 / 8   +--+    	Inactive derailer (with one or two arrows) - to be placed besides the left rail (as shown, running towards right)
	;       +/----------+ _|_		p8 is also the hatch seed point.
	;  -----/-----.------/   \----
	;      /            (-----)
	;                    \___/		(Second signal, if signals for both driving directions are needed)
	;
	;     
	;  The derailer symbol does not include the integrated derailer signal.
	;
	(setq
		blockName 				(strcat _SIG_ "GSP-"		"GLEISSPERRE"		)
		description 			(strcat 					"GLEISSPERRE"		)
		singleOrDoubleText		(cond ((= singleOrDouble _single_) "EINFACH") ((= singleOrDouble _double_) "DOPPELT"))
		activeOrInactiveText	(cond ((= activeOrInactive _active_) "AUFGELEGT") ((= activeOrInactive _inactive_) "ABLIEGEND"))
		signalText				(cond ((= nSignals 0) "SIGNALLOS") ((= nSignals 1) "EINZELSIGNAL") ((= nSignals 2) "DOPPELSIGNAL"))
		operationText			(cond ((= operation _manual_) "ORTSGESTELLT") ((= operation _machine_) "FERNGESTELLT"))
		blockName				(strcat blockName "-" singleOrDoubleText "-" activeOrInactiveText "-" signalText "-" operationText "-" (rtos quadrant 2 0))
		description				(strcat description ", " singleOrDoubleText ", " activeOrInactiveText ", " signalText ", " operationText)
	)
	(if (= activeOrInactive _active_)
		(setq
			x	2.0					; Active => Box across track
			y	6.0
			p1	'( 0.000 -3.000)
			p2	'( 0.000 -4.000)
			p3	'( 1.500 -4.000)
			p4	'( 2.500 -4.000)
			p5	'( 2.500 -3.000)
			p6	'( 2.500 -5.000)
			p7	'(-0.500  0.500)
			p8	'( 0.500 -0.500)
			r	1.000
		)
	;else
	(setq
			x	6.0					; Inactive => Alongside box
			y	2.0
			p1	'(-3.000  0.000)
			p2	'(-4.000  0.000)
			p3	'(-4.000  1.500)
			p4	'(-4.000  2.500)
			p5	'(-3.000  2.500)
			p6	'(-5.000  2.500)
			p7	'(-0.500  0.500)
			p8	'( 0.500 -0.500)
			r	1.000
		)
	)
	(defun LocalGraphicsArrow ( p0 / arrowWidth p1 p2 p3 )
		; Draw a 45 degrees inclined arrow with solid arrow point, centered at point p0
		;
		;      3
		;   / 2 |
		;    /
		;   0		p0 = given point as argument
		;  /
		; 1
		;
		(setq
			arrowWidth	0.500
			p1	(AddVectors p0 '(-2.500 -2.500))
			p2	(AddVectors p0 '( 1.750  1.750))
			p3	(AddVectors p0 '( 2.500  2.500))
		)
		(command
			_POLYLINE_ ; arrow head
				p1
				p2
				_setPolylineWidth_ arrowWidth _lwZero_		; Start width -> end width
				p3
				_setPolylineWidth_ _lwZero_ _lwZero_		; Reset polyline width after use
				_ENTER_
		)
	)

	(cond
		((= nSignals 1)
			(DrawLine layDef_Zero p1 p2)
			(DrawLine layDef_Zero p2 p3)
			(DrawLine layDef_Zero p5 p6)
			(DrawCircleAtPos layDef_Zero p4 r _noWipeout_)
		)
		((= nSignals 2)
			(DrawLine layDef_Zero p1 p2)
			(DrawLine layDef_Zero p2 p3)
			(DrawLine layDef_Zero p5 p6)
			(DrawCircleAtPos layDef_Zero p4 r _noWipeout_)
			(MirrorAboutXaxis _eraseMirrorSource_)
			(MirrorAboutYaxis _eraseMirrorSource_)
			(DrawLine layDef_Zero p1 p2)
			(DrawLine layDef_Zero p2 p3)
			(DrawLine layDef_Zero p5 p6)
			(DrawCircleAtPos layDef_Zero p4 r _noWipeout_)
		)
	)
	(DrawBox layDef_Zero x y layDef_Derailer_Wipeout)
	(if (= operation _manual_)
		(DrawHatch _sparseHatch_)
	;else
		(DrawHatch _denseHatch_)
	)
	(if (= singleOrDouble _single_) 
		(LocalGraphicsArrow _origin_)
	;else
		(progn
			(LocalGraphicsArrow p7)
			(LocalGraphicsArrow p8)
		)
	)	
	(AddDescriptionBelowOrigin description (HalfOf y))
	(if (= activeOrInactive _inactive_) (MoveUp (* _threeQuarters_ y))) ; Move up an inactive derailer, to place it visibly outside the track
	
	; Position the derailer in the specified quadrant, add text, then create blocks
	(MoveToQuadrant quadrant _selectAll_); Move to specified quadrant
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
