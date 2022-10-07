;=========================================================================================================================
;
; ANYADM Ocs High Voltage Protection Screen.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================


; Protection screen (earthed shield to protect exposed areas against high voltage hazard)
;
; EH-ASK Avskjerming (beskyttelse) diverse objekter

(defun ANYADM-OCS-HIGH-VOLTAGE-PROTECTION-SCREEN ( / )
	; Implemented for all administrations:

	; Implemented only for some administrations:
	(cond 
		((= _ADM_ _XXGL_) 
		)
		((= _ADM_ _NOBN_) 
			; See Elkraftportalen, EH.705338 Avskjerming (beskyttelsesskjerm) for åk (0.900m bredde, 1.400m høyde (utvendige mål), Ø32 rør-ramme)
			; EH.705338, two 900x1400 screens mounted with the "ears" facing away from track as seen in "both" direction from the track
			; EH.705339, Various waus I..VI to mount the two 900x1400 screens on an OCS mast or yoke suspension mast.
			; See EH.704042, beskyttelsesskjerm for åk (2.000m bredde, 0.775m høyde (utvendige mål), Ø32 rør-ramme)
			(NOBN-AVSKJERMING "1") 	; 190mm c-c between closest ears (tunnel mast suspension, Ø190-Ø200 mast is ideal)
			(NOBN-AVSKJERMING "2") 	; 170mm c-c between closest ears (B-mast etc).
			(NOBN-AVSKJERMING "3") 	; 350mm c-c between closest ears (100x150 yoke suspension mast, narrow side towards tack).
			(NOBN-AVSKJERMING "45") ; EH.704042, one 2000x775 screen (type I+II=one bug or two smaller wowen steel nets), meant for yoke
									; mounting, insert in direction "both" ("ears" on the rear side)
			(NOBN-AVSKJERMING "6") 	; 350mm c-c between closest ears (100x150 yoke suspension mast, narrow side towards track).
			(NOBN-AVSKJERMING "704042") ; 2000 x 775 screen, two panes, for yoke mounting on two  vertical bars suspended from the yoke.
		)
		((= _ADM_ _FRSR_) 
			; TODO - model FRSR objects, do not use NOBN objects:
			(NOBN-AVSKJERMING "1") 	; 190mm c-c between closest ears (tunnel mast suspension, Ø190-Ø200 mast is ideal)
			(NOBN-AVSKJERMING "2") 	; 170mm c-c between closest ears (B-mast etc).
			(NOBN-AVSKJERMING "3") 	; 350mm c-c between closest ears (100x150 yoke suspension mast, narrow side towards tack).
			(NOBN-AVSKJERMING "45") ; EH.704042, one 2000x775 screen (type I+II=one bug or two smaller wowen steel nets), meant for yoke
									; mounting, insert in direction "both" ("ears" on the rear side)
			(NOBN-AVSKJERMING "6") 	; 350mm c-c between closest ears (100x150 yoke suspension mast, narrow side towards track).
			(NOBN-AVSKJERMING "704042") ; 2000 x 775 screen, two panes, for yoke mounting on two  vertical bars suspended from the yoke.
		)
		((= _ADM_ _DEDB_) 
		)
		((= _ADM_ _JPTX_) 
			; TODO 2022-03-15 - Replace NOBN stuff with JPTX graphics:
			(NOBN-AVSKJERMING "1") 	; 190mm c-c between closest ears (tunnel mast suspension, Ø190-Ø200 mast is ideal)
			(NOBN-AVSKJERMING "2") 	; 170mm c-c between closest ears (B-mast etc).
			(NOBN-AVSKJERMING "3") 	; 350mm c-c between closest ears (100x150 yoke suspension mast, narrow side towards tack).
			(NOBN-AVSKJERMING "45") ; EH.704042, one 2000x775 screen (type I+II=one bug or two smaller wowen steel nets), meant for yoke
									; mounting, insert in direction "both" ("ears" on the rear side)
			(NOBN-AVSKJERMING "6") 	; 350mm c-c between closest ears (100x150 yoke suspension mast, narrow side towards track).
			(NOBN-AVSKJERMING "704042") ; 2000 x 775 screen, two panes, for yoke mounting on two  vertical bars suspended from the yoke.
		)
	)
)



(defun NOBN-AVSKJERMING ( variation / blockName )
	(setq
		blockName	(strcat "NO-BN-2D-JBTEH_ASK-AVSKJERMING" "-" variation)
		description	(strcat "KL AVSKJERMING TYPE " variation)
	)
	(defun LocalGraphics (/)
		(cond 
			((= variation "1") 
				; As shown in EH.705339 "Utf.I"
				; Offset 190/2 mm sideways (0.95m) and 376/2 towards track (0.188m), with two 900x1400 screens,
				; Two "arms" with extrusion for a Ø200 mast clamped around an Ø200 mast.
				(progn
					(NOBN_DrawRightSideScreen) ; First screen
					(command 
						_POLYLINE_ ; Right brace around Ø200 mast
							"-0.030,0.406"
							"0.020,0.406"
							"0.040,0.386"
							"0.040,-0.010"
							"0.020,-0.030"
							"-0.030,-0.030"
							"-0.030,0.095"
							"0.015,0.145"
							"0.015,0.231"
							"-0.030,0.281"
							_closedPolyline_
							_ENTER_
						_RECTANGLE_ "-0.025,-0.081" "0.025,0.401" ; Side arm...
						_ROTATE_ _lastSelection_ _ENTER_ "0,0.376" _angle29_ ; ...rotated 29 deg around rear bracket hole to fit front of sidebar with screen's middle ear
						_CIRCLE_ "0.000,0.376" "0.011" ; Rear hole
						_MOVE_ _selectAll_ _ENTER_ _origin_ "0.095,-0.188" ; Move everything right and down such that Ø200 mast center is in the origin (0,0)
						_MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_ ; Add left side copy
						_RECTANGLE_ "-0.120,0.128" "0.120,0.148"  ; Rear 240xØ20 threaded iron...
						_COPY_ _lastSelection_ _ENTER_ "0,0.138" "0,-0.138" ; ...front threaded iron
						_RECTANGLE_ "-0.092,0.121" "-0.072,0.155"  ; Nut upper left...
						_COPY_ _lastSelection_ _ENTER_ "0,0.138" "0,-0.138" ; ...lower left
						_RECTANGLE_ "0.092,0.121" "0.072,0.155"  ; Nut upper right...
						_COPY_ _lastSelection_ _ENTER_ "0,0.138" "0,-0.138" ; ...lower right
					)
				)
			)
			((= variation "2") 
				; As shown in EH.705339 "Utf.II"
				; Offset (240+2x35)/2 mm (0.155m) sideways and 376/2 towards track, with two 900x1400 screens 
				(progn
					(NOBN_DrawRightSideScreen)
					(command 
						_RECTANGLE_ "-0.035,-0.025" "0.025,0.406"  ; Right arm
						_RECTANGLE_ "-0.035,-0.025" "-0.029,0.406"  ; L-bar
						_RECTANGLE_ "-0.025,-0.081" "0.025,0.401"  ; Side arm...
						_ROTATE_ _lastSelection_ _ENTER_ "0,0.376" _angle29_ ; ...rotated 29 deg around rear bracket hole to fit front of sidebar with screen's middle ear
						_CIRCLE_ "0.000,0.376" "0.011" ; Rear hole
						_MOVE_ _selectAll_ _ENTER_ _origin_ "0.155,-0.188" ; Move everything right and down such that 240x240 HEB or concrete mast is centered in origin (0,0)
						_MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_ ; Add left side copy
						_RECTANGLE_ "-0.190,0.155" "0.190,0.175"  ; Rear 380xØ20 threaded iron...
						_COPY_ _lastSelection_ _ENTER_ "0,0.165" "0,-0.165" ; ...front threaded iron
						_RECTANGLE_ "-0.146,0.148" "-0.126,0.182"  ; Nut upper left...
						_COPY_ _lastSelection_ _ENTER_ "0,0.165" "0,-0.165" ; ...lower left
						_RECTANGLE_ "0.146,0.148" "0.126,0.182"  ; Nut upper right...
						_COPY_ _lastSelection_ _ENTER_ "0,0.16538" "0,-0.165" ; ...lower right
					)
				)
			)
			((= variation "3")
				; As shown in EH.705339 "Utf.III"
				; Offset 170/2 mm (0.85m) sideways and (140/2+35)mm (0.105m) towards track, touching the flat wide side of a B-mast.
				; B2:UNP-120, B3:UNP-140, B4:UNP-160, B5:UNP-180, B6:UNP-200
				; The 2D symbol here is suitable for UNP-140 beams (B3 masts, the most common one).
				(progn 
					(NOBN_DrawRightSideScreen)
					(command 
						_MOVE_ _selectAll_ _ENTER_ _origin_ "0.085,-0.105"
						_RECTANGLE_ "0.221,-0.100" "0.241,0.100"  ; 200mm x Ø20 threaded bar (right)
						_RECTANGLE_ "0.214,0.076" "0.248,0.096"  ; Nut upper right...
						_RECTANGLE_ "0.214,-0.076" "0.248,-0.096"  ; Nut lower right...
						_MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_ ; Left side copy
						_RECTANGLE_ "-0.325,-0.130" "0.325,-0.070"  ; Long 650mm x L60 bar in front of B-mast...
						_RECTANGLE_ "-0.325,-0.076" "0.325,-0.070"  ; ...L60-shaped
						_RECTANGLE_ "-0.270,0.070" "0.270,0.130"  ; Shorter 540mm x L60 bar to be mounted in rear B-mast...
						_RECTANGLE_ "-0.270,0.070" "0.270,0.076"  ; ...L60-shaped
					)
				)
			)
			((= variation "45")
				; As shown in EH.705339 "Utf.IV/V".
				; Same as "Utf.III" but rear bar has been exchanged with two or four 'claws' clamping to the B- or H-mast's U-beams.
				; Offset 170/2 mm (0.85m) sideways and (300/2+35)mm (0.185m), touching the narrow side of a B- / touching a H-mast
				; Note: The mast is assumed to be 300mm wide/deep where the screen is mounted (the mast narrows towards the top) 
				(progn 
					(NOBN_DrawRightSideScreen)
					(command 
						_MOVE_ _selectAll_ _ENTER_ _origin_ "0.085,-0.185"
						_MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_ ; Left side copy
						_RECTANGLE_ "-0.325,-0.210" "0.325,-0.150"  ; Long 650mm x L60 bar in front of B-mast...
						_RECTANGLE_ "-0.325,-0.156" "0.325,-0.150"  ; ...L60-shaped
					)
				)
			)
			((= variation "6")
				; As shown in EH.705339 "Utf.VI"
				; Offset 170/2 mm (0.85m) sideways and (150/2+35)mm (0.110m), touching the side of a suspended 100x150 mast (from yoke), i.e.narrow mast side towards track
				; We chose to mount it with the 'ears' facing away from the track, i.e. a reversed version of Utf.VI as shown in EH.705339.
				(progn 
					(NOBN_DrawRightSideScreen)
					(command 
						_MOVE_ _selectAll_ _ENTER_ _origin_ "0.175,0.110"  ; 100x150 yoke mast/2 + half mounting L-bar = 0.075+0.035=0.110
						_MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_ ; Add left side screen
						_RECTANGLE_ "-0.420,0.075" "0.420,0.135"  ; Long 840mm x L60 bar to be mounted on rear of 100w x 150d suspension mast (away from track)
						_RECTANGLE_ "-0.420,0.075" "0.420,0.081"  ; ...L60-shaped
						_RECTANGLE_ "-0.085,-0.075" "0.085,-0.135"  ; Short 170mm x L60 bar to be mounted on front of 100w x 150d suspension mast (facing the track)
						_RECTANGLE_ "-0.085,-0.075" "0.085,-0.081"  ; ...L60-shaped
						_RECTANGLE_ "0.055,-0.100" "0.075,0.100"  ; 200mm x Ø20 threaded bar (right)
						_RECTANGLE_ "-0.055,-0.100" "-0.075,0.100"  ; 200mm x Ø20 threaded bar (left)
						_RECTANGLE_ "-0.082,0.081" "-0.048,0.097"  ; Nut - upper left (16mm thick M20 nuts, not 20mm)
						_RECTANGLE_ "0.082,0.081" "0.048,0.097"  ; Nut - upper right
						_RECTANGLE_ "-0.082,-0.081" "-0.048,-0.097"  ; Nut - lower left
						_RECTANGLE_ "0.082,-0.081" "0.048,-0.097"  ; Nut - lower right
					)
				)
			)
			((= variation "704042")
				; (EH-704042):
				;(setq
				;	oldX 2.000 ; Old type, for yoke mounting (also with Ø32 tubing as the frame)
				;	oldEarThickness 0.010  ; Flattstål 50x125x10 mm med Ø17 hull, senter plassert 27mm fra bakkant flattstål. To og to i høyden på baksiden.
				;	oldEarDepth 0.125
				;	earX 0.200 ; From center of frame to outer edge of ear
				;	tube 0.032 ; Ø32 tube frame
				;)
				(command 
					_RECTANGLE_ (list 0.190 0.016) (list 0.200 (+ 0.016 0.125)) 
					_LINE_ (list 0.190 (+ 0.016 0.125 (- 0.019))) (list 0.200 (+ 0.016 0.125 (- 0.019))) _ENTER_
					_LINE_ (list 0.190 (+ 0.016 0.125 (- 0.035))) (list 0.200 (+ 0.016 0.125 (- 0.035))) _ENTER_
					_MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
					_RECTANGLE_ (list (- 1.000) 0) (list 1.000 0.032) 
					_MOVE_ _selectAll_ _ENTER_ _origin_ "0.0,-0.114" 
				)
			)
		)
	)
	(TraceLevel3 blockName)
	
	; Schematic symbol
	(SetLayer layDef_Zero)
	(LocalGraphics)
	(AddDescriptionBelowOrigin description 0.5)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	; Metric symbol
	(SetLayer layDef_MetricDetails)
	(LocalGraphics)
	(CreateMetricBlockFromCurrentGraphics blockName)
)



;==========================
; Draw...X...() functions
;==========================
(defun NOBN_DrawRightSideScreen ( / x y tube earX earY earHoleRadius earDisplacement ear1 ear2 ear3 )
	; Ref EH-705338
	; Draw "right side of tunnel mast screen", ears on the back (positive y)
	(setq
		x 0.900 ; BESKYTTELSESSKJERM utvendig bredde (høyden er 1400mm)
		y 0.032 ; Rørdimensjon Ø32 (seen from the above)
		tube 0.032 ; Ø32
		earX 0.065 ; 10mm flattstål 0.065 x 0.065 side-sveiset til rundstålet i senter stål høyde oppe og nede (6 ears total in 3D, rear side of screen)
		earY 0.065
		earHoleRadius (/ 0.022 2)
		earDisplacement (+ (/ tube 2) 0.050)
		ear1 0.076				; center of ear1 in X direction
		ear2 (+ ear1 0.210)
		ear3 (+ ear2 0.240)
	)
	(command 
		_RECTANGLE_ _origin_ (list x y) 
		_RECTANGLE_ (list (- ear1 (/ earX 2)) y) (list (+ ear1 (/ earX 2)) (+ y earY)) 
		_CIRCLE_ (list ear1 (+ y (/ earY 2))) earHoleRadius 
		_RECTANGLE_ (list (- ear2 (/ earX 2)) y) (list (+ ear2 (/ earX 2)) (+ y earY)) 
		_CIRCLE_ (list ear2 (+ y (/ earY 2))) earHoleRadius 
		_RECTANGLE_ (list (- ear3 (/ earX 2)) y) (list (+ ear3 (/ earX 2)) (+ y earY)) 
		_CIRCLE_ (list ear3 (+ y (/ earY 2))) earHoleRadius 
		_MOVE_ _selectAll_ _ENTER_ (list ear1 (+ y (/ earY 2))) _origin_ ; Ear 1 = origin
	)
)
