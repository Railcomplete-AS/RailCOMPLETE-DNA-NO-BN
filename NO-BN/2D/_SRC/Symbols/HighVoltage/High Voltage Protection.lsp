;=========================================================================================================================
;
; High Voltage Protection.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================


; Protection screen (earthed shield to protect exposed areas against high voltage hazard)
;
; EH-XBE Beskyttelse diverse objekter

(defun C:HIGH-VOLTAGE-PROTECTION ( / )
	; See Elkraftportalen, EH.705338 Beskyttelsesskjerm for åk (0.900m bredde, 1.400m høyde (utvendige mål), Ø32 rør-ramme)
	; EH.705338, two 900x1400 screens mounted with the "ears" facing away from track as seen in "both" direction from the track
	; EH.705339, Various waus I..VI to mount the two 900x1400 screens on an OCS mast or yoke suspension mast.
	; See EH.704042, beskyttelsesskjerm for åk (2.000m bredde, 0.775m høyde (utvendige mål), Ø32 rør-ramme)
	(BESKYTTELSESSKJERM "1") 	; 190mm c-c between closest ears (tunnel mast suspension, Ø190-Ø200 mast is ideal)
	(BESKYTTELSESSKJERM "2") 	; 170mm c-c between closest ears (B-mast etc).
	(BESKYTTELSESSKJERM "3") 	; 350mm c-c between closest ears (100x150 yoke suspension mast, narrow side towards tack).
	(BESKYTTELSESSKJERM "45") 	; EH.704042, one 2000x775 screen (type I+II=one bug or two smaller wowen steel nets), meant for yoke
								; mounting, insert in direction "both" ("ears" on the rear side)
	(BESKYTTELSESSKJERM "6") 	; 350mm c-c between closest ears (100x150 yoke suspension mast, narrow side towards track).
	(BESKYTTELSESSKJERM "704042") ; 2000 x 775 screen, two panes, for yoke mounting on two  vertical bars suspended from the yoke.
)



(defun BESKYTTELSESSKJERM ( variant / blockName )
	(setq
		blockName (strcat "NO-BN-2D-JBTKL-BESKYTTELSE-SKJERM" "-" variant)
	)
	(cond 
		((= variant "1") 
			; As shown in EH.705339 "Utf.I"
			; Offset 190/2 mm sideways (0.95m) and 376/2 towards track (0.188m), with two 900x1400 screens,
			; Two "arms" with extrusion for a Ø200 mast clamped around an Ø200 mast.
			(progn
				(drawRightSideScreen) ; First screen
				(command 
					"._PLINE" ; Right brace around Ø200 mast
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
						"_CLOSE"
						""
					"._RECTANGLE" "-0.025,-0.081" "0.025,0.401" "" ; Side arm...
					"._ROTATE" "_LAST" "" "0,0.376" "29.0" ; ...rotated 29 deg around rear bracket hole to fit front of sidebar with screen's middle ear
					"._CIRCLE" "0.000,0.376" "0.011" ; Rear hole
					"._MOVE" "_ALL" "" "0,0" "0.095,-0.188" ; Move everything right and down such that Ø200 mast center is in the origo (0,0)
					"._MIRROR" "_ALL" "" "0,0" "0,1" "_NO" ; Add left side copy
					"._RECTANGLE" "-0.120,0.128" "0.120,0.148" "" ; Rear 240xØ20 threaded iron...
					"._COPY" "_LAST" "" "0,0.138" "0,-0.138" ; ...front threaded iron
					"._RECTANGLE" "-0.092,0.121" "-0.072,0.155" "" ; Nut upper left...
					"._COPY" "_LAST" "" "0,0.138" "0,-0.138" ; ...lower left
					"._RECTANGLE" "0.092,0.121" "0.072,0.155" "" ; Nut upper right...
					"._COPY" "_LAST" "" "0,0.138" "0,-0.138" ; ...lower right
				)
			)
		)
		((= variant "2") 
			; As shown in EH.705339 "Utf.II"
			; Offset (240+2x35)/2 mm (0.155m) sideways and 376/2 towards track, with two 900x1400 screens 
			(progn
				(drawRightSideScreen)
				(command 
					"._RECTANGLE" "-0.035,-0.025" "0.025,0.406" "" ; Right arm
					"._RECTANGLE" "-0.035,-0.025" "-0.029,0.406" "" ; L-bar
					"._RECTANGLE" "-0.025,-0.081" "0.025,0.401" "" ; Side arm...
					"._ROTATE" "_LAST" "" "0,0.376" "29.0" ; ...rotated 29 deg around rear bracket hole to fit front of sidebar with screen's middle ear
					"._CIRCLE" "0.000,0.376" "0.011" ; Rear hole
					"._MOVE" "_ALL" "" "0,0" "0.155,-0.188" ; Move everything right and down such that 240x240 HEB or concrete mast is centered in origo (0,0)
					"._MIRROR" "_ALL" "" "0,0" "0,1" "_NO" ; Add left side copy
					"._RECTANGLE" "-0.190,0.155" "0.190,0.175" "" ; Rear 380xØ20 threaded iron...
					"._COPY" "_LAST" "" "0,0.165" "0,-0.165" ; ...front threaded iron
					"._RECTANGLE" "-0.146,0.148" "-0.126,0.182" "" ; Nut upper left...
					"._COPY" "_LAST" "" "0,0.165" "0,-0.165" ; ...lower left
					"._RECTANGLE" "0.146,0.148" "0.126,0.182" "" ; Nut upper right...
					"._COPY" "_LAST" "" "0,0.16538" "0,-0.165" ; ...lower right
				)
			)
		)
		((= variant "3")
			; As shown in EH.705339 "Utf.III"
			; Offset 170/2 mm (0.85m) sideways and (140/2+35)mm (0.105m) towards track, touching the flat wide side of a B-mast.
			; B2:UNP-120, B3:UNP-140, B4:UNP-160, B5:UNP-180, B6:UNP-200
			; The 2D symbol here is suitable for UNP-140 beams (B3 masts, the most common one).
			(progn 
				(drawRightSideScreen)
				(command 
					"._MOVE" "_ALL" "" "0,0" "0.085,-0.105"
					"._RECTANGLE" "0.221,-0.100" "0.241,0.100" "" ; 200mm x Ø20 threaded bar (right)
					"._RECTANGLE" "0.214,0.076" "0.248,0.096" "" ; Nut upper right...
					"._RECTANGLE" "0.214,-0.076" "0.248,-0.096" "" ; Nut lower right...
					"._MIRROR" "_ALL" "" "0,0" "0,1" "_NO" ; Left side copy
					"._RECTANGLE" "-0.325,-0.130" "0.325,-0.070" "" ; Long 650mm x L60 bar in front of B-mast...
					"._RECTANGLE" "-0.325,-0.076" "0.325,-0.070" "" ; ...L60-shaped
					"._RECTANGLE" "-0.270,0.070" "0.270,0.130" "" ; Shorter 540mm x L60 bar to be mounted in rear B-mast...
					"._RECTANGLE" "-0.270,0.070" "0.270,0.076" "" ; ...L60-shaped
				)
			)
		)
		((= variant "45")
			; As shown in EH.705339 "Utf.IV/V".
			; Same as "Utf.III" but rear bar has been exchanged with two or four 'claws' clamping to the B- or H-mast's U-beams.
			; Offset 170/2 mm (0.85m) sideways and (300/2+35)mm (0.185m), touching the narrow side of a B- / touching a H-mast
			; Note: The mast is assumed to be 300mm wide/deep where the screen is mounted (the mast narrows towards the top) 
			(progn 
				(drawRightSideScreen)
				(command 
					"._MOVE" "_ALL" "" "0,0" "0.085,-0.185"
					"._MIRROR" "_ALL" "" "0,0" "0,1" "_NO" ; Left side copy
					"._RECTANGLE" "-0.325,-0.210" "0.325,-0.150" "" ; Long 650mm x L60 bar in front of B-mast...
					"._RECTANGLE" "-0.325,-0.156" "0.325,-0.150" "" ; ...L60-shaped
				)
			)
		)
		((= variant "6")
			; As shown in EH.705339 "Utf.VI"
			; Offset 170/2 mm (0.85m) sideways and (150/2+35)mm (0.110m), touching the side of a suspended 100x150 mast (from yoke), i.e.narrow mast side towards track
			; We chose to mount it with the 'ears' facing away from the track, i.e. a reversed version of Utf.VI as shown in EH.705339.
			(progn 
				(drawRightSideScreen)
				(command 
					"._MOVE" "_ALL" "" "0,0" "0.175,0.110"  ; 100x150 yoke mast/2 + half mounting L-bar = 0.075+0.035=0.110
					"._MIRROR" "_ALL" "" "0,0" "0,1" "_NO" ; Add left side screen
					"._RECTANGLE" "-0.420,0.075" "0.420,0.135" "" ; Long 840mm x L60 bar to be mounted on rear of 100w x 150d suspension mast (away from track)
					"._RECTANGLE" "-0.420,0.075" "0.420,0.081" "" ; ...L60-shaped
					"._RECTANGLE" "-0.085,-0.075" "0.085,-0.135" "" ; Short 170mm x L60 bar to be mounted on front of 100w x 150d suspension mast (facing the track)
					"._RECTANGLE" "-0.085,-0.075" "0.085,-0.081" "" ; ...L60-shaped
					"._RECTANGLE" "0.055,-0.100" "0.075,0.100" "" ; 200mm x Ø20 threaded bar (right)
					"._RECTANGLE" "-0.055,-0.100" "-0.075,0.100" "" ; 200mm x Ø20 threaded bar (left)
					"._RECTANGLE" "-0.082,0.081" "-0.048,0.097" "" ; Nut - upper left (16mm thick M20 nuts, not 20mm)
					"._RECTANGLE" "0.082,0.081" "0.048,0.097" "" ; Nut - upper right
					"._RECTANGLE" "-0.082,-0.081" "-0.048,-0.097" "" ; Nut - lower left
					"._RECTANGLE" "0.082,-0.081" "0.048,-0.097" "" ; Nut - lower right
				)
			)
		)
		((= variant "704042")
			; (EH-704042):
			;(setq
			;	oldX 2.000 ; Old type, for yoke mounting (also with Ø32 tubing as the frame)
			;	oldEarThickness 0.010  ; Flattstål 50x125x10 mm med Ø17 hull, senter plassert 27mm fra bakkant flattstål. To og to i høyden på baksiden.
			;	oldEarDepth 0.125
			;	earX 0.200 ; From center of frame to outer edge of ear
			;	tube 0.032 ; Ø32 tube frame
			;)
			(command 
				"._RECTANGLE" (list 0.190 0.016) (list 0.200 (+ 0.016 0.125)) ""
				"._LINE" (list 0.190 (+ 0.016 0.125 (- 0.019))) (list 0.200 (+ 0.016 0.125 (- 0.019))) ""
				"._LINE" (list 0.190 (+ 0.016 0.125 (- 0.035))) (list 0.200 (+ 0.016 0.125 (- 0.035))) ""
				"._MIRROR" "_ALL" "" "0,0" "0,1" "_NO"
				"._RECTANGLE" (list (- 1.000) 0) (list 1.000 0.032) ""
				"._MOVE" "_ALL" "" "0,0" "0.0,-0.114" 
			)
		)
	)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



;==========================
; draw...X...() functions
;==========================
(defun drawRightSideScreen ( / x y tube earX earY earHoleRadius earDisplacement ear1 ear2 ear3 )
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
		"._RECTANGLE" (list 0 0) (list x y) ""
		"._RECTANGLE" (list (- ear1 (/ earX 2)) y) (list (+ ear1 (/ earX 2)) (+ y earY)) ""
		"._CIRCLE" (list ear1 (+ y (/ earY 2))) earHoleRadius ""
		"._RECTANGLE" (list (- ear2 (/ earX 2)) y) (list (+ ear2 (/ earX 2)) (+ y earY)) ""
		"._CIRCLE" (list ear2 (+ y (/ earY 2))) earHoleRadius ""
		"._RECTANGLE" (list (- ear3 (/ earX 2)) y) (list (+ ear3 (/ earX 2)) (+ y earY)) ""
		"._CIRCLE" (list ear3 (+ y (/ earY 2))) earHoleRadius ""
		"._MOVE" "_ALL" "" (list ear1 (+ y (/ earY 2))) (list 0 0) ; Ear 1 = origo
	)
)
