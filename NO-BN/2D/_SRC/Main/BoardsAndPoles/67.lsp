;=========================================================================================================================
;
; 67.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Signal 67 - Typhoon (horn)

; For debugging:
; (67A) (67B) (67C) (67D) (67E)

(defun 67A ( / blockName description side )
	; 67A Orientation - "Orienteringssignal" - Sound horn - "Tog kommer"
	;
	;       +
	;     // \\
	;   //     \\  
	;  ++       ++ ; Losange with wipeout and fat lining
	;   \\     //
	;     \\ //
	;       .
	;
	(setq
		blockName "NO-BN-2D-JBTKO_SKT-SKILT-KJOERENDE-SIGNAL-67A-ORIENTERINGSIGNAL"
		description "SKILT SIGNAL 67A ORIENTERINGSIGNAL"
		side	(GetLosangeSide)	; Losange side (see 60.lsp)
	)
	(DrawLosangeWithLining)
	(MoveUp (* (DDcos _angle45_) side))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 67B ( / blockName description side x y p1 p2 p3 p4 p5 p6 )
	; 67B Orientation for level crossing - "Orienteringssignal for planovergang" - Sound horn - "Tog kommer"
	;       +
	;     /p1 \
	;   /p2--p3 \  
	;  +         +  ; Losange with wipeout and fat lining plus horizontal bar
	;   \p4--p5 /
	;     \p6 /
	;       .
	;
	(setq 
		blockName "NO-BN-2D-JBTKO_SKT-SKILT-KJOERENDE-SIGNAL-67B-ORIENTERINGSIGNAL-PLANOVERGANG"
		description "SKILT SIGNAL 67B ORIENTERINGSIGNAL FOR PLANOVERGANG"
		side	(GetLosangeSide)	; Losange side
		x		(* (sqrt 2) side)
		y		(* (sqrt 2) side)
		p1		(list (*  0.000 x) (*  0.450 y))
		p2		(list (* -0.433 x) (*  0.017 y))
		p3		(list (*  0.433 x) (*  0.017 y))
		p4		(list (*  0.433 x) (* -0.017 y))
		p5		(list (* -0.433 x) (* -0.017 y))
		p6		(list (*  0.000 x) (* -0.450 y))
	)
	(DrawLosange)
	(command _POLYLINE_ p1 p2 p3 _closedPolyline_)
	(command _POLYLINE_ p4 p5 p6 _closedPolyline_)
	(DrawHatch _solidHatch_)
	(MoveUp (* (DDcos _angle45_) side))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 67C ( / blockName description side x y p1 p2 p3 p4 p5 p6 )
	; 67C Orientation for passenger exchange - "Orienteringssignal for holdeplass" - Sound horn - "Tog kommer"
	;       +
	;     /   \
	;   //p3 p4\\  
	;  +p1 | | p6+  ; Losange with wipeout and fat lining plus vertical bar
	;   \\p2 p5//
	;     \   /
	;       .
	(setq
		blockName "NO-BN-2D-JBTKO_SKT-SKILT-KJOERENDE-SIGNAL-67C-ORIENTERINGSIGNAL-HOLDEPLASS"
		description "SKILT SIGNAL 67C ORIENTERINGSIGNAL FOR HOLDEPLASS"
		side	(GetLosangeSide)	; Losange side
		x		(* (sqrt 2) side)
		y		(* (sqrt 2) side)
		p1		(list (* -0.450 x) (*  0.000 y))
		p2		(list (* -0.017 x) (* -0.433 y))
		p3		(list (* -0.017 x) (*  0.433 y))
		p4		(list (*  0.017 x) (*  0.433 y))
		p5		(list (*  0.017 x) (* -0.433 y))
		p6		(list (*  0.450 x) (*  0.000 y))
	)
	(DrawLosange)
	(command _POLYLINE_ p1 p2 p3 _closedPolyline_)
	(command _POLYLINE_ p4 p5 p6 _closedPolyline_)
	(DrawHatch _solidHatch_)
	(MoveUp (* (DDcos _angle45_) side))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 67D ( / blockName description side d a h p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 )
	; 67D Orientation for level crossing and passenger exchange - "Orienteringssignal for planovergang og holdeplass"
	; Sound horn - "Tog kommer"
	;       +
	;     /1 4\
	;   /2 3 5 6\  
	;  +         +  ; Losange with wipeout and fat lining plus vertical bar
	;   \7 8 1011
	;     \9 12
	;       .
	(setq
		blockName "NO-BN-2D-JBTKO_SKT-SKILT-KJOERENDE-SIGNAL-67D-ORIENTERINGSIGNAL-PLANOVERGANG-HOLDEPLASS"
		description "SKILT SIGNAL 67D ORIENTERINGSIGNAL FOR PLANOVERGANG OG HOLDEPLASS"
		side	(GetLosangeSide)	; Losange side
		d		(* 0.05 side)
		a		(- (* (sqrt 0.5) side) (* (+ (sqrt 2) 0.5) d))
		h		(* 0.5 d)
		p1		(list (- h) (+ a))
		p2		(list (- a) (+ h))
		p3		(list (- h) (+ h))

		p4		(list (+ h) (+ a))
		p5		(list (+ h) (+ h))
		p6		(list (+ a) (+ h))

		p7		(list (- a) (- h))
		p8		(list (- h) (- h))
		p9		(list (- h) (- a))

		p10		(list (+ h) (- h))
		p11		(list (+ a) (- h))
		p12		(list (+ h) (- a))
	)
	(DrawLosange)
	(command _POLYLINE_ p1 p2 p3 _closedPolyline_)
	(command _POLYLINE_ p4 p5 p6 _closedPolyline_)
	(command _POLYLINE_ p7 p8 p9 _closedPolyline_)
	(command _POLYLINE_ p10 p11 p12 _closedPolyline_)
	(DrawHatch _solidHatch_)
	(MoveUp (* (DDcos _angle45_) side))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 67E ( / blockName description x y)
	; Day / night - do not use typhooon by night
	;
	; TL-------TR
	; |   ___ / |
	; | / Black |
	; BL---.---BR 
	;
	(setq 
		blockName "NO-BN-2D-JBTKO_SKT-SKILT-KJOERENDE-SIGNAL-67E-ORIENTERINGSIGNAL-DAGTID"
		description "SKILT SIGNAL 67E ORIENTERINGSIGNAL DAGTID"
		x 4.5
		y 3.0
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(command _POLYLINE_ (PosBL x y) (PosBR x y ) (PosTR x y) _closedPolyline_)
	(DrawHatchAtPoint _solidHatch_ _slightlyBelow_ _angleZero_ _offsetZero_)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
