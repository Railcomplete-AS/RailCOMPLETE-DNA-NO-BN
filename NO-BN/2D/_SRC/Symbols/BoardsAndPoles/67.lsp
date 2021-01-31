;=========================================================================================================================
;
; 67.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
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
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-67A-ORIENTERINGSIGNAL"
		description "SKILT SIGNAL 67A ORIENTERINGSIGNAL"
		side	(getLosangeSide)	; Losange side (see 60.lsp)
	)
	(drawLosangeWithLining)
	(drawHatch _filledHatch_)
	(moveUp (* (DDcos 45) side))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 67B ( / blockName description side x y p1 p2 p3 p4 p5 p6 )
	; 67B Orientation for level crossing - "Orienteringssignal for planovergang" - Sound horn - "Tog kommer"
	;       +
	;     /p1 \
	;   /p2--p3\  
	;  +        +  ; Losange with wipeout and fat lining plus horizontal bar
	;   \p4--p5/
	;     \p6 /
	;       .
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-67B-ORIENTERINGSIGNAL-PLANOVERGANG"
		description "SKILT SIGNAL 67B ORIENTERINGSIGNAL FOR PLANOVERGANG"
		side	(getLosangeSide)	; Losange side
		x		(* (sqrt 2) side)
		y		(* (sqrt 2) side)
		p1		(list (*  0.000 x) (*  0.450 y))
		p2		(list (* -0.433 x) (*  0.017 y))
		p3		(list (*  0.433 x) (*  0.017 y))
		p4		(list (*  0.433 x) (* -0.017 y))
		p5		(list (* -0.433 x) (* -0.017 y))
		p6		(list (*  0.000 x) (* -0.450 y))
	)
	(drawLosange)
	(command "._PLINE" p1 p2 p3 _closed_)
	(command "._PLINE" p4 p5 p6 _closed_)
	(drawHatchFromPoint _filledHatch_ _origo_ 0 0)
	(moveUp (* (DDcos 45) side))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
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
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-67C-ORIENTERINGSIGNAL-HOLDEPLASS"
		description "SKILT SIGNAL 67C ORIENTERINGSIGNAL FOR HOLDEPLASS"
		side	(getLosangeSide)	; Losange side
		x		(* (sqrt 2) side)
		y		(* (sqrt 2) side)
		p1		(list (* -0.450 x) (*  0.000 y))
		p2		(list (* -0.017 x) (* -0.433 y))
		p3		(list (* -0.017 x) (*  0.433 y))
		p4		(list (*  0.017 x) (*  0.433 y))
		p5		(list (*  0.017 x) (* -0.433 y))
		p6		(list (*  0.450 x) (*  0.000 y))
	)
	(drawLosange)
	(command "._PLINE" p1 p2 p3 _closed_)
	(command "._PLINE" p4 p5 p6 _closed_)
	(drawHatchFromPoint _filledHatch_ _origo_ 0 0)
	(moveUp (* (DDcos 45) side))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 67D ( / blockName description side d a h x y p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 )
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
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-67D-ORIENTERINGSIGNAL-PLANOVERGANG-HOLDEPLASS"
		description "SKILT SIGNAL 67D ORIENTERINGSIGNAL FOR PLANOVERGANG OG HOLDEPLASS"
		side	(getLosangeSide)	; Losange side
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
	(drawLosange)
	(command "._PLINE" p1 p2 p3 _closed_)
	(command "._PLINE" p4 p5 p6 _closed_)
	(command "._PLINE" p7 p8 p9 _closed_)
	(command "._PLINE" p10 p11 p12 _closed_)
	(drawHatchFromPoint _filledHatch_ _origo_ 0 0)
	(moveUp (* (DDcos 45) side))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
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
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-67E-ORIENTERINGSIGNAL-DAGTID"
		description "SKILT SIGNAL 67E ORIENTERINGSIGNAL DAGTID"
		x 4.5
		y 3.0
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(command "._PLINE" (posBL x y) (posBR x y ) (posTR x y) _closed_)
	(drawHatchFromPoint _filledHatch_ _slightlyBelow_ 0 0)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
