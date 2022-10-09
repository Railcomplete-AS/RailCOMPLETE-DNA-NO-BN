;=========================================================================================================================
;
; Skilt Bevegelig Kryss.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Moveable point frog (heart)

; For debugging:
; (SKILT-BEVEGELIG-KRYSS)

(defun SKILT-BEVEGELIG-KRYSS ( / blockName description x y p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 pA pB )
	; Movable point frog
	;
	; TL-4----5-TR
	; 8\  \  /  /1
	; | \  6   / | "Bevegelig"
	; |  9    3  |   "kryss"
	; | /  12  \ |
	; 7/  /  \  \2
	; BL11-.--10BR
	; 
	(setq 
		blockName "NO-BN-2D-JBTSK_SKV-SKILT-VEDLIKEHOLD-BEVEGELIG-KRYSS"
		description "SKILT BEVEGELIG KRYSS"
		x 9.6
		y 4.8
		p1 (list (*  0.50 x) (*  0.35 y))    p4 (list (* -0.35 x) (*  0.50 y))    p7 (list (* -0.50 x) (* -0.35 y))    p10 (list (*  0.35 x) (* -0.50 y))
		p2 (list (*  0.50 x) (* -0.35 y))    p5 (list (*  0.35 x) (*  0.50 y))    p8 (list (* -0.50 x) (*  0.35 y))    p11 (list (* -0.35 x) (* -0.50 y))
		p3 (list (*  0.15 x) (*  0.00 y))    p6 (list (*  0.00 x) (*  0.15 y))    p9 (list (* -0.15 x) (*  0.00 y))    p12 (list (*  0.00 x) (* -0.15 y))
		pA '(0 3.6)
		pB '(0 1.6)
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(command _POLYLINE_ p1 p2 p3 _closedPolyline_)
	(command _POLYLINE_ p4 p5 p6 _closedPolyline_)
	(command _POLYLINE_ p7 p8 p9 _closedPolyline_)
	(command _POLYLINE_ p10 p11 p12 _closedPolyline_)
	(MoveUp (HalfOf y))
	(AddTextAtPos layDef_Zero _th180_ pA "Bevegelig")
	(AddTextAtPos layDef_Zero _th180_ pB   "kryss"  )
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
