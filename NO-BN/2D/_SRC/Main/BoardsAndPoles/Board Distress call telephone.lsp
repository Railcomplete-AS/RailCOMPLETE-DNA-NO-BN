;=========================================================================================================================
;
; Board Distress call telephone.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Distress call telephone



(defun BOARD-DISTRESS-CALL-TELEPHONE ( / blockName description x y p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 )
	;
	; +-----------------+
	; |   _____2_____   |
	; |  /           \  |
	; | 1   4_____5   3 |
	; | 6    8   9    7 |  Old type telephone handset, 5 arcs
	; |  12_10   11_13  |
	; |                 |
	; |      S O S      |  "SOS" fixed text at p14
	; +--------.--------+
	;
	(setq blockName (strcat _TEL_ "SCH-" "SKILT-TREDJEPERSON-NOEDTELEFON"	))
	(setq description (strcat "SCHILD DRITTE PERSON, N" _uOSLASH_ "DTELEFON"	))
	(setq
		x 4.5
		y 4.5
		p1  (list (* -0.328 x) (*  0.200 y))
		p2  (list (*  0.000 x) (*  0.344 y))
		p3  (list (*  0.328 x) (*  0.200 y))
		p4  (list (* -0.120 x) (*  0.211 y))
		p5  (list (*  0.120 x) (*  0.211 y))
		p6  (list (* -0.327 x) (*  0.128 y))
		p7  (list (*  0.327 x) (*  0.128 y))
		p8  (list (* -0.113 x) (*  0.184 y))
		p9  (list (*  0.113 x) (*  0.184 y))
		p10 (list (* -0.112 x) (*  0.156 y))
		p11 (list (*  0.112 x) (*  0.156 y))
		p12 (list (* -0.282 x) (*  0.073 y))
		p13 (list (*  0.282 x) (*  0.073 y))
		p14 (list (*  0.000 x) (* -0.167 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(DrawArc layDef_Zero p1 p2 p3)
	(DrawArc layDef_Zero p1 p6 p12)
	(DrawArc layDef_Zero p3 p7 p13)
	(DrawArc layDef_Zero p4 p8 p10)
	(DrawArc layDef_Zero p5 p9 p11)
	(DrawLine layDef_Zero p4 p5)
	(DrawLine layDef_Zero p12 p10)
	(DrawLine layDef_Zero p11 p13)
	(AddTextAtPoint layDef_Zero _th180_ p14 "SOS")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

