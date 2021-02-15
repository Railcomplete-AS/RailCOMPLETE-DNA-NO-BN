;=========================================================================================================================
;
; UPS Distribution.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; UPS distribution panel

(defun C:UPS-DISTRIBUTION ( )
	(UPS-FORDELER)
)



(defun UPS-FORDELER ( / blockName description x y )
	;
	;  TL-------TR
	;  | \ 5\/8\ |  567 and 8910 = two arcs
	;  |  \      |
 	;  |   \.    |
	;  |     \   |
	;  | 1-2  \  |
	;  | 3-4   \ |
	;  BL-------BR
	;
		(setq
		blockName "NO-BN-2D-JBTEL-UPS-FORDELER"
		description "LAVSPENT UPS FORDELER"
		x 1.0
		y 1.0
		p1	(list (* -0.4 x) (* -0.2 y))
		p2	(list (*  0.0 x) (* -0.2 y))
		p3	(list (* -0.4 x) (* -0.3 y))
		p4	(list (*  0.0 x) (* -0.3 y))
					 
		p5	(list (* 0.00 x) (* 0.25 y))
		p6	(list (* 0.10 x) (* 0.15 y))
		p7	(list (* 0.20 x) (* 0.20 y))

		p8	(list (* 0.20 x) (* 0.20 y))
		p9	(list (* 0.30 x) (* 0.25 y))
		p10	(list (* 0.40 x) (* 0.15 y))
	)
	(drawBox layDef_Zero x y _noWipeout_)
	(drawLine layDef_Zero (posTL x y) (posBR x y))
	(drawLine layDef_Zero p1 p2)
	(drawLine layDef_Zero p3 p4)
	(drawArc layDef_Zero p5 p6 p7)
	(drawArc layDef_Zero p8 p9 p10)
	(addDescriptionBelowOrigo description (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
