;=========================================================================================================================
;
; Low Voltage Transfomer.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Low-voltage transformer

(defun C:LOW-VOLTAGE-TRANSFORMER ( / )
	(TRANSFORMATOR-H-TIL-L) ; Fra høyspent til lavspent (15 kV til 220 VAC)
)



(defun TRANSFORMATOR-H-TIL-L ( / blockName description x y r dx p1 p2 )
	;
	; TL------------TR
	; |   _______    |
	; |  /  / \  \   |
	; | (  1 . 2  )  |  Circles at p1 and p2 inside box
	; |  \__\_/__/   |
	; |              |
	; BL------------BR
	;
	(setq
		blockName	"NO-BN-2D-JBTEL-TRANSFORMATOR"
		description "LAVSPENT TRANSFORMATOR"
		x 2.0
		y 1.0
		r (* 0.4 y)
		dx (* r 0.6)
		p1 (list (- dx) 0)
		p2 (list (+ dx) 0)
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawCircleAtPos layDef_Zero p1 r _noWipeout_)
	(DrawCircleAtPos layDef_Zero p2 r _noWipeout_)
	(AddDescriptionBelowOrigo description y)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
