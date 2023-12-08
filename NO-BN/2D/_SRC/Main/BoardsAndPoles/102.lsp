;=========================================================================================================================
;
; 102.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503 
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Arrow board

; For debugging:
; (102-1) (102-2) (102-3)

(defun 102-1 ( / blockName description x y )
	; Right arrow board
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-102-1-PIL-H"
		description (strcat "SKILT SIGNAL 102-1 PIL H" _uOSLASH_ "YRE")
		x 6.0
		y 3.0
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(DrawRightArrow x y)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 102-2 ( / blockName description x y )
	; Left arrow board
	(setq
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-102-2-PIL-V"
		description "SKILT SIGNAL 102-2 PIL VENSTRE"
		x 6.0
		y 3.0
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(DrawLeftArrow x y)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 102-3 ( / blockName description x y arrowShaftStartX arrowShaftLength arrowHeadX arrowHeadY )
	; Right and left arrow board
	(setq
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-102-3-PIL-HV"
		description "SKILT SIGNAL 102-3 PIL BEGGE VEIER"
		x 6.0
		y 3.0
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(DrawDoubleArrow x y)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
