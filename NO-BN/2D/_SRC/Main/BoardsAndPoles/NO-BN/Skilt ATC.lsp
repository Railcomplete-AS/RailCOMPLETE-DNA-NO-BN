;=========================================================================================================================
;
; Skilt ATC.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-09-21 CLFEY Deprecated "ATC SLUTTER"
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; ATC operational state

; For debugging:
; (SKILT-ATC-INNKOBLET) (SKILT-ATC-UTKOBLET) (SKILT-ATC-SLUTTER)

(defun SKILT-ATC-INNKOBLET ( / blockName description x y p1 p2 )
	; End of ATC construction area (only SU balises plus a few others will be read by the train)
	;
	; +---------+
	; |  A T C  |
	; |INNKOBLET|
	; +----.----+
	; 
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-ATC-INNKOBLET"
		description "SKILT ATC INNKOBLET"
		x 9.0
		y 6.0
		p1 (list 0 (*  0.167 y))
		p2 (list 0 (* -0.25 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPos layDef_Zero _th250_ p1    "ATC"   )
	(AddTextAtPos layDef_Zero _th125_ p2 "INNKOBLET")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun SKILT-ATC-UTKOBLET ( / blockName description x y p1 p2 )
	; Start of ATC construction area (BU balises)
	;
	; +---------+
	; |  A T C  |
	; | UTKOBLET|
	; +----.----+
	; 
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-ATC-UTKOBLET"
		description "SKILT ATC UTKOBLET"
		x 9.0
		y 6.0
		p1 (list 0 (*  0.167 y))
		p2 (list 0 (* -0.25 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPos layDef_Zero _th250_ p1    "ATC"  )
	(AddTextAtPos layDef_Zero _th125_ p2 "UTKOBLET")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)


(defun SKILT-ATC-SLUTTER ( / blockName description x y p1 p2 )
	; End of ATC - transition into unequipped area
	;
	; +---------+
	; |  A T C  |
	; | SLUTTER |
	; +----.----+
	; 
	(setq 
		blockName "NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-ATC-SLUTTER"
		description "SKILT ATC UTKOBLET"
		x 9.0
		y 6.0
		p1 (list 0 (*  0.167 y))
		p2 (list 0 (* -0.25 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(AddTextAtPos layDef_Zero _th250_ p1   "ATC"  )
	(AddTextAtPos layDef_Zero _th125_ p2 "SLUTTER")
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
