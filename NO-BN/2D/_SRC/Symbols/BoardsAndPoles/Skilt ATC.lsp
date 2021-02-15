;=========================================================================================================================
;
; Skilt ATC.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-09-21 CLFEY Deprecated "ATC SLUTTER"
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; ATC operational state

; For debugging:
; (SKILT_ATC_INNKOBLET) (SKILT_ATC_UTKOBLET) (SKILT_ATC_SLUTTER)

(defun Skilt_ATC_Innkoblet ( / blockName description x y p1 p2 )
	; End of ATC construction area (only SU balises plus a few others will be read by the train)
	;
	; +---------+
	; |  A T C  |
	; |INNKOBLET|
	; +----.----+
	; 
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-ATC-INNKOBLET"
		description "SKILT ATC INNKOBLET"
		x 9.0
		y 6.0
		p1 (list 0 (*  0.167 y))
		p2 (list 0 (* -0.25 y))
	)
	(drawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(addTextAtPos layDef_Zero _th250_ p1    "ATC"   )
	(addTextAtPos layDef_Zero _th125_ p2 "INNKOBLET")
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun Skilt_ATC_utkoblet ( / blockName description x y p1 p2 )
	; Start of ATC construction area (BU balises)
	;
	; +---------+
	; |  A T C  |
	; | UTKOBLET|
	; +----.----+
	; 
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-ATC-UTKOBLET"
		description "SKILT ATC UTKOBLET"
		x 9.0
		y 6.0
		p1 (list 0 (*  0.167 y))
		p2 (list 0 (* -0.25 y))
	)
	(drawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(addTextAtPos layDef_Zero _th250_ p1    "ATC"  )
	(addTextAtPos layDef_Zero _th125_ p2 "UTKOBLET")
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)


(defun Skilt_ATC_Slutter ( / blockName description x y p1 p2 )
	; End of ATC - transition into unequipped area
	;
	; +---------+
	; |  A T C  |
	; | SLUTTER |
	; +----.----+
	; 
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-ATC-SLUTTER"
		description "SKILT ATC UTKOBLET"
		x 9.0
		y 6.0
		p1 (list 0 (*  0.167 y))
		p2 (list 0 (* -0.25 y))
	)
	(drawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(addTextAtPos layDef_Zero _th250_ p1   "ATC"  )
	(addTextAtPos layDef_Zero _th125_ p2 "SLUTTER")
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
