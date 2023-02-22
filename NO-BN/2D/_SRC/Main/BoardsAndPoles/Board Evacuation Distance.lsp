;=========================================================================================================================
;
; Board Evacuation Distance.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Distance to nearest escape route

; For debugging:
; (BOARD-EVACUATION-DISTANCE-LEFT) (BOARD-EVACUATION-DISTANCE-RIGHT) (BOARD-EVACUATION-DISTANCE-BOTH)



(defun BOARD-EVACUATION-DISTANCE ( / )
	(TraceLevel3 "BOARD-EVACUATION-DISTANCE-LEFT")		(BOARD-EVACUATION-DISTANCE-LEFT)
	(TraceLevel3 "BOARD-EVACUATION-DISTANCE-RIGHT")		(BOARD-EVACUATION-DISTANCE-RIGHT)
	(TraceLevel3 "BOARD-EVACUATION-DISTANCE-BOTH")	 	(BOARD-EVACUATION-DISTANCE-BOTH)
)



(defun BOARD-EVACUATION-DISTANCE-LEFT ( / blockName description x y p0 p1 p3 attMetersLeft )
	; Distance to nearest escape towards the left
	;
	; +-------------+
	; | ESCAPE DIST |
	; |     <==     | ; p1 = tip
	; |     275     | ; p3
	; +------.------+
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKP-SKILT-TREDJEPERSON-ROEMNINGSVEI-V"
		description (strcat "R" _uOSLASH_ "MNINGSAVSTAND MOT VENSTRE")
		x 12.0
		y 9.0
		p0 (list (*  0.00 x) (*  0.3 y))
		p1 (list (* -0.15 x) (*  0.0 y))
		p3 (list (*  0.00 x) (* -0.3 y))
		attMetersLeft	'("VDIST" (strcat "Avstand til r" _OSLASH_ "mning mot venstre") "275")
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)

	(AddTextAtPoint layDef_Zero _th125_ p0 (strcat "R" _uOSLASH_ "MNINGSVEI"))
	(DrawHollowArrowAtpoint x y p1 _west_)
	(AddTextAttributeAtPoint layDef_Zero _th180_ p3 attMetersLeft)

	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun BOARD-EVACUATION-DISTANCE-RIGHT ( / blockName description x y p0 p2 p4 attMetersRight )
	; Distance to nearest escape towards the right
	;
	; +-------------+
	; | ESCAPE DIST |
	; |     ==>     | ; p2 = tip
	; |     625     | ; p4
	; +------.------+
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKP-SKILT-TREDJEPERSON-ROEMNINGSVEI-H"
		description (strcat "R" _uOSLASH_ "MNINGSAVSTAND MOT H" _uOSLASH_ "YRE")
		x 12.0
		y 9.0
		p0 (list (*  0.00 x) (*  0.3 y))
		p2 (list (*  0.15 x) (*  0.0 y))
		p4 (list (*  0.00 x) (* -0.3 y))
		attMetersRight	'("HDIST" (strcat "Avstand til r" _OSLASH_ "mning mot h" _OSLASH_ "yre") "625")
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)

	(AddTextAtPoint layDef_Zero _th125_ p0 (strcat "R" _uOSLASH_ "MNINGSVEI"))
	(DrawHollowArrowAtpoint x y p2 _east_)
	(AddTextAttributeAtPoint layDef_Zero _th180_ p4 attMetersRight)

	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun BOARD-EVACUATION-DISTANCE-BOTH ( / blockName description x y p0 p1 p2 p3 p4 attMetersLeft attMetersRight )
	; Distance to nearest escape towards the right
	;
	; +-------------+
	; | RØMNINGSVEI | ; p0
	; |  <==   ==>  | ; p1 p2
	; |  275   625  | ; p3 p4
	; +------.------+
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKP-SKILT-TREDJEPERSON-ROEMNINGSVEI-VH"
		description (strcat "R" _uOSLASH_ "MNINGSAVSTAND MOT VENSTRE OG H" _uOSLASH_ "YRE")
		x 12.0
		y 9.0
		p0 (list (*  0.0  x) (*  0.3 y))
		p1 (list (* -0.4  x) (*  0.0 y))
		p3 (list (* -0.25 x) (* -0.3 y))
		p2 (list (*  0.4  x) (*  0.0 y))
		p4 (list (*  0.25 x) (* -0.3 y))
		attMetersLeft	'("VDIST" (strcat "Avstand til r" _OSLASH_ "mning mot venstre") "275")
		attMetersRight	'("HDIST" (strcat "Avstand til r" _OSLASH_ "mning mot h" _OSLASH_ "yre") "625")
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)

	(AddTextAtPoint layDef_Zero _th125_ p0 (strcat "R" _uOSLASH_ "MNINGSVEI"))
	(DrawHollowArrowAtpoint x y p1 _west_)
	(AddTextAttributeAtPoint layDef_Zero _th180_ p3 attMetersLeft)
	(DrawHollowArrowAtpoint x y p2 _east_)
	(AddTextAttributeAtPoint layDef_Zero _th180_ p4 attMetersRight)

	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
