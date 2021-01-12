;=========================================================================================================================
;
; 101.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved. 
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-08-08 CLFEY Removed deprecated 101-09 (2-line ERTMS ID board, use ordinary symbol instead)
; 2020-08-08 CLFEY Removed deprecated 101-10 (3-line ERTMS ID board, use ordinary symbol instead)
; 2021-01-17 CLFEY Release 2021.a
;
; TODO list:
; 2020-09-13 CLFEY The old 101-4 ERTMS ID board will be superseded by 101-9..101-14 ID-boards
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Signal ID boards

; For debugging:
; (101-1) (101-2) (101-3) (101-5) (101-6) (101-7) (101-8)

(defun 101-1 ( / blockName description x y att1 att2 att3 p1 p2 p3 )
	; 3 lines for main signals and distant signals
	;
	; TL-----TR
	; |   M   | p1
	; | 15894 | p2
	; |  XYZ  | p3
	; BL--.--BR
	;
	(setq
		blockName 	"NO-BN-2D-SKILT-KJOERENDE-SIGNAL-101-1-ID-3-LINJER"
		description	"SKILT SIGNAL 101-1 IDENTIFIKASJONSSKILT, 3 LINJER"
		x	6.0
		y	9.0
		att1 '(
			"ID_BOKSTAV"
			(strcat "ID-skilt " _oe_ "vre linje (bokstav)")
			"M"
		)
		att2 '(
			"ID_NUMMER"
			"ID-skilt midtre linje (signalnummer)"
			"15894"
		)
		att3 '(
			"ID_STED"
			"ID-skilt nedre linje (stedsforkortelse)"
			"XYZ"
		)
		p1 (list (*  0.00 x) (*  0.35 y))
		p2 (list (*  0.00 x) (*  0.05 y))
		p3 (list (*  0.00 x) (* -0.25 y))
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAttributeAtPos layer_Zero _th180_ p1 att1)
	(addTextAttributeAtPos layer_Zero _th180_ p2 att2)
	(addTextAttributeAtPos layer_Zero _th180_ p3 att3)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)

	description ; Used if table is created
)



(defun 101-2 ( / blockName description x y att1 att2 p1 p2 )
	; 2 lines for main signals and distant signals, or for combined letter + number in the top line
	;
	; TL-------TR
	; |  15894  | p1
	; |   XYZ   | p2
	; BL---.---BR
	;
	; or:
	;
	; TL-------TR
	; | M 15894 | p2 - i.e. putting more info in the first text line
	; |   XYZ   | p3
	; BL---.---BR
	;
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-101-2-ID-2-LINJER"
		description "SKILT SIGNAL 101-2 IDENTIFIKASJONSSKILT, 2 LINJER"
		x	9.0
		y	6.0
		att1 '(
			"ID_BOKSTAV_OG_NR"
			(strcat "ID-skilt " _oe_ "vre linje (Evt. bokstav, signalnummer)")
			"M 15894"
		)
		att2 '(
			"ID_STED"
			"ID-skilt nedre linje (stedsforkortelse)"
			"XYZ"
		)
		p1 (list (*  0.00 x) (*  0.21 y))
		p2 (list (*  0.00 x) (* -0.21 y))
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAttributeAtPos layer_Zero _th180_ p1 att1)
	(addTextAttributeAtPos layer_Zero _th180_ p2 att2)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 101-3 ( / blockName description r y att1 att2 att3 p1 p2 p3 )
	; Round, 3 lines for block signals and for distant signals on the line block
	;      _____ 
	;     /  A  \  
	;    ( 15891 ) . = center circle
	;     \ ZYX / 
	;       ---
	;
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-101-3-ID-RUNDT"
		description "SKILT SIGNAL 101-3 IDENTIFIKASJONSSKILT, RUNDT"
		r	4.5	; schematic Ø9
		y (* 2 r)
		att1 '(
			"ID_BOKSTAV"
			(strcat "ID-SKILT " _uOE_ "VRE LINJE (LITRA)")
			"A"
		)
		att2 '(
			"ID_NUMMER"
			"ID-SKILT MIDTRE LINJE (SIGNALNUMMER)"
			"15891"
		)
		att3 '(
			"ID_STED"
			"ID-SKILT NEDRE LINJE (STEDSFORKORTELSE)"
			"ZYX"
		)
		p1 (list 0 (*  0.30 y))
		p2 (list 0 (*  0.05 y))
		p3 (list 0 (* -0.20 y))
	)
	(drawCircle layer_Zero r layer_BoardOrPole_Wipeout)
	(addTextAttributeAtPos layer_Zero _th180_ p1 att1)
	(addTextAttributeAtPos layer_Zero _th180_ p2 att2)
	(addTextAttributeAtPos layer_Zero _th180_ p3 att3)
	(moveUp r)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



; TODO 2020-09-13 CLFEY The old 101-4 ERTMS ID board will be superseded by 101-9..101-14 ID-boards
;;; (defun 101-4 ...



(defun 101-5 ( / blockName description x y att1 )
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-101-5-ID-MINDRE-SIGNAL-180x100"
		description "SKILT SIGNAL 101-5 IDENTIFIKASJONSSKILT, MINDRE SIGNAL 180x100"
		x 4.5
		y 2.0
		att1 '(
			"ID_MINDRE_SIGNAL"
			"ID-skilt mindre signal signalnummer"
			"R12"
		)
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAttributeAtPos layer_Zero _th125_ (pos11 y) att1)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 101-6 ( / blockName description x y att1 )
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-101-6-ID-MINDRE-SIGNAL-300x100"
		description "SKILT SIGNAL 101-6 IDENTIFIKASJONSSKILT, MINDRE SIGNAL 300x100"
		x 6.0
		y 2.0
		att1 '(
			"ID_MINDRE_SIGNAL"
			"ID-skilt mindre signal signalnummer"
			"R1234"
		)
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAttributeAtPos layer_Zero _th125_ (pos11 y) att1)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 101-7 ( / blockName description x y att1 )
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-101-7-ID-MINDRE-SIGNAL-200x300"
		description "SKILT SIGNAL 101-7 IDENTIFIKASJONSSKILT W PLANOVERGANG 200x300"
		x 2.0
		y 3.0
		att1 '(
			"ID_MINDRE_SIGNAL"
			"ID-skilt planovergang"
			"W"
		)
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAttributeAtPos layer_Zero _th125_ (pos11 y) att1)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 101-8 ( / blockName description x y att1 )
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-101-8-ID-MINDRE-SIGNAL-300x200"
		description (strcat "SKILT SIGNAL 101-8 IDENTIFIKASJONSSKILT Z H" _uOE_ "YT SKIFTESIGNAL 300x200")
		x 3.0
		y 2.0
		att1 '(
			"ID_MINDRE_SIGNAL"
			"ID-skilt hoeyt skiftesignal signalnummer"
			"Z"
		)
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAttributeAtPos layer_Zero _th125_ (pos11 y) att1)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
