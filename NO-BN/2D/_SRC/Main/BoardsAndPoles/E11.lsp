;=========================================================================================================================
;
; E11.lsp
;
; Copyright (c) 2015-2026 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2026-02-12 New sign E11. Emergency stop mode confirmed.
;
;=========================================================================================================================
 
; Emergency stop mode confirmed
 
;=================================================================================================================================
; Signal E11 «Nødstoppmodus bekreftet»
;
; Vertikalt rektangulært skilt med tre sirkulære indikatorer (som et trafikklyssignal).
; Sort bakgrunn med tre hvite sirkler plassert vertikalt.
;
; Signal							Signalnummer og signalnavn			Signalbetydning
; ------------------------------	------------------------------		------------------------------
; Sort rektangulært skilt med		Signal E11							Nødstoppmodus er bekreftet.
; tre hvite sirkler.				«Nødstoppmodus bekreftet»
;=================================================================================================================================
 
 
 
(defun E11 ( / blockName description x y r p1 p2 p3 )
	;
	; +-----+
	; |     |
	; | (o) |  p1 - top circle
	; |     |
	; | (o) |  p2 - middle circle
	; |     |
	; | (o) |  p3 - bottom circle
	; |     |
	; +--.--+
	;
	(setq
		blockName "NO-BN-2D-JBTSA_MSS-SKILT-KJOERENDE-SIGNAL-E11-NOEDSTOPPMODUS-BEKREFTET"
		description (strcat "SKILT ERTMS E11 N" _uOSLASH_ "DSTOPPMODUS BEKREFTET")
		x 4.0
		y 8.0
		r 1.0
		p1 (list 0 (Point31 y))
		p2 (list 0 (Point32 y))
		p3 (list 0 (Point33 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(DrawCircleAtPoint layDef_Zero p1 r _noWipeout_)
	(DrawCircleAtPoint layDef_Zero p2 r _noWipeout_)
	(DrawCircleAtPoint layDef_Zero p3 r _noWipeout_)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)