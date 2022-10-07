;=========================================================================================================================
;
; ANYADM Ocs High Voltage Technical building
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; High voltage building or room

(defun ANYADM-HIGH-VOLTAGE-TECHNICAL-BUILDING ( / )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _OCS_ "TEB-" "TECHNICAL-BUILDING"			)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _OCS_ "TER-" "TEKNISK-BYGNING"			)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _OCS_ "BTT-" "BATIMENT-TECHNIQUE"			)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _OCS_ "GEB-" "GEBAEUDE"					)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _OCS_ "TEB-" "TECHNICAL-BUILDING"			)))
	)
	(cond
		((= _ADM_ _XXGL_) (setq description (strcat _OCS_NAME_ " TECHNICAL BUILDING"		)))
		((= _ADM_ _NOBN_) (setq description (strcat _OCS_NAME_ " TEKNISK BYGNING"			)))
		((= _ADM_ _FRSR_) (setq description (strcat _OCS_NAME_ " BATIMENT TECHNIQUE"		)))
		((= _ADM_ _DEDB_) (setq description (strcat _OCS_NAME_ " GEBAEUDE"					)))
		((= _ADM_ _JPTX_) (setq description (strcat _OCS_NAME_ " TECHNICAL BUILDING"		)))
	)
	(cond
		((= _ADM_ _XXGL_) 
		)
		((= _ADM_ _NOBN_)
			(TraceLevel3 "NOBN-TEKNISK-BEBYGGELSE-HOEYSPENTKIOSK")			(NOBN-TEKNISK-BEBYGGELSE-HOEYSPENTKIOSK)
		)
		((= _ADM_ _FRSR_) 
			; FR-SR actions:
		)
		((= _ADM_ _DEDB_) 
		)
		((= _ADM_ _JPTX_) 
			; TODO 2022-03-15 - Replace NOBN stuff with JPTX graphics:
			(TraceLevel3 "NOBN-TEKNISK-BEBYGGELSE-HOEYSPENTKIOSK")			(NOBN-TEKNISK-BEBYGGELSE-HOEYSPENTKIOSK)
		)
	)
	; Cleanup temp globals:
	(setq blockName nil description nil)
)



;================== NOBN functions ==================================================================
(defun NOBN-TEKNISK-BEBYGGELSE-HOEYSPENTKIOSK ( / x y )
	;
	; +-------------------+
	; |                   |
	; |         .         |
	; |                   |
	; +-------------------+
	;  Teknisk bygg - AT-kiosk 2930x3430
	;
    (setq 
		; Real size (as shown if drawing scale 4:1 is selected):
		x	3.430
		y	2.930
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(ScaleAll _four_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
