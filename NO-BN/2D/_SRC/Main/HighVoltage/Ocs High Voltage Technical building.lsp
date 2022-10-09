;=========================================================================================================================
;
; Ocs High Voltage Technical building
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
	(TraceLevel3 "NOBN-TEKNISK-BEBYGGELSE-HOEYSPENTKIOSK")			(NOBN-TEKNISK-BEBYGGELSE-HOEYSPENTKIOSK)
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
	(setq blockName (strcat _OCS_ "TER-" "TEKNISK-BYGNING"	))
	(setq description (strcat _OCS_NAME_ " TEKNISK BYGNING"	))
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
