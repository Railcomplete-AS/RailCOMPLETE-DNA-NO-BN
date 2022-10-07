;=========================================================================================================================
;
; ANYADM Wayside telephone.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Telephone

(defun ANYADM-WAYSIDE-TELEPHONE ( / blockName description x y gx gy )
	; Box (geo with text "T"), centre top is insertion point.
	;
	;  TL--.--TR
	;  |  "T"  |
	;  BL-----BR
	;
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _TEL_ "TEL-" "WAYSIDE-TELEPHONE"		)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _TEL_ "TLT-" "TOGLEDERTELEFON"		)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _TEL_ "TEL-" "TELEPHONE-AU-SIGNAL"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _TEL_ "TEL-" "STRECKEN-TELEFON"		)))
	)
	(cond
		((= _ADM_ _XXGL_) (setq description (strcat "WAYSIDE TELEPHONE"		)))
		((= _ADM_ _NOBN_) (setq description (strcat "TOGLEDERTELEFON"		)))
		((= _ADM_ _FRSR_) (setq description (strcat "TELEPHONE AU SIGNAL"	)))
		((= _ADM_ _DEDB_) (setq description (strcat "STRECKEN-TELEFON"		)))
	)
	(setq 
		x 3.0
		y 3.0
	)
	; Schematic symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	(MoveDown (HalfOf y))
	(AddDescriptionBelowOrigin description y)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	(AddTextAtPos layDef_Zero (* 0.8 y) _origin_ "T")
	(MoveDown (HalfOf y))
	(AddDescriptionBelowOrigin description y)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)
