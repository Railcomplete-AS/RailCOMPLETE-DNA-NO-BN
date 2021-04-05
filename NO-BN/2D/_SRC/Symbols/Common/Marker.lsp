;=========================================================================================================================
;
; Marker.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Marker for positions and other stuff which needs a pin-needle symbol and a text in a drawing.

(defun C:MARKER ( / blockName description att )
	(setq
		blockName "NO-BN-2D-JBTFE-MARKER-HAIRPIN"
		description "MARKER"
		att '("FARGE" "Farge" "" "1,2")
	)
	(command _CIRCLE_ "1,1" "0.4")
	(command _LINE_ _origo_ "0.7563,0.6828" _ENTER_)
	(command _LINE_ _origo_ "0.6828,0.7563" _ENTER_)
	(AddTextAttributeAtPos layDef_Zero _th100_ _origo_ att)
	(AddDescriptionBelowOrigo description _one_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
