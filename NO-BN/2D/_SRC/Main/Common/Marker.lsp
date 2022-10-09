;=========================================================================================================================
;
; Marker.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Marker for positions and other stuff which needs a pin-needle symbol and a text in a drawing.

(defun MARKERS ( / blockName description att )
	;     _____
	;    /     \
	;   (       )
	;    \     / 
	;     /  /
	;    / /
	;   //
	;  .
	;
	(setq blockName (strcat _COM_ "MRK-" "MARKOER"	))
	(setq description (strcat "MARKOER"				))

	(command _CIRCLE_ "1,1" "0.4")
	(command _LINE_ _origin_ "0.7563,0.6828" _ENTER_)
	(command _LINE_ _origin_ "0.6828,0.7563" _ENTER_)
	(AddDescriptionBelowOrigin description _one_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
