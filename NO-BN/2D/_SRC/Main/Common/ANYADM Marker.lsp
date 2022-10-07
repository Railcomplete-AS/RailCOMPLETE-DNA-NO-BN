;=========================================================================================================================
;
; ANYADM Marker.lsp
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
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _COM_ "MRK-" "MARKER"		)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _COM_ "MRK-" "MARKOER"		)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _COM_ "MRQ-" "MARQUEUR"		)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _COM_ "MRK-" "MARKIERUNG"	)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _COM_ "MRK-" "MARKER"		)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "MARKER"		)))
		((= _ADM_ _NOBN_) (setq description (strcat "MARKOER"		)))
		((= _ADM_ _FRSR_) (setq description (strcat "MARQUEUR"		)))
		((= _ADM_ _DEDB_) (setq description (strcat "MARKIERUNG"	)))
		((= _ADM_ _JPTX_) (setq description (strcat "MARKER"		)))
	)
	(command _CIRCLE_ "1,1" "0.4")
	(command _LINE_ _origin_ "0.7563,0.6828" _ENTER_)
	(command _LINE_ _origin_ "0.6828,0.7563" _ENTER_)
	(AddDescriptionBelowOrigin description _one_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
