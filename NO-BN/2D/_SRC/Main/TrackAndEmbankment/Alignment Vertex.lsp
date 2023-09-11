;=========================================================================================================================
;
; Alignment Vertex.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Characteristic locations in track (gradient, tangent/curve/spiral, cant ramp)

(defun ALIGNMENT-VERTICES ( / )
	(ALIGNMENT-VERTEX)
)



; Symmetrical short line transversal to track, to be centered on alignment axis (whereas isolated joints are located on one of the rails).
 (defun ALIGNMENT-VERTEX ( / blockName description )
	(setq blockName (strcat _TRK_ "DIV-" "KARAKTERISTISK-PUNKT"	))
	(setq description (strcat "KARAKTERISTISK PUNKT" 			))
	(command _LINE_ (list 0 (- _railHeadDistance_)) (list 0 _railHeadDistance_) _ENTER_ )
	(AddDescriptionBelowOrigin description _railHeadDistance_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
