;=========================================================================================================================
;
; Alignment Characteristic Point.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Characteristic locations in track (gradient, tangent/curve/spiral, cant ramp)

(defun ALIGNMENT-CHARACTERISTIC-POINTS ( / )
	(ALIGNMENT-CHARACTERISTIC-POINT)
)



; Symmetrical short line transversal to track, to be centered on alignment axis (whereas insulated joints are located on one of the rails).
 (defun ALIGNMENT-CHARACTERISTIC-POINT ( / blockName description )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _TRK_ "DIV-" "CHARACTERISTIC-POINT"		)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _TRK_ "DIV-" "KARAKTERISTISK-PUNKT"		)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _TRK_ "DIV-" "POINT-CARACTERISTIQUE"		)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _TRK_ "DIV-" "CHARAKTERISTISCHER-PUNKT"	)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _TRK_ "DIV-" "CHARACTERISTIC-POINT"		)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "CHARACTERISTIC POINT" 			)))
		((= _ADM_ _NOBN_) (setq description (strcat "KARAKTERISTISK PUNKT" 			)))
		((= _ADM_ _FRSR_) (setq description (strcat "POINT CARACTERISTIQUE" 		)))
		((= _ADM_ _DEDB_) (setq description (strcat "CHARAKTERISTISCHER PUNKT"		)))
		((= _ADM_ _JPTX_) (setq description (strcat "CHARACTERISTIC POINT" 			)))
	)
	(command _LINE_ (list 0 (- _railHeadDistance_)) (list 0 _railHeadDistance_) _ENTER_ )
	(AddDescriptionBelowOrigin description _railHeadDistance_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
