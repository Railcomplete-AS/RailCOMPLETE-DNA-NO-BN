;=========================================================================================================================
;
; Track Axis Critical Location.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Characteristic locations in track (gradient, tangent/curve/spiral, cant ramp)

(defun C:TRACK-AXIS-CRITICAL-LOCATION ( / )
	(DrawAlignmentCharacteristicLocation)
)



; Symmetrical short line transversal to track, to be centered on alignment axis (whereas insulated joints are located on one of the rails).
 (defun DrawAlignmentCharacteristicLocation ( / blockName description )
	(setq 
		blockName "NO-BN-2D-JBTOB-TRACK-AXIS-CRITICAL-LOCATION"
		description "ALIGNMENT CHARACTERISTIC POINT"
	)
	(command _LINE_ (list 0 (- _cantReferenceGauge_)) (list 0 _cantReferenceGauge_) _ENTER_ )
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
