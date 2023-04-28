;=========================================================================================================================
;
; Ocs Portal.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Overhead Catenary System (OCS) Portals (yoke and gantry)

(defun OCS-PORTAL-AND-GANTRY ( / counter )
	(setq counter 10)
	(repeat 34
		(TraceLevel3 (strcat "OCS PORTAL: " (rtos counter _decimal_ 0) " m"))
		(OCS-PORTAL counter) ; 10 to 43 inclusive
		(setq counter (+ counter 1))
	)
  	(OCS-GANTRY 6.000)
)



(defun OCS-PORTAL ( len / blockName description )
	(setq blockName (strcat _OCS_ "AAK-" "KONTAKTLEDNINGS" _uARING_ "K-"	(rtos (* 1000 len) _decimal_ 0)	))
	(setq description (strcat "KL-" _uARING_ "K "   						(rtos (* 1000 len) _decimal_ 0)	))

	; Schematic symbol
	; Adapt to schematic plan with _schematicTrackSpacing_ units between center tracks (instead of standard ~4.7 in shunting yards
	(DrawLine layDef_Zero _origin_ (list 0 (atoi (rtos (* (/ _schematicTrackSpacing_ _geographicTrackSpacing_) (- (+ 1 len))) _decimal_ 0)))) ; scaled, floored to int
	(AddDescriptionBelowOrigin description -1.0) ; A little *above* yoke
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	; Just annotative text, no graphics
	(AddDescriptionBelowOrigin description -1.0) ; A little *above* yoke
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(DrawLine layDef_Zero _origin_ (list 0 (- len)))
	(CreateMetricBlockFromCurrentGraphics blockName)
)



(defun OCS-GANTRY ( len / blockName description )
	(setq blockName (strcat _OCS_ "AAK-" "UTLIGGER" _uARING_ "K-"	(rtos (* 1000 len) _decimal_ 0)	))
	(setq description (strcat "KL-UTLIGGER" _uARING_ "K "			(rtos (* 1000 len) _decimal_ 0)	))

	; Schematic symbol
	; Adapt to schematic plan with _schematicTrackSpacing_ units between center tracks (instead of standard ~4.7 in shunting yards
	(DrawLine layDef_Zero _origin_ (list 0 (atoi (rtos (* (/ _schematicTrackSpacing_ _geographicTrackSpacing_) (- (+ 1 len))) _decimal_ 0)))) ; scaled, floored to int
	(AddDescriptionBelowOrigin description -1.0) ; A little *above* yoke
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	; Just annotative text, no graphics
	(AddDescriptionBelowOrigin description -1.0) ; A little *above* yoke
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; Metric symbol
	(DrawLine layDef_Zero _origin_ (list 0 (- len)))
	(CreateMetricBlockFromCurrentGraphics blockName)
)
