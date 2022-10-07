;=========================================================================================================================
;
; ANYADM Ocs Yoke.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Yoke

(defun ANYADM-OCS-YOKE ( / counter )
	(setq counter 10)
	(repeat 34
		(TraceLevel3 (strcat "YOKE: " (rtos counter _decimal_ 0) " m"))
		(ANYADM-CATENARY-YOKE counter) ; 10 to 43 inclusive
		(setq counter (+ counter 1))
	)
  	(ANYADM-CATENARY-GALLEY 6.000)
)



(defun ANYADM-CATENARY-YOKE ( len / blockName description )
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _OCS_ "YOK-" "YOKE-" 							(rtos (* 1000 len) _decimal_ 0)	)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _OCS_ "AAK-" "KONTAKTLEDNINGS" _uARING_ "K-"	(rtos (* 1000 len) _decimal_ 0)	)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _OCS_ "POR-" "PORTIQUE-"						(rtos (* 1000 len) _decimal_ 0)	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _OCS_ "POR-" "PORTAL-"						(rtos (* 1000 len) _decimal_ 0)	)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _OCS_ "YOK-" "YOKE-" 							(rtos (* 1000 len) _decimal_ 0)	)))
	)	
	(cond 	
		((= _ADM_ _XXGL_) (setq description (strcat "CATENARY YOKE "      (rtos (* 1000 len) _decimal_ 0)	)))
		((= _ADM_ _NOBN_) (setq description (strcat "KL-" _uARING_ "K "   (rtos (* 1000 len) _decimal_ 0)	)))
		((= _ADM_ _FRSR_) (setq description (strcat "PORTIQUE CATENAIRE " (rtos (* 1000 len) _decimal_ 0)	))) ; Trouble with E _uEACUTE_
		((= _ADM_ _DEDB_) (setq description (strcat "OBERLEITUNGSPORTAL " (rtos (* 1000 len) _decimal_ 0)	)))
		((= _ADM_ _JPTX_) (setq description (strcat "CATENARY YOKE "      (rtos (* 1000 len) _decimal_ 0)	)))
	)
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



(defun ANYADM-CATENARY-GALLEY ( len / blockName description )
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _OCS_ "GAL-" "GALLEY-" 												(rtos (* 1000 len) _decimal_ 0)	)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _OCS_ "AAK-" "KONTAKTLEDNINGS" _uARING_ "K-UTLIGGER" _uARING_ "K-"	(rtos (* 1000 len) _decimal_ 0)	)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _OCS_ "POT-" "POTENCE-"												(rtos (* 1000 len) _decimal_ 0)	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _OCS_ "GAL-" "GALGE-"													(rtos (* 1000 len) _decimal_ 0)	)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _OCS_ "GAL-" "GALLEY-" 												(rtos (* 1000 len) _decimal_ 0)	)))
	)	
	(cond 	
		((= _ADM_ _XXGL_) (setq description (strcat "CATENARY GALLEY "			(rtos (* 1000 len) _decimal_ 0)	)))
		((= _ADM_ _NOBN_) (setq description (strcat "KL-UTLIGGER" _uARING_ "K "	(rtos (* 1000 len) _decimal_ 0)	)))
		((= _ADM_ _FRSR_) (setq description (strcat "POTENCE CATENAIRE "		(rtos (* 1000 len) _decimal_ 0)	)))
		((= _ADM_ _DEDB_) (setq description (strcat "OBERLEITUNGSGALGE "		(rtos (* 1000 len) _decimal_ 0)	)))
		((= _ADM_ _JPTX_) (setq description (strcat "CATENARY GALLEY "			(rtos (* 1000 len) _decimal_ 0)	)))
	)
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
