;=========================================================================================================================
;
; 23_TrackAndEmbankment.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Track And Embankment objects top-level LISP routine

(setq f (strcat rootFolder "\\Main\\TrackAndEmbankment"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)

(defun 23_GENERATE-TRACK-AND-EMBANKMENT-OBJECTS ( / )
	(SetCadSystemDefaults)  

	; Implemented for all administrations:
	(TraceLevel2 "RAIL-JOINTS") 					(RAIL-JOINTS) ; TRACK object when built, but designed by Signalling or Return Current specialists.
	(TraceLevel2 "BUFFER-STOPS")					(BUFFER-STOPS)
	(TraceLevel2 "ALIGNMENT-VERTICES")				(ALIGNMENT-VERTICES)
	(TraceLevel2 "CONNECTOR-CONTINUATIONS")			(CONNECTOR-CONTINUATIONS)
	(TraceLevel2 "CONNECTOR-CROSSINGS")				(CONNECTOR-CROSSINGS)
	(TraceLevel2 "CONNECTOR-BIFURCATIONS")			(CONNECTOR-BIFURCATIONS)
	(TraceLevel2 "SWITCH-TONGUES")					(SWITCH-TONGUES)

	; Specific to this administration:
	(TraceLevel2 "DEFLECTION-BAR")					(DEFLECTION-BAR)
)
