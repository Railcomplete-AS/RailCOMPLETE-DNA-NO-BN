;=========================================================================================================================
;
; 23_TrackAndEmbankment.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
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
	(TraceLevel2 "ISOLATED-JOINTS") 				(ISOLATED-JOINTS) ; TRACK object when built, but designed by Signaling or Return Current specialists.
	(TraceLevel2 "BUFFER-STOPS")					(BUFFER-STOPS)
	(TraceLevel2 "ALIGNMENT-CHARACTERISTIC-POINTS")	(ALIGNMENT-CHARACTERISTIC-POINTS)
	(TraceLevel2 "ANYADM-CONNECTOR-SPLICE")			(ANYADM-CONNECTOR-SPLICE)
	(TraceLevel2 "CONNECTOR-CROSSINGS")				(CONNECTOR-CROSSINGS)
	(TraceLevel2 "CONNECTOR-SWITCHES")				(CONNECTOR-SWITCHES)

	; Specific to this administration:
	(TraceLevel2 "SWITCH-TONGUE")					(SWITCH-TONGUE)
	(TraceLevel2 "DEFLECTION-BAR")					(DEFLECTION-BAR)
)
