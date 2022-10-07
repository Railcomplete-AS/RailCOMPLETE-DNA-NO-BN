;=========================================================================================================================
;
; 20_Common.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; ANYADM objects top-level LISP routine

(setq f (strcat rootFolder "\\Main\\Common"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)

(defun 20_GENERATE-COMMON-OBJECTS ( / )
	(SetCadSystemDefaults)

	; Implemented for all administrations:
	(TraceLevel2 "CAD-POINTS")			(CAD-POINTS)
	(TraceLevel2 "CONTROLLERS")			(CONTROLLERS)
	(TraceLevel2 "LABELS")				(LABELS)
	(TraceLevel2 "MARKERS")				(MARKERS)
	(TraceLevel2 "SECTIONS")			(SECTIONS)
	(TraceLevel2 "WATCHES")				(WATCHES)
	(TraceLevel2 "ANNOTATION TEXTS")	(ANNOTATION-TEXTS)

	; Specific to one administration:
	(cond 
		((= _ADM_ _XXGL_)
		)
		((= _ADM_ _NOBN_) 
		)
		((= _ADM_ _FRSR_)
		)
		((= _ADM_ _DEDB_)
			(TraceLevel2 "DEDB-POSITIONS-AND-DISTANCES")				(DEDB-POSITIONS-AND-DISTANCES)
		)
		((= _ADM_ _JPTX_)
		)
	)
)
