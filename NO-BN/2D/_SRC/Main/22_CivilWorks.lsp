;=========================================================================================================================
;
; 22_CivilWorks.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Civil Works objects top-level LISP routine

(setq f (strcat rootFolder "\\Main\\CivilWorks"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)

(defun 22_GENERATE-CIVIL-WORKS-OBJECTS ( / )
	(SetCadSystemDefaults)

	; Implemented for all administrations:
	(TraceLevel2 "ANYADM-MANHOLE")			(ANYADM-MANHOLE) ; Round, square, etc. - shows external dimensions and location of the lid. Insertion point is the centre top edge of the the lid "sticks up".

	; Implemented only for some administrations:
	(cond 
		((= _ADM_ _XXGL_) 
			; XX-GL actions:
		)
		((= _ADM_ _NOBN_) 
			; NO-BN actions:
			(TraceLevel2 "NOBN-FUNDAMENT-FOR-SKAP")						(NOBN-FUNDAMENT-FOR-SKAP) 
			(TraceLevel2 "NOBN-FUNDAMENT-FOR-MAST")						(NOBN-FUNDAMENT-FOR-MAST) ; Signal masts and KL masts, stretcher or spanner foundations - large, which requires tamping of tracks after installation
			(TraceLevel2 "NOBN-LITE-MONTASJEELEMENT")					(NOBN-LITE-MONTASJEELEMENT) ; Smaller fittings, under 100 kg, which can be dug by hand without destabilising the track
			(TraceLevel2 "NOBN-FUNDAMENT-FOR-TELEINSTALLASJON")			(NOBN-FUNDAMENT-FOR-TELEINSTALLASJON) ; Various foundations for tele-objects (monitor stand, ticket machine, etc.)
		)
		((= _ADM_ _FRSR_) 
			; FR-SR actions:
		)
		((= _ADM_ _DEDB_) 
			; DE-DB actions:
		)
	)
)
