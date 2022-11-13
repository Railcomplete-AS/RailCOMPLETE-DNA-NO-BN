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
	(TraceLevel2 "MANHOLE")			(MANHOLE) ; Round, square, etc. - shows external dimensions and location of the lid. Insertion point is the centre top edge of the the lid "sticks up".

	; Specific to this administration:
	(TraceLevel2 "NOBN-FUNDAMENT-FOR-SKAP")				(NOBN-FUNDAMENT-FOR-SKAP) 
	(TraceLevel2 "NOBN-FUNDAMENT-FOR-MAST")				(NOBN-FUNDAMENT-FOR-MAST) 		; Signal masts and OCS masts, guywire or spanner foundations - large, requires tamping after installation
	(TraceLevel2 "NOBN-LITE-MONTASJEELEMENT")			(NOBN-LITE-MONTASJEELEMENT) 	; Smaller fittings, under 100 kg, which can be dug by hand without destabilising the track
	(TraceLevel2 "NOBN-FUNDAMENT-FOR-TELEINSTALLASJON")	(NOBN-FUNDAMENT-FOR-TELEINSTALLASJON) ; Various foundations for tele-objects (monitor stand, ticket machine, etc.)
)
