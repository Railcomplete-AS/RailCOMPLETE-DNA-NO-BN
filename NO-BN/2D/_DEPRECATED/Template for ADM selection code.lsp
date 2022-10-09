;=========================================================================================================================
;
; Template for ADM selection code.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Template objects top-level LISP routine

(setq f (strcat rootFolder "\\Main\\MySubFolder"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)

(defun TEMPLATE-FUNCTION ( / )
	(SetCadSystemDefaults)
	
	(TraceLevel2 "TEMPLATE CALL")	(TEMPLATE-CALL)

	; Implemented for all administrations:
	; (nothing)

	; Specific to this administration:
	(cond 
		((= _ADM_ _XXGL_) 
			; XX-GL actions:
		)
		((= _ADM_ _NOBN_) 
			; NO-BN actions:
		)
		((= _ADM_ _FRSR_) 
			; FR-SR actions:
		)
		((= _ADM_ _DEDB_) 
			; DE-DB actions:
		)
	)

	; Example - set blockName and description:
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _OCS_ "MAS-" "CATENARY-MAST-HEB-NOBN-OCS-H-BEAM-POLE"			)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _OCS_ "MAS-" "BJELKEMAST"								)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _OCS_ "SUP-" "SUPPORT-CATENAIRE-POUTRE-HEB-EN-ACIER"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _OCS_ "MAS-" "OBERLEITUNGSMAST-HEB-STAHLTRAEGER"		)))
	)	
	(cond 	
		((= _ADM_ _XXGL_) (setq description (strcat "CATENARY MAST, HEB STEEL BEAM"						)))
		((= _ADM_ _NOBN_) (setq description (strcat "KL-MAST, HEB BJELKEMAST"							)))
		((= _ADM_ _FRSR_) (setq description (strcat "SUPPORT CATENAIRE, POUTRE HEB EN ACIER"			)))
		((= _ADM_ _DEDB_) (setq description (strcat "OBERLEITUNGSMAST, HEB STAHLTR" _uAUML_ "GER"		)))
	)

)
