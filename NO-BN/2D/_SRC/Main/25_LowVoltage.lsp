;=========================================================================================================================
;
; 25_LowVoltage.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Low-voltage objects top-level LISP routine

(setq f (strcat rootFolder "\\Main\\LowVoltage"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)

(defun 25_GENERATE-LOWVOLTAGE-OBJECTS ( / )
	(SetCadSystemDefaults)
	(TraceLevel2 "LIGHTING")					(LIGHTING)
	(TraceLevel2 "POWER-DISTRIBUTION-CABINET")	(POWER-DISTRIBUTION-CABINET)
	(TraceLevel2 "LOW-VOLTAGE-TRANSFORMER")		(LOW-VOLTAGE-TRANSFORMER)
	(TraceLevel2 "UPS-DISTRIBUTION")			(UPS-DISTRIBUTION)
)
