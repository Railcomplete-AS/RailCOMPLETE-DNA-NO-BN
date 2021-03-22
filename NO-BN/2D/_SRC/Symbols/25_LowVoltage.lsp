;=========================================================================================================================
;
; 25_LowVoltage.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Low-voltage objects top-level LISP routine

(setq f (strcat rootFolder "\\Symbols\\LowVoltage"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)

(defun C:GENERATE-LOWVOLTAGE-OBJECTS ( / )
	(SetCadSystemDefaults)
	(TraceLevel2 "LIGHTING")					(C:LIGHTING)
	(TraceLevel2 "POWER-DISTRIBUTION-CABINET")	(C:POWER-DISTRIBUTION-CABINET)
	(TraceLevel2 "LOW-VOLTAGE-TRANSFORMER")		(C:LOW-VOLTAGE-TRANSFORMER)
	(TraceLevel2 "UPS-DISTRIBUTION")			(C:UPS-DISTRIBUTION)
)
