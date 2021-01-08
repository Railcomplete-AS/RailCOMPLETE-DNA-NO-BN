;=========================================================================================================================
;
; LowVoltage.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Low-voltage objects top-level LISP routine

(setq f (strcat rootFolder "\\Symbols\\LowVoltage"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(loadfolder f)

(defun C:GENERATE-LOWVOLTAGE-OBJECTS ( / )
	(setCadSystemDefaults)
	(subStep "LIGHTING")					(C:LIGHTING)
	(subStep "POWER-DISTRIBUTION-CABINET")	(C:POWER-DISTRIBUTION-CABINET)
	(subStep "LOW-VOLTAGE-TRANSFORMER")		(C:LOW-VOLTAGE-TRANSFORMER)
	(subStep "UPS-DISTRIBUTION")			(C:UPS-DISTRIBUTION)
)
