;=========================================================================================================================
;
; 24_HighVoltage.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; High-voltage objects top-level LISP routine

(setq f (strcat rootFolder "\\Symbols\\HighVoltage"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)

(defun C:GENERATE-HIGH-VOLTAGE-OBJECTS ( / )
	(SetCadSystemDefaults)
	(TraceLevel2 "OCS-MAST")					(C:OCS-MAST) ; file: mast.lsp
	(TraceLevel2 "TENSIONING-DEVICE")			(C:TENSIONING-DEVICE)
	(TraceLevel2 "HIGH-VOLTAGE-PROTECTION")		(C:HIGH-VOLTAGE-PROTECTION)
	(TraceLevel2 "TRACTION-POWER-SWITCH")		(C:TRACTION-POWER-SWITCH)
	(TraceLevel2 "CONNECTOR-HIGH-VOLTAGE")		(C:CONNECTOR-HIGH-VOLTAGE)
	(TraceLevel2 "SUSPENSION-MAST")				(C:SUSPENSION-MAST) ; I tunneltak, under bru, i åk
	(TraceLevel2 "HIGH-VOLTAGE-TRANSFORMER")	(C:HIGH-VOLTAGE-TRANSFORMER)
	(TraceLevel2 "HIGH-VOLTAGE-ISOLATOR")		(C:HIGH-VOLTAGE-ISOLATOR)
	(TraceLevel2 "EARTHING")					(C:EARTHING)
	(TraceLevel2 "TENSIONING-DEVICE-ANCHOR")	(C:TENSIONING-DEVICE-ANCHOR)
	(TraceLevel2 "YOKE")						(C:YOKE)
	(TraceLevel2 "CANTILEVER")					(C:CANTILEVER)
)
