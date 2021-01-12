;=========================================================================================================================
;
; HighVoltage.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; High-voltage objects top-level LISP routine

(setq f (strcat rootFolder "\\Symbols\\HighVoltage"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(loadfolder f)

(defun C:GENERATE-HIGH-VOLTAGE-SCALED-OBJECTS ( / )
	(setCadSystemDefaults)
	(subStep "OCS-MAST")					(C:OCS-MAST) ; file: mast.lsp
	(subStep "TENSIONING-DEVICE")			(C:TENSIONING-DEVICE)
	(subStep "HIGH-VOLTAGE-PROTECTION")		(C:HIGH-VOLTAGE-PROTECTION)
	(subStep "TRACTION-POWER-SWITCH")		(C:TRACTION-POWER-SWITCH)
	(subStep "CONNECTOR-HIGH-VOLTAGE")		(C:CONNECTOR-HIGH-VOLTAGE)
	(subStep "SUSPENSION-MAST")				(C:SUSPENSION-MAST) ; I tunneltak, under bru, i åk
	(subStep "HIGH-VOLTAGE-TRANSFORMER")	(C:HIGH-VOLTAGE-TRANSFORMER)
	(subStep "HIGH-VOLTAGE-ISOLATOR")		(C:HIGH-VOLTAGE-ISOLATOR)
	(subStep "EARTHING")					(C:EARTHING)
	(subStep "TENSIONING-DEVICE-ANCHOR")	(C:TENSIONING-DEVICE-ANCHOR)
)



(defun C:GENERATE-HIGH-VOLTAGE-FIXED-SCALE-OBJECTS ( / )
	(setCadSystemDefaults)
	(subStep "YOKE")						(C:YOKE)
	(subStep "CANTILEVER")					(C:CANTILEVER)
)
