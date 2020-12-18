;=========================================================================================================================
;
; HighVoltage.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; High-voltage objects top-level LISP routine

(loadFolder (findfile "HighVoltage"))

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
