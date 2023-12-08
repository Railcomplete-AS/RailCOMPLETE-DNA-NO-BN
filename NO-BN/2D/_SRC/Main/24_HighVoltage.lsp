;=========================================================================================================================
;
; 24_HighVoltage.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; High-voltage objects top-level LISP routine

(setq f (strcat rootFolder "\\Main\\HighVoltage"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)

(defun 24_GENERATE-HIGH-VOLTAGE-OBJECTS ( / )
	(SetCadSystemDefaults)

	; Implemented for all administrations:
	(TraceLevel2 "EARTHING")							(EARTHING)
	(TraceLevel2 "TRACTION-POWER-INTERRUPTER")			(TRACTION-POWER-INTERRUPTER)
	(TraceLevel2 "OCS-UPRIGHT-POLE")					(OCS-UPRIGHT-POLE)
	(TraceLevel2 "OCS-SUSPENSION-POLE")					(OCS-SUSPENSION-POLE)
	(TraceLevel2 "OCS-PORTAL-AND-GANTRY")				(OCS-PORTAL-AND-GANTRY)
	(TraceLevel2 "OCS-CANTILEVER")						(OCS-CANTILEVER 10) ;[mm] step size (10 is a production value, 500 is suitable for debugging)
	(TraceLevel2 "OCS-WIRE-TENSIONING-BALANCER")		(OCS-WIRE-TENSIONING-BALANCER)
	(TraceLevel2 "OCS-WIRE-TENSIONING-ANCHOR")			(OCS-WIRE-TENSIONING-ANCHOR)
	(TraceLevel2 "OCS-HIGH-VOLTAGE-WIRE-CONNECTOR")		(OCS-HIGH-VOLTAGE-WIRE-CONNECTOR)
	(TraceLevel2 "OCS-HIGH-VOLTAGE-INSULATOR")			(OCS-HIGH-VOLTAGE-INSULATOR)
	(TraceLevel2 "OCS-HIGH-VOLTAGE-TRANSFORMER")		(OCS-HIGH-VOLTAGE-TRANSFORMER)
	(TraceLevel2 "OCS-HIGH-VOLTAGE-PROTECTION-SCREEN")	(OCS-HIGH-VOLTAGE-PROTECTION-SCREEN)
	(TraceLevel2 "HIGH-VOLTAGE-TECHNICAL-BUILDING")		(HIGH-VOLTAGE-TECHNICAL-BUILDING)
	(TraceLevel2 "OCS-SWITCH-ACTUATOR")					(OCS-SWITCH-ACTUATOR)

	; Specific to this administration:
)
