;=========================================================================================================================
;
; 24_HighVoltage.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
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
	(TraceLevel2 "ANYADM-EARTHING")								(ANYADM-EARTHING)
	(TraceLevel2 "ANYADM-TRACTION-POWER-INTERRUPTER")			(ANYADM-TRACTION-POWER-INTERRUPTER)
	(TraceLevel2 "ANYADM-OCS-UPRIGHT-POLE")						(ANYADM-OCS-UPRIGHT-POLE)
	(TraceLevel2 "ANYADM-OCS-SUSPENSION-POLE")					(ANYADM-OCS-SUSPENSION-POLE)
	(TraceLevel2 "ANYADM-OCS-PORTAL-AND-GALLEY")				(ANYADM-OCS-PORTAL-AND-GALLEY)
	(TraceLevel2 "ANYADM-OCS-CANTILEVER")						(ANYADM-OCS-CANTILEVER 10) ;[mm] step size (10 is a production value, 500 is suitable for debugging)
	(TraceLevel2 "ANYADM-OCS-WIRE-TENSIONING-BALANCER")			(ANYADM-OCS-WIRE-TENSIONING-BALANCER)
	(TraceLevel2 "ANYADM-OCS-WIRE-TENSIONING-ANCHOR")			(ANYADM-OCS-WIRE-TENSIONING-ANCHOR)
	(TraceLevel2 "ANYADM-OCS-HIGH-VOLTAGE-WIRE-CONNECTOR")		(ANYADM-OCS-HIGH-VOLTAGE-WIRE-CONNECTOR)
	(TraceLevel2 "ANYADM-OCS-HIGH-VOLTAGE-ISOLATOR")			(ANYADM-OCS-HIGH-VOLTAGE-ISOLATOR)
	(TraceLevel2 "ANYADM-OCS-HIGH-VOLTAGE-TRANSFORMER")			(ANYADM-OCS-HIGH-VOLTAGE-TRANSFORMER)
	(TraceLevel2 "ANYADM-OCS-HIGH-VOLTAGE-PROTECTION-SCREEN")	(ANYADM-OCS-HIGH-VOLTAGE-PROTECTION-SCREEN)
	(TraceLevel2 "ANYADM-HIGH-VOLTAGE-TECHNICAL-BUILDING")		(ANYADM-HIGH-VOLTAGE-TECHNICAL-BUILDING)
	(TraceLevel2 "ANYADM-OCS-SWITCH-ACTUATOR")					(ANYADM-OCS-SWITCH-ACTUATOR)

	; Specific to this administration:
)