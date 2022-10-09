;=========================================================================================================================
;
; 26_Signaling.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved. 
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Signaling objects top-level LISP routine

(setq f (strcat rootFolder "\\Main\\Signaling"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)

(defun 26_GENERATE-SIGNALING-OBJECTS ( / )
	(SetCadSystemDefaults)

	; Implemented for all administrations:
	(TraceLevel2 "ANYADM-AXLE-COUNTER-SYSTEM")						(ANYADM-AXLE-COUNTER-SYSTEM)
	(TraceLevel2 "ANYADM-BALISE-SYSTEM") 							(ANYADM-BALISE-SYSTEM)
	(TraceLevel2 "ANYADM-TECHNICAL-BUILDING")						(ANYADM-TECHNICAL-BUILDING)
	(TraceLevel2 "ANYADM-CABINETS-AND-DISTRIBUTION-BOXES") 			(ANYADM-CABINETS-AND-DISTRIBUTION-BOXES)
	; Local control panel, point machine, derailer point machines. And may be level crossing boom machines:
	(TraceLevel2 "ANYADM-SWITCH-CONTROL-EQUIPMENT")					(ANYADM-SWITCH-CONTROL-EQUIPMENT)
	; Derailer is a signaling object, because it controls flank protection and front / rear collisions:
	(TraceLevel2 "ANYADM-DERAILER") 								(ANYADM-DERAILER)
	(TraceLevel2 "ANYADM-VIRTUAL-SIGNAL")							(ANYADM-VIRTUAL-SIGNAL)

	; Specific to this administration:
	(TraceLevel2 "NOBN-AVSPORINGSINDIKATOR") 						(NOBN-AVSPORINGSINDIKATOR)
	(TraceLevel2 "NOBN-REFLEKS")									(NOBN-REFLEKS)
	(TraceLevel2 "NOBN-ERTMS-SKILT")					 			(NOBN-ERTMS-SKILT) ; Anything other than STOP and LOCATION marker boards
	(TraceLevel2 "NOBN-KLASSE-B-LYSSIGNAL")							(NOBN-KLASSE-B-LYSSIGNAL)
	(TraceLevel2 "NOBN-SPORSPERRESIGNAL") 							(NOBN-SPORSPERRESIGNAL)
	(TraceLevel2 "NOBN-SPORVEKSELSIGNAL") 							(NOBN-SPORVEKSELSIGNAL)
	(TraceLevel2 "NOBN-TOGSPORSIGNAL")								(NOBN-TOGSPORSIGNAL)
	(TraceLevel2 "NOBN-PLANOVERGANGSSIGNAL") 						(NOBN-PLANOVERGANGSSIGNAL)
)
