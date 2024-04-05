;=========================================================================================================================
;
; 26_Signalling.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503 
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Signalling objects top-level LISP routine

(setq f (strcat rootFolder "\\Main\\Signalling"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)

(defun 26_GENERATE-SIGNALLING-OBJECTS ( / )
	(SetCadSystemDefaults)

	; Implemented for all administrations:
	(TraceLevel2 "AXLE-COUNTER-SYSTEM")								(AXLE-COUNTER-SYSTEM)
	(TraceLevel2 "BALISE-SYSTEM") 									(BALISE-SYSTEM)
	(TraceLevel2 "TECHNICAL-BUILDING")								(TECHNICAL-BUILDING)
	(TraceLevel2 "CABINETS-AND-DISTRIBUTION-BOXES") 				(CABINETS-AND-DISTRIBUTION-BOXES)
	; Local control panel, point machine, derailer point machines. And may be level crossing boom machines:
	(TraceLevel2 "SWITCH-CONTROL-EQUIPMENT")						(SWITCH-CONTROL-EQUIPMENT)
	; Derailer is a signaling object, because it controls flank protection and front / rear collisions:
	(TraceLevel2 "DERAILER") 										(DERAILER)
	(TraceLevel2 "VIRTUAL-SIGNAL")									(VIRTUAL-SIGNAL)

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
