;=========================================================================================================================
;
; 26_Signaling.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved. 
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Signaling objects top-level LISP routine

(setq f (strcat rootFolder "\\Symbols\\Signaling"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)

(defun C:GENERATE-SIGNALING-OBJECTS ( / )
	; Alphabetically:
	(TraceLevel2 "AXLE-COUNTER-SYSTEM")						(C:AXLE-COUNTER-SYSTEM)
	(TraceLevel2 "DERAILMENT-INDICATOR") 					(C:DERAILMENT-INDICATOR)
	(TraceLevel2 "BALISE") 									(C:BALISE)
	(TraceLevel2 "VIRTUAL-SIGNAL") 							(C:VIRTUAL-SIGNAL)
	(TraceLevel2 "REFLECTIVE-MARKER") 						(C:REFLECTIVE-MARKER)
	(TraceLevel2 "RELAY-ROOM")								(C:RELAY-ROOM)
	(TraceLevel2 "ERTMS-BOARDS")				 			(C:ERTMS-BOARDS)
	(TraceLevel2 "SIGNAL-COMBINATIONS")			 			(C:SIGNAL-COMBINATIONS) ; Loop which iterates over many signal combinations
	(TraceLevel2 "DERAILER-SIGNAL") 						(C:DERAILER-SIGNAL)
	(TraceLevel2 "SWITCH-SIGNAL") 							(C:SWITCH-SIGNAL)
	(TraceLevel2 "SHARED-TRACK-SIGNAL")			 			(C:SHARED-TRACK-SIGNAL)
	(TraceLevel2 "LEVEL-CROSSING-ROAD-SIGNAL") 				(C:LEVEL-CROSSING-ROAD-SIGNAL)
	(TraceLevel2 "CABINETS-AND-CABLE-DISTRIBUTION-BOXES") 	(C:CABINETS-AND-CABLE-DISTRIBUTION-BOXES)
	(TraceLevel2 "SWITCH-CONTROL-EQUIPMENT")				(C:SWITCH-CONTROL-EQUIPMENT) ; Local control panel, point machine, derailer point machines. And may be level crossing boom machines.
	(TraceLevel2 "DERAILER") 								(C:DERAILER) ; Yes, this one is categorized as a signaling object and not a track object (because it controls flank protection and front / rear collisions). 
)
