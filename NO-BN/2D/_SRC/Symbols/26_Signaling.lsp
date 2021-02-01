;=========================================================================================================================
;
; Signaling.lsp
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
(loadfolder f)

(defun C:GENERATE-SIGNALING-OBJECTS ( / )
	; Alphabetically:
	(subStep "AXLE-COUNTER-SYSTEM")						(C:AXLE-COUNTER-SYSTEM)
	(subStep "DERAILMENT-INDICATOR") 					(C:DERAILMENT-INDICATOR)
	(subStep "BALISE") 									(C:BALISE)
	(subStep "VIRTUAL-SIGNAL") 							(C:VIRTUAL-SIGNAL)
	(subStep "REFLECTIVE-MARKER") 						(C:REFLECTIVE-MARKER)
	(subStep "RELAY-ROOM")								(C:RELAY-ROOM)
	(subStep "ERTMS-BOARDS")				 			(C:ERTMS-BOARDS)
	(subStep "SIGNAL-COMBINATIONS")			 			(C:SIGNAL-COMBINATIONS) ; Loop which iterates over many signal combinations
	(subStep "DERAILER-SIGNAL") 						(C:DERAILER-SIGNAL)
	(subStep "SWITCH-SIGNAL") 							(C:SWITCH-SIGNAL)
	(subStep "SHARED-TRACK-SIGNAL")			 			(C:SHARED-TRACK-SIGNAL)
	(subStep "LEVEL-CROSSING-ROAD-SIGNAL") 				(C:LEVEL-CROSSING-ROAD-SIGNAL)
	(subStep "CABINETS-AND-CABLE-DISTRIBUTION-BOXES") 	(C:CABINETS-AND-CABLE-DISTRIBUTION-BOXES)
	(subStep "SWITCH-CONTROL-EQUIPMENT")				(C:SWITCH-CONTROL-EQUIPMENT) ; Local control panel, point machine, derailer point machines. And may be level crossing boom machines.
	(subStep "DERAILER") 								(C:DERAILER) ; Yes, this one is categorized as a signaling object and not a track object (because it controls flank protection and front / rear collisions). 
)
