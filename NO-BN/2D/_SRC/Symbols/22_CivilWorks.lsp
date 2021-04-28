;=========================================================================================================================
;
; 22_CivilWorks.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Civil Works objects top-level LISP routine

(setq f (strcat rootFolder "\\Symbols\\CivilWorks"))
(princ (vl-directory-files (findfile f)))
(princ "\n")
(LoadFolder f)

(defun C:GENERATE-CIVIL-WORKS-OBJECTS ( / )
	(SetCadSystemDefaults)
	(TraceLevel2 "CABINET-FOUNDATION")		(C:CABINET-FOUNDATION) ; Brukes både for store signalskap, gruppeskap, andre
	(TraceLevel2 "MAST-FOUNDATION")			(C:MAST-FOUNDATION) ; Signalmaster og KL-master, bardunfester i bakken - store, som krever pakking av spor etterpå
	(TraceLevel2 "LIGHTWEIGHT-FOUNDATION")	(C:LIGHTWEIGHT-FOUNDATION) ; Mindre fundamenter, under 100 kg, som kan graves ned for hånd uten å destabilisere sporet
	(TraceLevel2 "TELECOM-FOUNDATION")		(C:TELECOM-FOUNDATION) ; Diverse fundamenter for tele-objekter (monitorstativ, billettautomat med mer)
	(TraceLevel2 "MANHOLE")					(C:MANHOLE) ; Rund, firkantet osv - viser ytre mål samt plassering av lokk. Innsettingspunkt er senter overkant øver ring / kasse (lokket "stikker opp").
)
