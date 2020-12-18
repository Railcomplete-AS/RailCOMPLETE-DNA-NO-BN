;=========================================================================================================================
;
; Substructure.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Substructure objects top-level LISP routine

(loadFolder (findfile "Substructure"))

(defun C:GENERATE-SUBSTRUCTURE-OBJECTS ( / )

	(setCadSystemDefaults)

	(subStep "CABINET-FOUNDATION")	(C:CABINET-FOUNDATION) ; Brukes både for store signalskap, gruppeskap, andre
	(subStep "MAST-FOUNDATION")		(C:MAST-FOUNDATION) ; Signalmaster og KL-master, bardunfester i bakken - store, som krever pakking av spor etterpå
	(subStep "LIGHTWEIGHT-FOUNDATION")	(C:LIGHTWEIGHT-FOUNDATION) ; Mindre fundamenter, under 100 kg, som kan graves ned for hånd uten å destabilisere sporet
	(subStep "TELECOM-FOUNDATION")		(C:TELECOM-FOUNDATION) ; Diverse fundamenter for tele-objekter (monitorstativ, billettautomat med mer)
	(subStep "MANHOLE")				(C:MANHOLE) ; Rund, firkantet osv - viser ytre mål samt plassering av lokk. Innsettingspunkt er senter overkant øver ring / kasse (lokket "stikker opp").
)
