;
; Underbygningobjekter.lsp
;
(loadFolder (findfile "Underbygning"))

(defun C:GENERATE-CIVIL-WORKS-OBJECTS ()
	(setCadSystemDefaults)
	(C:APPARATSKAP-FUNDAMENT) ; Brukes både for store signalskap, gruppeskap, andre
	(C:MASTEFUNDAMENT) ; Signalmaster og KL-master, bardunfester i bakken - store, som krever pakking av spor etteropå
	(C:MONTASJEELEMENT) ; Mindre fundamenter, under 100 kg, som kan graves ned for hånd uten å destabilisere sporet
	(C:TELEFUNDAMENT) ; Diverse fundamenter for tele-objekter (monitorstativ, billettautomat med mer)
	(C:TREKKEKUMMER) ; Rund, firkantet osv - viser ytre mål samt plassering av lokk. Innsettingspunkt er senter overkant øver ring / kasse (lokket "stikker opp").
)



(defun drawFoundationLocator (discipline textHeight / locatorRadius )
	; Set LAYER correctly first, to a layer that can be turned on/off using RC-ShowLayer.
	; NB: Layer is not returned to its previous setting.
	(setq
		; 'discipline' is: S=Signaling, KL=contact wire, T=telecomm, M=Misc/Mounting element, OB=superstructure (track etc), UB=substructure (foundations)
		locatorRadius 1.5   ; Two letters: use text Height 1.8. One letter: Use text height 2.5 (centered inside a circle of radius 1.5).
	)
	(command "._LAYER" "SET" "JBTUB__FE_FUN_LOKALISERING" "" "._COLOR" "ByLayer")
	(command "._CIRCLE" (list 0 0) locatorRadius)
	(addMText discipline "0,0" textHeight textHeight 0 "iso" "MC")
)

