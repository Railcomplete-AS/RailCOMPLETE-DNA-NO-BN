;
; Overbygningobjekter.lsp
;
(loadFolder (findfile "Overbygning"))

(defun C:GENERATE-TRACK-AND-EMBANKMENT-OBJECTS ( )
	(setCadSystemDefaults)  
  
	; Se detaljert deklarasjon av layeregenskaper i StyleDefinitions.xml (objektenes ObjectType deklarasjoner legger ting på disse lagene):
	(newLayer "JBTOB__KO_SPV_MIN_60_CM_SKINNESEPARASJON" 62 "")
	(newLayer "JBTOB__KO_SPV_LANGSVILLEPARTI_FRA_BK" 62 "Overbygning - langsvilleparti utenfor bakkant")
	(newLayer "JBTOB__KO_SPV_KORTSVILLEPARTI_FRA_BK" 62 "Overbygning - kortsvilleparti utenfor bakkant")
	(newLayer "JBTOB__KO_SPV_SENTERLINJER" 62 "Senterlinje for avviksspor i sporveksel")
	(newLayer "JBTFE__SPV_WIPEOUT" "yellow" "Wipeout for sporveksler")

	(C:CONNECTION)	; Symbols for connecting the topological track network (continuations, switches, crossings)
	(C:KARAKTERISTISK-TRASEPUNKT)
	(C:OPPKJOERSBJELKE)
	(C:SPORSTOPPER)
	(C:SPORVEKSELTUNGE)
)