;
; Signalobjekter.lsp
;
(loadFolder (findfile "Signal"))

(defun C:GENERATE-SIGNALING-OBJECTS ()
	; Alphabetically:
	(C:AKSELTELLER)
	(C:AKSELTELLER-SOPP)
	(C:AVSPORINGSINDIKATOR)
	(C:BALISE)
	(C:ERTMS) ; Skiltene for ERTMS er å betrakte som på linje med de optiske, siden de er start/slutt for togveier. Og litt PLO...
	(C:FIKTIVT-PUNKT)
	(C:ISOLERT-SKJOET)
	(C:REFLEKS)
	(C:RELEROM)
	(C:SIGNAL-DVERGSIGNAL-FRITTSTAAENDE)
	(C:SIGNAL-HOEYT-SKIFTESIGNAL-FRITTSTÅENDE)
	(C:SIGNAL-HOVEDSIGNAL)
	(C:SIGNAL-MANGLENDE-SYMBOL)
	(C:SIGNAL-TOGSPORSIGNAL)
	(C:SKAP-OG-BOKSER-FOR-SIGNALFAGET)
	(C:SPOROMLEGGING) ; Lokalstiller, sporvekseldrivmaskin, sporsperredrivmaskin
	(C:SPORSPERRE) ; Ja, denne er faktisk kategorisert som en signal-dings (ikke spordings)
	(C:VEISIGNAL)
)
