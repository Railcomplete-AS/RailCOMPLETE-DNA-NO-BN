;
; Hovedsignal.lsp  (inkl høyt skiftesignal og andre formsignaler som henger på hovedsignals mast)
;
(defun C:SIGNAL-HOVEDSIGNAL ( / 
		systemVars
		tempSettings
		userSettings
		pathDWG
		pathFunctions
		feste
	)
	(setCadSystemDefaults)
	(command "ZOOM" "-11,-5" "11,5")
	(setq feste nil)
	(repeat 2 ; En gang for egen mast, egen gang for feste i åk

	    ; (drawLanterns 0 0 1 0 0 0 0 (+ 2 (* 2.67 (/ (sqrt 3.0) 2))) feste)  ; Nei, Denne blir utført under C:FRITTSTAANDE-DVERGSIGNAL (8 varianter)
		; Frittstående høyt skiftesignal er håndtert i egen funksjon (4 varianter).

		;            H F D M K L S topOfPole feste  (se mer forklaring lenger ned)
		(drawLanterns 0 1 0 0 0 0 0 5 feste) ;Frittstående Fs

		(drawLanterns 1 0 0 0 0 0 0 6 feste) ;Stopplykt

		(drawLanterns 2 0 0 0 0 0 0 6 feste)	;2-lys Hs
		(drawLanterns 2 0 1 0 0 0 0 6 feste)
		(drawLanterns 2 0 1 1 0 0 0 8 feste)
		(drawLanterns 2 0 1 0 0 1 0 8 feste)
		(drawLanterns 2 0 1 0 1 0 0 8 feste)
		(drawLanterns 2 0 1 1 1 0 0 8 feste)
		(drawLanterns 2 0 1 1 0 0 0 8 feste)
		(drawLanterns 2 0 1 1 0 1 0 8 feste)
		(drawLanterns 2 1 0 0 0 0 0 6 feste)
		(drawLanterns 2 1 0 0 1 0 0 8 feste)
		(drawLanterns 2 1 0 1 0 0 0 6 feste)
		(drawLanterns 2 1 1 0 0 0 0 10 feste)
		(drawLanterns 2 1 1 0 0 1 0 12 feste)
		(drawLanterns 2 1 1 0 1 0 0 12 feste)
		(drawLanterns 2 1 1 1 0 0 0 10 feste)
		(drawLanterns 2 1 1 1 0 1 0 12 feste)
		(drawLanterns 2 1 1 1 1 0 0 12 feste)
		(drawLanterns 3 0 0 0 0 0 0 6 feste)
		(drawLanterns 3 0 0 0 0 1 0 6 feste)   
		(drawLanterns 3 0 1 0 0 0 0 6 feste)
		(drawLanterns 3 0 1 1 0 0 0 8 feste)
		(drawLanterns 3 1 0 0 0 0 0 6 feste)
		(drawLanterns 3 1 0 0 0 1 0 8 feste)
		(drawLanterns 3 1 0 0 1 0 0 8 feste)
		(drawLanterns 3 1 0 1 0 0 0 6 feste)
		(drawLanterns 3 1 1 0 0 0 0 10 feste)
		(drawLanterns 3 1 1 0 0 1 0 12 feste)
		(drawLanterns 3 1 1 0 1 0 0 12 feste)
		(drawLanterns 3 1 1 0 1 1 0 12 feste)
		(drawLanterns 3 1 1 1 0 0 0 10 feste)
		(drawLanterns 3 1 1 1 0 1 0 12 feste)
		(drawLanterns 3 1 1 1 1 0 0 12 feste)
		
		(drawLanterns 2 0 1 0 0 0 1 8 feste)
		(drawLanterns 2 0 1 1 0 0 1 8 feste)
		(drawLanterns 2 0 1 0 0 1 1 10 feste)
		(drawLanterns 2 0 1 0 1 0 1 10 feste)
		(drawLanterns 2 0 1 1 1 0 1 10 feste)
		(drawLanterns 2 0 1 1 0 0 1 8 feste)
		(drawLanterns 2 0 1 1 0 1 1 10 feste)
		(drawLanterns 2 1 0 0 0 0 1 8 feste)
		(drawLanterns 2 1 0 0 1 0 1 8 feste)
		(drawLanterns 2 1 0 1 0 0 1 8 feste)
		(drawLanterns 2 1 1 0 0 0 1 12 feste)
		(drawLanterns 2 1 1 0 0 1 1 12 feste)
		(drawLanterns 2 1 1 0 1 0 1 12 feste)
		(drawLanterns 2 1 1 1 0 0 1 12 feste)
		(drawLanterns 2 1 1 1 0 1 1 12 feste)
		(drawLanterns 2 1 1 1 1 0 1 12 feste)
		(drawLanterns 3 0 0 0 0 0 1 6 feste)
		(drawLanterns 3 0 0 0 0 1 1 6 feste)   
		(drawLanterns 3 0 1 0 0 0 1 8 feste)
		(drawLanterns 3 0 1 1 0 0 1 8 feste)
		(drawLanterns 3 1 0 0 0 0 1 8 feste)
		(drawLanterns 3 1 0 0 0 1 1 8 feste)
		(drawLanterns 3 1 0 0 1 0 1 8 feste)
		(drawLanterns 3 1 0 1 0 0 1 8 feste)
		(drawLanterns 3 1 1 0 0 0 1 12 feste)
		(drawLanterns 3 1 1 0 0 1 1 12 feste)
		(drawLanterns 3 1 1 0 1 0 1 12 feste)
		(drawLanterns 3 1 1 0 1 1 1 12 feste)
		(drawLanterns 3 1 1 1 0 0 1 12 feste)
		(drawLanterns 3 1 1 1 0 1 1 12 feste)
		(drawLanterns 3 1 1 1 1 0 1 12 feste)
		(togveiSlutt 6 feste)
		(setq feste "AAK")   ; Forberede for runde nr to - lag åk-feste varianten av alle signaler.
	)
)



(defun drawLanterns (H F D MK FK HL S68E topOfPole feste / blockName r mksRadius)
;   H - Antall lanterner i Hovedsignal [0..3]
;	F - Antall Forsignaler [0..1]
;	D -  Antall Dvergsignal [0..1]
;	MK - Antall Middelkontrollampe [0..1]
;	FK - Antall signal Forsiktig Kjøring [0..1]
;	HL - Antall Hovedlinjesignal [0..1]
;	S68E - Antall Lysende hastighetssignal (signal 68), avvikende hastighet i sporveksel (annen enn 40 km/h) [0..1]
;	topOfPole - Lengde på mast, oppgis i tegneenheter
;	feste - mast eller åk [nil | "AAK"]

	(setq 
		r (getSradius)
		mksRadius (getMKsradius)
	)
	(if (= feste "AAK")
		(progn
			(setq aak 2.0)			;
			(cond
				((= D 1)
					(drawPole 2 topOfPole)
				)				;1st
				((= F 1)
					(if (or (= HL 1) (= FK 1) (= S68E 1))
						(drawPole 2.0 topOfPole)
						(if (= 0 (+ H D MK FK HL S68E))
							(drawPole (- topOfPole (* 2 r)) topOfPole)
							(drawPole (- topOfPole (* 3 r)) topOfPole)
						);if
					);if
				)
			);cond
			(drawPole (+ topOfPole (* 2 H r)) (+ aak topOfPole (* 2 H r))) ;oppheng aak
		);progn
		(progn
			(drawBase)
			(drawPole 0 topOfPole)
		)
	);if
	(cond 
		((= H 1) ;'H'=Hs
			(Hs1 topOfPole)
		)
		((= H 2)
			(Hs2 topOfPole)
		)
		((= H 3)
			(Hs3 topOfPole)
		)
	)
	(if (= F 1) ;'F'=Fs
		(if	(= 0 (+ H D MK FK HL))
			(drawFs topOfPole)  ; Frittstående, alene
			(drawFs topOfPole)  ; Kombinert med andre signaler
		)
	)
	(if (= D 1) ;'D'=Ds
		(drawDs)
	)
	(if (= MK 1) ;'M'=MK
		(drawMKs topOfPole)
	)
	(if (= FK 1) ;'K'=FK
		(drawFKs topOfPole F)
	)
	(if (= HL 1) ;'L'=HLs
		(if	(= F 1)
			(if (= FK 1)
				(drawHLs2 topOfPole)
				(drawHLs topOfPole)
			)
			(drawHLs (+ topOfPole 4))
		)
	)
	(if (= S68E 1) ;'S'=S68E
		(if	(= F 1)
			(if (or (= FK 1) (= HL 1))
				(if (= MK 1)
					(drawS68E2 (- topOfPole (* 2.0 mksRadius)))
					(if (= (+ FK HL) 2)
						(drawS68E2 (- topOfPole 2.0))
						(drawS68E2 topOfPole)
					)
				)
				(drawS68E topOfPole)
			)
			(if (= HL 1) 
				(if (= (+ D FK HL) 1)
					(if (= (+ D FK) 0)
						(drawS68E (+ topOfPole 2))
						(drawS68E (+ topOfPole 4))
					)
					(if (= (+ D FK) 0)
						(draws68E topOfPole)
						(drawS68E (+ topOfPole 2))	
					)
				)
				(if (= FK 1)
					(drawS68E (+ topOfPole 2))
					(drawS68E (+ topOfPole 4))
				)		
			)
		)
	)
	(if (= feste "AAK")
		(if ( and (= 0 (+ H D MK FK HL)) (= F 1))
			(progn
				(drawPole (+ aak topOfPole (* 2 H r))
				(+ aak topOfPole r (* 2 H r)))
				(command "._MOVE" "ALL" "" "DISPLACEMENT" (list (- (+ 2.0 topOfPole r (* 2 H r))) 0))
			)
			(command "._MOVE" "ALL" "" "DISPLACEMENT" (list (- (+ 2.0 topOfPole (* 2 H r))) 0))
		)
	)
	(setq blockName "NO-BN-2D-JBTSI-SIGNAL-")
	(if (= H 1)
		(setq blockName (strcat blockName "20-HS1-"))
	)
	(if (= H 2)
		(setq blockName (strcat blockName "20-HS2-"))
	)
	(if (= H 3)
		(setq blockName (strcat blockName "20-HS3-"))
	)

	(if (and (= H 0) (= F 1))
		(setq blockName (strcat blockName "23-FS-"))
		(if (= F 1) ;else if
			(setq blockName (strcat blockName "FS-"))
		)
	)
	(if (and (= H 0) (= F 0) (= D 1))
		(setq blockName (strcat blockName "43-DS-"))
		(if (= D 1) ;else if
			(setq blockName (strcat blockName "DS-"))
		)
	)
	(if (and (= H 0) (= F 0) (= D 0) (= MK 1))
		(setq blockName (strcat blockName "4C-MKS-"))
		(if (= MK 1) ;else if
			(setq blockName (strcat blockName "MKS-"))
		)
	)
	(if (and (= H 0) (= F 0) (= D 0) (= MK 0) (= FK 1))
		(setq blockName (strcat blockName "32-FKS-"))
		(if (= FK 1)
			(setq blockName (strcat blockName "FKS-"))
		)
	)
	(if (and (= H 0) (= F 0) (= D 0) (= MK 0) (= FK 0) (= HL 1))
		(setq blockName (strcat blockName "35B-HLS-"))
		(if (= HL 1)
			(setq blockName (strcat blockName "HLS-"))
		)
	)
	(if (= S68E 1)
		(setq blockName (strcat blockName "LHS-"))
	)
	(if (= 0 (+ H F D MK FK HL))
		(setq blockName (strcat blockName "TGV-SLUTT-"))
	)
	(if (= feste "AAK")
		(setq blockName (strcat blockName "AAK-"))
	)
	(command "._ROTATE" "ALL" "" "0,0" "90")
	(setq blockName (vl-string-right-trim "-" blockName))
	(newBlock blockName)
	blockName
)



(defun Hs1 (topOfPole / lantern1Pos) 
	(setq	lantern1Pos (list  (+ topOfPole (getSradius)) 0.0))
	(drawLantern lantern1Pos)
)



(defun Hs2 (topOfPole / lantern2Pos)
	(Hs1 topOfPole)
	(setq lantern2Pos (list  (+ topOfPole (* 3 (getSradius))) 0.0))
	(drawLantern lantern2Pos)
)



(defun Hs3 (topOfPole / lantern3Pos)
	(Hs2 topOfPole)
	(setq lantern3Pos (list  (+ topOfPole (* 5 (getSradius))) 0.0))
	(drawLantern lantern3Pos)
)

