;
; 68.lsp
; Speed boards
;
(defun 68A (/ blockName description side)
	(setq 
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-68A-NEDSATT-HASTIGHET"
		description "Signal 68A Nedsatt kjørehastighet"
		side 4.0
	)
	(drawTriangle side)
	(addAtt "HAST_5" "Bank eller \"5\"" "" ;no value
		"1.061,2.922" 1.0 0.0 "iso" "MC" 16)
	(addAtt "HAST_10" "Vent-hastighet (10-ere):" 12
		"0.058,1.956" 1.25 0.0 "iso" "MC" 16)
	(newBlock blockName)
	description
)



(defun 68B (/ blockName description side)
	(setq 
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-68B-OEKT-HASTIGHET"
		description "Signal 68B økt kjørehastighet"
		side 4.0
	)
	(drawTriangle side)
	(command 
		"._ROTATE" "ALL" ""
	   (strcat "0," (rtos (* (/ side 2) (cos (D->R 30))) 2 10)) "180"
	)
	(addAtt "HAST_5" "Blank eller \"5\":" "" ;no value
		"1.1831,-0.1009" 1.0 0.0 "iso" "BR" 16)
	(addAtt "HAST_10" "Kjør-hastighet (0/5):" 12
		"0,1.1547" 1.25 0.0 "iso" "MC" 16)
	(newBlock blockName)
	description
)



(defun 68C (/ blockName description  side)
	(setq
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-68C-NEDSATT-HASTIGHET"
		description "Signal 68C Nedsatt kjørehastighet"
		radius 1.5
	)
	(command
		"._CIRCLE" (list 0 radius) radius
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._POLYGON" "120" (list 0 radius) "Inscribed" radius
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
	)
	(addAtt "HAST_10" "Hastighet (10-ere)" 6
		"0,1.4000" 1.25 0.0 "iso" "MC" 16)
	(addAtt "HAST_5" "Blank eller \"5\":" "" ;endret prompt ift TRV_500
		"0.4584,2.2878" 0.9 0.0 "iso" "MC" 16)
	(newBlock blockName)
	description
)



(defun 68D-1 (/ blockName description side)
	(setq
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-68D-1-MARKERINGSMERKE"
		description "Signal 68D-1 Markeringsmerke"
		side (+ 2 (/ 2 3.0))
	)
	(drawTriangle side)
	(newBlock blockName)
	description
)



(defun drawTriangle (side / ang)
	(setq ang (D->R 60))
	(command 
		"._PLINE" "0,0"
			(list (* side (cos ang)) (* side (sin ang)))
			(list (- (* side (cos ang))) (* side (sin ang)))
			"CLOSE"
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	)
)


 
(defun 68D-2 (/ blockName description ang side1 side2 shift blockName)
	(setq
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-68D-2-MARKERINGSMERKE-MOTRETTET"
		description "Signal 68D-2 Markeringsmerke på motrettet signal 68B"
		side 2.66667
	)
	(command
		"._PLINE"
			"-0.22222212,0.3849"
			"-2,0.3849"
			"-1.11111106,1.92450081"
			"Close"
			"._Mirror" "L" ""
			"0,0" "0,1"
			"No"
			"._PLINE"
			"-0.88888879,2.30940108"
			"0.88888879,2.30940108"
			"0,3.84900162"
			"CLOSE"
	)
	(drawHatchOptions 0.01 0 0.01 "ANSI31" "ALL")
	(drawTriangle side)
	(newBlock blockName)
	description
)



(defun 68D-3 (/ blockName description ang side1 side2 shift blockName)
	(setq
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-68D-3-MARKERINGSMERKE-MOTRETTET-MIDLERTIDIG"
		description "Signal 68D-2 Markeringsmerke på motrettet signal 69B"
		side 2.66667
	)
	(command 
		"._PLINE"
			"-0.75,1.299"
			"-0.75,0.385"
			"-0.222,0.385"
			"Close"
			"._Mirror" "L" ""
			"0,0" "0,1"
			"No"
			"._PLINE"
			"0.75,2.309"
			"0.75,2.635"
			"0,3.385"
			"-0.75,2.635"
			"-0.75,2.309"
			"Close"
	)
	(drawHatchOptions 0.01 0 0.01 "ANSI31" "ALL")
	(drawTriangle side)
	(newBlock blockName)
	description
)



(defun 68F (/ blockName description side)
	(setq
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-68F-TILLEGGSHASTIGHET"
		description "Signal 68F Tilleggshastighet"
		x 3.0
		y 1.5
	)
	(command
		"._RECTANG"
			(list (/ x (- 2)) 0)
			(list (/ x 2) y)
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	  )
	(addAtt "PLUSS_HAST" "Pluss-hastighet" "+5" ;endret prompt ift TRV_500
		"0,0.7500" 1.0 0.0 "iso" "MC" 16)
	(newBlock blockName)
	description
)
 
 
 
(defun 68G (/ blockName description side)
	(setq
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-68G-HASTIGHET-KRENGETOG"
		description "Signal 68G Hastighet for krengetog"
		x 3.0
		y 1.5
	)
	(command
		"._RECTANG"
			(list (/ x (- 2)) 0)
			(list (/ x 2) y)
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
	)
	(addAtt "KRENGE_HAST" "Krengetoghastighet (km/h)" "160" ;endret prompt ift TRV_500
		"0,0.7500" 1.25 0.0 "iso" "MC" 16)
	(newBlock blockName)
	description
)
