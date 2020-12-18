;
; Balise.lsp
;
(defun C:BALISE ()
	(Balisegruppe_ref)
	(Balisegruppe)
	(newLayer "JBTSI__SA_ATB_METALLFRITT_PROFIL" 62 "Signal - Metallfritt profil")
	(newLayer "JBTSI__SA_ATB_SNAPLINJER_3m" 62 "Signal - Snaplinjer balise 3m")
	(newLayer "JBTSI__SA_ATB_SNAPLINJER_8m" 200 "Signal - Snaplinjer balise 8m")
	(newLayer "JBTSI__SA_ATB_SNAPLINJER_12m" 240 "Signal - Snaplinjer balise 12m")
	(command "._LAYER" "SET" "0" "")
	(setq 
		config 1
		osmode (getvar 'OSMODE)
	)
	(setvar "OSMODE" 0)
	(repeat 4
		(drawBalise config)
		(setq config (+ 1 config))
	)
	(princ)
)

(defun drawBalise (config / side dx)
	(setq 
		side 1.5
		dx 0.05
		y (* side (sin (D->R 60)) 0.5)
	)
	(command 
		"._PLINE"
			"0,0"
			(list side 0)
			(strcat "@" (rtos side) "<120")
			"Close"
	)
	(cond 
		((= config 1)
			(setq blockName "NO-BN-2D-JBTSI-ATC-BALISE-TOM-FAST")
		)
		((= config 2)
			(setq blockName "NO-BN-2D-JBTSI-ATC-BALISE-TOM-STYRT")
			(command 
				"._RECTANG" 
				(list 0 (* side (sin (D->R 60)))) 
				(list side (+ (* side (sin (D->R 60))) dx))
			)
			(drawHatch "0.03")
		)
		((= config 3)
			(drawHatchSelectPoint "0.03" "0.1,0.1" 0 0.1)
			(setq blockName "NO-BN-2D-JBTSI-ATC-BALISE-FYLT-FAST")
		)
		((= config 4)
			(drawHatchSelectPoint "0.03" "0.1,0.1" 0 0.1)
			(setq blockName "NO-BN-2D-JBTSI-ATC-BALISE-FYLT-STYRT")
			(command 
				"._RECTANG"
				(list 0 (* side (sin (D->R 60))))
				(list side (+ (* side (sin (D->R 60))) dx))
			)
			(drawHatch "0.03")
		)
		(setvar "OSMODE" osmode)
	)
	;; shift symbol
	(command "._MOVE" "ALL" "" "DISPLACEMENT" (list (/ side -2) 0))

	;https://trv.jbv.no/wiki/Signal/Bygging/ATC/Parallellbaliser_og_kodere#label-fig:Metallfritt område, og avstand til kryssende kabler
	;Metallfritt område
	(setq 
		x_dim 0.6
		y_dim 0.5
		A 0.5
		B 0.5
		C 0.2
		x (+ (/ x_dim 2) A)
		y (+ (/ y_dim 2) C)
	)
	(command 
		"._LAYER" "Set" "JBTSI__SA_ATB_METALLFRITT_PROFIL" ""
		"._COLOR" "ByLayer"
		"._RECTANG" (list (- x) (- y)) (list x y)
	)
	(command 
		"._LAYER" "Set" "JBTSI__SA_ATB_SNAPLINJER_3m" ""
		"._COLOR" "ByLayer"
		"._PLINE" (list 3 0) "A" "CE" (list 0 0) "A" 15 ""
		"._MIRROR" "L" "" (list 0 0) (list 0 1) "N"
		"._MIRROR" "L" "" (list 0 0) (list 1 0) "N"
		"._MIRROR" "L" "" (list 0 0) (list 0 1) "N"
	)
	(addText "3" (list (- 2.9) (- 0.5)) 0.2 0 "iso" "TL")
	(command "._MIRROR" "L" "" (list 0 0) (list 0 1) "N")
	(command 
		"._LAYER" "Set" "JBTSI__SA_ATB_SNAPLINJER_8m" ""
		"._COLOR" "ByLayer"
		"._PLINE" (list 8 0) "A" "CE" (list 0 0) "A" 9 ""
		"._MIRROR" "L" "" (list 0 0) (list 0 1) "N"
		"._MIRROR" "L" "" (list 0 0) (list 1 0) "N"
		"._MIRROR" "L" "" (list 0 0) (list 0 1) "N"
	)
	(addText "8" (list (- 7.9) (- 0.95)) 0.2 0 "iso" "TL")
	(command "._MIRROR" "L" "" (list 0 0) (list 0 1) "N")
	(command 
		"._LAYER" "Set" "JBTSI__SA_ATB_SNAPLINJER_12m" ""
		"._COLOR" "ByLayer"
		"._PLINE" (list 12 0) "A" "CE" (list 0 0) "A" 10 ""
		"._MIRROR" "L" "" (list 0 0) (list 0 1) "N"
		"._MIRROR" "L" "" (list 0 0) (list 1 0) "N"
		"._MIRROR" "L" "" (list 0 0) (list 0 1) "N"
	)
	(addText "12" (list (- 11.8) (- 1.8)) 0.2 0 "iso" "TL")
	(command "._MIRROR" "L" "" (list 0 0) (list 0 1) "N")
	(command 
		"._LAYER" "SET" "0" ""
		"._COLOR" "ByBlock"
	)
	(addAtt "TEKSTOVER" "Tekst over" "" (list 0 2.0) 1.0 0 "iso" "MC" 16)
	(addAtt "TEKSTUNDER" "Tekst under" "" (list 0 (- 0.8)) 1.0 0 "iso" "MC" 16)
	(newBlock blockName)
	(princ)
)

(defun Balisegruppe_ref (/ blockName)
	(setq blockName "NO-BN-2D-JBTSI-ATC-BALISEGRUPPE-REFERANSE")
	(command 
		"._LINE" "0,1.5981" "-0.3,1.0785" ""
		"._LINE" "-0.6,0.5588" "-0.9,0.0392" ""
		"._LINE" "-1.2,-0.4804" "-1.5,-1" "-0.9,-1" ""
		"._LINE" "-0.3,-1" "0,-1" ""
		"._MIRROR" "ALL" "" "0,0" "0,1" "NO"
		"._MOVE" "ALL" "" "Displacement" "0,1"
	)
	(newBlock blockName)
	blockName
)
  		
(defun Balisegruppe (/ blockName)
	(setq blockName "NO-BN-2D-JBTSI-ATC-BALISEGRUPPE")
	(command 
		"._LINE" "0,1.5981" "-0.3,1.0785" ""
		"._LINE" "-0.6,0.5588" "-0.9,0.0392" ""
		"._LINE" "-1.2,-0.4804" "-1.5,-1" "-0.9,-1" ""
		"._LINE" "-0.3,-1" "0,-1" ""
		"._MIRROR" "ALL" "" "0,0" "0,1" "NO"
		"._MOVE" "ALL" "" "Displacement" "0,1"
		"._SCALE" "ALL" "" "0,0" "0.5"
	)
	(newBlock blockName)
	blockName
)	
