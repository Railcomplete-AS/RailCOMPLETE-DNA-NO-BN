;
; Akselteller.lsp
;
(defun C:AKSELTELLER (/ blockName rad 2rad scale osmode)
	(setq	
		blockName "NO-BN-2D-JBTSI-TOGDETEKSJON-TELLEPUNKT-SENSOR"
		rad	  0.5
		2rad	  (* rad 2.0)
		scale	0.75
		osmode	  (getvar 'OSMODE)
	)
	(setvar "OSMODE" 0)
	(command 
		"._LAYER" "SET" "0" ""
	   "._LINE" "0,0" (list 0 2rad) ""
	   "._LINE" (list (- rad) 2rad) (list rad 2rad) ""
	   "._LINE" (list (- (+ 2rad 2rad)) 2rad) (list (- (+ 2rad rad)) 2rad) ""
	   "._LINE" (list (+ 2rad 2rad) 2rad) (list (+ 2rad rad) 2rad) ""
	   "._CIRCLE" (list (- 2rad) 2rad) rad
	)
	(drawHatch 0.05)
	(command "._CIRCLE" (list 2rad 2rad) rad)
	(drawHatch 0.05)
	(command 
		"Color" "ByBlock"
		"._MOVE" "ALL" "" "D" (list 0 (- 2rad))
		"._SCALE" "ALL" "" "0,0" scale
    )
	(newLayer "JBTSI__SA_TEL_RESERVERT_OMRÅDE_SOPP"
	    62
	    "Signal - ugyldig område for plassering av akselteller i SPV"
	)
	(command 
		"._LAYER" "SET" "JBTSI__SA_TEL_RESERVERT_OMRÅDE_SOPP" ""
		"._COLOR" "ByLayer"
		"._RECTANG"  "-2,2.75" "2,3.25"
	)
	(drawHatch 0.1)
	(newBlock blockName)
	(setvar "OSMODE" osmode)
	(princ)
)