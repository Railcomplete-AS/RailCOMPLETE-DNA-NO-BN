;
; Utligger.lsp
;
(defun C:UTLIGGER (/)
	(newLayer "JBTKL__EH_UTL_TYPENAVN" 62 "Utliggertype")
	(newLayer "JBTKL__EH_UTL_KRAFTRETNING" 2 "Utligger kraftretning")
  	(UTLIGGER-BRAKETT) ; For mounting of multiple cantilevers on the same mast. Argument: Separation of cantilevers.
	(UTLIGGER-20-25-35 "20A" "S" )
	(UTLIGGER-20-25-35 "20A" "T")
	(UTLIGGER-20-25-35 "20B" "S")
	(UTLIGGER-20-25-35 "20B" "T")
	(UTLIGGER-20-25-35 "20C" "S")
	(UTLIGGER-20-25-35 "20C" "T")
  	(UTLIGGER-20-25-35 "25" "S")
  	(UTLIGGER-20-25-35 "25" "T")
  	(UTLIGGER-20-25-35 "35" "S")
  	(UTLIGGER-20-25-35 "35" "T")
  	(UTLIGGER-CARIBONI)
)


; See Elkraftportalen EH.??????
(defun UTLIGGER-BRAKETT ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTKL-UTLIGGER-BRAKETT"
		len 1.200
	)
	(command 
		"._LAYER" "SET" "0" ""
		"._COLOR" "ByBlock"
		"._LINE" (list (/ len (- 2)) 0) (list (/ len 2) 0) ""
	)
	(newBlock blockName)
)



(defun UTLIGGER-20-25-35 (var kraftretning / blockName len)
	(setq blockName (strcat "NO-BN-2D-JBTKL-UTLIGGER-" var "-" kraftretning))
	(if (= kraftretning "S")  ; System 20C har tidligere hatt lengde 2.0 på "pilstammen". System 35 hadde 2.0 i tunnel.
		(setq len 3.5) ; if "S"
		(setq len 3.5) ; ...else
	)
	(command 
		"._LAYER" "SET" "0" ""
		"._COLOR" "ByBlock"
		"._LINE" (list 0 0) (list 0 (- len)) ""
	)
	(command
		"._LAYER" "SET" "JBTKL__EH_UTL_KRAFTRETNING" ""
		"._COLOR" "ByBlock"
	)
	(if (= kraftretning "S")  ; System 20C har tidligere hatt lengde 0.5 på "pilbladene"
		(command 
			"._LINE" (list 0 (- (/ len 1.0))) "@2<-45" ""
			"MIRROR" "L" "" "0,0" "0,1" "N"
		)
		(command
			"._LINE" (list 0 (- (/ len 1.0))) "@2<45" "" 
			"MIRROR" "L" "" "0,0" "0,1" "N"
		)
	) 
	(command
		"._LAYER" "SET" "JBTKL__EH_UTL_TYPENAVN" ""
		"._COLOR" "ByLayer"
	)
	; Font is set with (setq RcStyle "xxxx...x") in the already-loaded utility function for setting standard "ISO" Style
	(if (= kraftretning "S") ; S=strekk (pull-off), T=trykk (push-off)
		(addMText (strcat var "/" kraftretning) (list 2.5 (- (+ len 2))) 0.75 2.5 0 "ISO" "MC")
		(addMText (strcat var "/" kraftretning) (list 2 (- (+ len 1.5))) 0.75 2.5 0 "ISO" "MC")
	)
	(newBlock blockName)
	blockName
)



(defun UTLIGGER-CARIBONI (/ blockName rad len)
	(setq 
		blockName "NO-BN-2D-JBTKL-UTLIGGER-CARIBONI"
		len 1.215
	)
	(command
		"._LAYER" "SET" "0" ""
		"._COLOR" "ByBlock"
		"._LINE" (list 0 0) (list 0 (- len)) ""
	)
	(command
		"._LAYER" "SET" "JBTKL__EH_UTL_TYPENAVN" ""
		"._COLOR" "ByLayer"
	)
	(addMText "Car" (list 2 (-(+ len 1.5))) 0.75 2.5 0 "ISO" "MC")
	(newBlock blockName)
	blockName
)
