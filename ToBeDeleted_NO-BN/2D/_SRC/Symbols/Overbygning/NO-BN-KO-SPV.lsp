;
; NO-BN-KO-SPV.lsp
;
; Bane NOR, konstruksjon/overbygning, sporvekselsymboler
;
; Sporvekslene har et tungeparti, midtpart og bakkant - definert via et Excelark med koordinater hentet fra TRV. (se etter f.eks. NO-BN switches.xlsx')
; Etter bakkant (BK) kommer et langsvilleparti (der begge spors skinner deler sville). 
; Etter langsvillepartiet kommer et kortsvilleparti (der hvert spor har sine egne sviller, men de er korte siden det ikke 
; ennå er sporavstand nok. Det er også vanlig at kortsvillepartiet gir en gradvis overgang fra rette skinner til skråstilling,
; siden skinner bygges ofteste rett-opp-og-ned inni sporvekselen, men med skinnehelning 1:20 på strekningen forøvrig.
;
(defun NO-BN-KO-SPV (quadrant Drawing_Number 
	/ 	blockName
		switchParameters
		crossType  A B C D E F L R x railProfile ang radius
		str pt pts
	)
	(setq 
		switchParameters (getSwitchParameters Drawing_Number)
		crossType	(cadr (assoc "Skinnekryss" switchParameters))
		A			(/ (cadr (assoc "A" switchParameters)) 1000.0)
		B			(/ (cadr (assoc "B" switchParameters)) 1000.0)
		C			(/ (cadr (assoc "C" switchParameters)) 1000.0)
		D			(/ (cadr (assoc "D" switchParameters)) 1000.0)
		E			(/ (cadr (assoc "E" switchParameters)) 1000.0)
		F			(/ (cadr (assoc "F" switchParameters)) 1000.0)
		L			(/ (cadr (assoc "L" switchParameters)) 1000.0)
		R			(cadr (assoc "R" switchParameters))
		x			(cadr (assoc "x" switchParameters))
		railProfile	(cadr (assoc "Skinneprofil" switchParameters))
		ang (R->D (atan (/ 1.0 x))) ; sporvekselsymbol
		radius 0.75
	)
	(setq str "")
	(if (< x 10)
		(setq str "0")
	)
	(setq blockName (strcat "NO-BN-2D-JBTOB-CONNECTION-SWITCH-" str (rtos x 2 2) "-R" (rtos R 2 0) "-H-" railProfile "-" crossType "-" (rtos quadrant 2 0)))
 	(command
		"._LAYER" "Set" "0" ""
		"._COLOR" "ByBlock"
		"._LINE" "0,0" (list A 0) ""
		"._LINE" (list A 1) (list A -1) ""
		"._LINE" (list 0 1) (list 0 -1) ""
		"._ARC" (list L 0) "C" (list A 0) "Angle" (+ ang 1.5)
		"._ARC" (list L 0) "C" (list A 0) "Angle" -1.5
		"._PLINE"
			(list A 0)
			(list (+ A C) 0)
			"Arc" "CE" (list A 0) "Angle" ang 
			"Line" "Close"
	)
	(drawHatch "0.06")
	(command 
		"._PLINE" 
			(list (+ A C) 0)
			"Arc" "CE" (list A 0)
			"Angle" ang 
			"Line" (strcat "@" (rtos D) "<" (rtos ang))
			"Arc" "CE" (list A 0)
			"Angle" (- ang)
			"Line" "Close"
	)
	(drawHatch "0.2")

	;Avviksspor / senterlinje
	(command
		"._LAYER" "Set" "JBTOB__KO_SPV_SENTERLINJER" ""
		"._COLOR" "ByLayer"
		"._PLINE" 
			"0,0"
			"Arc" "Direction" "1,0" (polar (list A 0) (D->R ang) C)
			"Line" (strcat "@" (rtos D) "<" (rtos ang))
			""
		"._PLINE" "0,0" (list L 0) ""
	) 
  
	; Vis langsvillepartiet utenfor BK (bakkant sporveksel)
	(command 
		"._LAYER" "Set" "JBTOB__KO_SPV_LANGSVILLEPARTI_FRA_BK" ""
		"._COLOR" "ByLayer"
		"._PLINE"
			(list L 0)
			"Arc" "CE" (list A 0) "Angle" ang
			"Line" (strcat "@" (rtos E) "<" (rtos ang))
			"Arc" "CE" (list A 0) "Angle" (- ang)
			"Line" 
			"Close"
	)
	; (drawHatchOptions 0.25 0 0.01 "ANSI37" "L") ; Vi valgte å utelate hatch i dette partiet
	;END Langsvilleparti

	; Vis kortsvillepartiet utenfor BK (bakkant sporveksel, etter langsvillepartiet)
	(command
		"._LAYER" "Set" "JBTOB__KO_SPV_KORTSVILLEPARTI_FRA_BK" ""
		"._COLOR" "ByLayer"
		"._PLINE"
			(list (+ L E) 0)
			"Arc" "CE" (list A 0) "Angle" ang
			"Line" (strcat "@" (rtos F) "<" (rtos ang))
			"Arc" "CE" (list A 0) "Angle" (- ang) 
			"Line" "Close"
    )
	; (drawHatchOptions 0.25 0 0.01 "ANSI37" "L") ; Vi valgte å utelate hatch i dette partiet
	; Add 'division line' to illustrate that there are now one set of sleepers for each turnout leg:
	(command
		"._COLOR" "ByLayer"
		"._LINE" (list (+ A (* (+ B E) (cos (D->R(/ ang 2.0))))) (* (+ B E) (sin (D->R (/ ang 2.0))))) (strcat "@" (rtos F) "<" (rtos (/ ang 2.0))) ""
	)
	;END Kortsvilleparti
    
	; Forbudt-område for akseltellere - tellepunkter (der det er vingeskinner, eller der nærmeste skinnestreng er mindre enn 60 cm unna)
	(command
		"._LAYER" "Set" "JBTOB__KO_SPV_MIN_60_CM_SKINNESEPARASJON" ""
		"._COLOR" "ByLayer"
	)
	(setq pts (getArea1 Drawing_Number))
	(command "._PLINE" (foreach pt pts (command pt)))
	(drawHatchOptions 0.25 0 0.01 "ANSI37" "L")
	(setq pts (getArea2 Drawing_Number))
	(command "._PLINE" (foreach pt pts (command pt)))
	(drawHatchOptions 0.25 0 0.01 "ANSI37" "L")
	;END Forbudtområde for tellepunkter

	(mirrorSelection quadrant "ALL")
	(newBlock blockName)
	(command
		"._LAYER" "Set" "0" ""
		"._COLOR" "ByBlock"
	)
	blockName
)



