;
; NO-BN-EH-SPV.lsp
;
; Bane NOR, Elektro/Høyspent, sporvekselsymboler
; Skravert sporvekselsymbol betyr "strømløst i avvikessporet".
;
(defun NO-BN-EH-SPV (quadrant Drawing_Number 
	/ 
		blockName
		switchParameters
		crossType
		A B R x railProfile ang 
		str i hatchVariant
	)
	(setq
		switchParameters (getSwitchParameters Drawing_Number)
		crossType 	(cadr (assoc "Skinnekryss" switchParameters))
		A			(/ (cadr (assoc "A" switchParameters)) 1000.0)
		B			(/ (cadr (assoc "B" switchParameters)) 1000.0)
		R 			(cadr (assoc "R" switchParameters))
		x			(cadr (assoc "x" switchParameters))
		railProfile	(cadr (assoc "Skinneprofil" switchParameters))
		ang	  	(R->D (atan (/ 1.0 x))) ; sporvekselsymbol
	)
	(setq str "")
	(if (< x 10)
		(setq str "0")
	)
	(setq i 0)
	(repeat 2
		(cond
			((= i 0) (setq hatchVariant "LEDNING-ENKELT-SPOR"))
			((= i 1) (setq hatchVariant "LEDNING-BEGGE-SPOR"))
		)
		(setq
			blockName (strcat "NO-BN-2D-JBTKL-CONNECTION-SWITCH-" str (rtos x 2 2) "-R" (rtos R 2 0) "-" railProfile "-" crossType "-" hatchVariant "-" (rtos quadrant 2 0))
		)
		(command 
			"._LAYER" "SET" "0" ""
			"._COLOR" "ByBlock"
		)
		(if (= hatchVariant "LEDNING-ENKELT-SPOR")
			(progn
				(command "._PLINE" (list A 0) (list (+ A (/ B 2.0)) 0) (list (+ A (/ B 2.0)) (* (/ B 2.0) (tan (D->R ang)))) "CLOSE")
				(drawHatchOptions 0.06 0 0 "ANSI37" "L")
				(command	"._PLINE" (list (+ A (/ B 2.0)) 0) (list (+ A (* B 0.75)) 0) (list (+ A (* B 0.75)) (* (* B 0.75) (tan (D->R ang)))) (list (+ A (/ B 2.0)) (* (/ B 2.0) (tan (D->R ang)))) "CLOSE")
				(drawHatch 0.06)
			)
			(progn
				(command "._PLINE" (list A 0) (list (+ A (* B 0.75)) 0) (list (+ A (* B 0.75)) (* (* B 0.75) (tan (D->R ang)))) "CLOSE")
				(drawHatchOptions 0.06 0 0 "ANSI37" "L")
			)
		)
		(command
			"._LINE" (list (+ A (* B 0.75)) 0) (list (+ A B) 0) ""
			"._LINE" (list (+ A (* B 0.75)) (* (* B 0.75) (tan (D->R ang)))) (list (+ A B) (* B (tan (D->R ang)))) ""
		)
		;Mirror 0x, 1x eller 2x til korrekt kvadrant:
		(mirrorSelection quadrant "ALL")
		(newBlock blockName)
		(setq i (+ 1 i))
	);repeat
	(command
		"._LAYER" "Set" "0" ""
		 "._COLOR" "ByBlock"
	)
	blockName
)