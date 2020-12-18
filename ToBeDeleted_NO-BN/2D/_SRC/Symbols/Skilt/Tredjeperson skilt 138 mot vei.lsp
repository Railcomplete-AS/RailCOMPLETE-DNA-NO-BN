;
; Tredjeperson skilt 138 mot vei.lsp
;
(defun SKILT-TREDJEPERSON-138-MOT-VEI (/ blockName description)
	(setq 
		blockName "NO-BN-2D-JBTSI-SKILT-TREDJEPERSON-138-MOT-VEI"
		description "Trafikkskilt 138 for Jernbanespor"
	)
	(command "._RECTANGLE" "0,0" "-4.50,1.50")
	(drawHatch 0.03)
	(command "._PLINE" "0.950962,0" "0,0" "0,1.50" "1.817253,1.50" "")
	(command "._MOVE" "ALL" "" "Displacement" "-1.817253,0")
	(command "._ROTATE" "ALL" "" "0,1.50" "-30")
	(command "._MIRROR" "ALL" "" "-1.50,0.633974" "0,0.633974" "NO")
	(command "._MIRROR" "ALL" "" "0,1.50" "0,0" "NO")
	(command "._SCALE" "ALL" "" "0,0" 0.25)
	(command "._MOVE" "ALL" "" "Displacement" "0,0.058")
	(newBlock blockName)
)
  
  
  
(defun SKILT-TREDJEPERSON-138-MOT-VEI-FLERE-SPOR (/ blockName description)
	(setq 
		blockName "NO-BN-2D-JBTSI-SKILT-TREDJEPERSON-138-MOT-VEI-FLERE-SPOR"
		description "Trafikkskilt 138 for Jernbanespor"
	)
	(command "._RECTANGLE" "0,0" "-4.50,1.50")
	(drawHatch 0.03)
	(command "._LINE" "0,0" "0.950962,0" "")
	(command "._LINE" "0,1.50" "1.817253,1.50" "")
	(command "._MOVE" "ALL" "" "Displacement" "-1.817253,0")
	(command "._ROTATE" "ALL" "" "0,1.50" "-30")
	(command "._MIRROR" "ALL" "" "-1.50,0.633974" "0,0.633974" "NO")
	(command "._MIRROR" "ALL" "" "0,1.50" "0,0" "NO")
	(command "._RECTANGLE" "-2.150962,-1.732051" "-5.450962,-3.232051")
	(command "._ROTATE" "L" "" "0,-1.732051" "30")
	(command "._MIRROR" "L" "" "0,0" "0,1" "NO")
	(drawHatchSelectPoint 0.03 (list (- 2.80) (- 4.00)) (/ 3.14 -3) 0)
	(drawHatchSelectPoint 0.03 (list 2.80 (- 4.00)) (/ 3.14 -6) 0)
	(command "._LINE" "0,-1.732051" "-2.150962,-1.732051" "")
	(command "._ROTATE" "L" "" "0,-1.732051" "30")
	(command "._MIRROR" "L" "" "0,0" "0,1" "NO")
	(command "._LINE" "-0.866025,-3.232051" "-2.150962,-3.232051" "")
	(command "._ROTATE" "L" "" "0,-1.732051" "30")
	(command "._MIRROR" "L" "" "0,0" "0,1" "NO")
	(command "._SCALE" "ALL" "" "0,0" 0.25)
	(command "._MOVE" "ALL" "" "Displacement" "0,0.058")
	(newBlock blockName)
)
