;
; Trekkekummer.lsp
;
(defun C:TREKKEKUMMER ()
	(newLayer "JBTUB__KU_KFOE_TYPE" 62 "Type føringsvei")
	(TREKKEKUM-RUND "1400" 0.700 0.126 0.137) ; Diameter, cover diameter, cover offset X and Y
	(TREKKEKUM-REKTANGULAER "1600"  "900" 0 1.32 0.68  0.000 0.450) ; Length (mm along track), depth [mm] (across track), cover diameter [m]  (or zero), cover length [m] (or don't care) cover depth [m] (or don't care), cover offset X and Y [m]
	(TREKKEKUM-REKTANGULAER "1400" "1400" 0.660 0 0 -0.225 0.925) ; Length [mm] (along track), depth [mm] (across track), cover diameter [m] (or zero), cover length [m] (or don't care) cover depth [m] (or don't care), cover offset X and Y [m]
	(TREKKEKUM-REKTANGULAER "2300" "2300" 0.660 0 0 -0.625 1.775) ; Length [mm] (along track), depth [mm] (across track), cover diameter [m] (or zero), cover length [m] (or don't care) cover depth [m] (or don't care), cover offset X and Y [m]
)


	
(defun TREKKEKUM-RUND (manholeDiameter coverDiameter coverOffsetX coverOffsetY / blockName)
	(setq				   
		blockName (strcat "NO-BN-2D-JBTUB-TREKKEKUM-RUND-" manholeDiameter)
	)
	(command
		"._CIRCLE" (list 0 0) (/ (ATOF manholeDiameter) 2000.0)
		"._CIRCLE" (list coverOffsetX coverOffsetY) (/ coverDiameter 2)
	)
	(command "._LAYER" "SET" "JBTUB__KU_KFOE_TYPE" "" "._COLOR" "ByLayer")
	(addMText (strcat "TREKKEKUM RUND " manholeDiameter) (list 0 (- (/ (ATOF manholeDiameter) -2000) 0.5)) 0.18 1.5 0 "iso" "TC")
	(newBlock blockName)
)


(defun TREKKEKUM-REKTANGULAER (manholeLength manholeDepth coverDiameter coverLength coverDepth coverOffsetX coverOffsetY / blockName)
	(setq				   
		blockName (strcat "NO-BN-2D-JBTUB-TREKKEKUM-L" manholeLength "-D" manholeDepth)
	)
	; Draw 'box':
	(command
		"._RECTANGLE" (list (/ (ATOF manholeLength) -2000.0) 0) (list (/ (ATOF manholeLength) 2000.0) (/ (ATOF manholeDepth) 1000.0))
	)
	(if (= coverDiameter 0)
		; Rectangular cover:
		(command
			"._RECTANGLE" 
				(list (+ coverOffsetX (/ coverLength -2)) (+ coverOffsetY (/ coverDepth -2)))
				(list (+ coverOffsetX (/ coverLength 2)) (+ coverOffsetY (/ coverDepth 2)))
		)
	;else
		;Circular cover:
		(command 
			"._CIRCLE" (list coverOffsetX coverOffsetY) (/ coverDiameter 2)
		)
    )
	(command "._LAYER" "SET" "JBTUB__KU_KFOE_TYPE" "" "._COLOR" "ByLayer")
	(addMText (strcat "TREKKEKUM L=" manholeLength ", D=" manholeDepth) (list 0 -0.5) 0.18 1.5 0 "iso" "TC")
	(newBlock blockName)
)
