(defun C:TELEFON (/)
	(newLayer "JBTFE__FE_DIV_OBJEKTTYPENAVN" 62 "Objekttypenavn")
	(TELEFON-LANGS-SPOR)
)



(defun TELEFON-LANGS-SPOR (/ blockName halfWidth halfHeight)
	(setq 
		blockName "NO-BN-2D-JBTTE-TELEFON-LANGS-SPOR"
		halfWidth (/ 2.0 2)
		halfHeight (/ 2.0 2)
	)
	(command 
		"._RECTANGLE" (list (- halfWidth) (- halfHeight)) (list halfWidth halfHeight)
	)
	(command "._LAYER" "SET" "JBTFE__FE_DIV_OBJEKTTYPENAVN" "" "._COLOR" "ByLayer")
	(addMText "Tlf." "0,-0.6" 0.18 1.5 0 "ISO" "TC")
	(newBlock blockName)
	blockName
)
