(defun C:RELEROM ()
	(RELEROM)
)

(defun RELEROM (/ blockName x y)
	(setq 
		x 12
		y 9
		blockName "NO-BN-2D-JBTSI-TEKNISK-BEBYGGELSE-RELEROM"
	)
	(command "._RECTANGLE" (list (/ x -2) 0) (list (/ x 2) y))
	(newBlock blockName)
	blockName
)