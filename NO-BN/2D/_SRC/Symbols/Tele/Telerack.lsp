(defun C:TELERACK (/)
	(TELERACK)
)

(defun TELERACK (/ blockName halfWidth halfHeight radius)
	(setq
		blockName "NO-BN-2D-JBTTE-TELERACK"
		halfWidth (/ 5.62 2.0)
		halfHeight (/ 2.4167 2.0)
		radius 0.08
	)
	(command 
		"._RECTANGLE" (list (- halfWidth) (- halfHeight)) (list halfWidth halfHeight)
		"._LINE" (list (- halfWidth)  (- (- halfHeight 0.15))) (list halfWidth  (- (- halfHeight 0.15))) ""
		"._CIRCLE" (list 0 0) radius
	)
	(addText "Telerack" (list 0 0.6) 0.5 0 "iso" "MC")
	(newBlock blockName)
	blockName
)
