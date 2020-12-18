(defun C:ISOLATOR (/)
  	(LINEISOLATOR)
  	(SEKSJONSISOLATOR)
	)

(defun LINEISOLATOR (/ blockName x y)
	(setq blockName "NO-BN-2D-JBTKL-ISOLATOR-LINEISOLATOR"
	x (/ 4.59 4)
	y (/ 1.0 4)
	)
	(command "._RECTANGLE" (list (- x) (- y)) (list x y)
		 "._LINE" (list (- x) (+ (- y) (/ (* y 2) 5))) (list x (+ (- y) (/ (* y 2) 5))) ""
		 "._LINE" (list (- x) (+ (- y) (* (/ (* y 2) 5) 2))) (list x (+ (- y) (* (/ (* y 2) 5) 2))) ""
		 "._LINE" (list (- x) (+ (- y) (* (/ (* y 2) 5) 3))) (list x (+ (- y) (* (/ (* y 2) 5) 3))) ""
		 "._LINE" (list (- x) (+ (- y) (* (/ (* y 2) 5) 4))) (list x (+ (- y) (* (/ (* y 2) 5) 4))) ""
		"._ROTATE" "ALL" "" (list 0 0) "-90"
	)
  (newBlock blockName)
  blockName
)

(defun SEKSJONSISOLATOR (/ blockName x y len)
	(setq blockName "NO-BN-2D-JBTKL-ISOLATOR-SEKSJONSISOLATOR"
		x (/ 1.75 8)
		y (/ 3.5 8)
		len (/ 3.5 4)
		)
	(command "._LINE" (list (- x) (- y)) (list (- x) y) ""
		"._LINE" (list x (- y)) (list x y) ""
		"._LINE" (list (- x) 0) (list (+ (- x) (- len)) 0) ""
		"._LINE" (list x 0) (list (+ x len) 0) ""
		"._ROTATE" "ALL" "" (list 0 0) "-90"
		)
	(newBlock blockName)
)