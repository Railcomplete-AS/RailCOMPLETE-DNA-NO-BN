(defun C:ARMATUR (/ )
	(ARMATUR)
	)

(defun ARMATUR (/ blockName radius)
  (setq blockName "NO-BN-2D-JBTEL-BELYSNING-ARMATUR"
		radius 1.5)
  (command "._CIRCLE" (list 0 0) radius
		   "._LINE" (list (- radius) 0) (list radius 0) ""
		   "._LINE" (list 0 radius) (list 0 (- radius)) ""
		   "._ROTATE" "ALL" "" (list 0 0) 45
	   )
  (newBlock blockName)
  blockName
)