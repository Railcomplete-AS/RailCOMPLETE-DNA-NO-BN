(defun C:TRAFO (/ )
	(TRAFO)
	)

(defun TRAFO ( / blockName width height radius)
  (setq blockName "NO-BN-2D-JBTEL-TRANSFORMATOR"
		width (/ 4.0 2.0)
		height (/ 2.5 2.0)
		radius 0.9)
  (command "._RECTANGLE" (list (- width) (- height)) (list width height)
		   "._CIRCLE" (list (- 0.6) 0) radius
		   "._CIRCLE" (list 0.6 0) radius
	   )
  (newBlock blockName)
  blockName
)