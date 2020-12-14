(defun C:LAVSPENTSKAP (/)
	(LAVSPENTSKAP)
	)

(defun LAVSPENTSKAP (/ blockName width height)
  (setq blockName "NO-BN-2D-JBTEL-SKAP-LAVSPENT"
		width (/ 5.62 2.0)
		height (/ 2.4167 2.0)
		)
  (command "._RECTANGLE" (list (- width) (- height)) (list width height)
		   "._LINE" (list (- width) height) (list 0.1144 (- 0.6379)) (list (- 0.1282) 0.7197) (list width (- (- height 0.15))) ""
		   "._LINE" (list (- width)  (- (- height 0.15))) (list width  (- (- height 0.15))) ""
	   )
  (newBlock blockName)
  blockName
)