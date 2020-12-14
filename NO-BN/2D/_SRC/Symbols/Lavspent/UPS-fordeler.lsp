(defun C:UPS-FORDELER (/)
	(UPS-FORDELER)
	)

(defun UPS-FORDELER (/ blockName width height radius len)
  (setq blockName "NO-BN-2D-JBTEL-UPS-FORDELER"
		width (/ 0.5 2.0)
		height width
		radius 0.071
		len 0.181)
  (command "._RECTANGLE" (list (- width) (- height)) (list width height)
		   "._LINE" (list (- width) height) (list width (- height)) ""
		   "._LINE" (list (- 0.01) (- 0.085)) (list (- (+ 0.01 len)) (- 0.085)) ""
		   "._LINE" (list (- 0.01) (- 0.1)) (list (- (+ 0.01 len)) (- 0.1)) ""
		   "._ARC"  (list (- 0.033) 0.126) (list 0.016 0.085) (list 0.079 0.101)
		   "._ARC"  (list 0.079 0.101) (list 0.141 0.126) (list 0.193 0.084)
	   )
  (newBlock blockName)
  blockName
)