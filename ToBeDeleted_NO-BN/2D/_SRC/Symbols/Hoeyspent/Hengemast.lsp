;
; Hengemast.lsp
;
(defun C:HENGEMAST (/)
	(HENGEMAST-I-TUNNEL)
	(HENGEMAST-CARIBONI)
	(HENGEMAST-I-AAK)
)



(defun HENGEMAST-I-TUNNEL (/ blockName x y)
  (setq 
		blockName "NO-BN-2D-JBTKL-HENGEMAST-I-TUNNEL"
		x (/ 1.5 2) ; halvbredde
		y (/ 2.0 2) ; halvhøyde
	)
	(command 
		"._RECTANGLE" (list (- x) (- y)) (list x y)
		"._LINE" (list (- x) (- y)) (list x y) ""
		"._LINE" (list (- x) y) (list x (- y)) ""
	)
	(newBlock blockName)
	blockName
)



(defun HENGEMAST-CARIBONI (/ blockName x y offset)
	(setq
		blockName "NO-BN-2D-JBTKL-HENGEMAST-CARIBONI"
		x (/ 1.0 2) ;halvbredde
		y (/ 2.0 2) ;halvhøyde
		offset 0.075 ;offset pga kort utligger og masten må plasseres nær sporet
	)
	(command "._RECTANGLE" (list (- x) (- y)) (list x y)
		"._LINE" (list (- x) (- y)) (list x y) ""
		"._LINE" (list (- x) y) (list x (- y)) ""
		"._MOVE" "ALL" "" (list 0 0) (list 0 (- y offset))
	)
	(newBlock blockName)
	blockName
)



(defun HENGEMAST-I-AAK (/ blockName rad)
	(setq 
		blockName "NO-BN-2D-JBTKL-HENGEMAST-I-AAK"
		rad 0.525
	)
	(command "._CIRCLE" "0,0" rad)
	(drawHatch 0.030)
	(newBlock blockName)
	blockName
)



