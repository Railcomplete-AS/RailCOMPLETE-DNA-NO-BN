;
; draw.lsp
;
(defun drawPole (topOfPole startOfPole / )
	(command "._LINE"
		(list startOfPole 0)
		(list topOfPole 0)
		""
	)
	(princ)
)



(defun drawLantern (lanternPos / )
	(command "._CIRCLE" lanternPos (getSradius))
)



(defun drawBase (/ halfHeight halfWidth pt1 pt2)
	(setq 
		halfHeight 0.15
		halfWidth 1.0
		pt1 (list (- halfHeight) (- halfWidth))
		pt2 (list  halfHeight  halfWidth)
	)
	(command "._RECTANG" pt1 pt2)
	(drawHatch 0.01)
) 

