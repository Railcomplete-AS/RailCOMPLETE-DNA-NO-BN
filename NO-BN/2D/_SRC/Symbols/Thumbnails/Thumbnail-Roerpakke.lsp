;
; THUMBNAIL-Roerpakke.lsp
;
(defun C:THUMBNAIL-ROERPAKKE (/)
	(ALIGNMENT-ROERPAKKE)
)



(defun ALIGNMENT-ROERPAKKE (/ blockName x y r) 
  (setq blockName "NO-BN-2D-JBTUB-THUMBNAIL-ROERPAKKE"
		y 0.155
		x 0.45
		r 0.055
	)
	(command 
		"._PLINE" 
			(list (/ x 2.0) 0)
			(list (/ x 2.0) (* y 0.9))
			(list 0.1954 y)
			(list 0 y)
			""
		"._MIRROR" "ALL" "" "0,0" "0,1" "NO"
		"._CIRCLE" (list 0 0.07) r
		"._CIRCLE" (list 0.14 0.07) r
		"._MIRROR" "L" "" "0,0" "0,1" "NO"
		"._MIRROR" "ALL" "" "0,0" "1,0" "NO"
		;
		"._SCALE" "ALL" "" (list 0 0) "10"
	)
	(newBlock blockName)
	blockName
)

