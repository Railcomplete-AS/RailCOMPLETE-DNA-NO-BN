;
; THUMBNAIL-Tabell.lsp
;
(defun C:THUMBNAIL-TABELL (/)
	(TABLE-GENERELL)
)



(defun TABLE-GENERELL (/ blockName x y) 
	(setq blockName (strcat "NO-BN-2D-JBTFE-THUMBNAIL-TABELL-GENERELL")
		x (/ 5.0 2)
		y (/ 3.0 2)
	)
	(command "._RECTANGLE" (list (- x) (- y)) (list x y)
		"._LINE" (list (- x) (* (/ y 3) 2)) (list x (* (/ y 3) 2)) ""
		"._LINE" (list (+ (- x) (* (/ x 5) 2)) y) (list (+ (- x) (* (/ x 5) 2)) (- y)) ""
	)
	(newBlock blockName)
	blockName
)

