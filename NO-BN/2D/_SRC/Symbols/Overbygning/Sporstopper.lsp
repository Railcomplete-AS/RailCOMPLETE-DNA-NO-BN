;
; Sporstopper.lsp
;
(defun C:SPORSTOPPER (/)
	(sporstopper "FAST")
	(sporstopper "GLIDBAR")
	(sporstopper "HYDRAULISK")
)

(defun sporstopper (spec / blockName x y)
	(setq	
		blockName (strcat "NO-BN-2D-JBTOB-SPOROBJEKT-SPORSTOPPER-" spec)
		x 2.394
		y 2.12
	)
	(command "._RECTANGLE" (list 0 (- (/ y 2.0))) (list x (/ y 2.0)))
	(if (= spec "GLIDBAR")
		(drawHatchSelectPoint 0.1 (list (/ x 2.0) 0) 0 0)
	)
	(if (= spec "HYDRAULISK")
		(drawHatchSelectPoint 0.0375 (list (/ x 2.0) 0) 0 0)
	)
	(command "._ROTATE" "ALL" "" (list 0 0) 90)
	(newBlock blockName)
    blockName
)