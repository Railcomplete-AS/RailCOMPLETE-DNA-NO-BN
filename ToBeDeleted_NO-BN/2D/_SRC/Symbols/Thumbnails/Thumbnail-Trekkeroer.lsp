;
; THUMBNAIL-Trekkeroer.lsp
;
(defun C:THUMBNAIL-TREKKEROER ( )
	(ALIGNMENT-TREKKEROER)
)



(defun ALIGNMENT-TREKKEROER (/ blockName r1 r2) 
	(setq 
		blockName "NO-BN-2D-JBTUB-THUMBNAIL-TREKKEROER"
		r1 0.5
		r2 0.45
	)
	(command
		"._PLINE"
			(list 0 0) (list 1 0) 
			"A" "CE" (list 1.0 r1)
			"A" "180" "R" r2
			"A" "180" "-90" "R" r1
			"A" "180" "90" "R" r2
			"A" "180" "-90" "R" r1
			"A" "180" "90" "R" r2
			"A" "22" "-168" 
			""
	)
	(newBlock blockName)
	blockName
)

