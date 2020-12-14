;
; Karakteristisk trasepunkt
;
;
(defun C:KARAKTERISTISK-TRASEPUNKT (/)
  (drawTrasepunkt)
  )

; Symmetrical short line transversal to track, to be centered on alignment axis (whereas insulated joints are located on one of the rails).
 (defun drawTrasepunkt ( / blockName sideLength)
	(setq 
		blockName "NO-BN-2D-JBTOB-KARAKTERISTISK-TRASEPUNKT"
		sideLength 1.5
	)
	(command
		"._LINE" (list 0 (- sideLength)) (list 0 sideLength) ""
	)
	(newBlock blockName)
)
