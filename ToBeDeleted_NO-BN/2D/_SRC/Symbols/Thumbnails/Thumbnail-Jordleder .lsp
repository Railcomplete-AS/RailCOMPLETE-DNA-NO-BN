;
; THUMBNAIL-Jordleder.lsp
;
(defun C:THUMBNAIL-JORDLEDER (/)
	(ALIGNMENT-JORDLEDER)
)


(defun ALIGNMENT-JORDLEDER (/ blockName) 
  (setq blockName "NO-BN-2D-JBTKL-THUMBNAIL-JORDLEDER")
	(command 
		"._LINE" (list 0 10) (list 5 10) ""		
		"._LINE" (list 10 10) (list 15 10) ""
		"._LINE" (list 20 10) (list 25 10) ""
		; (Gap here, for 'jl' letters)
		"._LINE" (list 40 10) (list 45 10) ""
		"._LINE" (list 50 10) (list 55 10) ""
		"._LINE" (list 60 10) (list 65 10) ""

		; 'jl' letters:
		"._LINE" (list 30 15) (list 30 12) ""
		"._PLINE" 
		(list 28 10)
		(list 31 10)
		(list 31 2)
		(list 29 0)
		(list 27 0)
		""

		"._PLINE" 
		(list 34 15)
		(list 36 15)
		(list 36 4)
		(list 34 4)
		(list 38 4)
		""

		; Scale down and move:
		"._SCALE" "All" "" (list 0 0) "0.1"
		"._MOVE" "All" "" (list 2.5 0.5) (list 0 0)
	)
	(newBlock blockName)
	blockName
)


  