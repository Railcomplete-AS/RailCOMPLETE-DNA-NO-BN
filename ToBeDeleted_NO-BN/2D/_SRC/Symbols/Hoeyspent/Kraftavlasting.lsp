;
; Kraftavlasting.lsp
;
(defun C:KRAFTAVLASTING (/)
	(BARDUNFESTE-TUNNEL)	; Bolts to tunnel wall, holding a bracket which holds a rod terminated in a ring where the OCS wire stretcher is to be fastened. See EH-702679.
	(BARDUNFESTE-ENKEL)		; Bracket which is screwed onto a foundation for a single OCS wire strecher (e.g., bardunfundament boret 2500mm)
	(BARDUNFESTE-DOBBEL)	; Bracket which is screwed onto a foundation for two OCS wire strechers (going to the same OCS mast) (e.g., bardunfundament boret 2500mm)
	(STREVERFESTE)			; Bracket which is screwed onto a foundation for a OCS wire spanners (e.g., bardunfundament boret 2500mm)
)




(defun BARDUNFESTE-TUNNEL (/ blockName x y)
  (setq 
		blockName "NO-BN-2D-JBTKL-KRAFTAVLASTING-TUNNELFESTE"
		x (/ 1.5 2) ; halvbredde
		y (/ 2.0 2) ; halvhøyde
	)
	(command 
		"._LINE" (list (- x) (- y)) (list x y) ""
		"._LINE" (list (- x) y) (list x (- y)) ""
	)
	(newBlock blockName)
	blockName
)



(defun BARDUNFESTE-ENKEL ( / blockName len len2 rot)
	(setq
		blockName "NO-BN-2D-JBTKL-KRAFTAVLASTING-FOR-BARDUN-ENKEL"
		len 2.0
		len2 5
		rot 14.0
	)
	(command
		"._LINE" (list len2 0) (list (- len2 len) 0) ""
		"._ROTATE" "L" "" (list len2 0) rot
		"._MIRROR" "LAST" "" (list 0 0) (list 1 0) "N"
		"._LINE" (list 0 0) (list len2 0) ""
		"._ROTATE" "ALL" "" (list 0 0) "-90"
		"._MOVE" "ALL" "" (list 0 0) (list 0 len2)
	)
	(newBlock blockName)
	blockName
)		


	
(defun BARDUNFESTE-DOBBEL ( / blockName len len2 rot)
	(setq
		blockName "NO-BN-2D-JBTKL-KRAFTAVLASTING-FOR-BARDUN-DOBBEL"
		len 2.0
		len2 5
		rot 14.0
	)
	(command
		"._LINE" (list len2 0) (list (- len2 len) 0) ""
		"._ROTATE" "L" "" (list len2 0) rot
		"._MIRROR" "LAST" "" (list 0 0) (list 1 0) "N"
		"._COPY" "ALL" "" (list 1 0) (list 0 0)
		"._LINE" (list 0 0) (list len2 0) ""
		"._ROTATE" "ALL" "" (list 0 0) "-90"
		"._MOVE" "ALL" "" (list 0 0) (list 0 len2)
	)
	(newBlock blockName)
	blockName
)		
	


(defun STREVERFESTE (/ blockName len len2 rot)
	(setq
		blockName "NO-BN-2D-JBTKL-KRAFTAVLASTING-FOR-STREVER"
		len 2.0
		len2 5
		rot 14.0
	)
	(command
		"._LINE" (list len2 0) (list (- len2 len) 0) ""
		"._ROTATE" "L" "" (list len2 0) rot
		"._MIRROR" "LAST" "" (list 0 0) (list 1 0) "N"
		"._LINE" (list 0 0) (list len2 0) ""
		"._ROTATE" "ALL" "" (list 0 0) "90"
		"._LINE" (list (/ len -2) 0) (list (/ len 2) 0) ""
	)
	(newBlock blockName)
	blockName
)
