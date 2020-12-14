;
; Refleks.lsp
;

(defun C:REFLEKS (/)
	(REFLEKS_HEL)
	(REFLEKS_HALV)
)



(defun REFLEKS_HEL (/ blockName rad)
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-REFLEKS-HEL"
		rad 1.5
	)
	(command
		"._POLYGON" 6 "0,0" "Inscribed" rad
		"._LINE" (list (- rad) 0) (list rad 0) ""
		"._LINE" (list (- (/ rad 2)) (* rad (/ (sqrt 3) 2))) (list (/ rad 2) (- (* rad (/ (sqrt 3) 2)))) ""
		"._LINE" (list (/ rad 2) (* rad (/ (sqrt 3) 2))) (list (- (/ rad 2)) (- (* rad (/ (sqrt 3) 2)))) ""
		"._MOVE" "ALL" "" "0,0" (list 0 (* rad (/ (sqrt 3) 2)))
	)
	(newBlock blockName)
	blockName
)



(defun REFLEKS_HALV (/ blockName rad)
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-REFLEKS-HALV"
		rad 1.5
	)
	(command
		"._LINE"
			(list (- rad) 0) (list rad 0)
			(list (/ rad 2) (- (* rad (/ (sqrt 3) 2))))
			(list (- (/ rad 2)) (- (* rad (/ (sqrt 3) 2))))
			(list (- rad) 0) 
			""
		"._LINE" "0,0" (list (- (/ rad 2)) (- (* rad (/ (sqrt 3) 2)))) ""
		"._LINE" "0,0"(list (/ rad 2) (- (* rad (/ (sqrt 3) 2)))) ""
		"._MOVE" "ALL" "" "0,0" (list 0 (* rad (/ (sqrt 3) 2)))
	)
	(newBlock blockName)
	blockName
)
	