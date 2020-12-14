;
; Stroemkrets.lsp
;
(defun C:TRANSFORMATOR (/)
	(SUGETRANSFORMATOR 2 0.5)
	(SUGETRANSFORMATOR-I-KIOSK 2 0.5)
	(newLayer "JBTKL__EH_AUT_POLER" 62 "Poler for autotransformator")
	(AUTOTRANSFORMATOR 1.5 0.5)
	)



(defun drawCoil (len r /)
	(command
		"._PLINE"
			(list 0 0)
			(list len 0)
			"A" "R" r
			(list len (* 2 r)) 
			"R" (- r)
			(list len (* 4 r))
			"R" (- r)
			(list len (* 6 r))
			"R" (- r)
			(list len (* 8 r))
			"L"
			(list 0 (* 8 r))
			""
	)
)



(defun SUGETRANSFORMATOR (len r / blockName)
	(setq
		blockName "NO-BN-2D-JBTKL-TRANSFORMATOR-SUGETRAFO"
	)
	(drawCoil len r)
	(command 
		"._ROTATE" "ALL" "" "0,0" 90
		"._MOVE" "ALL" "" "0,0" (list (* 4 r) 0)
	)
	(newBlock blockName)
	blockName
)



(defun SUGETRANSFORMATOR-I-KIOSK (len r / blockName)
	(setq
		blockName "NO-BN-2D-JBTKL-TRANSFORMATOR-SUGETRAFO-I-KIOSK"
	)
	(drawCoil len r)
	(command 
		"._ROTATE" "ALL" "" "0,0" 90
		"._MOVE" "ALL" "" "0,0" (list (* 4 r) 0)
	)
	(command
		"._RECTANGLE" 
			(list (* (- 5) r) (- r))
			(list (* (+ 5) r) (+ len r r))
	)
	(newBlock blockName)
	blockName
)



(defun AUTOTRANSFORMATOR (len r / blockName)
	(setq 
		blockName "NO-BN-2D-JBTKL-TRANSFORMATOR-AUTOTRAFO"
	)
	(drawCoil len r)
	(command 
		"._MOVE" "ALL" "" (list len 0) "0,0" 
		"._LINE" (list 0 0) (list (+ len r) 0) "" 
		"._LINE" (list 0 (* 4 r)) (list (+ r len) (* 4 r)) ""
	)
	(command 
		"._LAYER" "SET" "JBTKL__EH_AUT_POLER" ""
		"._COLOR" "ByLayer"
	)
	(addText "NL" (list (- (+ len r r)) (* 8 r)) 1.8 0 "iso" "MC")
	(addText "PL" (list (- (+ len r r)) 0) 1.8 0 "iso" "MC")
	(command "._LAYER" "SET" "0" "" "._COLOR" "ByBlock")
	(newBlock blockName)
	blockName
)


