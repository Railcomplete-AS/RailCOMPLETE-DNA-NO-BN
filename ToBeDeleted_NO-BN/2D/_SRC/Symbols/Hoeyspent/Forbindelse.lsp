;
; Forbindelse.lsp
;
(defun C:FORBINDELSE (/)
	(STROEMBRO)
	(SVEVENDE-KRYSS)
)



;EH.705051 'Strømbro i avspenningsfelt'. Strømbro mellom to ledninger i et vekslingsfelt. Kan være enkel (1 stk) eller dobbel (2 stk).
(defun STROEMBRO (/ blockName x y)
	(setq
		blockName "NO-BN-2D-JBTKL-FORBINDELSE-STROEMBRO"
		x 0.9128
		y 2.0413
	)
	(command
		"._ARC" (list x y) "C" (list 0 0) (list x (- y)) ""
		"._PLINE" (list 1.26 2.363) (list 2.0 1.0) (list 0.709 1.855) "CLOSE"
		"._MIRROR" "LAST" "" (list 0 0) (list 1 0) "N"
	)
	(drawHatchSelectPoint 0.03 (list 1.2 1.7) 0 0)
	(drawHatchSelectPoint 0.03 (list 1.2 (- 1.7)) 90 0)
	(newBlock blockName)
	blockName
)



; EH.707403 'Svevende kontakttrådkryss for 12 mm2 kontakttråd'. 2500mm tråd med hengetrådklemme i hver ende. Strømper på bærelinene om nødvendig.
(defun SVEVENDE-KRYSS (/ blockName len)
	(setq
		blockName "NO-BN-2D-JBTKL-FORBINDELSE-SVEVENDE-KRYSS"
		len (/ 3.0 2.0)
	)
	(command
		"._LINE" (list (- len) 0) (list len 0) ""
		"._ROTATE" "L" "" (list 0 0) 45
		"._MIRROR" "LAST" "" (list 0 0) (list 1 0) "N")
	(newBlock blockName)
	blockName
)


