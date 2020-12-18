;
; THUMBNAIL-Kontaktledning.lsp
;
(defun C:THUMBNAIL-KONTAKTLEDNING ( )
	(ALIGNMENT-KONTAKTLEDNING)
)

(defun ALIGNMENT-KONTAKTLEDNING (/ blockName radiusWire radiusSmall radiusLarge) 
	(setq
		blockName (strcat "NO-BN-2D-JBTKL-THUMBNAIL-KONTAKTLEDNING")
		radiusWire 6.0
		radiusSmall 0.3
		radiusLarge 0.4
	)
	(command
		"._PLINE" (list 0 (- radiusWire)) "A" "CE" "0,0" "A" "93"
		"R" radiusSmall "A" "64" "122"
		"L" "@3.075<154"
		"A" "R" radiusLarge "A" "-104" "102"
		"L" "@2.092<50"
		"A" "R" radiusSmall "A" "84" "91.80051135"
		"CE" "0,0" (list 0 radiusWire) ""
		"._MIRROR" "ALL" "" "0,0" "0,1" "NO"
	)
  	(drawHatchSelectPoint 0.4 (list 0 0) 0 0)
	(command "._SCALE" "ALL" "" (list 0 0) "0.2")
	(newBlock blockName)
	blockName
)