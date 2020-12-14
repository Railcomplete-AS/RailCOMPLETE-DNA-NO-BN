;
; Avspenning.lsp
;
;
; Note: Unicode hex 'nnnn' is '\U+nnnn' in AutoCAD text
; Æ = Unicode decimal 198 - hex 00C6
; Ø = Unicode decimal 216 - hex 00D8
; Å = Unicode decimal 197 - hex 00C5
; æ = Unicode decimal 230 - hex 00E6
; ø = Unicode decimal 248 - hex 00F8
; å = Unicode decimal 229 - hex 00E5
;
; See Bane NOR Elkraftportalen: EH.707487 - Contact wire tensioning device (2xLodd)
;
 (defun C:AVSPENNING (/)
	(newLayer "JBTKL__EH_AEH_TYPENAVN" 62 "Avspenningstype")
	(AVSPENNING-FIXPUNKT)
	(AVSPENNING-FAST-ENKEL)
	(AVSPENNING-FAST-DOBBEL)
	(AVSPENNING-LODD-ENKEL)
	(AVSPENNING-LODD-DOBBEL)
	(AVSPENNING-FJAER-ENKEL)
	(AVSPENNING-FJAER-DOBBEL)
)



(defun AVSPENNING-FIXPUNKT (/ blockName x r)
	(setq blockName "NO-BN-2D-JBTKL-AVSPENNING-FIXPUNKT"
		x (/ 5.0 2)
		r 1.0
	)
	(command "._CIRCLE" (list 0 0) r)
	(drawHatchSelectPoint 0.03 (list 0 0) 0 0)
	(command
		"._LINE" (list (- x) 0) (list x 0) ""
		"._LINE" (list 0 x) (list 0 (- x)) ""
		"._ROTATE" "ALL" "" (list 0 0) 45
	)
	(command
		"._LAYER" "SET" "JBTKL__EH_AEH_TYPENAVN" ""
		"._COLOR" "ByLayer"
	)
	(addMText "FIX" (list 0.0 (- x)) 0.75 2.5 0 "ISO" "MC")
	(newBlock blockName)
	blockName
)



(defun AVSPENNING-FAST-ENKEL (/ blockName g g2 h )
	(setq
		blockName "NO-BN-2D-JBTKL-AVSPENNING-FAST-ENKEL"
		g 0.8
		g2 (/ g 2.0)
		h 1.25
	)
	(command
		"._LINE" (list (- g) 0) (list g 0) ""
		"._LINE" (list 0 g2) (list 0 (- g2)) ""
		"._PLINE" (list g g2) (list (+ g h) 0) (list g (- g2)) "CLOSE"
	)
	(drawHatchSelectPoint 0.03 (list (+ g 0.2) 0) 0 0)
	(command
		"._ROTATE" "ALL" "" (list 0 0) -90
		"._MOVE" "ALL" "" "Displacement" (LIST 0 (+ g h)) 
	)
	(command
		"._LAYER" "SET" "JBTKL__EH_AEH_TYPENAVN" ""
		"._COLOR" "ByLayer"
	)
	(addMText "1xFAST" (list 0.0 (- g)) 0.75 2.5 0 "ISO" "MC")
	(newBlock blockName)
	blockName
)



(defun AVSPENNING-FAST-DOBBEL (/ blockName g g2 h )
	(setq
		blockName "NO-BN-2D-JBTKL-AVSPENNING-FAST-DOBBEL"
		g 0.8
		g2 (/ g 2.0)
		h 1.25
	)
	(command
		"._LINE" (list (- g) 0) (list g 0) ""
		"._LINE" (list (- 0.2) g2) (list (- 0.2) (- g2)) ""
		"._LINE" (list 0.2 g2) (list 0.2 (- g2)) ""
		"._PLINE" (list g g2) (list (+ g h) 0) (list g (- g2)) "CLOSE"
	)
	(drawHatchSelectPoint 0.03 (list (+ g 0.2) 0) 0 0)
	(command
		"._ROTATE" "ALL" "" (list 0 0) -90
		"._MOVE" "ALL" "" "Displacement" (LIST 0 (+ g h)) 
	)
	(command
		"._LAYER" "SET" "JBTKL__EH_AEH_TYPENAVN" ""
		"._COLOR" "ByLayer"
	)
	(addMText "2xFAST" (list 0.0 (- g)) 0.75 2.5 0 "ISO" "MC")
	(newBlock blockName)
	blockName
)



(defun AVSPENNING-LODD-ENKEL (/  blockName g g2 h)
	(setq
		blockName "NO-BN-2D-JBTKL-AVSPENNING-LODD-ENKEL"
		g 0.8
		g2 (/ g 2.0)
		h 1.25
	)
	(command
		"._LINE" (list (- g) 0) (list g 0) ""
		"._LINE" (list 0 g2) (list 0 (- g2)) ""
		"._PLINE" (list g g2) (list (+ g h) 0) (list g (- g2)) "CLOSE"
		"._MIRROR" "LAST" "" (list 0 0) (list 0 1) "N"
	)
	(drawHatchSelectPoint 0.03 (list (+ g 0.2) 0) 0 0)	
	(drawHatchSelectPoint 0.03 (list (- (+ g 0.2)) 0) 0 0)
	(command
		"._ROTATE" "ALL" "" (list 0 0) -90
		"._MOVE" "ALL" "" "Displacement" (LIST 0 (+ g h)) 
	)
	(command
		"._LAYER" "SET" "JBTKL__EH_AEH_TYPENAVN" ""
		"._COLOR" "ByLayer"
	)
	(addMText "1xLODD" (list 0.0 (- g)) 0.75 2.5 0 "ISO" "MC")
	(newBlock blockName)
	blockName
)



(defun AVSPENNING-LODD-DOBBEL (/  blockName g g2 h)
	(setq
		blockName "NO-BN-2D-JBTKL-AVSPENNING-LODD-DOBBEL"
		g 0.8
		g2 (/ g 2.0)
		h 1.25
	)
	(command
		"._LINE" (list (- g) 0) (list g 0) ""
		"._LINE" (list (- 0.2) g2) (list (- 0.2) (- g2)) ""
		"._LINE" (list 0.2 g2) (list 0.2 (- g2)) ""
		"._PLINE" (list g g2) (list (+ g h) 0) (list g (- g2)) "CLOSE"
		"._MIRROR" "LAST" "" (list 0 0) (list 0 1) "N"
	)
	(drawHatchSelectPoint 0.03 (list (+ g 0.2) 0) 0 0)	
	(drawHatchSelectPoint 0.03 (list (- (+ g 0.2)) 0) 0 0)
	(command
		"._ROTATE" "ALL" "" (list 0 0) -90
		"._MOVE" "ALL" "" "Displacement" (LIST 0 (+ g h)) 
	)
	(command
		"._LAYER" "SET" "JBTKL__EH_AEH_TYPENAVN" ""
		"._COLOR" "ByLayer"
	)
	(addMText "2xLODD" (list 0.0 (- g)) 0.75 2.5 0 "ISO" "MC")
	(newBlock blockName)
	blockName
)



(defun AVSPENNING-FJAER-ENKEL (/  blockName g g2 h)
	(setq
		blockName "NO-BN-2D-JBTKL-AVSPENNING-FJAER-ENKEL"
		g 0.8
		g2 (/ g 2.0)
		h 1.25
	)
	(command
		"._LINE" (list (- g) 0) (list g 0) ""
		"._LINE" (list 0 g2) (list 0 (- g2)) ""
		"._PLINE" (list g g2) (list (+ g h) 0) (list g (- g2)) "CLOSE"
		"._MIRROR" "LAST" "" (list 0 0) (list 0 1) "N"
	)
	(drawHatchSelectPoint 0.03 (list (+ g 0.2) 0) 0 0)	
	(drawHatchSelectPoint 0.03 (list (- (+ g 0.2)) 0) 0 0)
	(command
		"._ROTATE" "ALL" "" (list 0 0) -90
		"._MOVE" "ALL" "" "Displacement" (LIST 0 (+ g h)) 
	)
	(command
		"._LAYER" "SET" "JBTKL__EH_AEH_TYPENAVN" ""
		"._COLOR" "ByLayer"
	)
	(addMText "1xFJ\U+00C6R" (list 0.0 (- g)) 0.75 2.5 0 "ISO" "MC") ; C6=Æ
	(newBlock blockName)
	blockName
)



(defun AVSPENNING-FJAER-DOBBEL (/  blockName g g2 h)
	(setq
		blockName "NO-BN-2D-JBTKL-AVSPENNING-FJAER-DOBBEL"
		g 0.8
		g2 (/ g 2.0)
		h 1.25
	)
	(command
		"._LINE" (list (- g) 0) (list g 0) ""
		"._LINE" (list (- 0.2) g2) (list (- 0.2) (- g2)) ""
		"._LINE" (list 0.2 g2) (list 0.2 (- g2)) ""
		"._PLINE" (list g g2) (list (+ g h) 0) (list g (- g2)) "CLOSE"
		"._MIRROR" "LAST" "" (list 0 0) (list 0 1) "N"
	)
	(drawHatchSelectPoint 0.03 (list (+ g 0.2) 0) 0 0)	
	(drawHatchSelectPoint 0.03 (list (- (+ g 0.2)) 0) 0 0)
	(command
		"._ROTATE" "ALL" "" (list 0 0) -90
		"._MOVE" "ALL" "" "Displacement" (LIST 0 (+ g h)) 
	)
	(command
		"._LAYER" "SET" "JBTKL__EH_AEH_TYPENAVN" ""
		"._COLOR" "ByLayer"
	)
	(addMText "2xFJ\U+00C6R" (list 0.0 (- g)) 0.75 2.5 0 "ISO" "MC") ; C6=Æ
	(newBlock blockName)
	blockName
)


