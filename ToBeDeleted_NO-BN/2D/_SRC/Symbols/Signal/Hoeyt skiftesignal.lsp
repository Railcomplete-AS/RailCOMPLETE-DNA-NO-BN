;
; Hoeyt skiftesignal.lsp
;
; NB! Kan ikke ha æøå i filnavn som skal loades av LISP.
;
; Høyt skiftesignal montert på hovedsignals mast (variantene som henger på et hovedsignal er håndtert i SIGNAL-HOVEDSIGNAL rutinen)
;
(defun C:SIGNAL-HOEYT-SKIFTESIGNAL-FRITTSTÅENDE (/ feste)
	(drawHoeytSkifteSignal nil nil)
	(drawHoeytSkifteSignal nil "MK")
	(drawHoeytSkifteSignal "AAK" nil)
	(drawHoeytSkifteSignal "AAK" "MK")
)



(defun drawHoeytSkifteSignal (feste MK / blockName ratio topOfPole X Y dy y1 MKsradius)
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-41-HOEYT-SKIFTESIGNAL"
        ratio (/ 2.0 3.0)
		topOfPole (* 9 ratio)
        X  (* 3.0 0.5 ratio)
		Y  (* 4.5 ratio)
		dY (* 0.35 ratio)
		y1 (* (- 2.0 (/ dy 2) ratio))
  	)
	(command 
		"._RECTANG" (list topOfPole X) (list (+ topOfPole Y) (- X))
		"._RECTANG" (list (+ topOfPole y1) X) (list (+ topOfPole y1 dY) (- X))
	)
	(drawHatch 0.01)
	(if (= feste "AAK")
		(progn 
			(drawPole (+ topOfPole Y) (+ 2.0 topOfPole Y))
		)
		(progn
			(drawPole 0 topOfPole)
			(drawBase)
		)
	)
	(if (= MK "MK")
		(progn
			(setq
				blockName (strcat blockName "-MKS")
				MKsradius (getMKsradius)
			)
			(if (= feste "AAK")
				(command "._CIRCLE" (list (+ topOfPole y MKsradius) (- MKsradius)) MKsradius)
				(command "._CIRCLE" (list (- topOfPole MKsradius) (- MKsradius)) MKsradius)
			)
		)
    ) 
	(command "._ROTATE" "ALL" "" "0,0" "90")
	(if (= feste "AAK")
		(progn
			(command "._MOVE" "ALL" "" "DISPLACEMENT" (list 0 (- (+ topOfPole 2.0 Y))))
			(setq blockName (strcat blockName "-AAK"))
		)
	)
	(newBlock blockName)
	blockName
)
