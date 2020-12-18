;
; Hoeyt skiftesignal.lsp
;
; NB! Kan ikke ha æøå i filnavn som skal loades av LISP.
;
; Høyt skiftesignal montert på hovedsignals mast (variantene som henger på et hovedsignal er håndtert i SIGNAL-HOVEDSIGNAL rutinen)
;
; 2020-08-02 CLFEY Deprecated code, is now included in main signal LISP routines.
;
(defun C:SIGNAL-HOEYT-SKIFTESIGNAL-FRITTSTÅENDE (/ feste)
	(HOEYT-SKIFTESIGNAL nil nil)
	(HOEYT-SKIFTESIGNAL nil "MK")
	(HOEYT-SKIFTESIGNAL "AAK" nil)
	(HOEYT-SKIFTESIGNAL "AAK" "MK")
)



(defun HOEYT-SKIFTESIGNAL ( feste MK / blockName poleZs yokePole x y y1 y2 MKsRadius )
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-41-HOEYT-SKIFTESIGNAL"
		poleZs 9.0
		yokePole 4.0
        x  3.0
		y  4.5
		y1 1.825
		y2 2.175
		MKsRadius (getMediumLanternRadius)
  	)
	(command 
		"._RECTANGLE" (list (/ x -2) 0) (list (/ x 2) y)  ; Signal head rectangle, bottom line center in origo
		"._RECTANGLE" (list (/ x -2) y1) (list (/ x 2) y2)  ; Area to hatch across signal head
	)
	(drawHatch _denseHatch_)
	(if (= feste "AAK")
		(progn 
			(command "._MOVE" "_ALL" "" (list 0 (+ y yokePole)) "0,0") ; move down
			(command "._LINE" "0,0" (list 0 (- yokePole)) "") ; add short pole from yoke
			(if (= MK "MK")
				(progn 
					(setq blockName (strcat blockName "-MKS"))
					(command "._CIRCLE" (list MKsRadius (- MKsRadius yokePole)) MKsRadius) ; Add lantern just above RIGHT of box
				)
			)
			(setq blockName (strcat blockName "-AAK"))
		)
		(progn
			(command "._MOVE" "_ALL" "" "0,0" (list 0 poleZs)) ; move up
			(if (= MK "MK")
				(progn
					(setq blockName (strcat blockName "-MKS"))
					(command "._CIRCLE" (list (- MKsRadius) (- poleZs MKsRadius)) MKsRadius) ; Add lantern just below LEFT of box
				)
			)
			(command "._LINE" "0,0" (list 0 poleZs) "") ; add upright pole
			(command "._ROTATE" "_ALL" "" "0,0" "-90") ; Rotate CCW before adding base
			(drawLyingHsBase)
			(command "._ROTATE" "_ALL" "" "0,0" "90") ; Rotate back CW
		)
	)
	(createGeoBlockInAllPaperScalesFromBlock blockName _twoThirds_ blockName) ; Use 2/3 of scaled schematic symbol as 1:500 cale etc
)
