;
; Toveislutt signal.lsp
;
; Used in Hovedsignal.lsp and in Dvergsignal.lsp
;
; 2020-08-02 CLFEY Deprecated code, is now included in main signal LISP routines.

(defun C:SIGNAL-TOGVEI-SLUTT ( )
	(TOGVEI-SLUTT nil)
	(TOGVEI-SLUTT "AAK")
)



(defun TOGVEI-SLUTT ( feste / blockName letterSHeight mainPole yokePole )
	(setq 
		blockName "NO-BN-2D-JBTSI-SIGNAL-66-TSS"
		letterSHeight 4.0
		mainPole 8.0
		yokePole 2.5
	)
	(drawLetterS) ; lying 'S'
	(if (= feste "AAK")
		(progn
			(setq blockName (strcat blockName "-AAK"))
			(command "._MOVE" "_ALL" "" (list (+ letterSHeight yokePole) 0) "0,0") ; shift left
			(drawLyingPole (- yokePole) 0) ; short yoke pole
		)
		(progn
			(command "._MOVE" "_ALL" "" "0,0" (list mainPole 0)) ; shift right
			(drawLyingPole 0 mainPole)
			(drawLyingHsBase)
		)
	)
	(command  "._ROTATE" "_ALL" "" "0,0" "90") ; rotate CCW
	(createGeoBlockInAllPaperScalesFromBlock blockName _twoThirds_ blockName) ; Use 2/3 of scaled schematic symbol as 1:500 cale etc
)



(defun drawLetterS ( / pt1 r1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 diff )
	; Create a lying 'S' shape with its lowest point located at origo
	(setq 
		pt1 "0.5,1.0"
		r1 1.25
		pt2 "0.5,-1.0"
		r2 1.00
		pt3 "1.75,-0.75"
		r3 1.50
		pt4 "2.0,0.0"
		r4 -1.50
		pt5 "2.25,0.75"
		r5 -1.00
		pt6 "3.5,1.0"
		r6 -1.25
		pt7 "3.5,-1.0"
	)
	(command 
		"._PLINE" pt1 "_ARC" "CE" "1.25,0" pt2 pt3 pt4 pt5 pt6 pt7 ""
	)
)
