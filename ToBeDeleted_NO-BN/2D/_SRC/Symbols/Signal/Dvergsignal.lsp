;
; Dvergsignal.lsp
;
(defun C:SIGNAL-DVERGSIGNAL-FRITTSTAAENDE (/ feste)
	(setq 
		feste nil
		pole 2
	)
	(repeat 2
		(Ds feste)
		(DsMKS feste)
		(Ds66 feste )
		(DsMKs66 feste)
		(setq feste "AAK")
	)
)



(defun Ds (feste / topOfDs blockName)
	(setq 
		topOfDs (+ 2 (* 2.67 (/ (sqrt 3.0) 2)))
		blockName "NO-BN-2D-JBTSI-SIGNAL-43-DS"
	)
	(if (= feste "AAK")
		(progn
			(setq blockName (strcat blockName "-AAK"))
			(drawPole topOfDs 2.0)
			(drawDs)
			(command 
				"._LINE" (list topOfDs 0) (list (+ topOfDs 2) 0) ""
				"._MOVE" "ALL" "" (list (+ topOfDs 2) 0) (list 0 0)
			)
		)
		(progn
			(drawBase)
			(drawPole topOfDs 0)
			(drawDs)
		)
	)
	(command "._ROTATE" "ALL" "" (list 0 0) 90)
	(newBlock blockName)
)



(defun DsMKS (feste / radiusMKs topOfDs blockName)
	(setq 
		radiusMKs (getMKsradius)
		topOfDs (+ 2 (* 2.67 (/ (sqrt 3.0) 2)))
		blockName "NO-BN-2D-JBTSI-SIGNAL-43-DS-MKS"
	)
	(if (= feste "AAK")
		(progn
			(setq blockName (strcat blockName "-AAK"))
			(drawDsMKs)
			(command 
				"._LINE" (list topOfDs 0) (list (+ topOfDs 2) 0) ""
				"._MOVE" "ALL" "" (list (+ topOfDs 2) 0) (list 0 0)
			)
		)
		(progn
			(drawBase)
			(drawDsMKs)
			(drawPole 2 0)
		)
	)
	(command "._ROTATE" "ALL" "" (list 0 0) 90)
	(newBlock blockName)
)



(defun Ds66 (feste / pole letterSHeight blockName)
	(setq
		pole 5.25
		letterSHeight 2.7369
		blockName "NO-BN-2D-JBTSI-SIGNAL-43-DS-66"
	)
	(drawTogveiSlutt pole feste)
	(if (= feste "AAK")
		(progn
			; (setq blockName (strcat blockName "-AAK")) - (this is done in subroutine for drawing TogveiSlutt, signal 66)
			(command
				"._MOVE" "ALL" "" (list (- (+ 2 letterSHeight)) 0) (list pole 0)
				"._LINE" (list 2.0 0) (list pole 0) ""
			)
			(drawDs)
			(command "._MOVE" "ALL" "" (list (+ pole 2 letterSHeight) 0) (list 0 0))
		)
		(drawDs)
	)
	(command "._ROTATE" "ALL" "" (list 0 0) 90)
	(newBlock blockName)
)



(defun DsMKs66 (feste / pole letterSHeight DsArcRadius topOfDs blockName)
	(setq
		pole 5.25
		letterSHeight 2.7369
		DsArcRadius 2.67
      	topOfDs (+ 2 (* DsArcRadius (/ (sqrt 3.0) 2)))
		blockName "NO-BN-2D-JBTSI-SIGNAL-43-DS-MKS-66"
	)
	(drawDsMKs)
	(command "._LINE" (list topOfDs 0) (list (+ topOfDs 0.9) 0) "")
	(drawLetterS (+ topOfDs 0.9))
	(if (= feste "AAK")
		(progn
			(setq blockName (strcat blockName "-AAK"))
			(drawPole (+ topOfDs 0.9 letterSHeight) (+ topOfDs 0.9 letterSHeight 2))
			(command "._MOVE" "ALL" "" (list (+ topOfDs 0.9 letterSHeight 2) 0) (list 0 0))
		)
		(progn
			(drawPole 2 0)
			(drawBase)
		)
	)
	(command "._ROTATE" "ALL" "" (list 0 0) 90)
	(newBlock blockName)
)



(defun drawDs (/ DsArcRadius x xbreak pt1 pt2 pt3 xrad yrad pt4)
; NB: This function requires that there exists a line which the BREAK command can act upon.
	(setq
		DsArcRadius 2.67
		x 2.0 ;dist from origo	
		xbreak (+ x (* (sqrt (/ 3 4.0)) DsArcRadius))
		pt1 (list x (- (/ DsArcRadius 2)))
		pt2 (list x (/ DsArcRadius 2))
		pt3 (list (+ x DsArcRadius) (/ DsArcRadius 2))
		xrad (* DsArcRadius (cos (* (- 0.25) pi)))
		yrad (* DsArcRadius (sin (* (- 0.25) pi)))
		pt4 (list (+ x xrad) (+ (/ DsArcRadius 2) yrad))
	)
	(command
		"._BREAK" (list x 0) (list xbreak 0)
		"._LINE" pt1 pt2 pt3 "" "._LINE" pt2 pt4 ""
		"._ARC" "C" pt2 pt1 pt3
	)
	(princ)
)



(defun drawDsMKs ( / topOfDs radiusMKs)
	(setq 
		DsArcRadius 2.67
		topOfDs (+ 2 (* DsArcRadius (/ (sqrt 3.0) 2)))
		radiusMKs (getMKsradius)
	)
	(drawPole topOfDs 2.0)
	(drawDs)
	(command "._CIRCLE" 
		(list 
			(+ 2.0 (/ DsArcRadius (sqrt 2.0)) (/ radiusMKs (sqrt 2.0))) 
			(- (+ (/ (- (* DsArcRadius (sqrt 2.0)) DsArcRadius) 2.0) (/ radiusMKs (sqrt 2.0))))
		)
		radiusMKs
	)
)



