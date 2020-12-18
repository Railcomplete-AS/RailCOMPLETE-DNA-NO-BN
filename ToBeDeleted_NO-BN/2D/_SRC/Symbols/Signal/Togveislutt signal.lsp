;
; Toveislutt signal.lsp
;
; Used in Hovedsignal.lsp and in Dvergsignal.lsp
;

(defun togveiSlutt (topOfPole feste / blockName)
  (setq blockName "NO-BN-2D-JBTSI-SIGNAL-66-TOGVEI-SLUTT")
  (drawTogveiSlutt topOfPole feste)	
  (command  "._ROTATE" "ALL" "" "0,0" 90)
  (newBlock blockName)
)



(defun drawTogveiSlutt ( topOfPole feste /)
	(drawLetterS topOfPole)
	(if (= feste "AAK")
    (progn
		(drawPole (+ topOfPole 2.73618173) (+ 2 topOfPole 2.73618173))
		(setq blockName (strcat blockName "-AAK"))
		(command 
			"._MOVE" "ALL" "" "DISPLACEMENT" (list (- (+ 2 topOfPole 2.73618173)) 0)
		)
	)
    (progn
		(drawPole 0 topOfPole)
		(drawBase)
    )
  )
)



(defun drawLetterS (topOfPole / pt1 r1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 diff)
	(setq 
		diff (- topOfPole 6.0) 
		pt1 (list (+ diff 6.3154) 0.6713)
		r1 0.7233
		pt2 (list (+ diff 6.5978) -0.6414)
		;r2 0.524
		pt3 (list (+ diff 7.0866) -0.4629)
		;r3 1.7749
		pt4 (list (+ diff 7.3604) -0.0203)
		;r4 1.8895
		pt5 (list (+ diff 7.6979) 0.5033)
		;r5 0.7517
		pt6 (list (+ diff 8.5045) 0.5071)
		;r6 0.6375
		pt7 (list (+ diff 8.7369) -0.2)
		;r7 0.6952
		pt8 (list (+ diff 8.4801) -0.5731)
  )
  (command "._PLINE" pt1 "ARC" "Radius" r1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 "")
)
