(defun BaliseSkilt ( / blockName x y s_top s_bot s_x)
	(setq blockName "NO-BN-2D-JBTSI-SKILT-VEDLIKEHOLD-SIGNAL-BALISE"
		  x 8.25
		  y 4.5
		  s_top 0.75
		  s_bot 1.125
		  s_x 1.125
	        )
  	(command "._RECTANGLE" "0,0" (list x y)
		 "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
	(stripe_top x y s_top s_x)
  	(stripe_top (- x (* 2 s_x)) y s_top s_x)
  	(stripe_top (- x (* 4 s_x)) y s_top s_x)
  	(stripe_corner_bot 0 s_top (/ s_top 2) s_bot s_x)
  	(stripe_bot (+ s_x (/ s_top 2)) 0 s_bot s_x)
  	(stripe_bot (+ (* 3 s_x) (/ s_top 2)) 0 s_bot s_x)
  	(addText "BALISE" (list (/ x 2) (+ (/ y 2) (/ s_top 4))) 1.8 0 "ISO" "MC")
  	(command "._MOVE" "ALL" "" "0,0" (list (- (/ x 2)) 0))
  (newBlock blockName)
  blockName
  )

(defun stripe_top (x y s_top s_x /)
		 (command "._LINE" (list x y) (strcat "@" (rtos (- s_top)) "," (rtos (- s_top)))
		 (strcat "@" (rtos (- s_x)) ",0")
		 (strcat "@" (rtos s_top) "," (rtos s_top)) ""
		 )
  (drawHatchSelectPoint 0.06 (list (- x (/ s_top 2)) (- y (/ s_top 3))) 0 0)
  )

(defun stripe_bot (x y s_bot s_x /)
		 (command "._LINE" (list x y) (strcat "@" (rtos s_bot) "," (rtos s_bot))
		 (strcat "@" (rtos s_x) ",0")
		 (strcat "@" (rtos (- s_bot)) "," (rtos (- s_bot))) ""
		 )
  (drawHatchSelectPoint 0.06 (list (+ x (/ s_bot 2)) (+ y (/ s_bot 3))) 0 0)
  )

(defun stripe_corner_bot (x y s_bot1 s_bot2 s_x /)
  (command "._LINE" (list x y) (strcat "@" (rtos s_bot1) "," (rtos s_bot1))
		 (strcat "@" (rtos s_x) ",0")
		 (strcat "@" (rtos (- s_bot2)) "," (rtos (- s_bot2))) ""
		 )
  (drawHatchSelectPoint 0.06 (list (+ x (/ s_bot1 2)) (+ y (/ s_bot1 3))) 0 0)
  )