(defun 102-1 (/ blockName description x)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-102-1-PILSKILT-H"
	description "Signal 102-1 Pilskilt høyre"
	x 3.0 )
  (draw102-1)
  (drawHatch 0.01)
  (command "._MOVE" "ALL" "" "DISPLACEMENT"
	   (list (- (/ x 2)) 0)
	   )
  (newBlock blockName)
  description
)

(defun 102-2 (/ blockName description x)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-102-2-PILSKILT-V"
	description "Signal 102-2 Pilskilt venstre"
	x 3.0
	)
  (draw102-1)
  (command "._MOVE" "ALL" "" "DISPLACEMENT"
	   (list (- (/ x 2)) 0)
	   "._MIRROR" "ALL" "" "0,0" "0,1" "YES"
	   )
  (drawHatch 0.01)
  (newBlock blockName)
  description
)
(defun 102-3 (/ blockName description x y a_pos a_len a_x a_y)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-102-3-PILSKILT-HV"
		description "Signal 102-3 Pilskilt dobbelpil")
  (setq x 3.0
	y 1.5
	a_pos 0.50 ; arrow
	a_len 1.50
	a_x   0.50
	a_y   0.25
    )
  (command "._RECTANG" (list 0 0) (list x y)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._LINE"
	   (list (+ a_pos a_x) (/ y 2))
	   (strcat "@" (rtos (- a_len a_x)) ",0")
	   ""
	   "._PLINE"
	   (list (+ a_pos a_len) (/ y 2))
	   (strcat "@0," (rtos a_y))
	   (strcat "@" (rtos a_x) ",-" (rtos a_y))
	   (strcat "@-" (rtos a_x) ",-" (rtos a_y))
	   (strcat "@0," (rtos a_y))
	   "")
  (drawHatch 0.01)
  (command "._PLINE"
	   (list (+ a_pos a_x) (/ y 2))
	   (strcat "@0," (rtos a_y))
	   (strcat "@-" (rtos a_x) ",-" (rtos a_y))
	   (strcat "@" (rtos a_x) ",-" (rtos a_y))
	   (strcat "@0," (rtos a_y))
	   ""
	   "._MOVE" "ALL" "" "DISPLACEMENT"
	   (list (- (/ x 2)) 0)
	   )
  (drawHatch 0.01)
  (newBlock blockName)
  description
)

(defun draw102-1 (/ x y a_pos a_len a_x a_y)
  (setq x 3.0
	y 1.5
	a_pos 0.50 ; arrow
	a_len 1.50
	a_x   "0.50"
	a_y   "0.25"
    )
  (command "._RECTANG" (list 0 0) (list x y)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._LINE"
	   (list a_pos (/ y 2))
	   (strcat "@" (rtos a_len) ",0")
	   ""
	   "._PLINE"
	   (list (+ a_pos a_len) (/ y 2))
	   (strcat "@0," a_y)
	   (strcat "@" a_x ",-" a_y)
	   (strcat "@-" a_x ",-" a_y)
	   (strcat "@0," a_y)
	   ""
	   )
)
