(defun 65A (/ blockName description  side dim1 dim2)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-65A-JORDETSEKSJON"
		description "Signal 65A Jordet seksjon"
	side 2.25
	dim1 0.60
	dim2 0.10
	)
  (command "._RECTANG"
	   "0,0"
	   (list side side)
	   "._RECTANG"
	   (list dim2 dim1)
	   (list (- side dim2) (- side dim1))
  	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (drawHatchSelectPoint "0.01" (list dim2 (/ dim1 2)) 0 0.01)
  (command "._MOVE" "ALL" "" "DISPLACEMENT"
	   (strcat (rtos (- (/ side 2)) 1 3) ",0")
	   )
  (newBlock blockName)
  description
)

(defun 65B (/ blockName description  side posCirc radius posRectang dim1 dim2)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-65B-KONTAKTLEDNING"
		description "Signal 65B Varselsignal for kontaktledningssignal"
	side 2.25
	posCirc (list (/ side 2) 1.3750)
	radius 0.3750
	posRectang (list 0.25 0.75)
	dim1 0.25
	dim2 1.25
	)
  (command "._RECTANG"
	   "0,0"
	   (list side side)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._CIRCLE"
	   posCirc
	   radius
	   "._RECTANG"
	   posRectang
	   (list (+ (car posRectang) dim1) (+ (cadr posRectang) dim2))
	   
  )
  (drawHatch 0.01)
  (command "._RECTANG"
	   (list (+ (car posRectang) dim1 dim2) (cadr posRectang))
	   (list (+ (car posRectang) dim1 dim1 dim2) (+ (cadr posRectang) dim2))
	   )
  (drawHatch 0.01)
  (command "._MOVE" "ALL" "" "DISPLACEMENT"
	   (strcat (rtos (- (/ side 2)) 1 3) ",0")
	   )
  (newBlock blockName)
  description
)

(defun 65C (/ blockName description  side side posCirc radius posRectang dim1 dim2)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-65C-UTKOPLING"
		description "Signal 65C utkopling foran dødseksjon"
	side 2.25
	posCirc (list (/ side 2) 1.3750)
	radius 0.3750
	posRectang (list 0.25 0.75)
	dim1 0.25
	dim2 1.25
	)
  (command "._RECTANG"
	   "0,0"
	   (list side side)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._CIRCLE"
	   posCirc
	   radius
	   "._RECTANG"
	   posRectang
	   (list (+ (car posRectang) dim1) (+ (cadr posRectang) dim2))
	   
  )
  (drawHatch 0.01)
  (command "._RECTANG"
	   (list (+ (car posRectang) dim1 dim2) (cadr posRectang))
	   (list (+ (car posRectang) dim1 dim1 dim2) (+ (cadr posRectang) dim2))
	   )
  (drawHatch 0.01)
  (command "._RECTANG"
	   (list dim1 dim1)
	   (list (- side dim1) (+ dim1 dim1))
	   )
  (drawHatch 0.01)
  
  (command "._MOVE" "ALL" "" "DISPLACEMENT"
	   (strcat (rtos (- (/ side 2)) 1 3) ",0")
	   )
  (newBlock blockName)
  description
)

(defun 65D (/ blockName description  side posRectang dim1 dim2)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-65D-INNKOPLING"
		description "Signal 65D Innkopling etter dødseksjon"
	side 2.25
	posRectang (list 0.25 0.75)
	dim1 0.25
	dim2 1.25
	)
  (command "._RECTANG"
	   "0,0"
	   (list side side)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" "")
  (command "._PLINE"
	   (list dim1 dim1)
	   (list dim1 (+ (cadr posRectang) dim2))
	   (list (* dim1 2.0) (+ (cadr posRectang) dim2))
	   (list (* dim1 2.0) (- dim2 (cadr posRectang)))
	   (list (* dim1 7.0) (- dim2 (cadr posRectang)))
	   (list (* dim1 7.0) (+ (cadr posRectang) dim2))
	   (list (* dim1 8.0) (+ (cadr posRectang) dim2))
	   (list (* dim1 8.0) dim1)
	   "CLOSE")
  (drawHatch 0.01)
  (command "._MOVE" "ALL" "" "DISPLACEMENT"
	   (strcat (rtos (- (/ side 2)) 1 3) ",0")
	   )
  (newBlock blockName)
  description
)

(defun 65E (/ blockName description  side posRectang)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-65E-SENKSTROEMAVTAKER"
		description "Signal 65E Senking av strømavtaker"
	side 2.25
	posRectang (list 0.1252 1.0008)
	)
  (command "._RECTANG"
	   "0,0"
	   (list side side)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._RECTANG"
	   posRectang
	   (list (- side (car posRectang)) (- side (cadr posRectang)))
	   )
  (drawHatch 0.01)
  (command "._MOVE" "ALL" "" "DISPLACEMENT"
	   (strcat (rtos (- (/ side 2)) 1 3) ",0")
	   )
  (newBlock blockName)
  description
)

(defun 65F (/ blockName description  side posRectang)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-65F-HEVSTROEMAVTAKER"
		description "Signal 65F Heving av strømavtaker"
	side 2.25
	posRectang (list 0.1252 1.0008)
	)
  (command "._RECTANG"
	   "0,0"
	   (list side side)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._RECTANG"
	   posRectang
	   (list (- side (car posRectang)) (- side (cadr posRectang)))
	   )
  (command "._MOVE" "ALL" "" "DISPLACEMENT"
	   (strcat (rtos (- (/ side 2)) 1 3) ",0")
	   "._ROTATE" "ALL" ""
	   (strcat "0," (rtos (/ side 2) 1 3)) "90"
	   )
  (drawHatch 0.01)
  (newBlock blockName)
  description
)

(defun 65G-1 (/ blockName description  side posCircle radius1 radius2 posRectang)
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-65G-1-STOPPELLOK"
		description "Signal 65G-1 Stopp for elektrisk lokomotiv")
  (draw65G-1)
  (newBlock blockName)
  description
)

(defun draw65G-1 (/ side posCircle radius1 radius2)
  (setq side 2.25
	posCircle (list (/ side 2) (/ side 2))
	radius1 0.85
	radius2 1.00
	)
  (command "._RECTANG"
	   "0,0"
	   (list side side)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._CIRCLE" posCircle radius1
	   "._CIRCLE" posCircle radius2
	   )
  (command "._MOVE" "ALL" "" "DISPLACEMENT"
	   (strcat (rtos (- (/ side 2)) 1 3) ",0")
	   )
  (drawHatchSelectPoint "0.01" (list 0 (- (/ side 2) (/ (+ radius1 radius2) 2))) 0 0.01)
)

(defun 65G-2 (/ blockName description  side side2 posCircle radius1 radius2 )
  (setq blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-65G-2-STOPPELLOK-VSIDE"
		description "Signal 65G-2 65G med høyrepil"
	side 2.25
	side2 3.0
	posCircle (list (/ side 2) (- side2 (/ side 2)))
	radius1 0.85
	radius2 1.00
	)
  (command "._RECTANG"
	   "0,0"
	   (list side side2)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._CIRCLE" posCircle radius1
	   "._CIRCLE" posCircle radius2
	   )
  ;arrow
  (setq posArrow (list 0.1250 0.4522)
	len "1.70"
	width "0.10"
	tipx "0.3"
	tipy "0.2250"
	tipy2 (rtos (- 0.2250 (/ 0.10 2)) 2 3)
    	)
  (command "._PLINE"
	   posArrow
	   (strcat "@" len ",0")
	   (strcat "@" "0,-" tipy2)
	   (strcat "@" tipx "," tipy)
	   (strcat "@-" tipx "," tipy)
	   (strcat "@" "0,-" tipy2)
	   (strcat "@-" len ",0")
	   "CLOSE"
	   )
  (drawHatch 0.01)
  (command "._MOVE" "ALL" "" "DISPLACEMENT"
	   (strcat (rtos (- (/ side 2)) 1 3) ",0")
	   )
  (drawHatchSelectPoint "0.01" (list 0 (- (- side2 (/ side 2)) (/ (+ radius1 radius2) 2))) 0 0.01)
  (newBlock blockName)
  description
)

(defun 65G-3 (/		blockName description  side	    side2     posCircle
	      radius1	radius2	  posArrow
	      len	width	  tipx	    tipy      tipy2
	     )
  (setq	blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-65G-3-STOPPELLOK-HSIDE"
		description "Signal 65G-3 65G med venstrepil"
	side	  2.25
	side2	  3.0
	posCircle (list (/ side 2) (- side2 (/ side 2)))
	radius1	  0.85
	radius2	  1.00
  )
  (command "._RECTANG"
	   "0,0"
	   (list side side2)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	   "._CIRCLE"
	   posCircle
	   radius1
	   "._CIRCLE"
	   posCircle
	   radius2
  )
					;arrow
  (setq	posArrow (list 0.1250 0.4522)
	len	 "1.70"
	width	 "0.10"
	tipx	 "0.3"
	tipy	 "0.2250"
	tipy2	 (rtos (- 0.2250 (/ 0.10 2)) 2 3)
  )
  (command "._PLINE"
	   posArrow
	   (strcat "@" len ",0")
	   (strcat "@" "0,-" tipy2)
	   (strcat "@" tipx "," tipy)
	   (strcat "@-" tipx "," tipy)
	   (strcat "@" "0,-" tipy2)
	   (strcat "@-" len ",0")
	   "CLOSE"
  )
  (command "._MIRROR" "LAST" ""
	   (list (/ side 2) 0)
	   (list (/ side 2) 1)
	   "Y"
	   )
  (drawHatch 0.01)
  (command "._MOVE" 
	   "ALL"
	   ""
	   "DISPLACEMENT"
	   (strcat (rtos (- (/ side 2)) 1 3) ",0")
  )
  (drawHatchSelectPoint "0.01"
    (list 0 (- (- side2 (/ side 2)) (/ (+ radius1 radius2) 2))) 0 0.01
  )
  (newBlock blockName)
  description
)