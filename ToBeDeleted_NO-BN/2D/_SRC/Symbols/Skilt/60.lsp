;
; 60.lsp
;
(defun 60A (/ blockName description side radius)
	(setq	
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-60A-ATC-FORSIGNAL"
		description "Signal 60A ATC forsignal"
		side	  (getside)	;square side
		radius	  0.6222	;triangle side
  	)
	(drawSquare45 side)
	(command
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
	)
	(command 
		"._CIRCLE" (list 0 (* side (cos (D->R 45)))) radius
	)
	(drawHatch 0.01)
	(newBlock blockName)
	description
)



(defun 60B (/ blockName description)
	(setq	
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-60B-ATC-REPETER-MAALPUNKT"
		description "Signal 60B  ATC rep.målpunkt"
		side 0.50
		pt1 (list 0.0 (- (* 2.0 (cos (D->R 45))) (/ side (sqrt 3.0))))
		pt2 (polar pt1 (D->R 60) side)
		pt3 (polar pt2 (D->R 180) side)
	)
	(draw60D)
	;inner triangle:
	(command "._PLINE" pt1 pt2 pt3 "Close")
	(drawHatch 0.01)
	(newBlock blockName)
	description
)



(defun 60C (/ blockName description)
	(setq	
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-60C-ATC-NOEDBREMS"
		description "Signal 60C ATC nødbrems"
		dim1 (/ 2.0 15)
		dim2 0.90
		side (getside)
		pt1 (list (- (/ dim2 2)) (- (* side (cos (D->R 45))) (/ dim1 2)))
		pt2 (list (/ dim2 2) (+ (* side (cos (D->R 45))) (/ dim1 2)))
	)
	(draw60D)
	(command "._RECTANG" pt1 pt2)
	(drawHatch 0.01)
	(newBlock blockName)
	description
)



(defun 60D (/ blockName description)
	(setq	
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-60D-ATC-REPETER-HASTIGHET"
		description "Signal 60D ATC repeter hastighet"
	)	
	(draw60D)
	(newBlock blockName)
	description
)



(defun 60D-10 (/ blockName description)
	(setq	
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-60D-ATC-REPETER-HASTIGHET-10"
		description "Signal 60D ATC repeter hastighet 10 km/h"
		side   (getside)
		center (list 0.0 (* side (cos (D->R 45))))
	)	
	(draw60D)
	(addText "1" center 0.90 0.0 "iso" "MC")
	(newBlock blockName)
	description
)



(defun 60D-40 (/ blockName description)
	(setq	
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-60D-ATC-REPETER-HASTIGHET-40"
		description "Signal 60D ATC repeter hastighet 40 km/h"
		side   (getside)
		center (list 0.0 (* side (cos (D->R 45))))
	)	
	(draw60D)
	(addText "4" center 0.90 0.0 "iso" "MC")
	(newBlock blockName)
	description
)



(defun draw60D (/)
	(setq	
		side   (getside)
		rad1   (/ 1.60 2)
		rad2   (/ 1.2876 2)
		center (list 0.0 (* side (cos (D->R 45))))
		pt (list 0 (/ (+ rad1 rad2) 2))
	)
	(drawSquare45 side)
	(command
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
	)
	(command "._CIRCLE" center rad1)
	(command "._CIRCLE" center rad2)
	(drawHatchSelectPoint "0.01" pt 0 0.01)
	description
)



(defun 60E (/ blockName description dim1 dim2 ang1 ang2 pt1 pt2 pt3 pt4 pt5 pt6 pt7)
	(setq	
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-60E-ATC-VARSEL"
		description "Signal 60E ATC varsel"
		side	  (getside)	;square side
		dim2	  1.3183	;triangle side
		ang2	  (/ pi 3)	;60 degrees
		y	  0.65
	)
	(drawSquare45 side)
	(command
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
	)
	(command "._PLINE"
		(list 0 y)
		(list (* dim2 (cos ang2)) (+ y (* dim2 (sin ang2))))
		(list (- (* dim2 (cos ang2))) (+ y (* dim2 (sin ang2))))
		"CLOSE"
	)
	(drawHatch 0.01)
	(newBlock blockName)
	description
)



(defun 60F (/ blockName description dim1 dim2)
	(setq 
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-60F-F-ATC"
		description "Signal 60F F-ATC"
		dim1 1.5
		dim2 3.0
	)
	(command 
		"._RECTANG"
		(list (- (/ dim2 2)) 0.0)
		(list (/ dim2 2) dim1)
	)
	(command 
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
	)
	(addText "FATC" (list 0 (/ dim1 2)) 0.90 0.0 "iso" "MC")
	(newBlock blockName)
	description
)



(defun 60G (/ blockName description dim1 dim2)
	(setq 
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-60G-D-ATC"
		description "Signal 60G D-ATC"
		dim1 1.5
		dim2 3.0
	)
	(command
		"._RECTANG"
			(list (- (/ dim2 2)) 0.0)
		(list (/ dim2 2) dim1)
	)	
	(command 
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
	)
	(addText "DATC" (list 0 (/ dim1 2)) 0.90 0.0 "iso" "MC")
	(newBlock blockName)
	description
)



(defun 60H (/ blockName  description dim1 dim2 pt1 pt2 offset ang xoff yoff)
	(setq 
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-60H-ATC-SLUTTER"
		description "Signal 60H ATC slutter"
    	dim1 1.5
		dim2 3.0
		pt1 (list (- (/ dim2 2)) 0.0)
		pt2 (list (/ dim2 2) dim1)
		offset 0.25
		ang (atan dim1 dim2)
		xoff (/ offset (sin ang))
		yoff (* xoff (tan ang))
  	)
	(command
		"._RECTANG" pt1 pt2
		"._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
		"._WIPEOUT" "Polyline" "L" "" "Yes"
		"._LAYER" "SET" "0" ""
		"._LINE" pt1 pt2 ""
		"._LINE" (polar pt1 0 xoff) (polar pt2 (D->R 270) yoff) ""
		"._LINE" (polar pt1 (D->R 90) yoff) (polar pt2 (D->R 180) xoff) ""
	)
	(addText "ATC" (list 0 (/ dim1 2)) 0.90 0.0 "iso" "MC")
	(newBlock blockName)
	description
)