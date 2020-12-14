(defun 66A (/	blockName description  side	    side2     posCircle
	      radius1	radius2	  description
	     )
  (setq	blockName "66A"
	description "Signal 66 Togvei slutt"
	side	  2.25
	side2	  3.0
  	)
  (command "._RECTANG"
	   (list (/ side (- 2.0)) 0)
	   (list (/ side 2) side2)
	   "._LAYER" "SET" "JBTSI__SKILT_WIPEOUT" ""
	   "._WIPEOUT" "Polyline" "L" "" "Yes"
	   "._LAYER" "SET" "0" ""
  	   )
  (setq shift 5.4
	ang 180
	blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-66-TOGVEI-SLUTT"
	pt1 (polar (list 6.3154 0.6713) (D->R ang) shift)
	r1 0.7233
	pt2 (polar (list 6.5978 -0.6414) (D->R ang) shift)
	;r2 0.524
	pt3 (polar (list 7.0866 -0.4629) (D->R ang) shift)
	;r3 1.7749
	pt4 (polar (list 7.3604 -0.0203) (D->R ang) shift)
	;r4 1.8895
	pt5 (polar (list 7.6979 0.5033) (D->R ang) shift)
	;r5 0.7517
	pt6 (polar (list 8.5045 0.5071) (D->R ang) shift)
	;r6 0.6375
	pt7 (polar (list 8.7369 -0.2) (D->R ang) shift)
	;r7 0.6952
	pt8 (polar (list 8.4801 -0.5731) (D->R ang) shift)
  	)
  (command "._PLINE" pt1 "ARC" "Radius" r1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 "")
  (command "._ROTATE" "L" "" "0,0" "90")
  (command "._SCALE" "L" "" "0,0.5" "0.70")
  description
  (newblock BlockName)
)