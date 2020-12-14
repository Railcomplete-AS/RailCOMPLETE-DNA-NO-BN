;
; 62.lsp
;
(defun 62A (/ blockName description ang x len offset 
		xoff yoff	  
		pt0 pt1 pt2 pt3 pt4 pt5 pt6	pt7 pt8 pt9
	)
	(setq
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-62-UGYLDIG-SIGNAL"
		description "Signal 62 Ugyldighetsskilt"
		ang	  27
		x	  2.25
		len	  (/ x (cos (D->R ang)))
		width	  0.25
		offset	  0.175
		xoff	  (/ offset (sin (D->R ang)))
		yoff	  (/ offset (cos (D->R ang)))
		;
		pt0	  (list 0 0)
		pt1	  (polar pt0 (D->R 90) yoff)
		pt2	  (polar pt1 (D->R (+ 180 ang)) len)
		pt3	  (polar pt2 (D->R (+ 90 ang)) width)
		pt4	  (polar pt3 (D->R ang) 2.3281)
		pt5	  (polar pt4 (D->R (- 180 ang)) 2.2031)
		pt6	  (polar pt5 (D->R (- 90 ang)) width)
		pt7	  (polar pt6 (D->R (- ang)) 2.3906)
		pt8	  (list 0 (car (cdr pt7)))
		pt9	  (polar pt5 (D->R (- 270 ang)) offset)
	)
	(command "._PLINE" pt1 pt2 pt3 pt4 pt5 pt6 pt8 "")

	;shadow
	(command
		"._LINE"
	   pt0
	   (polar pt0 (D->R (+ 180 ang)) 2.4281)
	   pt2
	   ""
	   "._LINE"
	   pt5
	   pt9
	   (polar pt9 (D->R (- ang)) 2.0718)
	   ""
	)
	(command "._MIRROR" "ALL" "" "0,-5" "0,5" "N")
	(drawHatchSelectPoint "0.01" (list 0 (+ offset (/ width 2))) 0 0.01)
	(newBlock blockName)
	description
)