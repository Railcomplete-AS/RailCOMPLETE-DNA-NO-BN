;
; Ugyldig signal.lsp
;
; 2019-07-26 CLFEY Added "---DEPRECATED" to function name - replaced by board 62a, see 62.lsp
; 2020-08-02 CLFEY Deprecated, symbol has been removed from 2D library,
;
(defun UGYLDIG-SIGNAL---DEPRECATED (topOfpole feste / blockName drawHatchPt )
	(setq
		blockName "NO-BN-2D-JBTSI-SKILT-KJOERENDE-SIGNAL-62-UGYLDIG-SIGNAL"
	)
	(defun drawHatchPt (pt)
		(command "._-HATCH" pt "Properties" "ANSI31" "0.01" "0" "ORIGIN" "_SET" "0,0.01" "_NO" "")
	)
	(setq	ang	  27
		x	  2.25
		l	  (/ x (cos (D->R ang)))
		width	  0.25
		offset	  0.175
		xoff	  (/ offset (sin (D->R ang)))
		yoff	  (/ offset (cos (D->R ang)))
		pt0	  (list 0 0)
		pt1	  (polar pt0 (D->R 90) yoff)
		pt2	  (polar pt1 (D->R (+ 180 ang)) l)
		pt3	  (polar pt2 (D->R (+ 90 ang)) width)
		pt4	  (polar pt3 (D->R ang) 2.3281)
		pt5	  (polar pt4 (D->R (- 180 ang)) 2.2031)
		pt6	  (polar pt5 (D->R (- 90 ang)) width)
		pt7	  (polar pt6 (D->R (- ang)) 2.3906)
		pt8	  (list 0 (car (cdr pt7)))
		pt9	  (polar pt5 (D->R (- 270 ang)) offset)
	)
	(command "._PLINE" pt1 pt2 pt3 pt4 pt5 pt6 pt8 _open_)
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
	(command "._MIRROR" "_ALL" "" "0,-5" "0,5" "_NO")
	(drawHatchPt (list 0 (+ offset (/ width 2))))
  
	(if (= feste "AAK")
		(progn
			(drawLyingPole 0.46729016 (+ 0.46729016 2))
			(command 
				"._ROTATE" "_LAST" "" "0,0" "90"
				"._MOVE" "_ALL" "" "DISPLACEMENT" (list 0 (- (+ 0.46729016 2)))
			)
			(setq blockName (strcat blockName "-AAK"))
		)
		(progn
			(command 
				"._ROTATE" "_ALL" "" "0,0" -90
				"._MOVE" "_ALL" "" "DISPLACEMENT" (list pole 0)
			)
			(drawLyingPole 0 pole)
			(drawLyingHsBase)
			(command "._ROTATE" "_ALL" "" "0,0" 90)
		)
	)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)
