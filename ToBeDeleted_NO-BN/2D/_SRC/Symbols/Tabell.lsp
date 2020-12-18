;
; Tabell.lsp
;
; =====
; 2019-05-07 CLFEY: This file is not in use! It's intended use was to make an internal document, an overview over all available symbols for Bane NOR.
; 2019-07-26 CLFEY: Cleaned up a little, it mighr become useful later.
; =====
;
(defun C:GENERATE-TABELL ( /  layerFrameName  layerFrameColor layerPointsName
	  layerPointsColor		  layerSymbolsName
	  layerSymbolsColor		  layerTextsName
	  layerTextsColor layerTopTextsName
	  layerTopTextsColor		  x1
	  x2		  x		  y
	  a3		  cols		  rows
	  ty1		  ty2		 n
	)

	(setq
		layerFrameName  "JBTSI_SYMBOLTABELL_RAMME"
		layerPointsName "JBTSI_SYMBOLTABELL_INNSETTINGSPUNKTER"
		layerSymbolsName "JBTSI_SYMBOLER_TRV_500"
		layerTextsName "JBTSI_SYMBOLTABELL_TEKSTER"
		layerTopTextsName "JBTSI_SYMBOLTABELL_TOPPTEKSTER"
	)
	(command
		"._LAYER"
			"NEW" layerFrameName
			"COLOR" "yellow" layerFrameName
			"NEW" layerPointsName
			"COLOR" "yellow" layerPointsName
			"NEW" layerSymbolsName
			"COLOR" "white" layerSymbolsName
			"NEW" layerTextsName
			"COLOR" "yellow" layerTextsName
			"NEW" layerTopTextsName
			"COLOR" "yellow" layerTopTextsName
			""
	)
	(setq
		blockNames (GetBlockNames)
		#symbols (length blockNames)
		;produce frame	
		x1	   8.75
		x2	   13.5
		x	   (+ x1 x2 1.5)
		y	   6.0
		a3	   (/ 297.0 420.0)	;a3 side ratio
		cols	   (round (sqrt (/ (* #symbols y) (* a3 x))))
		rows	   (1+ (fix (/ #symbols cols))) ;round up
		;top cells
		ty1 0.75
		ty2 3.75
	)
	(command
		"._Layer" "SET" layerFrameName ""
		"._RECTANG"
			"0,0"
			(list x1 y)
		"._RECTANG"
			(list x1 0)
			(list (+ x1 x2) y)
		"._ARRAY"
			"ALL" "" "R" 
			rows cols	;number
			y x			;separation
		"._LAYER" "LOCK" layerFrameName "" 
		;top frame:
		"._Layer" "SET" layerTopTextsName ""
		"._RECTANG" 
			(list 0 (* y rows))
			(list x1 (+ ty1 (* y rows)))
		"._RECTANG"
			(list x1 (* y rows)) (list (+ x1 x2) (+ ty1 (* y rows)))
		"._RECTANG"
			(list 0 (+ ty1 (* y rows)))
			(list x1 (+ ty1 ty2 (* y rows)))
		"._RECTANG"
			(list x1 (+ ty1 (* y rows)))
			(list (+ x1 x2) (+ ty1 ty2 (* y rows)))
		"._TEXT" "STYLE" "iso" "JUSTIFY" "MC"
			(list (/ x1 2) (+ ty1 (/ ty2 2) (* y rows))) 0.9 0 "SYMBOL"
		"._TEXT" "STYLE" "iso" "JUSTIFY" "MC"
			(list (+ x1 (/ x2 2)) (+ ty1 (/ ty2 2) (* y rows))) 0.9 0 "BETYDNING"
		"._ARRAY" "ALL" "" "R" 1 cols x
		"._LAYER" "LOCK" layerTopTextsName ""
	)
	(setq i 0 j 0 n 0)
	(while (and (< j cols) (< n #symbols))
		(while (and (< i rows) (< n #symbols))
			(if description
				(insertFunction x y x1 x2 i j rows cols (nth n blockNames) (nth n descriptions) layerPointsName layerSymbolsName layerTextsName)
				;else:
				(insertFunction x y x1 x2 i j rows cols (nth n blockNames) nil layerPointsName layerSymbolsName layerTextsName)
			)
			(setq
				i (1+ i)
				n (1+ n)
			)
		)
		(setq 
			j (1+ j)
			i 0
		)
	)
	(command "._LAYER" "LOCK" layerPointsName ""
	   "._LAYER" "LOCK" layerTextsName ""
	   )
) ;end TABLE



(defun insertFunction (x y x1 x2 i j rows cols blockName description
		    layerPointsName layerSymbolsName layerTextsName
		    / cellCorner radius insertBlock insertText
		    )
	(setq
		cellCorner (list (* j x) (* (- rows i 1) y))
		radius	   0.7321
		insertBlock(polar (polar cellCorner 0 (/ x1 2)) (D->R 90) (/ y 4))
		insertText (polar (polar cellCorner 0 (+ x1 radius)) (D->R 90) (- y radius))
	)
	(command
		"._LAYER" "SET" layerPointsName ""
		"._CIRCLE" insertBlock radius
		"._CIRCLE" insertText radius
		"._LAYER" "SET" layerSymbolsName ""
		"._INSERT" blockName	insertBlock "" "" ""
		"._LAYER" "SET" layerTextsName ""
		"-MTEXT" insertText
		"Height" 0.9
		"Justify" "TL"
		"Line" "At" 1 ;line style at least
		"Style" "iso"
		(strcat "@" (rtos (- x2 (* 2 radius))) ","	(rtos (- (* 2 radius) y)))
		; (list (+ (car insertText) (- x2 radius))
		; (+ (cadr insertText) (- radius y)))
		description ""
	)
	; (blockDescription "insertFunctionBlockDescription")
)



(defun GetBlockNames (/ adoc name blockNames)
	(vl-load-com)  
	(setq
		adoc (vla-get-activedocument (vlax-get-acad-object))
	) 
	(vlax-for blk (vla-get-blocks adoc)) ; Get all blocks...
    ; Exclude model and paper spaces, xref and anonymus blocks:
    (if (and  (equal (vla-get-IsLayout blk) :vlax-false)
              (equal (vla-get-IsXref blk) :vlax-false)
              (/= (substr (vla-get-Name blk) 1 1) "*")
		)
		(setq blockNames (cons (vla-get-Name blk) blockNames)) ; (setq lst (cons (vlax-vla-object->ename blk) lst))
    ) 
	blockNames
)