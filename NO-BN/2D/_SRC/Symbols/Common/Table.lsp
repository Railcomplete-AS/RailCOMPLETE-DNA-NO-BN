;=========================================================================================================================
;
; Table.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2019-05-07 CLFEY: This file is not in use! It's intended use was to make an internal document, an overview over all available symbols for Bane NOR.
; 2019-07-26 CLFEY: Cleaned up a little, it might become useful later.
; 2020-07-26 CLFEY: Moved function 'round' here, deleted support file round.lsp in folder Utilities (was only in use here).
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Table holding each generated 2D symbol, plus the symbols description (if provided).
; Useful for documentation of symbols in paper / PDF drawings.

(defun round ( n / ) (fix (+ n (if (minusp n) -0.5 0.5))))

(defun C:GENERATE-SYMBOL-OVERVIEW-TABLE ( /  
		layerFrameName  
		layerFrameColor
		layerTableName
		layerPointsColor
		layerSymbolsName
		layerSymbolsColor
		layerTextsName
		layerTextsColor
		layerTopTextsName
		layerTopTextsColor		  
		x1	x2
		x	y
		a3
		cols	rows
		ty1	ty2
		n
	)

	(setq
		layerFrameName		"JBTFE_DIV_SYMBOLTABELL_RAMME"
		layerTableName		"JBTFE_DIV_SYMBOLTABELL_INNSETTINGSPUNKTER"
		layerSymbolsName	"JBTFE_DIV_SYMBOLER_TRV_500"
		layerTextsName		"JBTFE_DIV_SYMBOLTABELL_TEKSTER"
		layerTopTextsName	"JBTFE_DIV_SYMBOLTABELL_TOPPTEKSTER"
	)
	(command
		_LAYER_
			_createNewLayer_ layerFrameName
			_colorizeLayer_ _colorYellow_ layerFrameName

			_createNewLayer_ layerTableName
			_colorizeLayer_ _colorYellow_ layerTableName

			_createNewLayer_ layerSymbolsName
			_colorizeLayer_ _colorWhite_ layerSymbolsName

			_createNewLayer_ layerTextsName
			_colorizeLayer_ _colorYellow_ layerTextsName

			_createNewLayer_ layerTopTextsName
			_colorizeLayer_ _colorYellow_ layerTopTextsName
			_ENTER_
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
		_LAYER_ _setLayer_ layerFrameName _ENTER_
		
		_RECTANGLE_
			_origo_
			(list x1 y)
		
		_RECTANGLE_
			(list x1 0)
			(list (+ x1 x2) y)

		_ARRAY_
			_selectAll_ _ENTER_ 
			_rectangularArray_ 
				rows cols	;number
				y x			;separation

		_LAYER_ _lockLayer_ layerFrameName _ENTER_ 

		;top frame:
		_LAYER_ _setLayer_ layerTopTextsName _ENTER_

		_RECTANGLE_ 
			(list 0 (* y rows))
			(list x1 (+ ty1 (* y rows)))

		_RECTANGLE_
			(list x1 (* y rows)) (list (+ x1 x2) (+ ty1 (* y rows)))

		_RECTANGLE_
			(list 0 (+ ty1 (* y rows)))
			(list x1 (+ ty1 ty2 (* y rows)))

		_RECTANGLE_
			(list x1 (+ ty1 (* y rows)))
			(list (+ x1 x2) (+ ty1 ty2 (* y rows)))

		_TEXT_ _setTextStyle_ _rcTextStyle_ _justifyText_ _middleCenter_
			(list (/ x1 2) (+ ty1 (/ ty2 2) (* y rows))) _th090_ _angleZero_ 
			"SYMBOL"

		_TEXT_ _setTextStyle_ _rcTextStyle_ _justifyText_ _middleCenter_
			(list (+ x1 (/ x2 2)) (+ ty1 (/ ty2 2) (* y rows))) _th090_ _angleZero_ 
			"BETYDNING"
		
		_ARRAY_ _selectAll_ _ENTER_ _rectangularArray_ 1 cols x

		_LAYER_ _lockLayer_ layerTopTextsName _ENTER_
	)
	(setq i 0 j 0 n 0)
	(while (and (< j cols) (< n #symbols))
		(while (and (< i rows) (< n #symbols))
			(if description
				(insertFunction x y x1 x2 i j rows cols (nth n blockNames) (nth n descriptions) layerTableName layerSymbolsName layerTextsName)
				;else:
				(insertFunction x y x1 x2 i j rows cols (nth n blockNames) nil layerTableName layerSymbolsName layerTextsName)
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
	(command 
		_LAYER_ _lockLayer_ layerTableName _ENTER_
		_LAYER_ _lockLayer_ layerTextsName _ENTER_
	)
) ;end TABLE



(defun insertFunction (x y x1 x2 i j rows cols blockName description
		    layerTableName layerSymbolsName layerTextsName
		    / cellCorner radius insertBlock insertText
		    )
	(setq
		cellCorner (list (* j x) (* (- rows i 1) y))
		radius	   0.7321
		insertBlock(polar (polar cellCorner 0 (/ x1 2)) (D->R _angle90_) (/ y 4))
		insertText (polar (polar cellCorner 0 (+ x1 radius)) (D->R _angle90_) (- y radius))
	)
	(command
		_LAYER__setLayer_ layerTableName _ENTER_
		_CIRCLE_ insertBlock radius
		_CIRCLE_ insertText radius
		_LAYER_ _setLayer_ layerSymbolsName _ENTER_
		_INSERT_ blockName insertBlock _ENTER_ _ENTER_ _ENTER_
		_LAYER_ _setLayer_ layerTextsName _ENTER_
		_MTEXT_
			insertText
			"Height" _th090_
			"Justify" _topLeft_
			"Line" "At" 1 ;line style at least
			"Style" _rcTextStyle_
			(strcat "@" (rtos (- x2 (* 2 radius))) ","	(rtos (- (* 2 radius) y)))
			; (list (+ (car insertText) (- x2 radius))
			; (+ (cadr insertText) (- radius y)))
			description
			_ENTER_
	)
)



(defun GetBlockNames (/ adoc name blockNames)
	(vl-load-com)  
	(setq
		adoc (vla-get-activedocument (vlax-get-acad-object))
	) 
	(vlax-for blk (vla-get-blocks adoc)) ; Get all blocks...
    ; Exclude model and paper spaces, xref and anonymous blocks:
    (if (and  (equal (vla-get-IsLayout blk) :vlax-false)
              (equal (vla-get-IsXref blk) :vlax-false)
              (/= (substr (vla-get-Name blk) 1 1) "*")
		)
		(setq blockNames (cons (vla-get-Name blk) blockNames)) ; (setq lst (cons (vlax-vla-object->ename blk) lst))
    ) 
	blockNames
)