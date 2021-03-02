;=========================================================================================================================
;
; Balise.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; ATC balise

(defun C:BALISE ( / ) 
	(subSubStep "BALISE-GROUP")			(BALISE-GROUP)
		
	(subSubStep "BALISE tom/fast")		(BALISE 1) ; variation 1 = tom/fast = empty / fixed telegram
	(subSubStep "BALISE tom/styrt")		(BALISE 2) ; variation 2 = tom/styrt = empty / controlled telegram
	(subSubStep "BALISE fylt/fast")		(BALISE 3) ; variation 3 = fylt/fast = hatched / fixed telegram
	(subSubStep "BALISE fylt/styrt")	(BALISE 4) ; variation 4 = fylt/styrt = hatched / controlled telegram
)



(defun BALISE ( variation / blockName side h dh bx by A B C mx my paperScale dwgScale  )
	; NB This routine contains metric info which must be generated for each scale in turn.
	;https://trv.jbv.no/wiki/Signal/Bygging/ATC/Parallellbaliser_og_kodere#label-fig:Metallfritt område, og avstand til kryssende kabler
	;Metallfritt område
	;
	; Uses Global 'paperScaleList'.
	;
	(setq 
		conversionScale 0.5 ; reduce schematic graphics by 0.5 to achieve a suitable 1:500 scale

		; metal free area:
		bx 0.6	; large balise physical length [m]
		by 0.5 	; large balise physical width [m]
		A 0.5  		; Metal-free tolerance in track direction, low-mileage side, ref Bane NOR minibalise installation manual
		B 0.5  		; Metal-free tolerance track direction, hi-mileage side
		C 0.2  		; Metal-free tolerance across track direction
		mx (+ (* _half_ bx) A)	; metal-free area along track direction, from centre of balise
		my (+ (* _half_ by) C)	; metal-free area across track direction, from centre of balise
	)

	; Schematic symbol
	(setq 
		blockName (drawBalise variation) ; NOTE: Schematic size graphics routine draws the balise and returns the basic block name
		description (strcat "BALISE " (cond 
			((= variation 1) "TOM/FAST")
			((= variation 2) "TOM/STYRT")
			((= variation 3) "FYLT/FAST")
			((= variation 4) "FYLT/STYRT"))
		)
	)
	(addDescriptionBelowOrigo description _one_) ; below TEKST_UNDER item
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	; Metric symbol
	(setLayer layDef_Balise_3m_Separation)
	(command 
		_POLYLINE_ (list 3 0) _setPolylineArcMode_ _setPolylineArcCenter_ _origo_ _setPolylineArcAngle_ 15 _ENTER_
		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _xAxis_ _keepMirrorSource_
		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
	)
	(addTextAtPosWithJustification layDef_Balise_3m_Separation _th020_ (list (- 2.9) (- 0.5)) "3" _topLeft_)
	(command _MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_)
		
	(setLayer layDef_Balise_8m_Separation)
	(command 
		_POLYLINE_ (list 8 0) _setPolylineArcMode_ _setPolylineArcCenter_ _origo_ _setPolylineArcAngle_ 9 _ENTER_
		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _xAxis_ _keepMirrorSource_
		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
	)
	(addTextAtPosWithJustification layDef_Balise_8m_Separation _th020_ (list (- 7.9) (- 0.95)) "8" _topLeft_)
	(command _MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_)

	(setLayer layDef_Balise_12m_Separation)
	(command 
		_POLYLINE_ (list 12 0) _setPolylineArcMode_ _setPolylineArcCenter_ _origo_ _setPolylineArcAngle_ 10 _ENTER_
		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _xAxis_ _keepMirrorSource_
		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
	)
	(addTextAtPosWithJustification layDef_Balise_12m_Separation _th020_  (list (- 11.8) (- 1.8)) "12" _topLeft_)
	(command _MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_)
		
	; Metal-free area:
	(setLayerAndObjectColor layDef_Balise_MetalFreeArea "_ByLayer")
	(command _RECTANGLE_ (list (- mx) (- my))   (list (+ mx) (+ my)))
	(createMetricBlockFromCurrentGraphics blockName)
)


 		
(defun BALISE-GROUP ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTSI-ATC-BALISEGRUPPE"
	)
	; Equilateral triangle, side = 6, draw a dashed line (without using the CAD system's dash-line mechanism):
	(setLayerAndObjectColor layDef_Zero "_ByBlock")
	(command
		_LINE_ "0,1.5981" "-0.3,1.0785" _ENTER_
		_LINE_ "-0.6,0.5588" "-0.9,0.0392" _ENTER_
		_LINE_ "-1.2,-0.4804" "-1.5,-1" "-0.9,-1" _ENTER_
		_LINE_ "-0.3,-1" "0,-1" _ENTER_
		_MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
		_MOVE_ _selectAll_ _ENTER_ _origo_ _yAxis_
	)
	
	; Twice as big as balise symbol (Bane NOR wants it that way):
	(command _SCALE_ _selectAll_ _ENTER_ _origo_ _two_)

	(createSchematicBlockFromCurrentGraphics blockName) 
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)	



;==========================
; draw...X...() functions
;==========================
(defun drawBalise ( variation / blockName side h dh )
	; Draw balise triangle in schematic size
	(setq 
		; Balise triangle:
		side 3.0 	; equilateral triangle symbol side
		h (* side (sin (D->R 60))) ; height of triangle, side * sqrt(3)/2
		dh 0.10  ; The height of the "controlled balise" bar above the triangle
	)
	
	(setLayer layDef_Zero)
	(command 
		_COLOR_ _ByBlock_
		_POLYLINE_
			(list (/ side -2) 0)
			(list (/ side 2) 0)
			(strcat "@" (rtos side) "<120")
			_closedPolyline_
	)
	(cond 
		((= variation 1)
			(setq blockName "NO-BN-2D-JBTSI-ATC-BALISE-TOM-FAST")
		)
		((= variation 2)
			(setq blockName "NO-BN-2D-JBTSI-ATC-BALISE-TOM-STYRT")
			(command 
				_RECTANGLE_ 
					(list (/ side -2.0) h)
					(list (/ side 2.0) (+ h dh))
			)
			(drawHatch _denseHatch_)
		)
		((= variation 3)
			(drawHatchFromPoint 0.02 "0.1,0.1" 0 0.1)
			(setq blockName "NO-BN-2D-JBTSI-ATC-BALISE-FYLT-FAST")
		)
		((= variation 4)
			(drawHatchFromPoint 0.02 "0.1,0.1" 0 0.1)
			(setq blockName "NO-BN-2D-JBTSI-ATC-BALISE-FYLT-STYRT")
			(command 
				_RECTANGLE_
					(list (/ side -2.0) h)
					(list (/ side 2.0) (+ h dh))
			)
			(drawHatch _denseHatch_)
		)
	)
	(addAtt "TEKSTOVER" "Tekst over" _ENTER_ (list 0 (* 1.333 side)) _th100_ _angleZero_ _rcTextStyle_ _middleCenter_)
	(addAtt "TEKSTUNDER" "Tekst under" _ENTER_ (list 0 (- (* 0.8 side))) _th100_ _angleZero_ _rcTextStyle_ _middleCenter_)
	blockName ; Must return block name to caller
)
