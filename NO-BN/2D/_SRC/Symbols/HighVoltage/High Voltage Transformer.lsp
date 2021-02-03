;=========================================================================================================================
;
; High Voltage Transformer.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; High-voltage transfromer

(defun C:HIGH-VOLTAGE-TRANSFORMER ( / )

	;2D layer/symbol: JBTEH_Komponenter / EH_Sugetrafo
	;Default insertion direction is "both" in track alignment, rotates with side of track.
	(SUGETRANSFORMATOR 4 1.25)

	;Default insertion direction is "both" in track alignment, rotates with side of track.
	(SUGETRANSFORMATOR-I-KIOSK 4 1.25)

	(AUTOTRANSFORMATOR 3 1.25)
)



(defun SUGETRANSFORMATOR ( len r / blockName )
	(setq
		blockName	"NO-BN-2D-JBTKL-TRANSFORMATOR-SUGETRAFO"
		description	(strcat "KL TRANSFORMATOR, SUGETRAFO")
	)
	(drawCoil len r)
	(command 
		_ROTATE_ _selectAll_ _ENTER_ _origo_ _angle90_
		_MOVE_ _selectAll_ _ENTER_ _origo_ (list (* 4 r) 0)
	)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun SUGETRANSFORMATOR-I-KIOSK ( len r / blockName )
	(setq
		blockName "NO-BN-2D-JBTKL-TRANSFORMATOR-SUGETRAFO-I-KIOSK"
		description	(strcat "KL TRANSFORMATOR, SUGETRAFO I KIOSK")
	)
	(drawCoil len r)
	(command 
		_ROTATE_ _selectAll_ _ENTER_ _origo_ _angle90_
		_MOVE_ _selectAll_ _ENTER_ _origo_ (list (* 4 r) 0)
	)
	(command
		_RECTANGLE_ 
			(list (* (- 5) r) (- r))
			(list (* (+ 5) r) (+ len r r))
	)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun AUTOTRANSFORMATOR ( len r / blockName )
	(setq 
		blockName "NO-BN-2D-JBTKL-TRANSFORMATOR-AUTOTRAFO"
		description	(strcat "KL TRANSFORMATOR, AUTOTRAFO")
	)
	(drawCoil len r)
	(command 
		_MOVE_ _selectAll_ _ENTER_ (list len 0) _origo_ 
		_LINE_ _origo_ (list (+ len r) 0) _ENTER_ 
		_LINE_ (list 0 (* 4 r)) (list (+ r len) (* 4 r)) _ENTER_
	)
	(setLayer layer_AutoTransformerTerminals)
	(addText "NL" (list (- (+ len r r)) (* 8 r)) _th180_ _angleZero_ _rcTextStyle_ _middleCenter_)
	(addText "PL" (list (- (+ len r r)) 0) _th180_ _angleZero_ _rcTextStyle_ _middleCenter_)
	(setLayer layer_Zero)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



;==========================
; draw...X...() functions
;==========================
(defun drawCoil ( len r / )
	(command
		_POLYLINE_
			_origo_
			(list len 0) ; start leg of coil

			_setPolylineArcMode_

			_setPolylineArcRadius_ r
			(list len (* 2 r)) ; endpoint of coil winding

			_setPolylineArcRadius_ (- r)
			(list len (* 4 r)) ; endpoint of coil winding
			
			_setPolylineArcRadius_ (- r)
			(list len (* 6 r)) ; endpoint of coil winding

			_setPolylineArcRadius_ (- r)
			(list len (* 8 r)) ; endpoint of coil winding

			_setPolylineLineMode_
			(list 0 (* 8 r)) ; end leg
			
			_ENTER_
	)
)
