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



(defun SUGETRANSFORMATOR ( len r / blockName description )
	(setq
		blockName	"NO-BN-2D-JBTKL-TRANSFORMATOR-SUGETRAFO"
		description	(strcat "KL TRANSFORMATOR, SUGETRAFO")
	)
	(DrawCoil len r)
	(command 
		_ROTATE_ _selectAll_ _ENTER_ _origo_ _angle90_
		_MOVE_ _selectAll_ _ENTER_ _origo_ (list (* 4 r) 0)
	)
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun SUGETRANSFORMATOR-I-KIOSK ( len r / blockName description )
	(setq
		blockName "NO-BN-2D-JBTKL-TRANSFORMATOR-SUGETRAFO-I-KIOSK"
		description	(strcat "KL TRANSFORMATOR, SUGETRAFO I KIOSK")
	)
	(DrawCoil len r)
	(command 
		_ROTATE_ _selectAll_ _ENTER_ _origo_ _angle90_
		_MOVE_ _selectAll_ _ENTER_ _origo_ (list (* 4 r) 0)
	)
	(command
		_RECTANGLE_ 
			(list (* (- 5) r) (- r))
			(list (* (+ 5) r) (+ len r r))
	)
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun AUTOTRANSFORMATOR ( len r / blockName description p1 p2 p3 p4 p5 )
	;
	; NL=5 ---+
	;          )
	;          )
	;          2__3
	;          )
	;          )
	; PL=4 ---.---1
	;
	(setq 
		blockName "NO-BN-2D-JBTKL-TRANSFORMATOR-AUTOTRAFO"
		description	(strcat "KL TRANSFORMATOR, AUTOTRAFO")
		p1	(list (+ len r) 0)
		p2	(list 0 (* 4 r))
		p3	(list (+ len r) (* 4 r))
		p4	(list (- (+ len r r)) 0)
		p5	(list (- (+ len r r)) (* 8 r))
	)
	(DrawCoil len r)
	(MoveLeft len)
	(DrawLine layDef_Zero _origo_ p1)
	(DrawLine layDef_Zero p2 p3)
	(AddTextAtPos layDef_AutoTransformerTerminals _th180_ p4 "PL")
	(AddTextAtPos layDef_AutoTransformerTerminals _th180_ p5 "NL")
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



;==========================
; Draw...X...() functions
;==========================
(defun DrawCoil ( len r / )
	;   ---
	;      )
	;      )
	;      )    8r high (each coil winding = 2r high)
	;      )
	;  .---		Line is len long
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
