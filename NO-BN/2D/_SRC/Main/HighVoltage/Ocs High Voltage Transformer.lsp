;=========================================================================================================================
;
; Ocs High Voltage Transformer.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; High-voltage transformer

(defun ANYADM-OCS-HIGH-VOLTAGE-TRANSFORMER ( / )
	; Implemented for all administrations:

	; Specific to this administration:
	;2D layer/symbol: JBTEH_Komponenter / EH_Sugetrafo
	(TraceLevel3 "NOBN-OCS-BOOSTER-LOCATED-ON-OCS-POLE")(NOBN-OCS-BOOSTER-LOCATED-ON-OCS-POLE 4 1.25)	;Default insertion direction is "both" in track alignment, rotates with side of track.
	(TraceLevel3 "NOBN-OCS-BOOSTER-LOCATED-IN-HOUSING")	(NOBN-OCS-BOOSTER-LOCATED-IN-HOUSING 4 1.25)	;Default insertion direction is "both" in track alignment, rotates with side of track.
	(TraceLevel3 "NOBN-OCS-AUTOTRANSFORMER")			(NOBN-OCS-AUTOTRANSFORMER 3 1.25)
)



(defun NOBN-OCS-BOOSTER-LOCATED-ON-OCS-POLE ( len r / blockName description )
	(setq
		blockName	"NO-BN-2D-JBTEH_SUG-SUGETRANSFORMATOR-I-MAST"
		description	(strcat "KL SUGETRANSFORMATOR I MAST")
	)
	(NOBN_DrawOcsTransformerCoil len r)
	(command 
		_ROTATE_ _selectAll_ _ENTER_ _origin_ _angle90_
		_MOVE_ _selectAll_ _ENTER_ _origin_ (list (* 4 r) 0)
	)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-OCS-BOOSTER-LOCATED-IN-HOUSING ( len r / blockName description )
	(setq
		blockName "NO-BN-2D-JBTEH_SUG-SUGETRANSFORMATOR-I-KIOSK"
		description	(strcat "KL SUGETRANSFORMATOR I KIOSK")
	)
	(NOBN_DrawOcsTransformerCoil len r)
	(command 
		_ROTATE_ _selectAll_ _ENTER_ _origin_ _angle90_
		_MOVE_ _selectAll_ _ENTER_ _origin_ (list (* 4 r) 0)
	)
	(command
		_RECTANGLE_ 
			(list (* (- 5) r) (- r))
			(list (* (+ 5) r) (+ len r r))
	)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-OCS-AUTOTRANSFORMER ( len r / blockName description p1 p2 p3 p4 p5 )
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
		blockName "NO-BN-2D-JBTEH_AUT-AUTOTRANSFORMATOR"
		description	(strcat "KL AUTOTRANSFORMATOR")
		p1	(list (+ len r) 0)
		p2	(list 0 (* 4 r))
		p3	(list (+ len r) (* 4 r))
		p4	(list (- (+ len r r)) 0)
		p5	(list (- (+ len r r)) (* 8 r))
	)
	(NOBN_DrawOcsTransformerCoil len r)
	(MoveLeft len)
	(DrawLine layDef_Zero _origin_ p1)
	(DrawLine layDef_Zero p2 p3)
	(AddTextAtPos layDef_AutoTransformerTerminals _th180_ p4 "PL")
	(AddTextAtPos layDef_AutoTransformerTerminals _th180_ p5 "NL")
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



;==========================
; Draw...X...() functions
;==========================
(defun NOBN_DrawOcsTransformerCoil ( len r / )
	;   ---
	;      )
	;      )
	;      )    8r high (each coil winding = 2r high)
	;      )
	;  .---		Line is len long
	(command
		_POLYLINE_
			_origin_
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
