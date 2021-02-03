;=========================================================================================================================
;
; Low Voltage Transfomer.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Low-voltage transformer

(defun C:LOW-VOLTAGE-TRANSFORMER ( )
	(TRANSFORMATOR-H-TIL-L) ; Fra høyspent til lavspent (15 kV til 220 VAC)
)



(defun TRANSFORMATOR-H-TIL-L ( / blockName x y halfWidth halfHeight radius dx )
	(setq
		blockName "NO-BN-2D-JBTEL-TRANSFORMATOR"
		x 2.0
		y 1.0
		halfWidth (/ x 2)
		halfHeight (/ y 2)
		radius (* halfHeight 0.8)
		dx (* radius 0.6)
	)
	(command
		_RECTANGLE_ (list (- halfWidth) (- halfHeight)) (list halfWidth halfHeight)
		_CIRCLE_ (list (- dx) 0) radius
		_CIRCLE_ (list dx 0) radius
	)
	(setLayerAndObjectColor layer_Description "_ByLayer")
	(addMText "Transformator" "0,-0.6" _descriptionTextHeight_ 1.5 0 _rcTextStyle_ _topCenter_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
