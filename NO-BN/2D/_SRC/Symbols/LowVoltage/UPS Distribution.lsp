;=========================================================================================================================
;
; UPS Distribution.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; UPS distribution panel

(defun C:UPS-DISTRIBUTION ( )
	(UPS-FORDELER)
)



(defun UPS-FORDELER ( / blockName x y halfWidth halfHeight radius dx )
	(setq
		blockName "NO-BN-2D-JBTEL-UPS-FORDELER"
		x 1.0
		y 1.0
		halfWidth (/ x 2)
		halfHeight (/ y 2)
		radius (* x 0.1)
	)
	(command
		"._RECTANGLE" (list (* x -0.5) (* y -0.5)) (list (* x 0.5) (* y 0.5))
		"._LINE" (list (* x -0.5) (* y 0.5)) (list (* x 0.5) (* y -0.5)) ""
		"._LINE" (list (* x -0.4) (* y -0.2)) (list 0 (* y -0.2)) ""
		"._LINE" (list (* x -0.4) (* y -0.3)) (list 0 (* y -0.3)) ""
		"._ARC"  (list (* x 0.00) (* y 0.25)) (list (* x 0.10) (* y 0.15)) (list (* x 0.20) (* y 0.20))
		"._ARC"  (list (* x 0.20) (* y 0.20)) (list (* x 0.30) (* y 0.25)) (list (* x 0.40) (* y 0.15))
	)
	(setLayerAndObjectColor layer_Description "_ByLayer")
	(addMText "UPS-fordeler" "0,-0.6" _descriptionTextHeight_ 1.5 0 "ISO" "_TC")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)
