;=========================================================================================================================
;
; Controller.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Pictogram showing the traditional self-regulating rotating mass device - here: may symbolize an interlocking.

(defun C:CONTROLLER ()
	(CONTROLLER)
)



(defun CONTROLLER ( / blockName )
	(setq
		blockName "NO-BN-2D-JBTFE-CONTROLLER"
	)
	(drawController)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun drawController ( / )
	(command
		"._LINE" "0,-0.875" "-0.875,0" ""
		"._LINE" "0,0.875" "-1.0607,-0.1857" ""
		"._CIRCLE" "-1.2676,-0.3926" 0.2927
		"._MIRROR" "_ALL" "" "0,0" "0,1" "_NO"
		"._LINE" "0,-2.125" "0,1.075" ""
	)
)
 