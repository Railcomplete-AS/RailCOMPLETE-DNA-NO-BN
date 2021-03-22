;=========================================================================================================================
;
; Controller.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
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
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun drawController ( / )
	(command
		_LINE_ "0,-0.875" "-0.875,0" _ENTER_
		_LINE_ "0,0.875" "-1.0607,-0.1857" _ENTER_
		_CIRCLE_ "-1.2676,-0.3926" 0.2927
		_MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
		_LINE_ "0,-2.125" "0,1.075" _ENTER_
	)
)
 