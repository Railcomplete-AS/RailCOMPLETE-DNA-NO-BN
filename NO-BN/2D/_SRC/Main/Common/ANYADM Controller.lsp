;=========================================================================================================================
;
; ANYADM Controller.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Pictogram showing the traditional self-regulating rotating mass device - here: may symbolize an interlocking.

(defun CONTROLLERS ()
	(CONTROLLER)
)



(defun CONTROLLER ( / blockName )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _COM_ "DIV-" "CONTROLLER"				)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _COM_ "DIV-" "PROSESSTYRING"				)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _COM_ "DIV-" "CONTROLEUR-DE-PROCESSUS"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _COM_ "DIV-" "ANSTEUERUNG"				)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _COM_ "DIV-" "CONTROLLER"				)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "CONTROLLER"				)))
		((= _ADM_ _NOBN_) (setq description (strcat "PROSESSTYRING"				)))
		((= _ADM_ _FRSR_) (setq description (strcat "CONTROLEUR-DE-PROCESSUS"	)))
		((= _ADM_ _DEDB_) (setq description (strcat "ANSTEUERUNG"				)))
		((= _ADM_ _JPTX_) (setq description (strcat "CONTROLLER"				)))
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
		_MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
		_LINE_ "0,-2.125" "0,1.075" _ENTER_
	)
)
 