;=========================================================================================================================
;
; Switch Tongue.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Switch tongues

(defun SWITCH-TONGUES ( / )
	(SetCadSystemDefaults)  

	; R190
	(SWITCH-TONGUE "07-R190"		"0.6490,0.75" _right_ 9.8740)
	(SWITCH-TONGUE "07-R190"		"0.6490,-0.75" _left_ 9.8740)
	(SWITCH-TONGUE "09-R190"		"0.6490,0.75" _right_ 9.8740)
	(SWITCH-TONGUE "09-R190"		"0.6490,-0.75" _left_ 9.8740)

	; R300
	(SWITCH-TONGUE "08.21-R300"	"0.8030,0.75" _right_ 15.8120)
	(SWITCH-TONGUE "08.21-R300"	"0.8030,-0.75" _left_ 15.8120)
	(SWITCH-TONGUE "09-R300"		"0.8030,0.75" _right_ 15.8120)
	(SWITCH-TONGUE "09-R300"		"0.8030,-0.75" _left_ 15.8120)

	; R500
	(SWITCH-TONGUE "11.66-R500"	"0.8030,0.75" _right_ 20.0)
	(SWITCH-TONGUE "11.66-R500"	"0.8030,-0.75" _left_ 20.0)
	(SWITCH-TONGUE "12-R500"		"0.8030,0.75" _right_ 20.0)
	(SWITCH-TONGUE "12-R500"		"0.8030,-0.75" _left_ 20.0)

	; R760
	(SWITCH-TONGUE "14-R760"		"0.8030,0.75" _right_ 26.305)
	(SWITCH-TONGUE "14-R760"		"0.8030,-0.75" _left_ 26.305)
	(SWITCH-TONGUE "15-R760"		"0.8030,0.75" _right_ 24.502)
	(SWITCH-TONGUE "15-R760"		"0.8030,-0.75" _left_ 24.502)

	; R1200
	(SWITCH-TONGUE "18.4-R1200"	"0.8030,0.75" _right_ 32.026)
	(SWITCH-TONGUE "18.4-R1200"	"0.8030,-0.75" _left_ 32.026)

	; R2500
	(SWITCH-TONGUE "26.1-R2500"	"0.8030,0.75" _right_ 47.306)
	(SWITCH-TONGUE "26.1-R2500"	"0.8030,-0.75" _left_ 47.306)
)



(defun SWITCH-TONGUE ( geometry toungePos leftOrRight toungeLen / side )
	(setq 
		side		(cond ((= leftOrRight _left_) "V") ((= leftOrRight _right_) "H") (T "?"))
		blockName	(strcat _TRK_ "TNG-" "SPORVEKSELTUNGER-" geometry "-" side)
		description (strcat "SPORVEKSELTUNGER 1:" geometry " " side)
	)
	(SetLayer layDef_Zero)
    (command _LINE_ toungePos (strcat "@" (rtos toungeLen) ",0") _ENTER_)
	(AddDescriptionBelowOrigin description (cond ((= leftOrRight _left_) 0.25) ((= leftOrRight _right_) (- 1.0))))
	(CreateMetricBlockFromCurrentGraphics blockName)
)
