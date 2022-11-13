;=========================================================================================================================
;
; Ocs Switch Actuator.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-03-01 KNHEL Created
;
;=========================================================================================================================

; Actuator for OCS power switch / interrupter, manual or machine-powered

(defun OCS-SWITCH-ACTUATOR ( / blockNameRoot description)
	(setq blockNameRoot (strcat _OCS_ "MAN-" "MAN" _uOSLASH_ "VERMASKIN"	))
	(setq description (strcat "KL MAN" _uOSLASH_ "VERMASKIN"				))
	(setq tOpen	(strcat _uARING_ "PEN")		tClosed (strcat "LUKKET")		)

	(TraceLevel3 "NOBN-OCS-SWITCH-ACTUATOR")			(NOBN-OCS-SWITCH-ACTUATOR)

	; Cleanup temp globals:
	(setq 
		tOpen 			nil
		tClosed			nil
		blockNameRoot	nil
		description		nil
	)
)



;================== NOBN functions ==================================================================
(defun NOBN-OCS-SWITCH-ACTUATOR  ( / r attTen )
	;      ___ 
	;     /   \    
	;    (  M  )	; M = 'motor'
	;     \___/  	; Corresponding power switch in "Open" position
	;		|
	;
	; .
	;
	(NOBN_DrawMotor tOpen)
	(setq blockName (strcat blockNameRoot "-" tOpen))
	(AddDescriptionBelowOrigin description -2)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
  	;      ___ 
	;     /   \    
	;    (  M  )	; M = 'motor'
	;     \___/  	; Corresponding power switch in "Closed" position
	;		|
	;		|
	; .		|
	;
	(NOBN_DrawMotor tClosed)
  	(setq blockName (strcat blockNameRoot "-" tClosed))
	(AddDescriptionBelowOrigin description -2)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN_DrawMotor ( variation / r p1 p2 p3 p4 )
	;
	;      +---+ 
	;     /     \   ; Radius r 
	;    (   1   )  ; Text 'M' at t1 = p1
	;     \     /   
	;      +-2-+
	;        |
	;        3
	;        :		; Lower part only for "Closed" variation
	; .      4
	; 
	(setq 
		r	1.650
		p1	'(3.000 4.650)
		p2	'(3.000 3.000)
		p3	'(3.000 1.750)
		p4	'(3.000 0.000)
		t1	p1
	)
	(DrawCircleAtPos layDef_HighVoltageSwitchActuator p1 r _noWipeout_)
	(AddTextAtPos layDef_HighVoltageSwitchActuator _th180_ t1 "M")
	(if (= variation tOpen)
		(DrawLine layDef_HighVoltageSwitchActuator p2 p3)
	;else
		(DrawLine layDef_HighVoltageSwitchActuator p2 p4)
    )
)
