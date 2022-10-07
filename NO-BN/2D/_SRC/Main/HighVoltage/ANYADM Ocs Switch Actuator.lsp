;=========================================================================================================================
;
; ANYADM Ocs Switch Actuator.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-03-01 KNHEL Created
;
;=========================================================================================================================

; Actuator for switch, manual or machine-powered

(defun ANYADM-OCS-SWITCH-ACTUATOR ( / blockNameRoot description)
	(cond 
		((= _ADM_ _XXGL_) (setq blockNameRoot (strcat _OCS_ "SWA-" "SWITCH-ACTUATOR"			)))
		((= _ADM_ _NOBN_) (setq blockNameRoot (strcat _OCS_ "MAN-" "MAN" _uOSLASH_ "VERMASKIN"	)))
		((= _ADM_ _FRSR_) )
		((= _ADM_ _DEDB_) )
		((= _ADM_ _JPTX_) (setq blockNameRoot (strcat _OCS_ "SWA-" "SWITCH-ACTUATOR"			)))
	)
	(cond
		((= _ADM_ _XXGL_) (setq description (strcat "OCS POWER SWITCH ACTUATOR"					)))
		((= _ADM_ _NOBN_) (setq description (strcat "KL MAN" _uOSLASH_ "VERMASKIN"				)))
		((= _ADM_ _FRSR_) )
		((= _ADM_ _DEDB_) )
		((= _ADM_ _JPTX_) (setq description (strcat "OCS POWER SWITCH ACTUATOR"					)))
	)
	(cond
		((= _ADM_ _XXGL_) (setq tOpen	(strcat "OPEN")				tClosed (strcat "CLOSED"))			)
		((= _ADM_ _NOBN_) (setq tOpen	(strcat _uARING_ "PEN")		tClosed (strcat "LUKKET"))			)
		((= _ADM_ _FRSR_) (setq tOpen	(strcat "OUVERT")			tClosed (strcat "FERM" _uEACUTE_))	)
		((= _ADM_ _DEDB_) (setq tOpen	(strcat _uOUML_ "FFEN")		tClosed (strcat "GESCHLOSSEN"))		)
		((= _ADM_ _JPTX_) (setq tOpen	(strcat "OPEN")				tClosed (strcat "CLOSED"))			)
	)
	(cond 
		((= _ADM_ _XXGL_) 
		)
		((= _ADM_ _NOBN_)
			(TraceLevel3 "NOBN-OCS-SWITCH-ACTUATOR")			(NOBN-OCS-SWITCH-ACTUATOR)
		)
		((= _ADM_ _FRSR_) 
		)
		((= _ADM_ _DEDB_) 
		)
		((= _ADM_ _JPTX_)
			; TODO 2022-03-15 - Replace NOBN stuff with JPTX graphics:
			(TraceLevel3 "NOBN-OCS-SWITCH-ACTUATOR")			(NOBN-OCS-SWITCH-ACTUATOR)
		)
	)
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
