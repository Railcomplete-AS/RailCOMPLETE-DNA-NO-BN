;=========================================================================================================================
;
; Switch Tongue.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Switch tongues

(defun C:SWITCH-TONGUE ( / )
	(SetCadSystemDefaults)  
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-09-R190" "0.6490,0.75" "H" 9.8740)
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-09-R300" "0.8030,0.75" "H" 15.8120)
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-11.66-R500" "0.8030,0.75" "H" 20.0)
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-12-R500" "0.8030,0.75" "H" 20.0)
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-14-R760" "0.8030,0.75" "H" 26.305)
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-15-R760" "0.8030,0.75" "H" 24.502)
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-18.4-R1200" "0.8030,0.75" "H" 32.026)
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-26.1-R2500" "0.8030,0.75" "H" 47.306)
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-09-R190" "0.6490,-0.75" "V" 9.8740)
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-09-R300" "0.8030,-0.75" "V" 15.8120)
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-11.66-R500" "0.8030,-0.75" "V" 20.0)
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-12-R500" "0.8030,-0.75" "V" 20.0)
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-14-R760" "0.8030,-0.75" "V" 26.305)
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-15-R760" "0.8030,-0.75" "V" 24.502)
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-18.4-R1200" "0.8030,-0.75" "V" 32.026)
	(DrawSwitchTongue "NO-BN-2D-JBTOB-SPORVEKSELTUNGE-26.1-R2500" "0.8030,-0.75" "V" 47.306)
)



(defun DrawSwitchTongue ( blockName toungePos side toungeLen / )
	(setq 
		description (strcat "SPORVEKSELTUNGE 1:" (substr blockName 32) " " side)
	)
	(setvar 'ATTDIA 0)
	(setvar 'ATTREQ 0)
	(SetLayer layDef_Zero)
    (command _LINE_ toungePos (strcat "@" (rtos toungeLen) ",0") _ENTER_)
	(AddDescriptionBelowOrigo description -1.0)
	(CreateMetricBlockFromCurrentGraphics (strcat blockName "-" side))
)