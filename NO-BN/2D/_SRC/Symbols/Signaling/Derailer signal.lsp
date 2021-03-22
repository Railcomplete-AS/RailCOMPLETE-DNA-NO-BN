;=========================================================================================================================
;
; Derailer signal.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Derailer signal

(defun C:DERAILER-SIGNAL ( / )
	(SPORSPERRESIGNAL)
)



(defun SPORSPERRESIGNAL ( / blockName description r p1 p2 )
	; Derailer signal
	;      ___ 
	;     / 1 \    
	;    (  .  ) Two semi-circles (circle with a vertical line)
	;     \_2_/  
	;      
	(setq 
		blockName "NO-BN-2D-JBTSI-SIGNAL-SPORSPERRESIGNAL"
		description "SPORSPERRESIGNAL"
		r (GetLargeLanternRadius)
		p1 (list 0 (+ r)) 
		p2 (list 0 (- r))
	)
	(DrawCircle layDef_Zero r nil)
	(DrawLine layDef_Zero p1 p2)
	(AddDescriptionBelowOrigo description r)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
