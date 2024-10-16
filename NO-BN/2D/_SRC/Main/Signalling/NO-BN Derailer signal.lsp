;=========================================================================================================================
;
; NO-BN Derailer signal.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Derailer signal

(defun NOBN-SPORSPERRESIGNAL ( / blockName description r p1 p2 )
	; Derailer signal
	;      ___ 
	;     / 1 \    
	;    (  .  ) Two semi-circles (circle with a vertical line)
	;     \_2_/  
	;      
	(setq 
		blockName (strcat _SIG_ "SIG-" "SIGNAL-SPORSPERRESIGNAL")
		description "SPORSPERRESIGNAL"
		r 	1.5
		p1 (list 0 (+ r)) 
		p2 (list 0 (- r))
	)
	(DrawCircle layDef_Zero r nil)
	(DrawLine layDef_Zero p1 p2)
	(AddDescriptionBelowOrigin description r)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
