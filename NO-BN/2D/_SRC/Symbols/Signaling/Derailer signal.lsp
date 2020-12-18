;=========================================================================================================================
;
; Derailer signal.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
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
	;    (  .  ) Two semi-circles
	;     \_2_/  
	;      
	(setq 
		blockName "NO-BN-2D-JBTSI-SIGNAL-SPORSPERRESIGNAL"
		description "SPORSPERRESIGNAL"
		r (getLargeLanternRadius)
		p1 (list 0 (+ r)) 
		p2 (list 0 (- r))
	)
	(drawCircle layer_Zero r nil)
	(drawLine layer_Zero p1 p2)
	(addDescriptionBelowOrigo description r)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)
