;=========================================================================================================================
;
; E200.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-06-02 KNHEL new sign
; 2024-06-16 SVNOE renamed
;
;=========================================================================================================================

; Speed restriction


(defun E200 ( /	blockName description r d rVar)
; ERTMS board - End of temporary speed restriction
	;
	;        
	;      _____
	;     /  ///\    
	;    (  /./  )    Circle with 3 slashes
	;     \///__/  
	;     
	;       
	;
	(setq	
		blockName "NO-BN-2D-JBTKO-SKT-SKILT-ERTMS-SPEED-RESTRICTION-LIFTED"
		description "SKILT ERTMS SPEED RESTRICTION LIFTED"
		r 2.0
		d 0.4
	)
	(setq rVar (sqrt (- (* r r) (* d d))))
	(DrawCircle layDef_Zero r _noWipeout_)
	(DrawLine layDef_Zero (list 0.0 (- r)) (list 0.0 r))
 	(DrawLine layDef_Zero (list d (- rVar)) (list d rVar))
	(DrawLine layDef_Zero (list (- d) (- rVar)) (list (- d) rVar))
	(command _ROTATE_ _selectAll_ _ENTER_ _origin_ _angleMinus45_)
	(MoveUp r)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
