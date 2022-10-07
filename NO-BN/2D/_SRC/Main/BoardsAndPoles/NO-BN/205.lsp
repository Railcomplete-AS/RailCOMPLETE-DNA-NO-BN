;=========================================================================================================================
;
; 205.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-06-02 KNHEL new sign
;
;=========================================================================================================================

; Speed restriction


(defun E205 ( /	blockName description r d rVar)
; Repeater balise group with explicit mentioning of release speed 40 km/h
	;
	;        
	;      ___ 
	;     / ///\    
	;    ( ///  )    Circle with 3 slashes
	;     \___/  
	;     
	;       
	;
	(setq	
		blockName "NO-BN-2D-JBTSA_MSS-SKILT-KJOERENDE-SIGNAL-E205-SPEED-RESTRICTION-LIFTED"
		description "SKILT SIGNAL E203 SPEED RESTRICTION LIFTED"
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
