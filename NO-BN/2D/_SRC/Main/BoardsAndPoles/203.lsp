;=========================================================================================================================
;
; 203.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-06-02 KNHEL new sign
;
;=========================================================================================================================

; Speed restriction


(defun E203 ( /	blockName description r )
; Repeater balise group with explicit mentioning of release speed 40 km/h
	;
	;        
	;      ___ 
	;     /   \    
	;    ( 20  )    Circle with '20'
	;     \___/  
	;     
	;       
	;
	(setq	
		blockName "NO-BN-2D-JBTSA_MSS-SKILT-KJOERENDE-SIGNAL-E203-SPEED-RESTRICTION-20"
		description "SKILT SIGNAL E203 SPEED RESTRICTION 20"
		r 2.0
	)
	(DrawCircle layDef_Zero r _noWipeout_)
	(AddTextAtPoint layDef_Zero _th200_	 _origin_ "20") 
	(MoveUp r)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
