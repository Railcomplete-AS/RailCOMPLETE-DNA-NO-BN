;=========================================================================================================================
;
; 202.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-06-02 KNHEL new sign
;
;=========================================================================================================================

; Speed restriction


(defun E202 ( /	blockName description r )
; Repeater balise group with explicit mentioning of release speed 40 km/h
	;
	;        
	;      ___ 
	;     /   \    
	;    ( 10  )    Circle with '10'
	;     \___/  
	;     
	;       
	;
	(setq	
		blockName "NO-BN-2D-JBTSA_MSS-SKILT-KJOERENDE-SIGNAL-E202-SPEED-RESTRICTION-10"
		description "SKILT SIGNAL E202 SPEED RESTRICTION 10"
		r 2.0
	)
	(DrawCircle layDef_Zero r _noWipeout_)
	(AddTextAtPos layDef_Zero _th200_	 _origin_ "10") 
	(MoveUp r)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)