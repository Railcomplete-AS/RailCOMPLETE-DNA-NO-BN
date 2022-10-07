;=========================================================================================================================
;
; FR-SR Commutateurs.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

(defun FRSR-COMMUTATEURS ( / )
	(ANNULATION-TRANSIT)
	(ANNULATION-PROTECTION)
;	(ZONE-ELEMENTAIRE-PROTECTION)  ---> A creer
)



(defun ANNULATION-TRANSIT ( / blockName description r )
	; (*.*)	
	;
	(setq 
		blockName	(strcat _SIG_ "BTN-" "ANNULATION-TRANSIT"	)
		description (strcat "ANNULATION TRANSIT"									)
		r 1.25
	)
	(DrawCircle layDef_Zero r _noWipeout_)
	(DrawHatch _denseHatch_)
	(AddDescriptionBelowOrigin description r)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun ANNULATION-PROTECTION ( / blockName description r )
	; (*.*)	
	;
	(setq 
		blockName	(strcat _SIG_ "BTN-" "ANNULATION-PROTECTION"	)
		description (strcat "ANNULATION PROTECTION"									)
		r 1.25
	)
	(DrawCircle layDef_Zero r _noWipeout_)
	(DrawHatch _denseHatch_)
	(AddDescriptionBelowOrigin description r)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

