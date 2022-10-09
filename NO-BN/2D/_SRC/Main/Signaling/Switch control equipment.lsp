;=========================================================================================================================
;
; Switch control equipment.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Turnout, derailer and level crossing boom operation (point machine, derailer machine, boom machine)

(defun ANYADM-SWITCH-CONTROL-EQUIPMENT ( / )
	(TraceLevel3 "ANYADM-POINT-MACHINE")		(ANYADM-POINT-MACHINE)
	(TraceLevel3 "ANYADM-DERAILER-MACHINE")		(ANYADM-DERAILER-MACHINE)
	(TraceLevel3 "ANYADM-LOCAL-CONTROL-PANEL")	(ANYADM-LOCAL-CONTROL-PANEL)
)



(defun ANYADM-POINT-MACHINE ( / blockName description r1 r2 )
	;    _____
	;   /  _  \
	;  | ( . ) |
	;   \_____/ 
	;       
	(setq blockName (strcat _SIG_ "DRV-" "SPORVEKSELDRIVMASKIN"	))
	(setq description (strcat "SPORVEKSELDRIVMASKIN"			))
	(setq 
		r1 1.5
		r2 (HalfOf r1)
	)
	(DrawCircle layDef_Zero r1 _noWipeout_)
	(DrawCircle layDef_Zero r2 _noWipeout_)
	; no hatch
	(AddDescriptionBelowOrigin description r1)

	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun ANYADM-DERAILER-MACHINE ( / blockName description r1 r2 )
	;    _____
	;   /  _  \
	;  | ( . ) |		; Same as switch point machine
	;   \_____/ 
	;       
	(setq blockName (strcat _SIG_ "SPD-" "SPORSPERREDRIVMASKIN"		))
	(setq description (strcat "SPORSPERREDRIVMASKIN"				))
	(setq 
		r1 1.5
		r2 (* 0.5 r1)
	)
	(DrawCircle layDef_Zero r1 _noWipeout_)
	(DrawCircle layDef_Zero r2 _noWipeout_)
	; no hatch
	(AddDescriptionBelowOrigin description r1)

	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun ANYADM-LOCAL-CONTROL-PANEL ( / blockName description r1 r2 )
	;    _____
	;   /  _  \
	;  | (*.*) |	Dense hatch in inner circle
	;   \_____/ 
	;       
	(setq blockName (strcat _SIG_ "LOK-" "LOKALSTILLER"						))
	(setq description (strcat "LOKALSTILLER FOR SPORVEKSEL OG SPORSPERRE"	))
	(setq 
		r1 1.5
		r2 (* 0.5 r1)
	)
	(DrawCircle layDef_Zero r1 _noWipeout_)
	(DrawCircle layDef_Zero r2 _noWipeout_)
	(DrawHatch _denseHatch_)
	(AddDescriptionBelowOrigin description r1)

	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
