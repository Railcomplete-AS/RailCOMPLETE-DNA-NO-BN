;=========================================================================================================================
;
; Switch control equipment.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Turnout, derailer and level crossing boom operation (point machine, derailer machine, boom machine)

(defun C:SWITCH-CONTROL-EQUIPMENT ( / )
	(TraceLevel3 "POINT-MACHINE")		(POINT-MACHINE)
	(TraceLevel3 "DERAILER-MACHINE")	(DERAILER-MACHINE)
	(TraceLevel3 "LOCAL-CONTROL-PANEL")	(LOCAL-CONTROL-PANEL)
)



(defun POINT-MACHINE ( / blockName description r1 r2 )
	(setq 
		blockName "NO-BN-2D-JBTSI-DRIVMASKIN-SPORVEKSEL"
		description	"DRIVMASKIN FOR SPORVEKSEL"
		r1 (GetLargeLanternRadius)
		r2 (* 0.5 r1)
	)
	(DrawCircle layDef_Zero r1 _noWipeout_)
	(DrawCircle layDef_Zero r2 _noWipeout_)
	; no hatch
	(AddDescriptionBelowOrigo description r1)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun DERAILER-MACHINE ( / blockName description r1 r2 )
	(setq 
		blockName "NO-BN-2D-JBTSI-DRIVMASKIN-SPORSPERRE"
		description	"DRIVMASKIN FOR SPORSPERRE"
		r1 (GetLargeLanternRadius)
		r2 (* 0.5 r1)
	)
	(DrawCircle layDef_Zero r1 _noWipeout_)
	(DrawCircle layDef_Zero r2 _noWipeout_)
	; no hatch
	(AddDescriptionBelowOrigo description r1)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun LOCAL-CONTROL-PANEL ( / blockName description r1 r2 )
	(setq 
		blockName	"NO-BN-2D-JBTSI-LOKALSTILLER"
		description	"LOKALOMSTILLER FOR SPORVEKSEL OG SPORSPERRE"
		r1 (GetLargeLanternRadius)
		r2 (* 0.5 r1)
	)
	(DrawCircle layDef_Zero r1 _noWipeout_)
	(DrawCircle layDef_Zero r2 _noWipeout_)
	(DrawHatch _denseHatch_)
	(AddDescriptionBelowOrigo description r1)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
