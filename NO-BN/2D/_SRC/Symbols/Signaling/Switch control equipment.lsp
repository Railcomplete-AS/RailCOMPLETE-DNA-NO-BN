;=========================================================================================================================
;
; Switch control equipment.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Turnout, derailer and level crossing boom operation (point machine, derailer machine, boom machine)

(defun C:SWITCH-CONTROL-EQUIPMENT ( / )
	(POINT-MACHINE)
	(DERAILER-MACHINE)
	(LOCAL-CONTROL-PANEL)
)



(defun POINT-MACHINE ( / blockName description r1 r2 )
	(setq 
		blockName "NO-BN-2D-JBTSI-DRIVMASKIN-SPORVEKSEL"
		description	"DRIVMASKIN FOR SPORVEKSEL"
		r1 (getLargeLanternRadius)
		r2 (* 0.5 r1)
	)
	(drawCircle layer_Zero r1 _noWipeout_)
	(drawCircle layer_Zero r2 _noWipeout_)
	; no hatch
	(addDescriptionBelowOrigo description r1)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun DERAILER-MACHINE ( / blockName description r1 r2 )
	(setq 
		blockName "NO-BN-2D-JBTSI-DRIVMASKIN-SPORSPERRE"
		description	"DRIVMASKIN FOR SPORSPERRE"
		r1 (getLargeLanternRadius)
		r2 (* 0.5 r1)
	)
	(drawCircle layer_Zero r1 _noWipeout_)
	(drawCircle layer_Zero r2 _noWipeout_)
	; no hatch
	(addDescriptionBelowOrigo description r1)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun LOCAL-CONTROL-PANEL ( / blockName description r1 r2 )
	(setq 
		blockName	"NO-BN-2D-JBTSI-LOKALSTILLER"
		description	"LOKALOMSTILLER FOR SPORVEKSEL OG SPORSPERRE"
		r1 (getLargeLanternRadius)
		r2 (* 0.5 r1)
	)
	(drawCircle layer_Zero r1 _noWipeout_)
	(drawCircle layer_Zero r2 _noWipeout_)
	(drawHatch _dense_)
	(addDescriptionBelowOrigo description r1)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)
