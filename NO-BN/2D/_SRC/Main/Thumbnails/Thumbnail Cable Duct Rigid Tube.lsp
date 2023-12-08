;=========================================================================================================================
;
; Thumbnail Cable Duct Rigid Tube.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail for concrete-encased semi-flexible tubes (alignment objects)
; Norw.: 'Rørpakke'

(defun THUMBNAIL-CABLE-DUCT-RIGID-TUBE ( / )
	(THUMBNAIL-CABLE-DUCT-RIGID-TUBE)
)



(defun THUMBNAIL-CABLE-DUCT-RIGID-TUBE ( / blockName r1 r2 r3 r4 p1 p2 p3 p4 p5 )
	(setq blockName (strcat _RC_ thumbnailInfix "-KABELFOERING-PRESSROER"				))
	(setq 
		; Rigid outer Ø160 tube:
		r1 0.160	; outer diameter outer tube
		r2 0.144	; inner diameter outer tube
		r3 0.050	; outer diameter inner tube 
		r4 0.044	; inner diameter inner tube
		
		; Five inner tubes placed in a pentagon facing down:
		p1 '( 0.052  0.072)
		p2 '( 0.084 -0.027)
		p3 '( 0.000 -0.089)
		p4 '(-0.084 -0.027)
		p5 '(-0.052  0.072)
	)

	; Outer rigid tube
	(DrawCircle layDef_Zero r1 _noWipeout_)
	(DrawCircle layDef_Zero r2 _noWipeout_)

; Careful - sometimes the VLIDE debugger will crash after using a hatch command - I have no idea why...	
;	(DrawHatchAtPoint (* _tenth_ _denseHatch_) (list 0 (HalfOf (+ r1 r2))) _angleZero_ _zero_)

	; Inner flexible tubes
	(DrawCircleAtPoint layDef_Zero p1 r3 _noWipeout_)		(DrawCircleAtPoint layDef_Zero p1 r4 _noWipeout_)
	(DrawCircleAtPoint layDef_Zero p2 r3 _noWipeout_)		(DrawCircleAtPoint layDef_Zero p2 r4 _noWipeout_)
	(DrawCircleAtPoint layDef_Zero p3 r3 _noWipeout_)		(DrawCircleAtPoint layDef_Zero p3 r4 _noWipeout_)
	(DrawCircleAtPoint layDef_Zero p4 r3 _noWipeout_)		(DrawCircleAtPoint layDef_Zero p4 r4 _noWipeout_)
	(DrawCircleAtPoint layDef_Zero p5 r3 _noWipeout_)		(DrawCircleAtPoint layDef_Zero p5 r4 _noWipeout_)
	
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
