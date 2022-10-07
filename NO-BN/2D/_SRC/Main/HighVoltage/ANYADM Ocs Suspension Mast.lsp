;=========================================================================================================================
;
; ANYADM OCS Suspension Mast.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Suspension mast - from yoke, under bridge or from tunnel ceiling

(defun ANYADM-OCS-SUSPENSION-MAST ( / )
	; Implemented for all administrations:

	; Implemented only for some administrations:
	(cond 
		((= _ADM_ _XXGL_)
		)
		((= _ADM_ _NOBN_)
			(TraceLevel3 "NOBN-OCS-SUSPENSION-MAST-UNDER-BRIDGE-OR-TUNNEL-CEILING")	(NOBN-OCS-SUSPENSION-MAST-UNDER-BRIDGE-OR-TUNNEL-CEILING)
			(TraceLevel3 "NOBN-OCS-SUSPENSION-MAST-UNDER-YOKE")						(NOBN-OCS-SUSPENSION-MAST-UNDER-YOKE)
			(TraceLevel3 "NOBN-OCS-SUSPENSION-MAST-TYPE-CARIBONI")					(NOBN-OCS-SUSPENSION-MAST-TYPE-CARIBONI)
		)
		((= _ADM_ _FRSR_)
			; TODO - model FRSR objects, do not use NOBN objects:
			(TraceLevel3 "NOBN-OCS-SUSPENSION-MAST-UNDER-BRIDGE-OR-TUNNEL-CEILING")	(NOBN-OCS-SUSPENSION-MAST-UNDER-BRIDGE-OR-TUNNEL-CEILING)
			(TraceLevel3 "NOBN-OCS-SUSPENSION-MAST-UNDER-YOKE")						(NOBN-OCS-SUSPENSION-MAST-UNDER-YOKE)
			(TraceLevel3 "NOBN-OCS-SUSPENSION-MAST-TYPE-CARIBONI")					(NOBN-OCS-SUSPENSION-MAST-TYPE-CARIBONI)
		)
		((= _ADM_ _DEDB_)
		)
		((= _ADM_ _JPTX_)
			; TODO - model JPTX objects, do not use NOBN objects:
			(TraceLevel3 "NOBN-OCS-SUSPENSION-MAST-UNDER-BRIDGE-OR-TUNNEL-CEILING")	(NOBN-OCS-SUSPENSION-MAST-UNDER-BRIDGE-OR-TUNNEL-CEILING)
			(TraceLevel3 "NOBN-OCS-SUSPENSION-MAST-UNDER-YOKE")						(NOBN-OCS-SUSPENSION-MAST-UNDER-YOKE)
		)
	)
)



(defun NOBN-OCS-SUSPENSION-MAST-UNDER-BRIDGE-OR-TUNNEL-CEILING ( / blockName description x y )
  (setq 
		blockName "NO-BN-2D-JBTEH_MAS-HENGEMAST-FOR-TUNNEL-OG-BRU"
		description	"HENGEMAST FOR TUNNEL OG BRU"
		x 1.5
		y 2.0
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawStAndrewCross layDef_Zero x y)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-OCS-SUSPENSION-MAST-TYPE-CARIBONI ( / blockName description x y offset )
	(setq
		blockName "NO-BN-2D-JBTEH_MAS-HENGEMAST-CARIBONI"
		description	"HENGEMAST I TUNNELHVELV FOR CARIBONI UTLIGGER"
		x 1.0
		y 2.0
		offset 0.075 ; offset pga kort utligger og masten må plasseres nær sporet
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawStAndrewCross layDef_Zero x y)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(MoveUp (- y offset))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-OCS-SUSPENSION-MAST-UNDER-YOKE ( / blockName description r )
	(setq 
		blockName	(strcat "NO-BN-2D-JBTEH_MAS-HENGEMAST-I-"_uARING_ "K")
		description (strcat "HENGEMAST I " _uARING_ "K")
		r 0.525
	)
	(DrawCircle layDef_Zero r _noWipeout_)
	(DrawHatch _denseHatch_)
	(AddDescriptionBelowOrigin description r)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

