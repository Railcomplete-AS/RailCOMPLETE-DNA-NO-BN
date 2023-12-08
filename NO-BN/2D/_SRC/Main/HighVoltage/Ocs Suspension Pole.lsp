;=========================================================================================================================
;
; OCS Suspension Pole.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Suspension mast - from yoke, under bridge or from tunnel ceiling

(defun OCS-SUSPENSION-POLE ( / )
	; Implemented for all administrations:

	; Specific to this administration:
	(TraceLevel3 "NOBN-OCS-SUSPENSION-POLE-UNDER-BRIDGE-OR-TUNNEL-CEILING")	(NOBN-OCS-SUSPENSION-POLE-UNDER-BRIDGE-OR-TUNNEL-CEILING)
	(TraceLevel3 "NOBN-OCS-SUSPENSION-POLE-UNDER-YOKE")						(NOBN-OCS-SUSPENSION-POLE-UNDER-YOKE)
	(TraceLevel3 "NOBN-OCS-SUSPENSION-POLE-TYPE-CARIBONI")					(NOBN-OCS-SUSPENSION-POLE-TYPE-CARIBONI)
)



(defun NOBN-OCS-SUSPENSION-POLE-UNDER-BRIDGE-OR-TUNNEL-CEILING ( / blockName description x y )
	;
	; +-----+
	; |\   /|
	; | \ / |
	; |  .  |
 	; | / \ |
	; |/   \|
	; +-----+
	;
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



(defun NOBN-OCS-SUSPENSION-POLE-TYPE-CARIBONI ( / blockName description x y offset )
	;
	; +---------+
	; | \     / |
	; |   \ /   |
	; |    .    |
 	; |   / \   |
	; | /     \ |
	; +---------+
	;
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



(defun NOBN-OCS-SUSPENSION-POLE-UNDER-YOKE ( / blockName description r )
	;
	;  (*)		Hatched disc
	;
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

