;=========================================================================================================================
;
; Cabinets and cable distribution boxes.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved. 
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Cabinets and cable distribution boxes for signaling
; Let doors be drawn towards the lower part of each 2D symbol - towards the observer.

(defun C:CABINETS-AND-CABLE-DISTRIBUTION-BOXES ( / )

	(APPARATSKAP-STORT "DOBBEL")
	(APPARATSKAP-STORT "VHENGSLET")
	(APPARATSKAP-STORT "HHENGSLET")
	(APPARATSKAP-LITE-PAA-STOLPE "VHENGSLET")
	(APPARATSKAP-LITE-PAA-STOLPE "HHENGSLET")
	(NOEKKELSKAP "VHENGSLET")
	(NOEKKELSKAP "HHENGSLET")
	(SVEIVSKAP "VHENGSLET")
	(SVEIVSKAP "HHENGSLET")
	(KABELBOKS "VHENGSLET")
	(KABELBOKS "HHENGSLET")
	(S-LAAS)
)



(defun APPARATSKAP-STORT ( door / blockName description gText x y gx gy )
	; Object:		Large interlocking / ATC cabinet.
	; Usage:		Cable distribution cabinet for interlocking or ATC.
	; Mounting:		To be mounted on a suitable foundation.
	; Symbol:		Schematic: A rectangular box with diagonals. Geo: Same.
	; Insertion:	Shown as '.' in the illustration below.
	;
	;             TL-------------TR
	;             |  \        /   |
	;             |       .       |  ; . = origo
	;             |  /        \   |
	;       hinge BL-----BC------BR lock ; left-hinged large door is shown. Can be left / right / double door.
	; \            \\            /  
	;   \           \\         /
	;     \----------\\------/ 
	;                 open
    (setq 
		blockName (strcat "NO-BN-2D-JBTSI-SKAP-APPARATSKAP-STORT-" door)
		description (strcat "APPARATSKAP, STORT, " door " D" _uOE_ "R")
		x	9.0		; schematic width
		y	4.5		; schematic depth
		gx	1.200	; geo width
		gy	0.500	; geo depth
		gText "AS"
	)
	; Schematic symbol
	(drawBox layer_Zero x y _noWipeout_)
	(drawStAndrewCross layer_Zero x y)
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ y) description)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols
	(drawBox layer_Zero gx gy _noWipeout_)
	; (no cross)
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ gy) description)
	(addTextAtPos layer_Zero (* 0.7 gy) _origo_ gText) ; Text height is 0.7 of total box height
	(cond 
		((= door "DOBBEL")
			(drawLeftDoor layer_Cabinet_ReservedSpaceForDoors (posBL gx gy) (posBC gx gy))
			(drawRightDoor layer_Cabinet_ReservedSpaceForDoors (posBR gx gy) (posBC gx gy))
		)
		((= door "VHENGSLET")
			(drawLeftDoor layer_Cabinet_ReservedSpaceForDoors (posBL gx gy) (posBR gx gy))
		)
		((= door "HHENGSLET")
			(drawRightDoor layer_Cabinet_ReservedSpaceForDoors (posBR gx gy) (posBL gx gy))
		)
	)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics 2.0 blockName) ; Double of metric size when drawn in 1:500 scale, real metric size in 1:250 scale
)



(defun APPARATSKAP-LITE-PAA-STOLPE ( door / blockName description gText x y gx gy )
	; Object:		Small interlocking / ATC cabinet.
	; Usage:		Cable distribution cabinet for interlocking or ATC.
	; Mounting:		To be mounted on a pole or wall, typically being alone on its pole (no controlled object there).
	; Symbol:		Schematic: A square box with diagonals. Geo: Same.
	; Insertion:	Shown as '.' in the illustration below.
	;
	;     TL---.---TR
	;     |  \   /  |
	;     |   "AS"  |   
	;     |  /   \  |
	;     BL-------BR
	;       \     // 
	;        \   //      /
	;         \-//------/
	;
    (setq 
		blockName (strcat "NO-BN-2D-JBTSI-SKAP-APPARATSKAP-LITE-" door)
		description (strcat "Lite apparatskap, paa stolpe, " door)
		x	4.5		; schematic width
		y	4.5		; schematic depth
		gx	0.500	; geo width TODO: 2020-08-07 CLFEY Check geo dimensions
		gy	0.320	; geo depth
		gText "AS"
	)
	; Schematic symbol
	(drawBox layer_Zero x y _noWipeout_)
	(drawStAndrewCross layer_Zero x y)
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ y) description)
	(moveDown (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols
	(drawBox layer_Zero gx gy _noWipeout_)
	; (no cross)
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ gy) description)
	(addTextAtPos layer_Zero (* 0.7 gy) _origo_ gText) ; Text height is 0.7 of total box height
	(cond 
		((= door "VHENGSLET")(drawLeftDoor layer_Cabinet_ReservedSpaceForDoors (posBL gx gy) (posBR gx gy)))
		((= door "HHENGSLET") (drawRightDoor layer_Cabinet_ReservedSpaceForDoors (posBR gx gy) (posBL gx gy)))
	)
	(moveDown (halfOf gy))
	(createGeoBlockInAllPaperScalesFromCurrentGraphics 2.0 blockName) ; Double of metric size when drawn in 1:500 scale, real metric size in 1:250 scale
)



(defun NOEKKELSKAP ( door / blockName description gText x y gx gy )
	; Object:		Work-area keylock cabinet
	; Usage:		Holds one key for blocking a work area.
	; Mounting:		To be mounted on a pole or wall.
	; Symbol:		Schematic: A square box with diagonals. Geo: A square box with the letter 'NS' depicted inside.
	; Insertion:	Shown as '.' in the illustration below.
	;
	;     TL---.---TR
	;     |  \   /  |
	;     |   "NS"  |   
	;     |  /   \  |
	;     BL-------BR
	;       \     // 
	;        \   //      /
	;         \-//------/
	;
    (setq 
		blockName (strcat "NO-BN-2D-JBTSI-SKAP-NOEKKELSKAP-" door)
		description (strcat "N\U+00F8kkelskap paa stolpe, " door)
		x	4.5		; schematic width
		y	4.5		; schematic depth
		gx	0.500	; geo width TODO: 2020-08-07 CLFEY Check geo dimensions
		gy	0.320	; geo depth
		gText "NS"
	)
	; Schematic symbol
	(drawBox layer_Zero x y _noWipeout_)
	(drawStAndrewCross layer_Zero x y)
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ y) description)
	(moveDown (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols
	(drawBox layer_Zero gx gy _noWipeout_)
	; (no cross)
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ gy) description)
	(addTextAtPos layer_Zero (* 0.7 gy) _origo_ gText) ; Text height is 0.7 of total box height
	(cond 
		((= door "VHENGSLET")(drawLeftDoor layer_Cabinet_ReservedSpaceForDoors (posBL gx gy) (posBR gx gy)))
		((= door "HHENGSLET") (drawRightDoor layer_Cabinet_ReservedSpaceForDoors (posBR gx gy) (posBL gx gy)))
	)
	(moveDown (halfOf gy))
	(createGeoBlockInAllPaperScalesFromCurrentGraphics 2.0 blockName) ; Double of metric size when drawn in 1:500 scale, real metric size in 1:250 scale
)



(defun SVEIVSKAP ( door / blockName description gText x y gx gy )
	; Object:		Crank cabinet
	; Usage:		Holds one ore more cranks for manual throwing of points with compatible point machines.
	; Mounting:		To be mounted on a pole or wall.
	; Symbol:		Schematic: A square box with diagonals. Geo: A square box with a crank depicted inside.
	; Insertion:	Shown as '.' in the illustration below.
	;
	;     TL---.---TR
	;     |  \   /  |
	;     |   "SV"  |   
	;     |  /   \  |
	;     BL-------BR
	;       \     // 
	;        \   //      /
	;         \-//------/
	;
    (setq 
		blockName (strcat "NO-BN-2D-JBTSI-SKAP-SVEIVSKAP-" door)
		description (strcat "Sveivskap paa stolpe, " door)
		x	4.5		; schematic width
		y	4.5		; schematic depth
		gx	0.500	; geo width TODO: 2020-08-07 CLFEY Check geo dimensions
		gy	0.320	; geo depth
		gText "SV"
	)
	; Schematic symbol
	(drawBox layer_Zero x y _noWipeout_)
	(drawStAndrewCross layer_Zero x y)
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ y) description)
	(moveDown (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols
	(drawBox layer_Zero gx gy _noWipeout_)
	; (no cross)
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ gy) description)
	(addTextAtPos layer_Zero (* 0.7 gy) _origo_ gText) ; Text height is 0.7 of total box height
	(cond 
		((= door "VHENGSLET")(drawLeftDoor layer_Cabinet_ReservedSpaceForDoors (posBL gx gy) (posBR gx gy)))
		((= door "HHENGSLET") (drawRightDoor layer_Cabinet_ReservedSpaceForDoors (posBR gx gy) (posBL gx gy)))
	)
	(moveDown (halfOf gy))
	(createGeoBlockInAllPaperScalesFromCurrentGraphics 2.0 blockName) ; Double of metric size when drawn in 1:500 scale, real metric size in 1:250 scale
)



(defun KABELBOKS ( door / blockName description gText x y gx gy )
	; Object:		Small cable box
	; Usage:		Connection box between object cable and the object's internal wiring. 
	; Mounting:		To be mounted on a pole below an object (signal or other).
	; Symbol:		A square box without diagonals (NB! This is not an official symbol, since schematic drawings regard this box as included in its object).
	; Insertion:	Shown as '.' in the illustration below.
	; 
	;     TL---.---TR
	;     |         |
	;     |   "KB"  |   
	;     |         |
	;     BL-------BR
	;       \     // 
	;        \   //      /
	;         \-//------/
	;
	(setq
		blockName (strcat "NO-BN-2D-JBTSI-SKAP-KABELBOKS-" door)
		description (strcat "Kabelboks paa annet objekts stolpe, " door)
		x	3.0		; schematic width
		y	3.0		; schematic depth
		gx	0.280	; geo width
		gy	0.190	; geo depth
		gText "KB"
	)
	; Schematic symbol
	(drawBox layer_Zero x y _noWipeout_)
	; (no cross)
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ y) description)
	(moveDown (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols
	(drawBox layer_Zero gx gy _noWipeout_)
	; (no cross)
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ gy) description)
	(addTextAtPos layer_Zero (* 0.7 gy) _origo_ gText) ; Text height is 0.7 of total box height
	(cond 
		((= door "VHENGSLET")(drawLeftDoor layer_Cabinet_ReservedSpaceForDoors (posBL gx gy) (posBR gx gy)))
		((= door "HHENGSLET") (drawRightDoor layer_Cabinet_ReservedSpaceForDoors (posBR gx gy) (posBL gx gy)))
	)
	(moveDown (halfOf gy))
	(createGeoBlockInAllPaperScalesFromCurrentGraphics 2.0 blockName) ; Double of metric size when drawn in 1:500 scale, real metric size in 1:250 scale
)



(defun S-LAAS ( / blockName description gText description x y gx gy )
	; Object:		S-lock
	; Reference:	S.000537, ...
	; Mounting:		To be mounted on a specialized pole (S.018812) or wall. The pole will feature a baseplate for one or baseplate for two S-locks.
	; Symbol:		A baseplate with a hatched box below.
	; Insertion:	Shown as '.' in the illustration below.
	;
	; 11---TL------.------TR---12  
	;      | / / / / / / / |
	;      BL-------------BR
	;       \           // 
	;        \         //   /
	;         \-------//---/
	;
	(setq 
		blockName "NO-BN-2D-JBTSI-SAMLELAAS"
		description "S-laas for montasje paa stolpe med bakplate"
		x	6.0		; schematic width
		y	2.0		; schematic depth
		b	9.0		; baseplate width
		gx	0.160	; geo width
		gy	0.160	; geo depth
		gb	0.500	; geo baseplate width (exaggerated)
	)
	; Schematic symbol
	(drawLine layer_Zero (posTL b y) (posTR b y)) 
	(drawBox layer_Zero x y _noWipeout_)
	(drawHatch 0.25) ; 0.25 - don't change, there shall be exactly 7 lines in this hatch
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ y) description)
	(moveDown (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols
	(drawLine layer_Zero (posTL gb gy) (posTR gb gy)) 
	(drawBox layer_Zero gx gy _noWipeout_)
	(drawHatch _sparseHatch_)
	(addTextAtPos layer_Description _descriptionTextHeight_ (posBelow _descriptionTextHeight_ gy) description)
	(addTextAtPos layer_Zero (* 0.7 gy) _origo_ gText) ; Text height is 0.7 of total box height
	(cond 
		((= door "VHENGSLET")(drawLeftDoor layer_Cabinet_ReservedSpaceForDoors (posBL gx gy) (posBR gx gy)))
		((= door "HHENGSLET") (drawRightDoor layer_Cabinet_ReservedSpaceForDoors (posBR gx gy) (posBL gx gy)))
	)
	(moveDown (halfOf gy))
	(createGeoBlockInAllPaperScalesFromCurrentGraphics 2.0 blockName) ; Double of metric size when drawn in 1:500 scale, real metric size in 1:250 scale
)
