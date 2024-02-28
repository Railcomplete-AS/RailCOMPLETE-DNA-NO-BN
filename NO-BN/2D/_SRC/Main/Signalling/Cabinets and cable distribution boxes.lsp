;=========================================================================================================================
;
; Cabinets and cable distribution boxes.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503 
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Cabinets and cable distribution boxes for signaling
; Let doors be drawn towards the lower part of each 2D symbol - towards the observer.

(defun CABINETS-AND-DISTRIBUTION-BOXES ( / )
	(NOBN-APPARATSKAP-OG-BOKSER-FOR-SIGNAL)
)



;================== NOBN functions ==================================================================
(defun NOBN-APPARATSKAP-OG-BOKSER-FOR-SIGNAL ( / )

	(TraceLevel3 "APPARATSKAP-STORT")
		(APPARATSKAP-STORT "DOBBEL")
		(APPARATSKAP-STORT "VHENGSLET")
		(APPARATSKAP-STORT "HHENGSLET")
	
	(TraceLevel3 "APPARATSKAP-LITE-PAA-STOLPE")
		(APPARATSKAP-LITE-PAA-STOLPE "VHENGSLET")
		(APPARATSKAP-LITE-PAA-STOLPE "HHENGSLET")
	
	(TraceLevel3 "NOEKKELSKAP")
		(NOEKKELSKAP "VHENGSLET")
		(NOEKKELSKAP "HHENGSLET")
	
	(TraceLevel3 "SVEIVSKAP")
		(SVEIVSKAP "VHENGSLET")
		(SVEIVSKAP "HHENGSLET")
	
	(TraceLevel3 "KABELBOKS")
		(KABELBOKS "VHENGSLET")
		(KABELBOKS "HHENGSLET")
	
	(TraceLevel3 "S-LAAS")
		(S-LAAS)
)



(defun APPARATSKAP-STORT ( door / blockName description x y gx gy gText )
	; Object:		Large interlocking / ATC cabinet.
	; Usage:		Cable distribution cabinet for interlocking or ATC.
	; Mounting:		To be mounted on a suitable foundation.
	; Symbol:		Schematic: A rectangular box with diagonals. Geo: Same.
	; Insertion:	Shown as '.' in the illustration below.
	;
	;             TL-------------TR
	;             |  \  "AS"  /   |
	;             |       .       |
	;             |  /        \   |
	;       hinge BL-----BC------BR lock ; left-hinged large door is shown. Can be left / right / double door.
	; \            \\            /  
	;   \           \\         /
	;     \----------\\------/ 
	;                 open
    (setq 
		blockName	(strcat _SIG_ "APS-" "APPARATSKAP-STORT-" door)
		description (strcat "APPARATSKAP, STORT, " door " D" _uOSLASH_ "R")
		x	9.0		; schematic width
		y	4.5		; schematic depth
		gx	1.200	; geo width
		gy	0.500	; geo depth
		gText "AS"
	)
	; Schematic symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawStAndrewCross layDef_Zero x y)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawBox layDef_Zero gx gy _noWipeout_)
	(DrawStAndrewCross layDef_Zero gx gy)
	(AddTextAtPoint layDef_Zero (* 0.7 gy) _origin_ gText) ; Text height is 0.7 of total box height
	(cond 
		((= door "DOBBEL")
			(DrawLeftDoor layDef_Cabinet_ReservedSpaceForDoors (PointBL gx gy) (PointBC gx gy))
			(DrawRightDoor layDef_Cabinet_ReservedSpaceForDoors (PointBR gx gy) (PointBC gx gy))
		)
		((= door "VHENGSLET")
			(DrawLeftDoor layDef_Cabinet_ReservedSpaceForDoors (PointBL gx gy) (PointBR gx gy))
		)
		((= door "HHENGSLET")
			(DrawRightDoor layDef_Cabinet_ReservedSpaceForDoors (PointBR gx gy) (PointBL gx gy))
		)
	)
	(ScaleAll _four_)
	(AddDescriptionBelowOrigin description (* _two_ gy))
	(CreateAnnotativeBlockFromCurrentGraphics blockName)	
)



(defun APPARATSKAP-LITE-PAA-STOLPE ( door / blockName description x y gx gy gText )
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
		blockName	(strcat _SIG_ "APS-" "APPARATSKAP-LITE-" door)
		description (strcat "LITE APPARATSKAP P" _uARING_ " EGEN STOLPE, " door)
		x	4.5		; schematic width
		y	4.5		; schematic depth
		gx	0.500	; geo width TODO: 2020-08-07 CLFEY Check geo dimensions
		gy	0.320	; geo depth
		gText "AS"
	)
	; Schematic symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawStAndrewCross layDef_Zero x y)
	(MoveDown (HalfOf y))
	(AddDescriptionBelowOrigin description y)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawBox layDef_Zero gx gy _noWipeout_)
	; (no cross)
	(AddTextAtPoint layDef_Zero (* 0.7 gy) _origin_ gText) ; Text height is 0.7 of total box height
	(cond 
		((= door "VHENGSLET")(DrawLeftDoor layDef_Cabinet_ReservedSpaceForDoors (PointBL gx gy) (PointBR gx gy)))
		((= door "HHENGSLET") (DrawRightDoor layDef_Cabinet_ReservedSpaceForDoors (PointBR gx gy) (PointBL gx gy)))
	)
	(MoveDown (HalfOf gy))
	(ScaleAll _four_)
	(AddDescriptionBelowOrigin description (* _four_ gy))
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



(defun NOEKKELSKAP ( door / blockName description x y gx gy gText )
	; Object:		Work-area keylock cabinet
	; Usage:		Holds one key for blocking a work area.
	; Mounting:		To be mounted on a pole or wall.
	; Symbol:		Schematic: A square box with diagonals. Geo: A square box with the letter 'NS' depicted inside.
	; Insertion:	Shown as '.' in the illustration below.
	;
	;     TL---.---TR
	;     |  \   /  |
	;     |  "NS"   |
	;     |  /   \  |
	;     BL-------BR
	;       \     // 
	;        \   //      /
	;         \-//------/
	;
    (setq 
		blockName	(strcat _SIG_ "APS-" "N" _uOSLASH_ "KKELSKAP-" door)
		description (strcat "N" _uOSLASH_ "KKELSKAP P" _uARING_ " EGEN STOLPE, " door)
		x	4.5		; schematic width
		y	4.5		; schematic depth
		gx	0.500	; geo width TODO: 2020-08-07 CLFEY Check geo dimensions
		gy	0.320	; geo depth
		gText "NS"
	)
	; Schematic symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawStAndrewCross layDef_Zero x y)
	(MoveDown (HalfOf y))
	(AddDescriptionBelowOrigin description y)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawBox layDef_Zero gx gy _noWipeout_)
	; (no cross)
	(AddTextAtPoint layDef_Zero (* 0.7 gy) _origin_ gText) ; Text height is 0.7 of total box height
	(cond 
		((= door "VHENGSLET")(DrawLeftDoor layDef_Cabinet_ReservedSpaceForDoors (PointBL gx gy) (PointBR gx gy)))
		((= door "HHENGSLET") (DrawRightDoor layDef_Cabinet_ReservedSpaceForDoors (PointBR gx gy) (PointBL gx gy)))
	)
	(MoveDown (HalfOf gy))
	(ScaleAll _four_)
	(AddDescriptionBelowOrigin description (* _four_ gy))
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



(defun SVEIVSKAP ( door / blockName description x y gx gy gText )
	; Object:		Crank cabinet
	; Usage:		Holds one ore more cranks for manual throwing of points with compatible point machines.
	; Mounting:		To be mounted on a pole or wall.
	; Symbol:		Schematic: A square box with diagonals. Geo: A square box with a crank depicted inside.
	; Insertion:	Shown as '.' in the illustration below.
	;
	;     TL---.---TR
	;     |  \   /  |
	;     |  "SV"   |
	;     |  /   \  |
	;     BL-------BR
	;       \     // 
	;        \   //      /
	;         \-//------/
	;
    (setq 
		blockName	(strcat _SIG_ "APS-" "SVEIVSKAP-" door)
		description (strcat "SVEIVSKAP P" _uARING_ " EGEN STOLPE, " door)
		x	4.5		; schematic width
		y	4.5		; schematic depth
		gx	0.500	; geo width TODO: 2020-08-07 CLFEY Check geo dimensions
		gy	0.320	; geo depth
		gText "SV"
	)
	; Schematic symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawStAndrewCross layDef_Zero x y)
	(MoveDown (HalfOf y))
	(AddDescriptionBelowOrigin description y)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawBox layDef_Zero gx gy _noWipeout_)
	; (no cross)
	(AddTextAtPoint layDef_Zero (* 0.7 gy) _origin_ gText) ; Text height is 0.7 of total box height
	(cond 
		((= door "VHENGSLET")(DrawLeftDoor layDef_Cabinet_ReservedSpaceForDoors (PointBL gx gy) (PointBR gx gy)))
		((= door "HHENGSLET") (DrawRightDoor layDef_Cabinet_ReservedSpaceForDoors (PointBR gx gy) (PointBL gx gy)))
	)
	(MoveDown (HalfOf gy))
	(ScaleAll _four_)
	(AddDescriptionBelowOrigin description (* _four_ gy))
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



(defun KABELBOKS ( door / blockName description x y gx gy gText )
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
		blockName 	(strcat _SIG_ "APS-" "KABELBOKS-" door)
		description (strcat "KABELBOKS P" _uARING_ " ANNET OBJEKTS STOLPE, " door)
		x	3.0		; schematic width
		y	3.0		; schematic depth
		gx	0.280	; geo width
		gy	0.190	; geo depth
		gText "KB"
	)
	; Schematic symbol
	(DrawBox layDef_Zero x y _noWipeout_)
	; (no cross)
	(MoveDown (HalfOf y))
	(AddDescriptionBelowOrigin description y)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawBox layDef_Zero gx gy _noWipeout_)
	; (no cross)
	(AddTextAtPoint layDef_Zero (* 0.7 gy) _origin_ gText) ; Text height is 0.7 of total box height
	(cond 
		((= door "VHENGSLET")(DrawLeftDoor layDef_Cabinet_ReservedSpaceForDoors (PointBL gx gy) (PointBR gx gy)))
		((= door "HHENGSLET") (DrawRightDoor layDef_Cabinet_ReservedSpaceForDoors (PointBR gx gy) (PointBL gx gy)))
	)
	(MoveDown (HalfOf gy))
	(ScaleAll _four_)
	(AddDescriptionBelowOrigin description (* _four_ gy))
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



(defun S-LAAS ( / blockName description x y b gx gy gb )
	; Object:		S-lock
	; Reference:	S.000537, ...
	; Mounting:		To be mounted on a specialized pole (S.018812) or wall. The pole will feature a baseplate for one or baseplate for two S-locks.
	; Symbol:		A baseplate with a hatched box below.
	; Insertion:	Shown as '.' in the illustration below.
	;
	; 11---TL------.------TR---12  
	;      |       *       |
	;      BL-------------BR
	;       \           // 
	;        \         //   /
	;         \-------//---/
	;
	(setq 
		blockName	(strcat _SIG_ "SAM-" "SAMLELAAS")
		description (strcat "S-L" _uARING_" S FOR MONTASJE P" _uARING_ " STOLPE MED BAKPLATE")
		x	6.0		; schematic width
		y	2.0		; schematic depth
		b	9.0		; baseplate width
		gx	0.160	; geo width
		gy	0.160	; geo depth
		gb	0.500	; geo baseplate width (exaggerated)
	)
	; Schematic symbol
	(DrawLine layDef_Zero (PointTL b y) (PointTR b y)) 
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawHatch 0.25) ; 0.25 - don't change, there shall be exactly 7 lines in this hatch
	(MoveDown (HalfOf y))
	(AddDescriptionBelowOrigin description y)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawLine layDef_Zero (PointTL gb gy) (PointTR gb gy)) 
	(DrawBox layDef_Zero gx gy _noWipeout_)
	(DrawHatch _solidHatch_)
	(AddTextAtPoint layDef_Zero (* 0.7 gy) _origin_ gText) ; Text height is 0.7 of total box height
	(cond 
		((= door "VHENGSLET")(DrawLeftDoor layDef_Cabinet_ReservedSpaceForDoors (PointBL gx gy) (PointBR gx gy)))
		((= door "HHENGSLET") (DrawRightDoor layDef_Cabinet_ReservedSpaceForDoors (PointBR gx gy) (PointBL gx gy)))
	)
	(MoveDown (HalfOf gy))
	(ScaleAll _four_)
	(AddDescriptionBelowOrigin description (* _four_ gy))
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)
