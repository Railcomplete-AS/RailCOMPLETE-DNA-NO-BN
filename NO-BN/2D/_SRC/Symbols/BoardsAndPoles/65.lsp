;=========================================================================================================================
;
; 65.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Unpowered section boards

; For debugging:
; (65A) (65B) (65C) (65D) (65E) (65F) (65G-1) (65G-2) (65G-3)

(defun 65A ( / blockName description x y p11 p12 p21 p22 )
	; Unpowered section, dead-end track - "Jordet seksjon"
	;
	; TL-----------TR
	; |             |
	; |             |
	; | p11-----p12 |
	; | |         | | Horizontal white bar on black background
	; | p21-----p22 |
	; |             |
	; |             |
	; BL-----.-----BR 
	; 
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-65A-JORDETSEKSJON"
		description "SKILT SIGNAL 65A JORDET SEKSJON"
		x 6.0
		y 6.0
		p11 (list (* -0.45 x) (*  0.67 y))
		p12 (list (*  0.45 x) (*  0.67 y))
		p21 (list (* -0.45 x) (*  0.33 y))
		p22 (list (*  0.45 x) (*  0.33 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(MoveUp (HalfOf y))
	(SetLayer layDef_Zero)
	(command _POLYLINE_ p11 p12 p22 p21 _closedPolyline_)
	(DrawHatchFromPoint _solidHatch_ (list 0 (* 0.1 y)) _angleZero_ _offsetZero_)
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 65B ( / blockName description x y r p0 p11 p12 p13 p14 p21 p22 p23 p24 )
	; Warning before unpowered section
	;
	;  TL-------------------TR
	;  |                     |
	;  | p11-p12     p21-p22 |
	;  | |     |     |     | |
	;  | |     |  _  |     | | ; Black bars on white background
	;  | |     | (0) |     | | ; Circle at p0 represents optional white lamp
	;  | |     |     |     | |
	;  | |     |     |     | |
	;  | p13-p14     p23-p24 | 
	;  |                     |
	;  |                     |
	;  |                     |
	;  BL---------.---------BR 
	; 
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-65B-KONTAKTLEDNING"
		description "SKILT SIGNAL 65B VARSELSIGNAL FOR KONTAKTLEDNINGSSIGNAL"
		x 5.25
		y 5.25
		r (* 0.14 x)
		p0 (list (*  0.00 x) (* 0.625 y) )
		p11 (list (* -0.40 x) (* 0.90 y))
		p12 (list (* -0.25 x) (* 0.90 y))
		p13 (list (* -0.40 x) (* 0.35 y))
		p14 (list (* -0.25 x) (* 0.35 y))
		p21 (list (*  0.25 x) (* 0.90 y))
		p22 (list (*  0.40 x) (* 0.90 y))
		p23 (list (*  0.25 x) (* 0.35 y))
		p24 (list (*  0.40 x) (* 0.35 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(MoveUp (HalfOf y))
	(SetLayer layDef_Zero)
	(command _POLYLINE_ p11 p12 p14 p13 _closedPolyline_)
	(DrawHatch _solidHatch_)
	(command _POLYLINE_ p21 p22 p24 p23 _closedPolyline_)
	(DrawHatch _solidHatch_)
	(DrawCircleAtPos layDef_Zero p0 r _noWipeout_)
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 65C ( / blockName description x y r p0 p11 p12 p13 p14 p21 p22 p23 p24 p31 p32 p33 p34 )
	; Disconnection before unpowered section
	; 
	;  TL-------------------TR
	;  |                     |
	;  | p11-p12     p21-p22 |
	;  | |     |     |     | |
	;  | |     |  _  |     | | ; Black bars on white background
	;  | |     | (0) |     | | ; Circle at p0 represents optional white lamp
	;  | |     |     |     | |
	;  | |     |     |     | |
	;  | p13-p14     p23-p24 | 
	;  |                     |
	;  | p31-------------p32 |
	;  | |                 | |
	;  | p33-------------p34 |
	;  |                     |
	;  BL---------.---------BR 
	; 
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-65C-UTKOBLING"
		description (strcat "SKILT SIGNAL 65C UTKOBLING FORAN D" _uOE_ "DSEKSJON")
		x 5.25
		y 5.25
		r	(* 0.14 x)
		p0 (list (*  0.00 x) (* 0.625 y) )
		p11 (list (* -0.40 x) (* 0.90 y))
		p12 (list (* -0.25 x) (* 0.90 y))
		p13 (list (* -0.40 x) (* 0.35 y))
		p14 (list (* -0.25 x) (* 0.35 y))
		p21 (list (*  0.25 x) (* 0.90 y))
		p22 (list (*  0.40 x) (* 0.90 y))
		p23 (list (*  0.25 x) (* 0.35 y))
		p24 (list (*  0.40 x) (* 0.35 y))
		p31 (list (* -0.40 x) (* 0.25 y))
		p32 (list (*  0.40 x) (* 0.25 y))
		p33 (list (* -0.40 x) (* 0.10 y))
		p34 (list (*  0.40 x) (* 0.10 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(MoveUp (HalfOf y))
	(SetLayer layDef_Zero)
	(command _POLYLINE_ p11 p12 p14 p13 _closedPolyline_)
	(DrawHatch _solidHatch_)
	(command _POLYLINE_ p21 p22 p24 p23 _closedPolyline_)
	(DrawHatch _solidHatch_)
	(command _POLYLINE_ p31 p32 p34 p33 _closedPolyline_)
	(DrawHatch _solidHatch_)
	(DrawCircleAtPos layDef_Zero p0 r _noWipeout_)
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 65D ( / blockName description x y p11 p12 p13 p21 p22 p23 p33 p34 )
	; Re-connection after unpowered section
	;
	;  TL-------------------TR
	;  |                     |
	;  | p11-p12     p21-p22 |
	;  | |     |     |     | |
	;  | |     |     |     | | ; Black bars on white background
	;  | |     |     |     | |
	;  | |     |     |     | |
	;  | |     |     |     | |
	;  | |   p13-----p23   | | 
	;  | |                 | |
	;  | |                 | |
	;  | p33-------------p34 |
	;  |                     |
	;  BL---------.---------BR 
	; 
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-65D-INNKOBLING"
		description (strcat "SKILT SIGNAL 65D INNKOBLING ETTER D" _uOE_ "DSEKSJON")
		x 5.25
		y 5.25
		p11 (list (* -0.40 x) (* 0.90 y))
		p12 (list (* -0.25 x) (* 0.90 y))
		p13 (list (* -0.25 x) (* 0.25 y))
		p21 (list (*  0.25 x) (* 0.90 y))
		p22 (list (*  0.40 x) (* 0.90 y))
		p23 (list (*  0.25 x) (* 0.25 y))
		p33 (list (* -0.40 x) (* 0.10 y))
		p34 (list (*  0.40 x) (* 0.10 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(MoveUp (HalfOf y))
	(SetLayer layDef_Zero)
	(command _POLYLINE_ p11 p12 p13 p23 p21 p22 p34 p33 _closedPolyline_)
	(DrawHatch _solidHatch_)
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)


 
(defun 65E ( / blockName description x y p11 p12 p21 p22 )
	; Lower pantograph before unpowered section
	;
	;  TL-----------TR
	;  |             |
	;  |             |
	;  | p11-----p12 |
	;  | |         | | Horizontal black bar on white background
	;  | p21-----p22 |
	;  |             |
	;  |             |
	;  BL-----.-----BR 
	; 
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-65E-SENKSTROEMAVTAKER"
		description (strcat "SKILT SIGNAL 65E SENKING AV STR" _uOE_ "MAVTAKER")
		x 5.25
		y 5.25
		p11 (list (* -0.45 x) (*  0.60 y))
		p12 (list (*  0.45 x) (*  0.60 y))
		p21 (list (* -0.45 x) (*  0.40 y))
		p22 (list (*  0.45 x) (*  0.40 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(MoveUp (HalfOf y))
	(SetLayer layDef_Zero)
	(command _POLYLINE_ p11 p12 p22 p21 _closedPolyline_)
	(DrawHatch _solidHatch_)
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 65F ( / blockName description x y p11 p12 p21 p22 )
	; Raise pantograph after unpowered section
	;
	;  TL---------TR
	;  |  p11-p12  |
	;  |  |     |  |
	;  |  |     |  |
	;  |  |     |  | Vertical black bar on white background
	;  |  |     |  |
	;  |  |     |  |
	;  |  p21-p22  |
	;  BL----.----BR 
	; 
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-65F-HEVSTROEMAVTAKER"
		description (strcat "SKILT SIGNAL 65F HEVING AV STR" _uOE_ "MAVTAKER")
		x 5.25
		y 5.25
		p11 (list (* -0.10 x) (*  0.95 y))
		p12 (list (*  0.10 x) (*  0.95 y))
		p21 (list (* -0.10 x) (*  0.05 y))
		p22 (list (*  0.10 x) (*  0.05 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(MoveUp (HalfOf y))
	(SetLayer layDef_Zero)
	(command _POLYLINE_ p11 p12 p22 p21 _closedPolyline_)
	(DrawHatch _solidHatch_)
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 65G-1 ( / blockName description x y r1 r2 )
	; Unpowered section (grounded section), dead-end track - "Jordet seksjon"
	;
	;  TL-------TR
	;  |   ___   |  
	;  |  /   \  |
	;  | (     ) | Fat black circle on white background
	;  |  \___/  |
	;  |         |
	;  BL---.---BR 
	; 
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-65G-1-STOPPELLOK"
		description "SKILT SIGNAL 65G-1 STOPP FOR ELEKTRISK LOKOMOTIV"
		x 5.25
		y 5.25
		r1 2.0
		r2 2.2
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(DrawFatCircle r1 r2)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 65G-2 ( / blockName description x y r1 r2 y2 dy p1 p2 )
	; Unpowered section (grounded section), track to the right - "Jordet seksjon mot høyre"
	;
	;  TL-------TR
	;  |   ___   |  
	;  |  /   \  |
	;  | (  1  ) | Fat black circle on white background
	;  |  \___/  |
	;  |         |
	;  |  ====>  |
	;  BL---.---BR 
	; 
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-65G-2-STOPPELLOK-VSIDE"
		description (strcat "SKILT SIGNAL 65G-2 65G MED H" _uOE_ "YREPIL")
		x 5.25
		y 5.25
		r1 2.0
		r2 2.2
		y2 3.0 ; Arrow-box
		dy 0.5 ; Circle-box and arrow-box overlap
		p1 (list 0 (+ (* 0.5 y) y2 (- dy))) 					; centre of circles
		p2 (list 0 (+ (* 0.5 y) y2 (- dy) (* 0.5 (+ r1 r2))))	; between circles (hatch seed)
	)
	(DrawBox layDef_Zero x (+ y (- y2 dy)) layDef_BoardOrPole_Wipeout) ; Overlapping boxes
	(MoveUp (HalfOf (- y dy)))
	(drawFatRightArrow x y2)
	(MoveUp (HalfOf y2))
	(DrawCircleAtPos layDef_Zero p1 r1 _noWipeout_)
	(DrawCircleAtPos layDef_Zero p1 r2 _noWipeout_)
	(DrawHatchFromPoint _solidHatch_ p2 _angleZero_ _offsetZero_)
	(AddDescriptionBelowOrigo description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 65G-3 ( / blockName description x y r1 r2 y2 dy p1 p2 )
	; Unpowered section (grounded section), track to the left - "Jordet seksjon mot venstre"
	;
	;  TL-------TR
	;  |   ___   |  
	;  |  /   \  |
	;  | (  1  ) | Fat black circle on white background
	;  |  \___/  |
	;  |         |
	;  |  <====  |
	;  BL---.---BR 
	; 
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-65G-3-STOPPELLOK-HSIDE"
		description "SKILT SIGNAL 65G-3 65G MED VENSTREPIL"
		x 5.25
		y 5.25
		r1 2.0
		r2 2.2
		y2 3.0 ; Arrow-box
		dy 0.5 ; Circle-box and arrow-box overlap
		p1 (list 0 (+ (* 0.5 y) y2 (- dy))) 					; centre of circles
		p2 (list 0 (+ (* 0.5 y) y2 (- dy) (* 0.5 (+ r1 r2))))	; between circles (hatch seed)
	)
	(DrawBox layDef_Zero x (+ y (- y2 dy)) layDef_BoardOrPole_Wipeout) ; Overlapping boxes
	(MoveUp (HalfOf (- y dy)))
	(DrawFatLeftArrow x y2)
	(MoveUp (HalfOf y2))
	(DrawCircleAtPos layDef_Zero p1 r1 _noWipeout_)
	(DrawCircleAtPos layDef_Zero p1 r2 _noWipeout_)
	(DrawHatchFromPoint _solidHatch_ p2 _angleZero_ _offsetZero_)
	(AddDescriptionBelowOrigo description _offsetZero_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
