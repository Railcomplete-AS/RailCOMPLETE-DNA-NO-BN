;=========================================================================================================================
;
; 65.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Unpowered section boards

; For debugging:
; (65A) (65B) (65C) (65D) (65E) (65F) (65G-1) (65G-2) (65G-3)
; (E65H) (E65J) (E65K) (E65L) (E65M) (E65N)

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
		blockName "NO-BN-2D-JBTEH_BKT-SKILT-KJOERENDE-SIGNAL-65A-JORDETSEKSJON"
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
	(DrawHatchAtPoint _solidHatch_ (list 0 (* 0.1 y)) _angleZero_ _offsetZero_)
	(AddDescriptionBelowOrigin description 0)
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
		blockName "NO-BN-2D-JBTEH_BKT-SKILT-KJOERENDE-SIGNAL-65B-KONTAKTLEDNING"
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
	(DrawCircleAtPoint layDef_Zero p0 r _noWipeout_)
	(AddDescriptionBelowOrigin description 0)
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
		blockName "NO-BN-2D-JBTEH_BKT-SKILT-KJOERENDE-SIGNAL-65C-UTKOBLING"
		description (strcat "SKILT SIGNAL 65C UTKOBLING FORAN D" _uOSLASH_ "DSEKSJON")
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
	(DrawCircleAtPoint layDef_Zero p0 r _noWipeout_)
	(AddDescriptionBelowOrigin description 0)
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
		blockName "NO-BN-2D-JBTEH_BKT-SKILT-KJOERENDE-SIGNAL-65D-INNKOBLING"
		description (strcat "SKILT SIGNAL 65D INNKOBLING ETTER D" _uOSLASH_ "DSEKSJON")
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
	(AddDescriptionBelowOrigin description 0)
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
		blockName "NO-BN-2D-JBTEH_BKT-SKILT-KJOERENDE-SIGNAL-65E-SENKSTROEMAVTAKER"
		description (strcat "SKILT SIGNAL 65E SENKING AV STR" _uOSLASH_ "MAVTAKER")
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
	(AddDescriptionBelowOrigin description 0)
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
		blockName "NO-BN-2D-JBTEH_BKT-SKILT-KJOERENDE-SIGNAL-65F-HEVSTROEMAVTAKER"
		description (strcat "SKILT SIGNAL 65F HEVING AV STR" _uOSLASH_ "MAVTAKER")
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
	(AddDescriptionBelowOrigin description 0)
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
		blockName "NO-BN-2D-JBTEH_BKT-SKILT-KJOERENDE-SIGNAL-65G-1-STOPPELLOK"
		description "SKILT SIGNAL 65G-1 STOPP FOR ELEKTRISK LOKOMOTIV"
		x 5.25
		y 5.25
		r1 2.0
		r2 2.2
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(DrawFatCircle r1 r2)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
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
		blockName "NO-BN-2D-JBTEH_BKT-SKILT-KJOERENDE-SIGNAL-65G-2-STOPPELLOK-VSIDE"
		description (strcat "SKILT SIGNAL 65G-2 65G MED H" _uOSLASH_ "YREPIL")
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
	(DrawFatRightArrow x y2)
	(MoveUp (HalfOf y2))
	(DrawCircleAtPoint layDef_Zero p1 r1 _noWipeout_)
	(DrawCircleAtPoint layDef_Zero p1 r2 _noWipeout_)
	(DrawHatchAtPoint _solidHatch_ p2 _angleZero_ _offsetZero_)
	(AddDescriptionBelowOrigin description 0)
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
		blockName "NO-BN-2D-JBTEH_BKT-SKILT-KJOERENDE-SIGNAL-65G-3-STOPPELLOK-HSIDE"
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
	(DrawCircleAtPoint layDef_Zero p1 r1 _noWipeout_)
	(DrawCircleAtPoint layDef_Zero p1 r2 _noWipeout_)
	(DrawHatchAtPoint _solidHatch_ p2 _angleZero_ _offsetZero_)
	(AddDescriptionBelowOrigin description _offsetZero_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun E65H ( / blockName description H W B a c e f g)
	;
	;  TL-------TR
	;  |    ____ |  
	;  |   |____||
	;  | ____    |
	;  ||____|   |
	;  BL---.---BR 
	; 
	(setq 
		blockName "NO-BN-2D-JBTEH_BKT-SKILT-KJOERENDE-SIGNAL-E65H-WARNING-LOWERING-OF-PANTOGRAPH"
		description "SKILT SIGNAL E65H WARNING LOWERING OF PANTOGRAPH"
		H 4.00
		W 4.00
		B 0.16
		a 0.40
		c 0.88
		e 1.37
		f 1.99
		g 0.62
	)
	(setq
		offsetX (+ (HalfOf W) B e (HalfOf f))
		offsetY (HalfOf (+ c g))
	)
	(DrawBox layDef_Zero W H layDef_BoardOrPole_Wipeout) 
	(DrawBox layDef_Zero (- W B) (- H B) _noWipeout_)
	(DrawBoxAtPoint layDef_Zero (list offsetX offsetY) f g _noWipeout_)	
	(DrawBoxAtPoint layDef_Zero (list (- offsetX) (- offsetY)) f g _noWipeout_)
	(MoveUp (HalfOf H))
	(AddDescriptionBelowOrigin description _offsetZero_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun E65J ( / blockName description H W B b2 c)
	;
	;  TL-------TR
	;  | _______ |
	;  ||_______||
	;  |         |
	;  BL---.---BR 
	; 
	(setq 
		blockName "NO-BN-2D-JBTEH_BKT-SKILT-KJOERENDE-SIGNAL-E65J-LOWERING-OF-PANTOGRAPH"
		description "SKILT SIGNAL E65J LOWERING OF PANTOGRAPH"
		H 4.00
		W 4.00
		B 0.16
		b2 3.04
		c 0.88

	)
	(DrawBox layDef_Zero W H layDef_BoardOrPole_Wipeout) 
	(DrawBox layDef_Zero (- W B) (- H B) _noWipeout_)
	(DrawBox layDef_Zero b2 c _noWipeout_)
	(MoveUp (HalfOf H))
	(AddDescriptionBelowOrigin description _offsetZero_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun E65K ( / blockName description H W B b2 c)
	;
	;  TL-------TR
	;  |    _    |
	;  |   | |   |
	;  |   | |   |
	;  |   |_|   |
	;  BL---.---BR 
	; 
	(setq 
		blockName "NO-BN-2D-JBTEH_BKT-SKILT-KJOERENDE-SIGNAL-E65K-RAISING-OF-PANTOGRAPH"
		description "SKILT SIGNAL E65K RAISING OF PANTOGRAPH"
		H 4.00
		W 4.00
		B 0.16
		b2 3.04
		c 0.88

	)
	(DrawBox layDef_Zero W H layDef_BoardOrPole_Wipeout) 
	(DrawBox layDef_Zero (- W B) (- H B) _noWipeout_)
	(DrawBox layDef_Zero c b2 _noWipeout_)
	(MoveUp (HalfOf H))
	(AddDescriptionBelowOrigin description _offsetZero_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun E65L ( / blockName description H W B g i m)
	;
	;  TL-------TR
	;  |  _   _  |
	;  | | | | | |
	;  | | | | | |
	;  | |_| |_| |
	;  BL---.---BR 
	;
	(setq 
		blockName "NO-BN-2D-JBTEH_BKT-SKILT-KJOERENDE-SIGNAL-E65L-WARNING-OF-DEAD-SECTION"
		description "SKILT SIGNAL E65L WARNING OF DEAD SECTION"
		H 4.00
		W 4.00
		B 0.16
		g 0.62
		i 1.52
		m 1.58
	)
	(setq
		offsetX (HalfOf (+ i g))
	)
	(DrawBox layDef_Zero W H layDef_BoardOrPole_Wipeout) 
	(DrawBox layDef_Zero (- W B) (- H B) _noWipeout_)
	(DrawBoxAtPoint layDef_Zero (list offsetX 0) g m _noWipeout_)	
	(DrawBoxAtPoint layDef_Zero (list (- offsetX) 0) g m _noWipeout_)
	(MoveUp (HalfOf H))
	(AddDescriptionBelowOrigin description _offsetZero_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun E65M ( / blockName description H W B g i j k)
	;
	;  TL-------TR
	;  |  _   _  |
	;  | | | | | |
	;  | |_| |_| |
	;  | _______ |
	;  ||_______||
	;  BL---.---BR 
	; 
	(setq 
		blockName "NO-BN-2D-JBTEH_BKT-SKILT-KJOERENDE-SIGNAL-E65M-DISCONNECTION-IN-DEAD-SECTION"
		description "SKILT SIGNAL E65M DISCONNECTION IN DEAD SECTION"
		H 4.00
		W 4.00
		B 0.16
		g 0.62
		i 1.52
		j 0.34
		k 1.76
	)
	(setq
		offsetX (HalfOf (+ i g))
		offSetY1 (- (HalfOf W) B j (HalfOf k))
		offSetY2 (+ (HalfOf (- W)) B j (HalfOf g))
	)
	(DrawBox layDef_Zero W H layDef_BoardOrPole_Wipeout) 
	(DrawBox layDef_Zero (- W B) (- H B) _noWipeout_)
	(DrawBoxAtPoint layDef_Zero (list offsetX offSetY1) g k _noWipeout_)	
	(DrawBoxAtPoint layDef_Zero (list (- offsetX) offSetY1) g k _noWipeout_)
	(DrawBoxAtPoint layDef_Zero (list 0 offSetY2) (+ (* 2 g) i) g _noWipeout_)
	(MoveUp (HalfOf H))
	(AddDescriptionBelowOrigin description _offsetZero_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun E65N ( / blockName description H W B g i j n p x1 x2 x3 x4 y1 y2 y3 p1 p2 p3 p4 p5 p6 p7 p8)
	;
	;  TL-------TR
	;  | _     _ |
	;  || |   | ||
	;  || |___| ||
	;  ||_______||
	;  BL---.---BR 
	; 
	(setq 
		blockName "NO-BN-2D-JBTEH_BKT-SKILT-KJOERENDE-SIGNAL-E65N-CONNECTION-AFTER-DEAD-SECTION"
		description "SKILT SIGNAL E65N CONNECTION AFTER DEAD SECTION"
		H 4.00
		W 4.00
		B 0.16
		g 0.62
		i 1.52
		j 0.34
		n 0.46
		p 2.38
	)
	(setq
		x1 (+ (- (HalfOf W)) B n )
		x2 (+ (- (HalfOf W)) B n g)
		x3 (+ (- (HalfOf W)) B n g i)
		x4 (+ (- (HalfOf W)) B n g i g)
		y1 (+ B j )
		y2 (+ B j g)
		y3 (+ B j g p)
	)
	(setq
		p1 (list x1 y3)
		p2 (list x1 y1)
		p3 (list x4 y1)
		p4 (list x4 y3)
		p5 (list x3 y3)
		p6 (list x3 y2)
		p7 (list x2 y2)
		p8 (list x2 y3)
	)

	(DrawBox layDef_Zero W H layDef_BoardOrPole_Wipeout) 
	(DrawBox layDef_Zero (- W B) (- H B) nil)
	(MoveUp (HalfOf H))
	(command _POLYLINE_ p1 p2 p3 p4 p5 p6 p7 p8 _closedPolyline_)
	(AddDescriptionBelowOrigin description _offsetZero_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
