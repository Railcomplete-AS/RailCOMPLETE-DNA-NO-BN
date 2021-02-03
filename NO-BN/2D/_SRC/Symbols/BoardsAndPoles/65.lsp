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

(defun 65A ( / blockName description x y r1 r2 )
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
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(moveUp (halfOf y))
	(setLayer layer_Zero)
	(command _POLYLINE_ p11 p12 p22 p21 _closedPolyline_)
	(drawHatchFromPoint _solidHatch_ (list 0 (* 0.1 y)) 0 0)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 65B ( / blockName description x y )
	; Warning before unpowered section
	;
	;  TL-------------------TR
	;  |                     |
	;  | p11-p12     p21-p22 |
	;  | |     |     |     | |
	;  | |     |  _  |     | | ; Black bars on white background
	;  | |     | (_) |     | | ; Circle at p99 represents optional white lamp
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
		p11 (list (* -0.40 x) (* 0.90 y))
		p12 (list (* -0.25 x) (* 0.90 y))
		p13 (list (* -0.40 x) (* 0.35 y))
		p14 (list (* -0.25 x) (* 0.35 y))
		p21 (list (*  0.25 x) (* 0.90 y))
		p22 (list (*  0.40 x) (* 0.90 y))
		p23 (list (*  0.25 x) (* 0.35 y))
		p24 (list (*  0.40 x) (* 0.35 y))
		p99 (list (*  0.00 x) (* 0.625 y) )
		r	(* 0.14 x)
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(moveUp (halfOf y))
	(setLayer layer_Zero)
	(command _POLYLINE_ p11 p12 p14 p13 _closedPolyline_)
	(drawHatch _solidHatch_)
	(command _POLYLINE_ p21 p22 p24 p23 _closedPolyline_)
	(drawHatch _solidHatch_)
	(drawCircleAtPos layer_Zero r p99 _noWipeout_)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 65C ( / blockName description x y )
	; Disconnection before unpowered section
	; 
	;  TL-------------------TR
	;  |                     |
	;  | p11-p12     p21-p22 |
	;  | |     |     |     | |
	;  | |     |  _  |     | | ; Black bars on white background
	;  | |     | (_) |     | | ; Circle at p99 represents optional white lamp
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
		p99 (list (*  0.00 x) (* 0.625 y) )
		r	(* 0.14 x)
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(moveUp (halfOf y))
	(setLayer layer_Zero)
	(command _POLYLINE_ p11 p12 p14 p13 _closedPolyline_)
	(drawHatch _solidHatch_)
	(command _POLYLINE_ p21 p22 p24 p23 _closedPolyline_)
	(drawHatch _solidHatch_)
	(command _POLYLINE_ p31 p32 p34 p33 _closedPolyline_)
	(drawHatch _solidHatch_)
	(drawCircleAtPos layer_Zero r p99 _noWipeout_)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 65D ( / blockName description x y )
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
	;  | |   p14-----p23   | | 
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
		p14 (list (* -0.25 x) (* 0.25 y))
		p21 (list (*  0.25 x) (* 0.90 y))
		p22 (list (*  0.40 x) (* 0.90 y))
		p23 (list (*  0.25 x) (* 0.25 y))
		p33 (list (* -0.40 x) (* 0.10 y))
		p34 (list (*  0.40 x) (* 0.10 y))
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(moveUp (halfOf y))
	(setLayer layer_Zero)
	(command _POLYLINE_ p11 p12 p14 p23 p21 p22 p34 p33 _closedPolyline_)
	(drawHatch _solidHatch_)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)


 
(defun 65E ( / blockName description x y )
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
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(moveUp (halfOf y))
	(setLayer layer_Zero)
	(command _POLYLINE_ p11 p12 p22 p21 _closedPolyline_)
	(drawHatch _solidHatch_)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 65F ( / blockName description x y )
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
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(moveUp (halfOf y))
	(setLayer layer_Zero)
	(command _POLYLINE_ p11 p12 p22 p21 _closedPolyline_)
	(drawHatch _solidHatch_)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 65G-1 ( / blockName description x y )
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
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(drawFatCircle r1 r2)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 65G-2 ( / blockName description x y r1 r2 y2 )
	; Unpowered section (grounded section), track to the right - "Jordet seksjon mot høyre"
	;
	;  TL-------TR
	;  |   ___   |  
	;  |  /   \  |
	;  | (     ) | Fat black circle on white background
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
		p1 (list 0 (+ (* 0.5 y) y2 (- dy))) ; centre of circles
		p2 (list 0  (+ (* 0.5 y) y2 (- dy) (* 0.5 (+ r1 r2))))
	)
	(drawBox layer_Zero x (+ y (- y2 dy)) layer_BoardOrPole_Wipeout) ; Overlapping boxes
	(moveUp (halfOf (- y dy)))
	(drawFatRightArrow x y2)
	(moveUp (halfOf y2))
	(drawCircleAtPos layer_Zero r1 p1 _noWipeout_)
	(drawCircleAtPos layer_Zero r2 p1 _noWipeout_)
	(drawHatchFromPoint _solidHatch_ p2 0 0)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 65G-3 ( / blockName description x y r1 r2 y2 )
	; Unpowered section (grounded section), track to the left - "Jordet seksjon mot venstre"
	;
	;  TL-------TR
	;  |   ___   |  
	;  |  /   \  |
	;  | (     ) | Fat black circle on white background
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
		p1 (list 0 (+ (* 0.5 y) y2 (- dy))) ; centre of circles
		p2 (list 0  (+ (* 0.5 y) y2 (- dy) (* 0.5 (+ r1 r2))))
	)
	(drawBox layer_Zero x (+ y (- y2 dy)) layer_BoardOrPole_Wipeout) ; Overlapping boxes
	(moveUp (halfOf (- y dy)))
	(drawFatLeftArrow x y2)
	(moveUp (halfOf y2))
	(drawCircleAtPos layer_Zero r1 p1 _noWipeout_)
	(drawCircleAtPos layer_Zero r2 p1 _noWipeout_)
	(drawHatchFromPoint _solidHatch_ p2 0 0)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
