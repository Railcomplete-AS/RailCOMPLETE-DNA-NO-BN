;=========================================================================================================================
;
; Skilt Tredjeperson 138 mot vei.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Level crossing (board mounted on road signal)

; For debugging:
; (SKILT-TREDJEPERSON-138-MOT-VEI) (SKILT-TREDJEPERSON-138-MOT-VEI-FLERE-SPOR)

(defun SKILT-TREDJEPERSON-138-MOT-VEI ( / blockName description x
		x1 x2 x3 x4 x5 x6
		y1 y2 y3 y4 y5 y6
		p11 p12 p13 p14 p15 p16
		p21 p22 p23 p24 p25 p26
		p31 p32 p33 p34 p35 p36
		p41 p42 p43 p44 p45 p46
	)
	; Road signal 138 - ; Level crossing (single railway track)
	;
	;   23     3 
	; 24/*25 2/*\
	;   \*/\ /\*/4  Arms: Rotated CW/CCW 30 decimal degrees
	;  25\  1  /5   Colors: Middle white (empty), outer red ("red" hatch), equal length parts red-white-red
	; 26=36   6=46
	;  35/  .  \45   , = 31 = 41 before translation up
	; 34/\ / \ /\44
	;   \*32 42*/      
	;   33     43
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKP-SKILT-TREDJEPERSON-138-MOT-VEI"
		description "TRAFIKKSKILT 138 MOT VEI FORAN JERNBANE PLANOVERGANG"
		x 9.0
		; Arms in all four quadrants when centered about origin:
		x1 (* 0.000 x)  y1 (* 0.064 x)
		x2 (* 0.117 x)  y2 (* 0.131 x)
		x3 (* 0.405 x)  y3 (* 0.298 x)
		x4 (* 0.461 x)  y4 (* 0.202 x)
		x5 (* 0.172 x)  y5 (* 0.035 x)
		x6 (* 0.111 x)  y6 (* 0.000 x)
		p11 (list (+ x1) (+ y1))    p21 (list (- x1) (+ y1))    p31 (list (- x1) (- y1))    p41 (list (+ x1) (- y1))
		p12 (list (+ x2) (+ y2))    p22 (list (- x2) (+ y2))    p32 (list (- x2) (- y2))    p42 (list (+ x2) (- y2))
		p13 (list (+ x3) (+ y3))    p23 (list (- x3) (+ y3))    p33 (list (- x3) (- y3))    p43 (list (+ x3) (- y3))
		p14 (list (+ x4) (+ y4))    p24 (list (- x4) (+ y4))    p34 (list (- x4) (- y4))    p44 (list (+ x4) (- y4))
		p15 (list (+ x5) (+ y5))    p25 (list (- x5) (+ y5))    p35 (list (- x5) (- y5))    p45 (list (+ x5) (- y5))
		p16 (list (+ x6) (+ y6))    p26 (list (- x6) (+ y6))    p36 (list (- x6) (- y6))    p46 (list (+ x6) (- y6))
	)
	; Upper 'X' arms:
	(command _POLYLINE_ p11 p13 p14 p16  p46 p44 p43 p41  p31 p33 p34 p36  p26 p24 p23 p21 _closedPolyline_) ; Contour
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _keepWipeoutSource_)	
	(command _POLYLINE_ p12 p13 p14 p15 _closedPolyline_)	(DrawHatch _solidHatch_)
	(command _POLYLINE_ p22 p23 p24 p25 _closedPolyline_)	(DrawHatch _solidHatch_)
	(command _POLYLINE_ p32 p33 p34 p35 _closedPolyline_)	(DrawHatch _solidHatch_)
	(command _POLYLINE_ p42 p43 p44 p45 _closedPolyline_)	(DrawHatch _solidHatch_)
	(MoveUp y1)
	; Epilog
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
  
  
  
(defun SKILT-TREDJEPERSON-138-MOT-VEI-FLERE-SPOR ( / blockName description x
		x1 x2 x3 x4 x5 x6
		y1 y2 y3 y4 y5 y6
		p11 p12 p13 p14 p15 p16
		p21 p22 p23 p24 p25 p26
		p31 p32 p33 p34 p35 p36
		p41 p42 p43 p44 p45 p46
		x1a x2a x3a x4a x5a x6a
		y1a y2a y3a y4a y5a y6a
		p51 p52 p53 p54 p55 p56
		p61 p62 p63 p64 p65 p66
	)
	; Road signal 138 - ; Level crossing (multiple railway tracks)
	;
	;   23     3 
	; 24/*25 2/*\
	;   \*/\ /\*/4  Arms: Rotated CW/CCW 30 decimal degrees
	;  25\  1  /5   Colors: Middle white (empty), outer red ("red" hatch), equal length parts red-white-red
	; 26=36   6=46
	;  35/  .  \45   , = 31 = 41 before translation up
	; 34/\ / \ /\44
	;   \*32 42*/      
	;   33     43
	;
	;     66=56 
	;     /   \ 
	;  65/61=51\55
	; 64/\ / \ /\54
	;   \*62 52*/
	;    63   53
	;
	(setq 
		blockName "NO-BN-2D-JBTSK_SKP-SKILT-TREDJEPERSON-138-MOT-VEI-FLERE-SPOR"
		description "TRAFIKKSKILT 138 MOT VEI FORAN JERNBANE PLANOVERGANG MED FLERE SPOR"
		x 9.0
		; Upper cross arms in all four quadrants when centered about origin:
		x1 (* 0.000 x)  y1 (* 0.064 x)
		x2 (* 0.117 x)  y2 (* 0.131 x)
		x3 (* 0.405 x)  y3 (* 0.298 x)
		x4 (* 0.461 x)  y4 (* 0.202 x)
		x5 (* 0.172 x)  y5 (* 0.035 x)
		x6 (* 0.111 x)  y6 (* 0.000 x)
		p11 (list (+ x1) (+ y1))    p21 (list (- x1) (+ y1))    p31 (list (- x1) (- y1))    p41 (list (+ x1) (- y1))
		p12 (list (+ x2) (+ y2))    p22 (list (- x2) (+ y2))    p32 (list (- x2) (- y2))    p42 (list (+ x2) (- y2))
		p13 (list (+ x3) (+ y3))    p23 (list (- x3) (+ y3))    p33 (list (- x3) (- y3))    p43 (list (+ x3) (- y3))
		p14 (list (+ x4) (+ y4))    p24 (list (- x4) (+ y4))    p34 (list (- x4) (- y4))    p44 (list (+ x4) (- y4))
		p15 (list (+ x5) (+ y5))    p25 (list (- x5) (+ y5))    p35 (list (- x5) (- y5))    p45 (list (+ x5) (- y5))
		p16 (list (+ x6) (+ y6))    p26 (list (- x6) (+ y6))    p36 (list (- x6) (- y6))    p46 (list (+ x6) (- y6))
		; Lower 'V' arms (after shifting up the upper arms)
		x1a (* 0.000 x)  y1a (* -0.111 x)
		x2a (* 0.138 x)  y2a (* -0.191 x)
		x3a (* 0.350 x)  y3a (* -0.313 x)
		x4a (* 0.294 x)  y4a (* -0.409 x)
		x5a (* 0.082 x)  y5a (* -0.287 x)
		x6a (* 0.000 x)  y6a (* -0.239 x)
		p51 (list (+ x1a) y1a)    p61 (list (- x1a) y1a)
		p52 (list (+ x2a) y2a)    p62 (list (- x2a) y2a)
		p53 (list (+ x3a) y3a)    p63 (list (- x3a) y3a)
		p54 (list (+ x4a) y4a)    p64 (list (- x4a) y4a)
		p55 (list (+ x5a) y5a)    p65 (list (- x5a) y5a)
		p56 (list (+ x6a) y6a)    p66 (list (- x6a) y6a)
	)
	; Upper 'X' arms:
	(command _POLYLINE_ p11 p13 p14 p16  p46 p44 p43 p41  p31 p33 p34 p36  p26 p24 p23 p21 _closedPolyline_) ; Contour
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _keepWipeoutSource_)	
	(command _POLYLINE_ p12 p13 p14 p15 _closedPolyline_)	(DrawHatch _solidHatch_)
	(command _POLYLINE_ p22 p23 p24 p25 _closedPolyline_)	(DrawHatch _solidHatch_)
	(command _POLYLINE_ p32 p33 p34 p35 _closedPolyline_)	(DrawHatch _solidHatch_)
	(command _POLYLINE_ p42 p43 p44 p45 _closedPolyline_)	(DrawHatch _solidHatch_)
	(MoveUp y1)
	; Lower reversed 'V':
	(command _POLYLINE_ p51 p53 p54 p56   p66 p64 p63 p61 _closedPolyline_) ; Contour
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _keepWipeoutSource_)	
	(command _POLYLINE_ p52 p53 p54 p55 _closedPolyline_)	(DrawHatch _solidHatch_)
	(command _POLYLINE_ p62 p63 p64 p65 _closedPolyline_)	(DrawHatch _solidHatch_)
	(MoveUp (- y6a))
	; Epilog
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
