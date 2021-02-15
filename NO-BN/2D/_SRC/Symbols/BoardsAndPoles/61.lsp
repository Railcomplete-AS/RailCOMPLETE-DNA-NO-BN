;=========================================================================================================================
;
; 61.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Distance (as a symbol) to next signal, marking the remaining distance with meter precision

; For debugging:
; (61A) (61B) (61C) (E61) 

(defun 61A ( / blockName description x y TL TR BL BR p11 p12 p13 p14 )
	; Distance board 1
	; 800m, legacy distance for conventional signaling
	;
	; TL----TR
	; |      |
	; p11\   |
	; p13\\p12
	; |   \p14 
	; |      |
	; |      |
	; |      |
	; |      |
	; |      |
	; |      |
	; |      |
	; |      |
	; BL-.--BR
	;
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-61A-AVSTAND1"
		description "SKILT SIGNAL 61A AVSTANDSSKILT 1"
    	x	4.5
		y	10.5
		TL	(posTL x y)
		TR	(posTR x y)
		BL	(posBL x y)
		BR	(posBR x y)
		p11 (list (* -0.5 x) (*  0.454 y))
		p12 (list (*  0.5 x) (*  0.167 y))
		p13 (list (* -0.5 x) (*  0.310 y))
		p14 (list (*  0.5 x) (*  0.024 y))
	)
	(drawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(command _POLYLINE_ p11 p12 TR TL _closedPolyline_)
	(drawHatch _blackHatch_) 
	(command _POLYLINE_ p13 p14 BR BL _closedPolyline_)
	(drawHatch _blackHatch_) 
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 61B ( / blockName description x y TL TR BL BR p11 p12 p13 p14 p21 p22 p23 p24 )
	; Distance board 2
	; 1000m, current distance for conventional signaling
	;
	; TL----TR
	; |      |
	; p11\   |
	; p13\\p12
	; |   \p14 
	; p21\   |
	; p23\\p22
	; |   \p24 
	; |      |
	; |      |
	; |      |
	; |      |
	; |      |
	; BL-.--BR
	;
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-61B-AVSTAND2"
		description "SKILT SIGNAL 61B AVSTANDSSKILT 2"
    	x	4.5
		y	10.5
		TL	(posTL x y)
		TR	(posTR x y)
		BL	(posBL x y)
		BR	(posBR x y)
		p11 (list (* -0.5 x) (*  0.454 y))
		p12 (list (*  0.5 x) (*  0.167 y))
		p13 (list (* -0.5 x) (*  0.310 y))
		p14 (list (*  0.5 x) (*  0.024 y))
		p21 (list (* -0.5 x) (*  0.241 y))
		p22 (list (*  0.5 x) (* -0.045 y))
		p23 (list (* -0.5 x) (*  0.098 y))
		p24 (list (*  0.5 x) (* -0.188 y))
	)
	(drawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(command _POLYLINE_ p11 p12 TR TL _closedPolyline_)
	(drawHatch _blackHatch_) 
	(command _POLYLINE_ p13 p14 p22 p21 _closedPolyline_)
	(drawHatch _blackHatch_) 
	(command _POLYLINE_ p23 p24 BR BL _closedPolyline_)
	(drawHatch _blackHatch_) 
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun 61C ( / blockName description x y TL TR BL BR p11 p12 p13 p14 p21 p22 p23 p24 p31 p32 p33 p34 )
	; Distance board 3
	; 250m, current distance for final braking down, useful for goods trains
	;
	; TL----TR
	; |      |
	; p11\   |
	; p13\\p12
	; |   \p14 
	; p21\   |
	; p23\\p22
	; |   \p24 
	; p31\   |
	; p33\\p32
	; |   \p34 
	; |      |
	; |      |
	; BL-.--BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-61C-AVSTAND3"
		description "SKILT SIGNAL 61C AVSTANDSSKILT 3"
     	x	4.5
		y	10.5
		TL	(posTL x y)
		TR	(posTR x y)
		BL	(posBL x y)
		BR	(posBR x y)
		p11 (list (* -0.5 x) (*  0.454 y))
		p12 (list (*  0.5 x) (*  0.167 y))
		p13 (list (* -0.5 x) (*  0.310 y))
		p14 (list (*  0.5 x) (*  0.024 y))
		p21 (list (* -0.5 x) (*  0.241 y))
		p22 (list (*  0.5 x) (* -0.045 y))
		p23 (list (* -0.5 x) (*  0.098 y))
		p24 (list (*  0.5 x) (* -0.188 y))
		p31 (list (* -0.5 x) (*  0.027 y))
		p32 (list (*  0.5 x) (* -0.259 y))
		p33 (list (* -0.5 x) (* -0.116 y))
		p34 (list (*  0.5 x) (* -0.402 y))
	)
	(drawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(command _POLYLINE_ p11 p12 TR TL _closedPolyline_)
	(drawHatch _blackHatch_) 
	(command _POLYLINE_ p13 p14 p22 p21 _closedPolyline_)
	(drawHatch _blackHatch_) 
	(command _POLYLINE_ p23 p24 p32 p31 _closedPolyline_)
	(drawHatch _blackHatch_) 
	(command _POLYLINE_ p33 p34 BR BL _closedPolyline_)
	(drawHatch _blackHatch_) 
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun E61 ( / blockName description x y TL TR BL BR p11 p12 p13 p14 )
	;
	; TL----TR
	; |      |
	; p11\   |
	; p13\\p12
	; |   \p14 
	; |      |
	; |      |
	; |      |
	; |      |
	; |      |
	; |      |
	; |      |
	; |      |
	; BL-.--BR
	;
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-E61-AVSTAND4"
		description "SKILT E61 AVSTANDSSKILT 4"
    	x	4.5
		y	10.5
		TL	(posTL x y)
		TR	(posTR x y)
		BL	(posBL x y)
		BR	(posBR x y)
		p11 (list (* -0.5 x) (*  0.4 y))
		p12 (list (*  0.5 x) (*  0.2 y))
		p13 (list (* -0.5 x) (*  0.3 y))
		p14 (list (*  0.5 x) (*  0.1 y))
	)
	(drawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(command _POLYLINE_ p11 p12 TR TL _closedPolyline_)
	(drawHatch _blueHatch_)  
	(command _POLYLINE_ p11 p12 p14 p13 _closedPolyline_)
	(drawHatch _yellowHatch_) 
	(command _POLYLINE_ p13 p14 BR BL _closedPolyline_)
	(drawHatch _blueHatch_) 
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
