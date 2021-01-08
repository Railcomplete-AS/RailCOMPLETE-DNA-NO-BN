;=========================================================================================================================
;
; 61.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Distance (as a symbol) to next signal, marking the remaining distance with meter precision

; For debugging:
; (61A) (61B) (61C) (E61) 

(defun 61A ( / blockName description x y TL TR BL BR pt11 pt12 pt13 pt14 )
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
		pt11 (list (* -0.5 x) (*  0.454 y))
		pt12 (list (*  0.5 x) (*  0.167 y))
		pt13 (list (* -0.5 x) (*  0.310 y))
		pt14 (list (*  0.5 x) (*  0.024 y))
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(command "._PLINE" pt11 pt12 TR TL _closed_)
	(drawHatch _blackHatch_) 
	(command "._PLINE" pt13 pt14 BR BL _closed_)
	(drawHatch _blackHatch_) 
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 61B ( / blockName description x y TL TR BL BR pt11 pt12 pt13 pt14 pt21 pt22 pt23 pt24 )
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
		pt11 (list (* -0.5 x) (*  0.454 y))
		pt12 (list (*  0.5 x) (*  0.167 y))
		pt13 (list (* -0.5 x) (*  0.310 y))
		pt14 (list (*  0.5 x) (*  0.024 y))
		pt21 (list (* -0.5 x) (*  0.241 y))
		pt22 (list (*  0.5 x) (* -0.045 y))
		pt23 (list (* -0.5 x) (*  0.098 y))
		pt24 (list (*  0.5 x) (* -0.188 y))
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(command "._PLINE" pt11 pt12 TR TL _closed_)
	(drawHatch _blackHatch_) 
	(command "._PLINE" pt13 pt14 pt22 pt21 _closed_)
	(drawHatch _blackHatch_) 
	(command "._PLINE" pt23 pt24 BR BL _closed_)
	(drawHatch _blackHatch_) 
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 61C ( / blockName description x y TL TR BL BR pt11 pt12 pt13 pt14 pt21 pt22 pt23 pt24 pt31 pt32 pt33 pt34 )
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
		pt11 (list (* -0.5 x) (*  0.454 y))
		pt12 (list (*  0.5 x) (*  0.167 y))
		pt13 (list (* -0.5 x) (*  0.310 y))
		pt14 (list (*  0.5 x) (*  0.024 y))
		pt21 (list (* -0.5 x) (*  0.241 y))
		pt22 (list (*  0.5 x) (* -0.045 y))
		pt23 (list (* -0.5 x) (*  0.098 y))
		pt24 (list (*  0.5 x) (* -0.188 y))
		pt31 (list (* -0.5 x) (*  0.027 y))
		pt32 (list (*  0.5 x) (* -0.259 y))
		pt33 (list (* -0.5 x) (* -0.116 y))
		pt34 (list (*  0.5 x) (* -0.402 y))
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(command "._PLINE" pt11 pt12 TR TL _closed_)
	(drawHatch _blackHatch_) 
	(command "._PLINE" pt13 pt14 pt22 pt21 _closed_)
	(drawHatch _blackHatch_) 
	(command "._PLINE" pt23 pt24 pt32 pt31 _closed_)
	(drawHatch _blackHatch_) 
	(command "._PLINE" pt33 pt34 BR BL _closed_)
	(drawHatch _blackHatch_) 
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun E61 ( / blockName description x y TL TR BL BR pt11 pt12 pt13 pt14 )
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
		pt11 (list (* -0.5 x) (*  0.4 y))
		pt12 (list (*  0.5 x) (*  0.2 y))
		pt13 (list (* -0.5 x) (*  0.3 y))
		pt14 (list (*  0.5 x) (*  0.1 y))
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(command "._PLINE" pt11 pt12 TR TL _closed_)
	(drawHatch _blueHatch_)  
	(command "._PLINE" pt11 pt12 pt14 pt13 _closed_)
	(drawHatch _yellowHatch_) 
	(command "._PLINE" pt13 pt14 BR BL _closed_)
	(drawHatch _blueHatch_) 
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
