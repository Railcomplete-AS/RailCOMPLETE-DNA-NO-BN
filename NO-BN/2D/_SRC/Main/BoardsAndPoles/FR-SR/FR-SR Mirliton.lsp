;=========================================================================================================================
;
; FR-SR Mirliton.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; Distance (as a symbol) to following signal or mirliton



(defun FRSR-MIRLITONS ( / )
	(FRSR-MIRLITON-1)
	(FRSR-MIRLITON-2)
	(FRSR-MIRLITON-3)
	(FRSR-MIRLITON-1-BAS)
	(FRSR-MIRLITON-2-BAS)
	(FRSR-MIRLITON-3-BAS)
)



(defun FRSR-MIRLITON-1 ( / blockName description x y p11 p12 p13 p14 )
	; Distance board 1
	; 100m to signal
	;
	; +------+
	; |      |
	; |      |
	; |      |
	; |      |
	; |      |
	; |   /12| 
	; |11//13|
	; |14/   |
	; |      |
	; |      |
	; |      |
	; |      |
	; |      |
	; +--.---+
	;
	(setq
		blockName (strcat _SIG_ "PAN-" "PANCARTE-CONDUCTEURS-MIRLITON-1")
		description "PANCARTE MIRLITON 1"
    	x	4.5
		y	10.5
		p11 (list (* -0.417 x) (* -0.061 y))
		p12 (list (*  0.417 x) (*  0.173 y))
		p13 (list (*  0.417 x) (*  0.100 y))
		p14 (list (* -0.417 x) (* -0.135 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(command _POLYLINE_ p11 p12 p13 p14 _closedPolyline_)
	(DrawHatch _solidHatch_) 
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun FRSR-MIRLITON-2 ( / blockName description x y p11 p12 p13 p14 p21 p22 p23 p24 )
	; Distance board 2
	; 200m to signal
	;
	; +------+
	; |      |
	; |      |
	; |   /12| 
	; |11//13|
	; |14/   |
	; |      |
	; |      |
	; |      |
	; |   /22|
	; |21//23|
	; |24/   |
	; |      |
	; |      |
	; +--.---+
	;
	(setq
		blockName (strcat _SIG_ "PAN-" "PANCARTE-CONDUCTEURS-MIRLITON-2")
		description "PANCARTE MIRLITON 2"
    	x	4.5
		y	10.5
		p11 (list (* -0.417 x) (*  0.140 y))
		p12 (list (*  0.417 x) (*  0.379 y))
		p13 (list (*  0.417 x) (*  0.306 y))
		p14 (list (* -0.417 x) (*  0.067 y))
		
		p21 (list (* -0.417 x) (* -0.226 y))
		p22 (list (*  0.417 x) (*  0.013 y))
		p23 (list (*  0.417 x) (* -0.061 y))
		p24 (list (* -0.417 x) (* -0.299 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(command _POLYLINE_ p11 p12 p13 p14 _closedPolyline_)
	(DrawHatch _solidHatch_) 
	(command _POLYLINE_ p21 p22 p23 p24 _closedPolyline_)
	(DrawHatch _solidHatch_) 
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun FRSR-MIRLITON-3 ( / blockName description x y p11 p12 p13 p14 p21 p22 p23 p24 p31 p32 p33 p34 )
	; Distance board 3
	; 300m to signal
	;
	; +------+
	; |      |
	; |   /12| 
	; |11//13|
	; |14/   |
	; |      |
	; |   /22|
	; |21//23|
	; |24/   |
	; |      |
	; |   /32|
	; |31//33|
	; |34/   |
	; |      |
	; +--.---+
	;
	(setq 
		blockName (strcat _SIG_ "PAN-" "PANCARTE-CONDUCTEURS-MIRLITON-3")
		description "PANCARTE MIRLITON 3"
     	x	4.5
		y	10.5
		p11 (list (* -0.417 x) (*  0.197 y))
		p12 (list (*  0.417 x) (*  0.440 y))
		p13 (list (*  0.417 x) (*  0.367 y))
		p14 (list (* -0.417 x) (*  0.124 y))

		p21 (list (* -0.417 x) (* -0.061 y))
		p22 (list (*  0.417 x) (*  0.182 y))
		p23 (list (*  0.417 x) (*  0.108 y))
		p24 (list (* -0.417 x) (* -0.135 y))

		p31 (list (* -0.417 x) (* -0.319 y))
		p32 (list (*  0.417 x) (* -0.076 y))
		p33 (list (*  0.417 x) (* -0.150 y))
		p34 (list (* -0.417 x) (* -0.393 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(command _POLYLINE_ p11 p12 p13 p14 _closedPolyline_)
	(DrawHatch _solidHatch_) 
	(command _POLYLINE_ p21 p22 p23 p24 _closedPolyline_)
	(DrawHatch _solidHatch_) 
	(command _POLYLINE_ p31 p32 p33 p34 _closedPolyline_)
	(DrawHatch _solidHatch_) 
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun FRSR-MIRLITON-1-BAS ( / blockName description x y p11 p12 p13 p14 )
	; Distance board 1, low
	; 100m to signal
	;
	; +--------------+
	; |              | 
	; |              | 
	; |       ____12 | 
	; |11____/ ____13|
	; | 14____/      |
	; |              | 
	; |              | 
	; +------.-------+
	;
	(setq
		blockName (strcat _SIG_ "PAN-" "PANCARTE-CONDUCTEURS-MIRLITON-1-BAS")
		description "PANCARTE MIRLITON 1, BAS"
    	x	4.5
		y	4.5
		p11 (list (* -0.333 x) (* -0.231 y))
		p12 (list (*  0.267 x) (*  0.292 y))
		p13 (list (*  0.333 x) (*  0.216 y))
		p14 (list (* -0.267 x) (* -0.307 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(command _POLYLINE_ p11 p12 p13 p14 _closedPolyline_)
	(DrawHatch _solidHatch_) 
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun FRSR-MIRLITON-2-BAS ( / blockName description x y p11 p12 p13 p14 p21 p22 p23 p24 )
	; Distance board 2, low
	; 200m to signal
	;
	; +--------------+
	; |       ____12 | 
	; |11____/ ____13|
	; | 14____/      |
	; |              | 
	; |       ____22 | 
	; |21____/ ____23|
	; | 24____/      |
	; +------.-------+
	;
	(setq
		blockName (strcat _SIG_ "PAN-" "PANCARTE-CONDUCTEURS-MIRLITON-2-BAS")
		description "PANCARTE MIRLITON 2, BAS"
    	x	4.5
		y	4.5
		p11 (list (* -0.400 x) (* -0.110 y))
		p12 (list (*  0.200 x) (*  0.413 y))
		p13 (list (*  0.266 x) (*  0.338 y))
		p14 (list (* -0.334 x) (* -0.186 y))
		
		p21 (list (* -0.259 x) (* -0.343 y))
		p22 (list (*  0.341 x) (*  0.181 y))
		p23 (list (*  0.407 x) (*  0.106 y))
		p24 (list (* -0.193 x) (* -0.419 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(command _POLYLINE_ p11 p12 p13 p14 _closedPolyline_)
	(DrawHatch _solidHatch_) 
	(command _POLYLINE_ p21 p22 p23 p24 _closedPolyline_)
	(DrawHatch _solidHatch_) 
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun FRSR-MIRLITON-3-BAS ( / blockName description x y p11 p12 p13 p14 p21 p22 p23 p24 p31 p32 p33 p34 )
	; Distance board 3, low
	; 300m to signal
	;
	; +--------------+
	; |       ____12 | 
	; |11____/ ____13|
	; | 14____/      |
	; |       ____22 | 
	; |21____/ ____23|
	; | 24____/      |
	; |       ____32 | 
	; |31____/ ____33|
	; | 34____/      |
	; +------.-------+
	;
	(setq 
		blockName (strcat _SIG_ "PAN-" "PANCARTE-CONDUCTEURS-MIRLITON-3-BAS")
		description "PANCARTE MIRLITON 3, BAS"
     	x	4.5
		y	4.5
		p11 (list (* -0.500 x) (*  0.000 y))
		p12 (list (*  0.000 x) (*  0.500 y))
		p13 (list (*  0.071 x) (*  0.4179 y))
		p14 (list (* -0.4179 x) (* -0.071 y))

		p21 (list (* -0.285 x) (* -0.215 y))
		p22 (list (*  0.215 x) (*  0.285 y))
		p23 (list (*  0.285 x) (*  0.215 y))
		p24 (list (* -0.215 x) (* -0.285 y))

		p31 (list (* -0.071 x) (* -0.4179 y))
		p32 (list (*  0.4179 x) (*  0.071 y))
		p33 (list (*  0.500 x) (*  0.000 y))
		p34 (list (*  0.000 x) (* -0.500 y))
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(command _POLYLINE_ p11 p12 p13 p14 _closedPolyline_)
	(DrawHatch _solidHatch_) 
	(command _POLYLINE_ p21 p22 p23 p24 _closedPolyline_)
	(DrawHatch _solidHatch_) 
	(command _POLYLINE_ p31 p32 p33 p34 _closedPolyline_)
	(DrawHatch _solidHatch_) 
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
