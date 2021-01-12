;=========================================================================================================================
;
; Skilt Balise.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Balise presence in track along platform
; (with no space for the usual pole or board)

; For debugging:
; (SKILT_BALISE)

(defun Skilt_Balise ( / blockName x y s_top s_bot s_x)
	; Board with text 'BALISE' located e.g. on the platform edge toward track - where no technial pole 64E can be placed.
	;
	; TL----1--2--3--4--5--6
	; |    /  /  /  /  /  /| 
	; |   7--8  9-10 11-12 |
	; |  B  A  L  I  S  E  |
	; | 13-14 15-16 17-18  |
	; |/  /  /  /  /  /    | 
	; 19-20-21-22.23-24---BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-VEDLIKEHOLD-BALISE"
		description "SKILT BALISE LANGS PLATTFORM"
		x 8.0
		y 4.5
		x0	(* -0.500 x)
		x1	(* -0.375 x)
		x2	(* -0.250 x)
		x3	(* -0.125 x)
		x4	(* -0.000 x)
		x5	(*  0.125 x)
		x6	(*  0.250 x)
		x7	(*  0.375 x)
		x8	(*  0.500 x)
		y2 (* 0.75 y)
		y1 (* 0.25 y)                                             
		p1 (list x3 y)		p7  (list x2 y2)		p13 (list x1 y1)		p19 (list x0 0)		
		p2 (list x4 y)		p8  (list x3 y2)		p14 (list x2 y1)		p20 (list x1 0)		
		p3 (list x5 y)		p9  (list x4 y2)		p15 (list x3 y1)		p21 (list x2 0)		
		p4 (list x6 y)		p10 (list x5 y2)		p16 (list x4 y1)		p22 (list x3 0)		
		p5 (list x7 y)		p11 (list x6 y2)		p17 (list x5 y1)		p23 (list x4 0)		
		p6 (list x8 y)		p12 (list x7 y2)		p18 (list x6 y1)		p24 (list x5 0)		
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
  	(addTextAtPos layer_Zero _th125_ (pos11 y) "BALISE")
	(moveUp (halfOf y))
	(command "._PLINE" p1 p2 p8 p7 _closed_)		(drawHatch _filledHatch_)
	(command "._PLINE" p3 p4 p10 p9 _closed_)		(drawHatch _filledHatch_)
	(command "._PLINE" p5 p6 p12 p11 _closed_)		(drawHatch _filledHatch_)
	(command "._PLINE" p13 p14 p20 p19 _closed_)	(drawHatch _filledHatch_)
	(command "._PLINE" p15 p16 p22 p21 _closed_)	(drawHatch _filledHatch_)
	(command "._PLINE" p17 p18 p24 p23 _closed_)	(drawHatch _filledHatch_)
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
