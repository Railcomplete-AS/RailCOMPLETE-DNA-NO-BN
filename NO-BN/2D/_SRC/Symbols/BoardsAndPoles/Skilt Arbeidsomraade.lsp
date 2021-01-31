;=========================================================================================================================
;
; Skilt Arbeidsområde.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
; TODO list:
; 2020-09-13 CLFEY Refactor LISP code for Work Area boards. Uppercase descriptions.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Work area (keylock cabinet protected area)

; For debugging:
; (SKILT_ARBEIDSOMRAADE_1-2) (SKILT_ARBEIDSOMRAADE_1-3) (SKILT_ARBEIDSOMRAADE_2-2) (SKILT_ARBEIDSOMRAADE_2-3) (SKILT_ARBEIDSOMRAADE_3)

(defun Skilt_Arbeidsomraade_1-2 ( / blockName description x y att1 att2 p1 p2 p3 p4 p5 p6 p7 p8 )
	; Inside work area (not at border of area), 1 column with 2 lines of text
	;
	; TL--------------TR
	; |  p1 TEKST1 p4  | p7
	; | p2          p5 |
	; |  p3 TEKST2 p6  | p8
	; BL-------.------BR
	;
	(setq
		blockName "NO-BN-2D-SKILT-VEDLIKEHOLD-ARBEIDSOMRAADE-1-2"
		description (strcat "SKILT ARBEIDSOMR" _uAA_ "DE 1-2")
		x 12.0
		y 4.5
		att1 '("TEKST1" "Tekst 1" "Arbeidsområde")
		att2 '("TEKST2" "Tekst 2" "OSL 1")
		p1 (list (* -0.40 x) (*  0.40 y))
		p2 (list (* -0.48 x) (*  0.00 y))
		p3 (list (* -0.40 x) (* -0.40 y))
		p4 (list (*  0.40 x) (*  0.40 y))
		p5 (list (*  0.48 x) (*  0.00 y))
		p6 (list (*  0.40 x) (* -0.40 y))
		p7 (list (*  0.00 x) (*  0.21 y))
		p8 (list (*  0.00 x) (* -0.21 y))
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(command "._PLINE" p1 p2 p3 _open_)
	(command "._PLINE" p4 p5 p6 _open_)
	(addTextAttributeAtPos layer_Zero _th125_ p7 att1)
	(addTextAttributeAtPos layer_Zero _th125_ p8 att2)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun Skilt_Arbeidsomraade_1-3 ( / blockName description x y att1 att2 p1 p2 p3 p4 p5 p6 p7 p8 p9 )
	; Inside work area (not at border of area), 1 column with 3 lines of text
	;
	; TL--------------TR
	; |  p1 TEKST1 p4  | p7
	; | p2  TEKST2  p5 | p8
	; |  p3 TEKST3 p6  | p9
	; BL-------.------BR
	;
	(setq
		blockName "NO-BN-2D-SKILT-VEDLIKEHOLD-ARBEIDSOMRAADE-1-3"
		description (strcat "SKILT ARBEIDSOMR" _uAA_ "DE 1-3")
		x 12.0
		y 6.0
		att1 '("TEKST1" "Tekst 1" "Arbeidsområde")
		att2 '("TEKST2" "Tekst 2" "OSL")
		att3 '("TEKST3" "Tekst 3" "OSL 1")
		p1 (list (* -0.40 x) (*  0.40 y))
		p2 (list (* -0.48 x) (*  0.00 y))
		p3 (list (* -0.40 x) (* -0.40 y))
		p4 (list (*  0.40 x) (*  0.40 y))
		p5 (list (*  0.48 x) (*  0.00 y))
		p6 (list (*  0.40 x) (* -0.40 y))
		p7 (list (*  0.00 x) (*  0.25 y))
		p8 (list (*  0.00 x) (*  0.00 y))
		p9 (list (*  0.00 x) (* -0.25 y))
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(command "._PLINE" p1 p2 p3 _open_)
	(command "._PLINE" p4 p5 p6 _open_)
	(addTextAttributeAtPos layer_Zero _th125_ p7 att1)
	(addTextAttributeAtPos layer_Zero _th125_ p8 att2)
	(addTextAttributeAtPos layer_Zero _th125_ p9 att3)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun Skilt_Arbeidsomraade_2-2 ( / blockName description x y att1 att2 p1 p2 p3 p4 p5 p6 p7 p8 p10 p11 p13 p14 )
	; Inside work area (not at border of area), 2 columns with 2 lines of text
	;
	;    TL----------p13---------TR
	; p7 |  p1 TEKST1 | TEKST4 p4  | p10
	;    | p2         |         p5 |    
	; p8 |  p3 TEKST2 | TEKST5 p6  | p11
	;    BL-----------.----------BR p14 = .
	;
	(setq
		blockName "NO-BN-2D-SKILT-VEDLIKEHOLD-ARBEIDSOMRAADE-2-2"
		description (strcat "SKILT ARBEIDSOMR" _uAA_ "DE 2-2")
		x 20.0
		y 4.5
		att1 '("TEKST1" "Venstre linje 1" "Arb.omr.")
		att2 '("TEKST2" "Venstre linje 2"   "OSL"   )
		att4 '("TEKST4" (strcat "H" _oe_ "yre linje 1") "Arb.omr.")
		att5 '("TEKST5" (strcat "H" _oe_ "yre linje 2")   "OSL "  )
		p1  (list (* -0.45 x) (*  0.40 y))
		p2  (list (* -0.49 x) (*  0.00 y))
		p3  (list (* -0.45 x) (* -0.40 y))
		p4  (list (*  0.45 x) (*  0.40 y))
		p5  (list (*  0.49 x) (*  0.00 y))
		p6  (list (*  0.45 x) (* -0.40 y))
		p7  (list (* -0.22 x) (*  0.25 y)) ; Left side texts
		p8  (list (* -0.22 x) (* -0.25 y))
		p10 (list (*  0.22 x) (*  0.25 y)) ; Right side texts
		p11 (list (*  0.22 x) (* -0.25 y))
		p13 (list (*  0.00 x) (*  0.50 y)) ; vertical line
		p14 (list (*  0.00 x) (* -0.50 y)) ; vertical line
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(command "._PLINE" p1 p2 p3 _open_)
	(command "._PLINE" p4 p5 p6 _open_)
	(addTextAttributeAtPos layer_Zero _th125_ p7 att1)
	(addTextAttributeAtPos layer_Zero _th125_ p8 att2)
	(addTextAttributeAtPos layer_Zero _th125_ p10 att4)
	(addTextAttributeAtPos layer_Zero _th125_ p11 att5)
	(drawLine layer_Zero p13 p14)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun Skilt_Arbeidsomraade_2-3 ( / blockName description x y att1 att2 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 )
	; Inside work area (not at border of area), 2 columns with 3 lines of text
	;
	;    TL----------p13----------TR
	; p7 |  p1 TEKST1 | TEKST4 p4  | p10
	; p8 | p2  TEKST2 | TEKST5  p5 | p11
	; p9 |  p3 TEKST3 | TEKST6 p6  | p12
	;    BL-----------.-----------BR p14 = .
	;
	(setq
		blockName "NO-BN-2D-SKILT-VEDLIKEHOLD-ARBEIDSOMRAADE-2-3"
		description (strcat "SKILT ARBEIDSOMR" _uAA_ "DE 2-3")
		x 20.0
		y 6.0
		att1 '("TEKST1" "Venstre linje 1" "Arb.omr.")
		att2 '("TEKST2" "Venstre linje 2"   "OSL"   )
		att3 '("TEKST3" "Venstre linje 3"  "OSL 19" )
		att4 '("TEKST4" (strcat "H" _oe_ "yre linje 1") "Arb.omr.")
		att5 '("TEKST5" (strcat "H" _oe_ "yre linje 2")   "OSL"   )
		att6 '("TEKST6" (strcat "H" _oe_ "yre linje 3")  "OSL 27" )
		p1  (list (* -0.45 x) (*  0.40 y))
		p2  (list (* -0.49 x) (*  0.00 y))
		p3  (list (* -0.45 x) (* -0.40 y))
		p4  (list (*  0.45 x) (*  0.40 y))
		p5  (list (*  0.49 x) (*  0.00 y))
		p6  (list (*  0.45 x) (* -0.40 y))
		p7  (list (* -0.22 x) (*  0.25 y)) ; Left side texts
		p8  (list (* -0.22 x) (*  0.00 y))
		p9  (list (* -0.22 x) (* -0.25 y))
		p10 (list (*  0.22 x) (*  0.25 y)) ; Right side texts
		p11 (list (*  0.22 x) (*  0.00 y))
		p12 (list (*  0.22 x) (* -0.25 y))
		p13 (list (*  0.00 x) (*  0.50 y)) ; vertical line
		p14 (list (*  0.00 x) (* -0.50 y)) ; vertical line
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(command "._PLINE" p1 p2 p3 _open_)
	(command "._PLINE" p4 p5 p6 _open_)
	(addTextAttributeAtPos layer_Zero _th125_ p7 att1)
	(addTextAttributeAtPos layer_Zero _th125_ p8 att2)
	(addTextAttributeAtPos layer_Zero _th125_ p9 att3)
	(addTextAttributeAtPos layer_Zero _th125_ p10 att4)
	(addTextAttributeAtPos layer_Zero _th125_ p11 att5)
	(addTextAttributeAtPos layer_Zero _th125_ p12 att6)
	(drawLine layer_Zero p13 p14)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun Skilt_Arbeidsomraade_3 ( / blockName description x y att1 att2 att3 att4 att5 att6 )
	(setq
		blockName "NO-BN-2D-SKILT-VEDLIKEHOLD-ARBEIDSOMRAADE-3"
		description (strcat "SKILT ARBEIDSOMR" _uAA_ "DE 3")
		x 12.0
		y 12.0
		att1 '("TEKST1" "Tekst 1" (strcat "Arbeidsomr" _aa_ "de"))
		att2 '("TEKST2" "Tekst 2" "Port leder til")
		att3 '("TEKST3" "Tekst 3" (strcat "omr" _aa_ "der"))
		att4 '("TEKST4" "Tekst 4" "OSL")
		att5 '("TEKST5" "Tekst 5" "OSL 19")
		att6 '("TEKST6" "Tekst 6" "OSL 27")
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAttributeAtPos layer_Zero _th125_ (pos61 y) att1)
	(addTextAttributeAtPos layer_Zero _th125_ (pos62 y) att2)
	(addTextAttributeAtPos layer_Zero _th125_ (pos63 y) att3)
	(addTextAttributeAtPos layer_Zero _th125_ (pos64 y) att4)
	(addTextAttributeAtPos layer_Zero _th125_ (pos65 y) att5)
	(addTextAttributeAtPos layer_Zero _th125_ (pos66 y) att6)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)
