;=========================================================================================================================
;
; 60.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; ATC functions (repeater balise group, braking curve target point, virtual distant signal, release speed)

; For debugging:
; (60A) (60B) (60C) (60D) (60D-10) (60D-40) (60E) (60F) (60F-TILLEGGSTEKST) (60G) (60G-TILLEGGSTEKST) (60H)

(defun 60A ( / blockName description side r)
	; Repeater balise group with function as virtual distant signal
	;       +
	;     /___\
	;   / / * \ \  
	;  + ( *** ) +  Filled circle in losange
	;   \ \_*_/ /
	;     \   /
	;       .
	;
	(setq	
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-60A-ATC-FORSIGNAL"
		description "SKILT SIGNAL 60A ATC FORSIGNAL"
		side	(getLosangeSide)	; Losange side
		r		(* 0.215 side)
  	)
	(drawLosangeWithCircle)
	(drawHatch _filledHatch_)
	(moveUp (* (DDcos 45) side))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 60B ( / blockName description side x y p1 p2 p3 )
	; Repeater balise group with braking curve target mark
	;
	;       +
	;     /___\
	;   / /1-2\ \  
	;  + ( \*/ ) +  Circle in losange with hatched equilateral triangle
	;   \ \_3_/ /
	;     \   /
	;       +
	;
	(setq	
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-60B-ATC-REPETER-MAALPUNKT"
		description (strcat "SKILT SIGNAL 60B ATC REP.M" _uAA_ "LPUNKT")
		side	(getLosangeSide)	; Losange side
		x	(* (sqrt 2) side)
		y	(* (sqrt 2) side)
		p1 (list (* -0.10 x) (*  0.07 y))
		p2 (list (*  0.10 x) (*  0.07 y))
		p3 (list (*  0.00 x) (* -0.10 y))
	)
	(drawLosangeWithCircle)
	(command "._PLINE" p1 p2 p3 _closed_) ; triangle
	(drawHatch _filledHatch_)
	(moveUp (* (DDcos 45) side))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 60C ( / blockName description side x y )
	; Repeater balise group with emergency brake application function
	;
	;       +
	;     /___\
	;   / /   \ \  
	;  + ( === ) +  Horizontal bar inside circle
	;   \ \___/ /
	;     \   /
	;       .
	;
	(setq	
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-60C-ATC-NOEDBREMS"
		description (strcat "SKILT SIGNAL 60C ATC N" _uOE_ "DBREMS")
		x 1.5 ; horizontal bar
		y 0.6 ; horizontal bar
	)
	(drawLosangeWithCircle)
	(drawBox layer_Zero x y _noWipeout_) ; Add bar
	(drawHatch _filledHatch_)
	(moveUp (* (DDcos 45) (getLosangeSide)))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 60D ( / blockName description )
	; Repeater balise group
	;
	;       +
	;     /___\
	;   / /   \ \  
	;  + (     ) +  Empty circle in losange
	;   \ \___/ /
	;     \   /
	;       .
	;
	(setq	
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-60D-ATC-REPETER-HASTIGHET"
		description "SKILT SIGNAL 60D ATC REPETER HASTIGHET"
	)	
	(drawLosangeWithCircle)
	(moveUp (* (DDcos 45) (getLosangeSide)))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 60D-10 ( / blockName description )
	; Repeater balise group with explicit mentioning of release speed 10 km/h
	;
	;       +
	;     /___\
	;   / /   \ \  
	;  + (  1  ) +  Circle with '1'
	;   \ \___/ /
	;     \   /
	;       .
	;
	(setq	
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-60D-ATC-REPETER-HASTIGHET-10"
		description "SKILT SIGNAL 60D ATC REPETER HASTIGHET 10 KM/H"
	)	
	(drawLosangeWithCircle)
	(addTextAtPos layer_Zero (* 0.45 (getLosangeSide)) _origo_ "1") ; Letter size is 45% of side
	(moveUp (* (DDcos 45) (getLosangeSide)))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 60D-40 ( / blockName description )
	; Repeater balise group with explicit mentioning of release speed 40 km/h
	;
	;       +
	;     /___\
	;   / /   \ \  
	;  + (  4  ) +  Circle with '4' in losange
	;   \ \___/ /
	;     \   /
	;       .
	;
	(setq	
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-60D-ATC-REPETER-HASTIGHET-40"
		description "SKILT SIGNAL 60D ATC REPETER HASTIGHET 40 KM/H"
	)	
	(drawLosangeWithCircle)
	(addTextAtPos layer_Zero (* 0.45 (getLosangeSide)) _origo_ "4") ; Letter size is 45% of side
	(moveUp (* (DDcos 45) (getLosangeSide)))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 60E ( / blockName description side dim2 ang2 y p1 p2 p3 )
	; Information board on main signal or distant signal with braking curve past the folowing signal, i.e. '/Px' (aka 'through-signaling')
	;
	;       +
	;     /   \
	;   / p1-p2 \  
	;  +   \ /   + Triangle in losange
	;   \   p3  /
	;     \   /
	;       .
	;
	(setq	
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-60E-ATC-VARSEL"
		description "SKILT SIGNAL 60E ATC VARSEL"
		side (getLosangeSide)
		x (* 2 (DDcos 45) side) ; Virtual box
		y x
		p1 (list (* -0.25 x) (*  0.10 y))
		p2 (list (*  0.25 x) (*  0.10 y))
		p3 (list (*  0.00 x) (* -0.34 y))
	)
	(drawLosange)
	(command "._PLINE" p1 p2 p3 _closed_) ; inner triangle
	(drawHatch _filledHatch_)
	(moveUp (* (DDcos 45) (getLosangeSide)))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 60F ( / blockName description dim1 dim2 )
	; Start of F-ATC area (all speed limits are supervised and shown in onboard ATP panel)
	;
	; TL---------------TR
	; |                 |  
	; |     F A T C     |
	; |                 |
	; BL---------------BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-60F-FATC"
		description "Signal 60F FATC"
		x	9.0
		y	6.0
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAtPos layer_Zero _th250_ _origo_ "FATC")
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 60F-TILLEGGSTEKST ( / blockName description dim1 dim2 )
	; Alternative board layout:
	; Start of F-ATC area (all speed limits are supervised and shown in onboard ATP panel)
	;
	; TL---------------TR
	; |                 |  
	; |     F A T C     |
	; |    BEGYNNER     |
	; BL---------------BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-60F-FATC-TILLEGGSTEKST"
		description "Signal 60F FATC MED TILLEGGSTEKST"
		x	9.0
		y	6.0
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAtPos layer_Zero _th250_ (pos31 y) "FATC")
	(addTextAtPos layer_Zero _th150_ (pos33 y) "BEGYNNER")
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 60G ( / blockName description x y )
	; Alternative board layout:
	; Start of D-ATC area (not all speed limits are supervised, speeds above 70 km/h are shown as '---' in onboard ATP panel)
	;
	; TL---------------TR
	; |                 |  
	; |     D A T C     |
	; |                 |
	; BL---------------BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-60G-DATC"
		description "SKILT SIGNAL 60G DATC"
		x	9.0
		y	6.0
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAtPos layer_Zero _th250_ _origo_ "DATC")
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 60G-TILLEGGSTEKST ( / blockName description x y )
	; Alternative board layout:
	; Start of D-ATC area (not all speed limits are supervised, speeds above 70 km/h are shown as '---' in onboard ATP panel)
	;
	; TL---------------TR
	; |                 |  
	; |     D A T C     |
	; |    BEGYNNER     |
	; BL---------------BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-60G-DATC-TILLEGGSTEKST"
		description "SKILT SIGNAL 60G DATC MED TILLEGGSTEKST"
		x	9.0
		y	6.0
	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAtPos layer_Zero _th250_ (pos31 y) "DATC")
	(addTextAtPos layer_Zero _th150_ (pos33 y) "BEGYNNER")
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)



(defun 60H ( / blockName description x y p1 p2 p3 p4 p5 p6 )
	; End of ATP supervision, transition into unequipped area
	;
	; TL-----------p4--p5
	; |          / / / p6  
	; |     /A/T/C      |
	; p1/ / /           |
	; p2--p3-----------BR
	;
	(setq 
		blockName "NO-BN-2D-SKILT-KJOERENDE-SIGNAL-60H-ATC-SLUTTER"
		description "SKILT SIGNAL 60H ATC SLUTTER"
		x	9.0
		y	6.0
		p1 (list (* -0.5 x) (* -0.4 y))
		p2 (list (* -0.5 x) (* -0.5 y))
		p3 (list (* -0.4 x) (* -0.5 y))
		p4 (list (*  0.4 x) (*  0.5 y))
		p5 (list (*  0.5 x) (*  0.5 y))
		p6 (list (*  0.5 x) (*  0.4 y))
  	)
	(drawBox layer_Zero x y layer_BoardOrPole_Wipeout)
	(addTextAtPos layer_Zero _th250_ _origo_ "ATC")
	(setLayer layer_Zero)
	(command
		"._LINE" p1 p4 ""
		"._LINE" p2 p5 ""
		"._LINE" p3 p6 ""
	)
	(moveUp (halfOf y))
	(addDescriptionBelowOrigo description 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
	description ; Used if table is created
)
