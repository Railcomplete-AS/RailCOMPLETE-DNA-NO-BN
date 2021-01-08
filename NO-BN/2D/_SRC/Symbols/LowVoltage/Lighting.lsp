;=========================================================================================================================
;
; Lighting.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Lighting

(defun C:LIGHTING ( / )
	(LAVSPENNINGSMAST)
	(LYSARMATUR)
	(BELYSNING "PUNKT")
	(BELYSNING "MAST-1-LYS")
	(BELYSNING "MAST-2-LYS")
	(BELYSNING "MAST-2-LYS-V")
	(BELYSNING "MAST-3-LYS")
	(BELYSNING "MAST-3-LYS-T")
	(BELYSNING "MAST-4-LYS")
)



(defun LAVSPENNINGSMAST ( / blockName radius r x y )
	; Free-standing mast - e.g. for mounting of cameras, loudspeakers etc.
	;
	;  \   /
	;   (*)  |  Filled circle at origo with four 'legs' that stick out
	;  /   \
	;
	(setq 
		blockName "NO-BN-2D-JBTEL-LAVSPENNINGSMAST"
		description "LAVSPENNINGSMAST"
		r (getLightFixtureRadius)
		rm (* 0.5 r) ; mast symbol radius
		x (* (sqrt 8) rm)
		y (* (sqrt 8) rm)
	)
	(drawCircle layer_Zero rm _noWipeout_)
	(drawHatch _filledHatch_)
	(drawStAndrewCross layer_Zero x y)
	(addDescriptionBelowOrigo description rm)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun LYSARMATUR ( / blockName description )
	; Single-point light fixture (ceiling mounted, wall mounted, platform edge element mounted etc)
	;     
	; TL-TR
	; |   |
	; |   |
	; |\ /|
	; | . |
	; |/ \|
	; |   |
	; |   |
	; BL-BR
	;     
	(setq 
		blockName "NO-BN-2D-JBTEL-BELYSNING-ARMATUR"
		description "LYSARMATUR"
		x (* 4 0.250) ; Cross in the middle + box width
		y (* 4 0.250) ; Cross in the middle
		len (* 4 1.200) ; Box length
	)
	(drawBox layer_Zero x len _noWipeout_)
	(drawStAndrewCross layer_Zero x y)
	(addDescriptionBelowOrigo description (halfOf len))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun BELYSNING ( variation / blockName description )
	; Single- or multi-point light fixture, posibly with own mast (upright mast or upright/suspended mast mounted on OCS yoke)
	;     
	;  	 (X)  '0' - Single point without mast (open  circle with St Andrew's cross inside the circle, at origo)
	;     
	;     .   '1'
	;    (0)
	;
	;    (1)
	;     .   '2'
	;    (0)
	;     
	;     .   '2V' (opening above 0 and below 180 degrees)
	;    / \
	;  (2) (3)
	;     
	; (4)   (5)
	;   \   /
	;     .   '3' (symmetric 120 degrees)
	;     |
	;    (6)
	;     
	; (7)-.-(8) '3T' (T-shaped 90/180 degrees)
	;     |
	;    (6)
	;     
	;    (9)
	;     |
	; (7)-.-(8) '4' (symmetric 90 degrees)
	;     |
	;    (6)
	;     
	(setq 
		blockName (strcat "NO-BN-2D-JBTEL-BELYSNING-LYS-" variation)
		description (strcat "BELYSNING " variation)
		r (getLightFixtureRadius)
		p0 '( 0.000 -1.500)
		p1 '( 0.000  1.500)
		p2 '(-1.250 -2.250)
		p3 '( 1.250 -2.250)
		p4 '(-1.500  0.800)
		p5 '( 1.500  0.800)
		p6 '( 0.000 -1.700)
		p7 '(-1.700  0.000)
		p8 '( 1.700  0.000)
		p9 '( 0.000  1.700)
	)
	(cond
		((= variation "PUNKT")
			(addLightFixture _origo_)
		)
		((= variation "MAST-1-LYS")
			(addLightFixture p0)
		)
		((= variation "MAST-2-LYS")
			(addLightFixture p0)
			(addLightFixture p1)
		)
		((= variation "MAST-2-LYS-V")
			(addLightFixture p2)
			(addLightFixture p3)
		)
		((= variation "MAST-3-LYS")
			(addLightFixture p4)
			(addLightFixture p5)
			(addLightFixture p6)
		)
		((= variation "MAST-3-LYS-T")
			(addLightFixture p6)
			(addLightFixture p7)
			(addLightFixture p8)
		)
		((= variation "MAST-4-LYS")
			(addLightFixture p6)
			(addLightFixture p7)
			(addLightFixture p8)
			(addLightFixture p9)
		)
		(T (alert (strcat "*** ERROR: BELYSNING() bad variation [" variation "] .")))
	)
	(cond 
		((= variation "PUNKT") 
			(addDescriptionBelowOrigo description r)
		)
		(T
			(drawCircle layer_Zero rm _noWipeout_)
			(drawHatch _filledHatch_)
			(addDescriptionBelowOrigo description (* 3 r)) ; well below...
		)
	)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



; Helpers
;--------------
(defun getLightFixtureRadius ( / r )
	(setq r 1.25)
)



(defun addLightFixture ( pos / r rm x y pX pY h p1 p2 )
	;
	;    (X) ; Place bulb at a position defined as (a*r,b*r) where r=bulb symbol radius
	;    /   ; Arm
	;   .    ; The arm start at rm/2 from origo and stops at r from bulb center (if bulb center is more than rm+r away from origo)
	;
	(setq 
		r (getLightFixtureRadius)
		rm (* 0.5 r) ; mast symbol radius (not drawn here)
		x (* (sqrt 2) r) ; St Andrew's cross in light bulb symbol
		y (* (sqrt 2) r)
		pX (* (nth 0 pos) r) ; Position for center light bulb was given as multiples of bulb radius
		pY (* (nth 1 pos) r)
		h (sqrt (+ (* pX pX) (* pY pY))) ; distance from origo to center light bulb
	)
	(drawCircleAtPos layer_Zero r (list pX pY) _noWipeout_)
	(drawStAndrewCrossAtPos layer_Zero x y (list pX pY))
	(cond
		((> h (+ rm r)) 
			(setq 
				p1 (list (* (/ rm h) pX)   (* (/ rm h) pY)) ; start of arm closest to origo where mast symbol ends
				p2 (list (* (/ (- h r) h) pX)   (* (/ (- h r) h) pY)) ; end of arm where light bulb circle starts
			)
			(drawLine layer_Zero p1 p2)
		)
	)
)
