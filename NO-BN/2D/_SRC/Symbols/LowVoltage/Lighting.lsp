;=========================================================================================================================
;
; Lighting.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Lighting

(defun C:LIGHTING ( / )
	(TraceLevel3 "LAVSPENNINGSMAST")		(LAVSPENNINGSMAST)
	(TraceLevel3 "LYSARMATUR")				(LYSARMATUR)
	(TraceLevel3 "BELYSNING-PUNKT")			(BELYSNING "PUNKT")
	(TraceLevel3 "BELYSNING-MAST-1-LYS")	(BELYSNING "MAST-1-LYS")
	(TraceLevel3 "BELYSNING-MAST-2-LYS")	(BELYSNING "MAST-2-LYS")
	(TraceLevel3 "BELYSNING-MAST-2-LYS-V")	(BELYSNING "MAST-2-LYS-V")
	(TraceLevel3 "BELYSNING-MAST-3-LYS")	(BELYSNING "MAST-3-LYS")
	(TraceLevel3 "BELYSNING-MAST-3-LYS-T")	(BELYSNING "MAST-3-LYS-T")
	(TraceLevel3 "BELYSNING-MAST-4-LYS")	(BELYSNING "MAST-4-LYS")
)



(defun LAVSPENNINGSMAST ( / blockName description r rm x y )
	; Free-standing mast - e.g. for mounting of cameras, loudspeakers etc.
	;
	;  \   /
	;   (*)  |  Filled circle at origo with four 'legs' that stick out
	;  /   \
	;
	(setq 
		blockName "NO-BN-2D-JBTEL-LAVSPENNINGSMAST"
		description "LAVSPENNINGSMAST"
		r (GetLightFixtureRadius)
		rm (* 0.5 r) ; mast symbol radius
		x (* (sqrt 8) rm)
		y (* (sqrt 8) rm)
	)
	(DrawCircle layDef_Zero rm _noWipeout_)
	(DrawHatch _solidHatch_)
	(DrawStAndrewCross layDef_Zero x y)
	(AddDescriptionBelowOrigo description rm)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun LYSARMATUR ( / blockName description x y len )
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
	(DrawBox layDef_Zero x len _noWipeout_)
	(DrawStAndrewCross layDef_Zero x y)
	(AddDescriptionBelowOrigo description (HalfOf len))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun BELYSNING ( variation / blockName description r p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 )
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
		r (GetLightFixtureRadius)
		rm (* 0.5 r) ; mast symbol radius
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
			(AddLightFixture _origo_)
		)
		((= variation "MAST-1-LYS")
			(AddLightFixture p0)
		)
		((= variation "MAST-2-LYS")
			(AddLightFixture p0)
			(AddLightFixture p1)
		)
		((= variation "MAST-2-LYS-V")
			(AddLightFixture p2)
			(AddLightFixture p3)
		)
		((= variation "MAST-3-LYS")
			(AddLightFixture p4)
			(AddLightFixture p5)
			(AddLightFixture p6)
		)
		((= variation "MAST-3-LYS-T")
			(AddLightFixture p6)
			(AddLightFixture p7)
			(AddLightFixture p8)
		)
		((= variation "MAST-4-LYS")
			(AddLightFixture p6)
			(AddLightFixture p7)
			(AddLightFixture p8)
			(AddLightFixture p9)
		)
		(T (alert (strcat "*** ERROR: BELYSNING() bad variation [" variation "] .")))
	)
	(cond 
		((= variation "PUNKT") 
			(AddDescriptionBelowOrigo description r)
		)
		(T
			(DrawCircle layDef_Zero rm _noWipeout_)
			(DrawHatch _solidHatch_)
			(AddDescriptionBelowOrigo description (* 3 r)) ; well below...
		)
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



; Helpers
;--------------
(defun GetLightFixtureRadius ( / r )
	(setq r 1.25)
)



(defun AddLightFixture ( pos / r rm x y pX pY h p1 p2 )
	;
	;    (X) ; Place bulb at a position defined as (a*r,b*r) where r=bulb symbol radius
	;    /   ; Arm
	;   .    ; The arm starts at rm/2 from origo and stops at r from bulb center (if bulb center is more than rm+r away from origo)
	;
	(setq 
		r (GetLightFixtureRadius)
		rm (* 0.5 r) ; mast symbol radius (not drawn here)
		x (* (sqrt 2) r) ; St Andrew's cross in light bulb symbol
		y (* (sqrt 2) r)
		pX (* (nth 0 pos) r) ; Position for center light bulb was given as multiples of bulb radius
		pY (* (nth 1 pos) r)
		h (sqrt (+ (* pX pX) (* pY pY))) ; distance from origo to center light bulb
	)
	(DrawCircleAtPos layDef_Zero (list pX pY) r _noWipeout_)
	(DrawStAndrewCrossAtPos layDef_Zero (list pX pY) x y)
	(cond
		((> h (+ rm r)) 
			(setq 
				p1 (list (* (/ rm h) pX)   (* (/ rm h) pY)) ; start of arm closest to origo where mast symbol ends
				p2 (list (* (/ (- h r) h) pX)   (* (/ (- h r) h) pY)) ; end of arm where light bulb circle starts
			)
			(DrawLine layDef_Zero p1 p2)
		)
	)
)
