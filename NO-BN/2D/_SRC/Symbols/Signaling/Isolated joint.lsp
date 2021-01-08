;=========================================================================================================================
;
; Isolated joint.lsp
;
; NB! Kan ikke ha æøå i filnavn som skal loades av LISP.
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Isolated rail joint (showing where the signal current is flowing)

(defun C:ISOLATED-JOINT ( / ) 
	; Isolated joints, for track circuit separation or for return current circuit drawings
	;
	; Note: New layers are created with color 62, but the graphic entities drawn on these layers shall be given color "_ByBlock",
	; because those entities are meant to reflect the object and not "helpful extra info" such as distances and metalfree areas etc.
	; Also note that these layers are named "JBTOB", i.e. track and embankment ('overbygning') because they physically belong to JBTOB (= KO = 'Konstruksjon overbygning').
	;
	;
	;             +--------- q4 = Quadrant IV  = Signal current exiting from right rail's joint in the direction of increasing mileage
	;             | +------- q3 = Quadrant III = Signal current entering into right rail's joint in the direction of increasing mileage
	;             | | +----- q2 = Quadrant II  = Signal current entering into left rail's joint in the direction of increasing mileage
	;             | | | +--- q1 = Quadrant I   = Signal current exiting from left rail's joint in the direction of increasing mileage
	;             | | | | 
	(SKINNESKJOET 0 0 0 0) ; No preceding signal current, no following signal current: Planned but yet UNFINISHED isolation.
	(SKINNESKJOET 0 0 0 1)
	(SKINNESKJOET 0 0 1 0)
	(SKINNESKJOET 0 0 1 1)
	(SKINNESKJOET 0 1 0 0)
	(SKINNESKJOET 0 1 0 1)
	(SKINNESKJOET 0 1 1 0)
	(SKINNESKJOET 0 1 1 1)
	(SKINNESKJOET 1 0 0 0)
	(SKINNESKJOET 1 0 0 1)
	(SKINNESKJOET 1 0 1 0)
	(SKINNESKJOET 1 0 1 1)
	(SKINNESKJOET 1 1 0 0)
	(SKINNESKJOET 1 1 0 1)
	(SKINNESKJOET 1 1 1 0)
	(SKINNESKJOET 1 1 1 1)
)



(defun SKINNESKJOET ( q4 q3 q2 q1 / blockName schematicGauge geoGauge paperScale dwgScale )
	;
	; Isolated joint ('Skinneskjøt').
	;
	; The isolated joint symbol has several 'views'. Schematic symbols hold three schematic views. Two are suitable for Bane NOR 1-line track drawings, one is suitable for 2-line with
	; 9 drawing units (meters) between rails. The geo symbols contain the same three viewss
	;
	; The symbols' insertion points are always centered in the track.
	;
	; In "JBTOB__KO_SKJ_SKJEMATISK_PLAN" view, we assume a 1-line representation of the centre track. 
	; Both schematic symbols and 1:500 symbols feature a 2.5 x 0.15 'bar' across the track.
	;
	; In "JBTOB__KO_SKJ_KABELPLAN" view, we assume a 1-line representation of the centre track.
	; Schematic symbols feature a 3 meter 'bar' across the track and 1.5 meter 'ears' along the track in the direction of track circuit current.
	; Geo symbols feature a 0.75 
	; 
	; In "JBTOB__KO_SKJ_SPORISOLERING_OG_RETURKRETS" view, we assume a 2-line representation of the centre track. Schematic and 1:500 symbols feature
	; a 3.0 meter 'bar' across the relevant rail pluss a 1.5 meter 'ear' along that rail in the direction of track circuit current presence.
	; If both rails have isolations at the same mileage, then 
	; 
	; The geo symbols support all three views mentioned above, but adapted to normal gauge distance between rails in 2-line track view mode.
	; You can toggle between 1-line and 2-line mode in RailCOMPLETE using the command RC-ShowTwoRails.
	;
	(setq
		blockName (strcat "NO-BN-2D-JBTKO-SKJ-SKINNESKJOET-" (rtos q4 2 0) "-" (rtos q3 2 0) "-" (rtos q2 2 0) "-" (rtos q1 2 0))
		schematicGauge 9.0 		; Spacing between rails in schematic 2-line signaling drawings
		geoGauge 1.5 			; Spacing between rails in geo 2-line drawings
	)
	
	; Schematic symbol
	; Schematic 1-line signaling / schematic plan ('skjematisk plan') view
	(setLayer layer_View_SchematicPlan)
	(drawIsolatedJointThickBarSymbol 1.0)

	; Schematic 1-line signaling / cable plan ('plan og kabelplan') view
	(setLayer layer_View_CablePlan)
	(drawIsolatedJointSymbolWithEars q4 q3 q2 q1 1.0 0.0) ; No scaling, no offset from centre track

	; Schematic 2-line signaling / track insulation plan OR high voltage / return current plan view
	; 9 units (meters) rail separation in Bane NOR schematic signaling drawings
	(setLayer layer_View_TrackIsolationPlan)
	(drawIsolatedJointSymbolWithEars q4 q3 q2 q1 1.0 (/ schematicGauge 2)) ; No scaling, then offset with half of schematic gauge from centre track

	; Create schematic symbol
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols
	; Note: The geo symbols must be created one by one because the rail gauge is the same in all scales, but the 'ears' vary in size.
	(foreach paperScale paperScaleList
		(setq dwgScale (/ (atof paperScale) 500.0)) ; The schematic symbol is also suitable for 1:500 geo scale.

		; Geo 1-line signaling view showing just a thick bar
		(setLayer layer_View_SchematicPlan)
		(drawIsolatedJointThickBarSymbol dwgScale)

		; Geo 1-line signaling view showing a bar with ears, centered on the track axis
		(setLayer layer_View_CablePlan)
		(drawIsolatedJointSymbolWithEars q4 q3 q2 q1 dwgScale 0.0) ; Scale, no offset

		; Geo 2-line signaling / return current view showing a bar with ear(s), centered on the left or right rail
		; (1.5 meters rail separation in all geo scales, of course)
		(setLayer layer_View_TrackIsolationPlan)
		(drawIsolatedJointSymbolWithEars q4 q3 q2 q1 dwgScale (/ geoGauge 2)) ; Scale, then offset with half of normal track gauge

		(createGeoBlockInCurrentPaperScaleFromCurrentGraphics paperScale blockName)
	)
)



;==========================
; draw...X...() functions
;==========================
(defun drawIsolatedJointThickBarSymbol ( scale / barLength barWidth )
	(setq
		barLength	2.5			; Length of thick bar across track
		barWidth	0.15		; Width of bar across track
	)
	(command 
		"._PLINE" 
			(list 0 (/ barLength -2)) "_W" barWidth barWidth (list 0 (/ barLength 2))
			"_W" 0 0 ; Always reset PLINE width after using non-standard settings
			""
	)
	(command "._SCALE" "_LAST" "" "0,0" scale)
)



(defun drawIsolatedJointSymbolWithEars ( q4 q3 q2 q1 scale offset / bar ear y x pt11 pt12 pt13 pt22 pt32 pt41 pt42 pt43 )
	; Points:
	;
	;  -x     0    +x
	;
	;  41    42    43  +(offset+y)
	; ---------------- +(offset) = left rail
	; (II)   32    (1) +(offset-y)
	;                   centre track
	; (III)  22   (IV) -(offset-y)
	; ---------------- -(offset) = right rail
	;  11    12    13  -(offset+y)
	;
	(setq
		bar		3.0							; Length of "bar" across track
		ear		1.5							; Length of "ear" along track
		y		(* (/ bar 2) scale)			; Half-length of scaled bar across track
		x		(* ear scale)				; Length of scaled ear along track
		pt11	(list (- x) (- (+ offset y)))
		pt12	(list (+ 0) (- (+ offset y)))
		pt13	(list (+ x) (- (+ offset y)))
		pt22	(list (+ 0) (- (- offset y)))
		pt32	(list (+ 0) (+ (- offset y)))
		pt41	(list (- x) (+ (+ offset y)))
		pt42	(list (+ 0) (+ (+ offset y)))
		pt43	(list (+ x) (+ (+ offset y)))
	)
	(if (> (+ q1 q2) 0) (command "._LINE" pt32 pt42 "")) ; upper bar
	(if (> (+ q3 q4) 0) (command "._LINE" pt22 pt12 "")) ; lower bar
	(if (= q1 1) (command "._LINE" pt42 pt43 "")) ; quadrant I
	(if (= q2 1) (command "._LINE" pt42 pt41 "")) ; quadrant II
	(if (= q3 1) (command "._LINE" pt12 pt11 "")) ; quadrant III
	(if (= q4 1) (command "._LINE" pt12 pt13 "")) ; quadrant IV
	(if (= (+ q4 q3 q2 q1) 0) ; Add two small circles to "0000" symbol, meaning "UNFINISHED - not configured yet"
		(progn 
			(command "._CIRCLE" pt12 y)
			(command "._CIRCLE" pt42 y)
		)
	)
)
