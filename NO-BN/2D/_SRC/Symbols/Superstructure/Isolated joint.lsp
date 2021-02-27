;=========================================================================================================================
;
; Isolated joint.lsp
;
; NB! Kan ikke ha æøå i filnavn som skal loades av LISP.
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Isolated rail joint (showing where the signal current is flowing)
;
; Test cases:
;		(setq  q4 1  q3 0  q2 1  q1 1)
;		(RAILJOINT q4 q3 q2 1)


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
	(subSubStep "RAILJOINT 0")	(RAILJOINT 0 0 0 0) ; No preceding signal current, no following signal current: Planned but yet UNFINISHED isolation.
	(subSubStep "RAILJOINT 1")	(RAILJOINT 0 0 0 1)
	(subSubStep "RAILJOINT 2")	(RAILJOINT 0 0 1 0)
	(subSubStep "RAILJOINT 3")	(RAILJOINT 0 0 1 1)
	(subSubStep "RAILJOINT 4")	(RAILJOINT 0 1 0 0)
	(subSubStep "RAILJOINT 5")	(RAILJOINT 0 1 0 1)
	(subSubStep "RAILJOINT 6")	(RAILJOINT 0 1 1 0)
	(subSubStep "RAILJOINT 7")	(RAILJOINT 0 1 1 1)
	(subSubStep "RAILJOINT 8")	(RAILJOINT 1 0 0 0)
	(subSubStep "RAILJOINT 9")	(RAILJOINT 1 0 0 1)
	(subSubStep "RAILJOINT A")	(RAILJOINT 1 0 1 0)
	(subSubStep "RAILJOINT B")	(RAILJOINT 1 0 1 1)
	(subSubStep "RAILJOINT C")	(RAILJOINT 1 1 0 0)
	(subSubStep "RAILJOINT D")	(RAILJOINT 1 1 0 1)
	(subSubStep "RAILJOINT E")	(RAILJOINT 1 1 1 0)
	(subSubStep "RAILJOINT F")	(RAILJOINT 1 1 1 1)
)



(defun RAILJOINT ( q4 q3 q2 q1 / blockName schematicGauge geoGauge )
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
	; Annotative symbol feature a 0.75 line from alignment axis to the rail where the sensor is actually located.
	; 
	; In "JBTOB__KO_SKJ_SPORISOLERING_OG_RETURKRETS" view, we assume a 2-line representation of the centre track. Schematic and 1:500 symbols feature
	; a 3.0 meter 'bar' across the relevant rail pluss a 1.5 meter 'ear' along that rail in the direction of track circuit current presence.
	; If both rails have isolations at the same mileage, then 
	; 
	; The geo symbols support all three views mentioned above, but adapted to normal gauge distance between rails in 2-line track view mode.
	; You can toggle between 1-line and 2-line mode in RailCOMPLETE using the command RC-ShowTwoRails.
	;
	(setq
		blockName	(strcat "NO-BN-2D-JBTOB-RAILJOINT-" (rtos q4 2 0) "-" (rtos q3 2 0) "-" (rtos q2 2 0) "-" (rtos q1 2 0))
		description (strcat "ISOLERT SKINNESKJ" _uOE_ "T, KVADRANT IV-III-II-I = " (rtos q4 2 0) "-" (rtos q3 2 0) "-" (rtos q2 2 0) "-" (rtos q1 2 0))
	)
	
	; Schematic symbol
	; Schematic 1-line signaling / schematic plan ('skjematisk plan') view
	(setLayer layDef_View_SchematicPlan)
	(drawIsolatedJointThickBarSymbol _one_)

	; Schematic 1-line signaling / cable plan ('plan og kabelplan') view
	(setLayer layDef_View_CablePlan)
	(drawIsolatedJointSymbolWithEars q4 q3 q2 q1 _one_ 0.0) ; No scaling, no offset from centre track

	; Schematic 2-line signaling / track insulation plan OR high voltage / return current plan view
	(setLayer layDef_View_TrackIsolationPlan)
	(drawIsolatedJointSymbolWithEars q4 q3 q2 q1 _one_ (* _half_ _schematicGauge_)) ; No scaling, then offset with half of schematic gauge from centre track

	; Schematic symbol
	(addDescriptionBelowOrigo description (* _threeQuarters_ _schematicGauge_))
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	; Geo 1-line signaling view showing just a thick bar
	; (setLayer layDef_View_SchematicPlan)  -   not meaningful for geo symbols
	; (drawIsolatedJointThickBarSymbol _one_)

	; Geo 1-line signaling view showing a bar with ears, centered on the track axis
	; (setLayer layDef_View_CablePlan)
	; (drawIsolatedJointSymbolWithEars q4 q3 q2 q1 _one_ 0.0) ; Scale, no offset

	; Geo 2-line signaling / return current view showing a bar with ear(s), centered on the left or right rail
	(setLayer layDef_View_TrackIsolationPlan)
	(drawIsolatedJointSymbolWithEars q4 q3 q2 q1 _one_ (* _half_ _cantReferenceGauge_)) ; Scale, then offset with half of normal gauge reference width
	
	(addDescriptionBelowOrigo description (* _threeQuarters_ _cantReferenceGauge_))
	(createAnnotativeBlockFromCurrentGraphics blockName)
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
		_POLYLINE_ 
			(list 0 (/ barLength -2)) "_W" barWidth barWidth (list 0 (/ barLength 2))
			_setPolylineWidth_ _zero_ _zero_ ; Always reset PLINE width after using non-standard settings
			_ENTER_
	)
	(command _SCALE_ _lastSelection_ _ENTER_ _origo_ scale)
)



(defun drawIsolatedJointSymbolWithEars ( q4 q3 q2 q1 scale offset / bar ear y x p11 p12 p13 p22 p32 p41 p42 p43 )
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
		bar		3.0							; Length of "bar" across track (Schematic size + 1:1 scale drawing size)
		ear		1.5							; Length of "ear" along track
		y		(* (/ bar 2) scale)			; Half-length of scaled bar across track
		x		(* ear scale)				; Length of scaled ear along track
		p11	(list (- x) (- (+ offset y)))
		p12	(list (+ 0) (- (+ offset y)))
		p13	(list (+ x) (- (+ offset y)))
		p22	(list (+ 0) (- (- offset y)))
		p32	(list (+ 0) (+ (- offset y)))
		p41	(list (- x) (+ (+ offset y)))
		p42	(list (+ 0) (+ (+ offset y)))
		p43	(list (+ x) (+ (+ offset y)))
	)
	(if (> (+ q1 q2) 0) (command _LINE_ p32 p42 _ENTER_)) ; upper bar
	(if (> (+ q3 q4) 0) (command _LINE_ p22 p12 _ENTER_)) ; lower bar
	(if (= q1 1) (command _LINE_ p42 p43 _ENTER_)) ; quadrant I
	(if (= q2 1) (command _LINE_ p42 p41 _ENTER_)) ; quadrant II
	(if (= q3 1) (command _LINE_ p12 p11 _ENTER_)) ; quadrant III
	(if (= q4 1) (command _LINE_ p12 p13 _ENTER_)) ; quadrant IV
	(if (= (+ q4 q3 q2 q1) 0) ; Add two small circles to "0000" symbol, meaning "UNFINISHED - not configured yet"
		(progn 
			(command _CIRCLE_ p12 y)
			(command _CIRCLE_ p42 y)
		)
	)
)
