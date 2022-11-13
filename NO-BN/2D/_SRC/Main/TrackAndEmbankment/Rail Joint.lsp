;=========================================================================================================================
;
; Rail joint.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Isolated rail joint (showing where the signal current is flowing)
;
; Test cases:
;		(setq  q4 1  q3 0  q2 1  q1 1)
;		(RAILJOINT q4 q3 q2 1)


(defun RAIL-JOINTS ( / ) 
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
	(TraceLevel3 "RAILJOINT 0")	(RAILJOINT 0 0 0 0) ; No preceding signal current, no following signal current: Planned but yet UNFINISHED isolation.
	(TraceLevel3 "RAILJOINT 1")	(RAILJOINT 0 0 0 1)
	(TraceLevel3 "RAILJOINT 2")	(RAILJOINT 0 0 1 0)
	(TraceLevel3 "RAILJOINT 3")	(RAILJOINT 0 0 1 1)
	(TraceLevel3 "RAILJOINT 4")	(RAILJOINT 0 1 0 0)
	(TraceLevel3 "RAILJOINT 5")	(RAILJOINT 0 1 0 1)
	(TraceLevel3 "RAILJOINT 6")	(RAILJOINT 0 1 1 0)
	(TraceLevel3 "RAILJOINT 7")	(RAILJOINT 0 1 1 1)
	(TraceLevel3 "RAILJOINT 8")	(RAILJOINT 1 0 0 0)
	(TraceLevel3 "RAILJOINT 9")	(RAILJOINT 1 0 0 1)
	(TraceLevel3 "RAILJOINT A")	(RAILJOINT 1 0 1 0)
	(TraceLevel3 "RAILJOINT B")	(RAILJOINT 1 0 1 1)
	(TraceLevel3 "RAILJOINT C")	(RAILJOINT 1 1 0 0)
	(TraceLevel3 "RAILJOINT D")	(RAILJOINT 1 1 0 1)
	(TraceLevel3 "RAILJOINT E")	(RAILJOINT 1 1 1 0)
	(TraceLevel3 "RAILJOINT F")	(RAILJOINT 1 1 1 1)
)



(defun RAILJOINT ( q4 q3 q2 q1 / blockName description )
	;
	; Isolated joint (Bane NOR: 'Skinneskjøt').
	;
	; The isolated joint symbol has several 'views'. Schematic symbols hold three schematic views. Two are suitable for Bane NOR 1-line track drawings, one is suitable for 2-line with
	; 9 drawing units (meters) between rails. The geo symbols contain the same three viewss
	;
	; The symbols' insertion points are always centered in the track.
	;
	; In schematic plan view, we assume a 1-line representation of the centre track. 
	; Both schematic symbols and 1:500 symbols feature a 2.5 x 0.15 'bar' across the track.
	;
	; In cable plan view, we assume a 1-line representation of the centre track.
	; Schematic symbols feature a 3 meter 'bar' across the track and 1.5 meter 'ears' along the track in the direction of track circuit current.
	; Annotative symbol feature a 0.75 line from alignment axis to the rail where the sensor is actually located.
	; 
	; In track isolation and return current plan view, we assume a 2-line representation of the centre track. Schematic and 1:500 symbols feature
	; a 3.0 meter 'bar' across the relevant rail pluss a 1.5 meter 'ear' along that rail in the direction of track circuit current presence.
	; If both rails have isolations at the same mileage, then 
	; 
	; The geo symbols support all three views mentioned above, but adapted to normal gauge distance between rails in 2-line track view mode.
	; You can toggle between 1-line and 2-line mode in RailCOMPLETE using the command RC-ShowTwoRails.
	;
	(setq blockName1 (strcat _TRK_ "SKJ-" "SKINNESKJOET"		))
	(setq description1 (strcat "SKINNESKJ" _uOSLASH_ "T"		))
	(setq
		blockName	(strcat blockName1 "-" (rtos q4 2 0) "-" (rtos q3 2 0) "-" (rtos q2 2 0) "-" (rtos q1 2 0))
		description (strcat description1 ", QUADRANT IV-III-II-I = " (rtos q4 2 0) "-" (rtos q3 2 0) "-" (rtos q2 2 0) "-" (rtos q1 2 0))
	)
	
	; Schematic graphics...
	; Schematic 1-line signaling / schematic plan ('skjematisk plan') view
	(SetLayer layDef_View_SchematicPlan)
	(DrawIsolatedJointThickBarSymbol _one_)

	; Schematic 1-line signaling / cable plan ('plan og kabelplan') view
	(SetLayer layDef_View_CablePlan)
	(DrawIsolatedJointSymbolWithEars q4 q3 q2 q1 _one_ 0.0) ; No scaling, no offset from centre track

	; Schematic 2-line signaling / track insulation plan OR high voltage / return current plan view
	(SetLayer layDef_View_TrackIsolationPlan)
	(DrawIsolatedJointSymbolWithEars q4 q3 q2 q1 _one_ (* _half_ _schematicGauge_)) ; No scaling, then offset with half of schematic gauge from centre track

	; Create schematic symbol
	(AddDescriptionBelowOrigin description (* _threeQuarters_ _schematicGauge_))
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative graphics...
	; Geo 1-line signaling / schematic plan ('skjematisk plan') view
	(SetLayer layDef_View_SchematicPlan)
	(DrawIsolatedJointThickBarSymbol _one_)

	; Geo 1-line signaling / cable plan ('plan og kabelplan') view
	(SetLayer layDef_View_CablePlan)
	(DrawIsolatedJointSymbolWithEars q4 q3 q2 q1 _one_ 0.0) ; No scaling, no offset from centre track

	; Geo 2-line signaling / return current view showing a bar with ear(s), centered on the left or right rail
	(SetLayer layDef_View_TrackIsolationPlan)
	(DrawIsolatedJointSymbolWithEars q4 q3 q2 q1 _one_ (* _two_ _railHeadDistance_)) ; Will display correctly as 2-line symbol in scale 4:1
	
	; Create geographic symbol
	(AddDescriptionBelowOrigin description (* _three_ _railHeadDistance_))
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



;==========================
; Draw...X...() functions
;==========================
(defun DrawIsolatedJointThickBarSymbol ( scale / barLength barWidth )
	;        _
	;       | |       A thick bar (polyline) perp to track
	;       | | 
	; - - - |.| - - - Insertion point in track centre line
	;       | |
	;       |_|
	;
	(setq
		barLength	2.5			; Length of thick bar across track
		barWidth	0.15		; Width of bar across track
	)
	(command 
		_POLYLINE_ 
			(list 0 (/ barLength -2))
			_setPolylineWidth_ barWidth barWidth (list 0 (/ barLength 2))
			_setPolylineWidth_ _zero_ _zero_ ; Always reset PLINE width after using non-standard settings
			_ENTER_
	)
	(command _SCALE_ _lastSelection_ _ENTER_ _origin_ scale)
)



(defun DrawIsolatedJointSymbolWithEars ( q4 q3 q2 q1 scale offset / bar ear y x p11 p12 p13 p22 p32 p41 p42 p43 )
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
	; Note: If offset is zero, then point 22 equals point 32, making a continued vertical line from 12 to 42.
	;
	; Example: "D" = "1101", offset=0, 1-line drawing:
	; ------------------------------------------------
	; q2=0   +-- q1=1
	;        |
	;        .       1-line track. Insertino point in centre track.  ---> Direction of increasing mileage
	;        |
	; q3=1 --+-- q4=1
	;
	;
	; Example: "D" = "1101", offset=0, 2-line drawing:
	; ------------------------------------------------
	; q2=0   +-- q1=1
	;        |
	;  ......|.......left rail.......
	;        |
	;        |
	;        
	;        .      Insertion point in centre track.  ---> Direction of increasing mileage
	;               
	;        |
	;        |
	;  ......|.......right rail......
	;        |
	; q3=1 --+-- q4=1
	;
	(setq
		bar		(* _two_ _railHeadDistance_)	; Length of "bar" across track (Schematic size + 1:1 scale drawing size)
		ear		_railHeadDistance_				; Length of "ear" along track
		y		(* _half_ bar scale)			; Half-length of scaled bar across track
		x		(* ear scale)					; Length of scaled ear along track
		p11	(list (- x) (- (+ offset y)))
		p12	(list (+ 0) (- (+ offset y)))
		p13	(list (+ x) (- (+ offset y)))
		p22	(list (+ 0) (- (- offset y)))
		p32	(list (+ 0) (+ (- offset y)))
		p41	(list (- x) (+ (+ offset y)))
		p42	(list (+ 0) (+ (+ offset y)))
		p43	(list (+ x) (+ (+ offset y)))
	)
	(if (> (+ q1 q2) 0) (command _LINE_ p32 p42 _ENTER_)) ; bar across rail
	(if (> (+ q3 q4) 0) (command _LINE_ p22 p12 _ENTER_)) ; bar across rail

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
