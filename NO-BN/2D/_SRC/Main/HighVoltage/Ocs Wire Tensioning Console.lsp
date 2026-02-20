;=========================================================================================================================
;
; Ocs Wire Tensioning Console.lsp
;
; Copyright (c) 2015-2026 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
; 2022-09-08 CLFEY Updated symbols.
; 2026-02-19 CLFEY Updated and added symbols. File fissioned from Ocs Wire Tensioning Footplate.lsp
;
;=========================================================================================================================

; Force balancing using console on OCS pole - between wire tensioing balancer, guywire or spanner.

(defun OCS-WIRE-TENSIONING-CONSOLE ( / )
	(TraceLevel3 "CONSOLE-ON-OCS-POLE-FOR-WTB")				(CONSOLE-ON-OCS-POLE-FOR-WTB)				; Console on OCS pole / portal droparm / tunnel droparm, for WTB
	(TraceLevel3 "CONSOLE-ON-OCS-POLE-FOR-WTB-AND-WTB")		(CONSOLE-ON-OCS-POLE-FOR-WTB-AND-WTB)		; Console on OCS pole / portal droparm / tunnel droparm, for WTBx2
	(TraceLevel3 "CONSOLE-ON-OCS-POLE-FOR-GUYWIRE")			(CONSOLE-ON-OCS-POLE-FOR-GUYWIRE)			; Console on OCS pole / portal droparm / tunnel droparm, for GW
	(TraceLevel3 "CONSOLE-ON-OCS-POLE-FOR-WTB-AND-GUYWIRE")	(CONSOLE-ON-OCS-POLE-FOR-WTB-AND-GUYWIRE)	; Console on OCS pole / portal droparm / tunnel droparm, for WTB+GW
	(TraceLevel3 "CONSOLE-ON-OCS-POLE-FOR-SPANNER")			(CONSOLE-ON-OCS-POLE-FOR-SPANNER)			; Console on OCS pole / portal droparm / tunnel droparm, for spanner
	(TraceLevel3 "CONSOLE-ON-OCS-POLE-FOR-WTB-AND-SPANNER")	(CONSOLE-ON-OCS-POLE-FOR-WTB-AND-SPANNER)	; Console on OCS pole / portal droparm / tunnel droparm, for WTB+spanner
)



(defun CONSOLE-ON-OCS-POLE-FOR-WTB ( / blockName description x y r1 p1 )
	; Console for transmission of forces on OCS pole.
	; Connects one wire tensioning device to an OCS mast, without a counteracting force.
	; This symbol has no equivalent in the Bane NOR symbol library.
	;
	;     +-------------+
	;     |             |
	;     |             | 
	;     |     _._     | 	
	;     |    /   \    | 
	;     |   |  1  |   |		; The circle accomodates one wire tensioning device (balancer or fixed).
	;     |    \___/    |
	;     +-------------+		; The symbol shall be rotated by RC in runtime such that the circle is on the WTB side.
	;
	(setq
		blockName (strcat _OCS_ "KPM-" "KONSOLL-PAA-MAST-FOR-AVSPENNING")
		description "KL KRAFTAVLASTING, KONSOLL PAA MAST FOR AVSPENNING"
		x	1.500
		y	1.500
		r1	0.250
		p1  '(0.000 -0.500)
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawCircleAtPoint layDef_Zero p1 r1 _noWipeout_)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)		



(defun CONSOLE-ON-OCS-POLE-FOR-WTB-AND-WTB ( / blockName description x y r1 p1 r2 p2 )
	; Console for transmission of forces on OCS pole.
	; Connects forces from one wire tensioning device on each side of an OCS mast.
	; This symbol has no equivalent in the Bane NOR symbol library.
	;
	;     +-------------+
	;     |    /   \    |
	;     |   |  2  |   |
	;     |    \   /    | 
	;     |     =.=     | 	
	;     |    /   \    | 
	;     |   |  1  |   |		; Each circle accomodates one wire tensioning device (balancer or fixed).
	;     |    \___/    |
	;     +-------------+		; The symbol shall be rotated by RC in runtime such that the circle is on the WTB side.
	;
	(setq
		blockName (strcat _OCS_ "KPM-" "KONSOLL-PAA-MAST-FOR-AVSPENNING-OG-AVSPENNING")
		description "KL KRAFTAVLASTING, KONSOLL PAA MAST FOR AVSPENNING OG AVSPENNING"
		x	1.500
		y	1.500
		r1	0.250
		p1  '(0.000 -0.500)
		r2	0.250
		p2  '(0.000  0.500)
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawCircleAtPoint layDef_Zero p1 r1 _noWipeout_)
	(DrawCircleAtPoint layDef_Zero p2 r2 _noWipeout_)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)		



(defun CONSOLE-ON-OCS-POLE-FOR-GUYWIRE ( / blockName description x y p2 p3 p4 )
	; Console for transmission of forces on OCS pole.
	; Connects a guywire to an OCS mast, normally to counteract large lateral forces on the cantilevel due to important track curvature.
	; This symbol has no equivalent in the Bane NOR symbol library.
	;
	;     +-------------+
	;     |             |
	;     |             |
	;     |             | 
	;     |      .      | 	
	;     |   3-----4   | 
	;     |    \   /    |		; The triangle accomodates guywire.
	;     |     \ /     |
	;     +------2------+		; The symbol shall be rotated by RC in runtime such that the triangle is one the guywire side.
	;
	(setq
		blockName (strcat _OCS_ "KPM-" "KONSOLL-PAA-MAST-FOR-BARDUN")
		description "KL KRAFTAVLASTING, KONSOLL PAA MAST FOR BARDUN"
		x	1.500
		y	1.500
		p2	'( 0.000 -0.750)
		p3	'(-0.500 -0.205)
		p4	'( 0.500 -0.205)
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(Drawline layDef_Zero p2 p3)
	(Drawline layDef_Zero p3 p4)
	(Drawline layDef_Zero p4 p2)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)		



(defun CONSOLE-ON-OCS-POLE-FOR-WTB-AND-GUYWIRE ( / blockName description x y r1 p1 p2 p3 p4 )
	; Console for transmission of forces on OCS pole.
	; Connects a wire tensioning device and a guywire to an OCS mast.
	; This symbol has no equivalent in the Bane NOR symbol library.
	; If the OCS pole features a spanner, than another anchoring device needs to be inserted, due to their different Z coordinates on the OCS pole.
	;
	;     +-------------+
	;     |    /   \    | 
	;     |   |  1  |   |		; The circle accomodates one or more wire tensioning device (balancer or fixed).
	;     |    \___/    |
	;     |      .      | 	
	;     |   3-----4   | 
	;     |    \   /    |		; The triangle accomodates one guywire.
	;     |     \ /     |
	;     +------2------+		; The symbol shall be rotated by RC in runtime such that the circle is on the WTB side.
	;
	(setq
		blockName (strcat _OCS_ "KPM-" "KONSOLL-PAA-MAST-FOR-AVSPENNING-OG-BARDUN")
		description "KL KRAFTAVLASTING, KONSOLL PAA MAST FOR AVSPENNING OG BARDUN"
		x	1.500
		y	1.500
		r1	0.250
		p1  '( 0.000  0.500)
		p2	'( 0.000 -0.750)
		p3	'(-0.500 -0.205)
		p4	'( 0.500 -0.205)
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawCircleAtPoint layDef_Zero p1 r1 _noWipeout_)
	(Drawline layDef_Zero p2 p3)
	(Drawline layDef_Zero p3 p4)
	(Drawline layDef_Zero p4 p2)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)		



(defun CONSOLE-ON-OCS-POLE-FOR-SPANNER ( / blockName description x y p6 p7 p8 )
	; Console for transmission of forces on OCS pole.
	; Connects one spanner to an OCS mast, without the wire tensioning device - for use in a sharp curve where a guywire is not possible.
	; This symbol has no equivalent in the Bane NOR symbol library.
	;
	;     +-------------+
	;     |             |
	;     |             | 
	;     |      .      | 	
	;     |      6      | 
	;     |     / \     |		; The triangle accomodates a spanner.
	;     |    /   \    |
	;     +---7-----8---+		; The symbol shall be rotated by RC in runtime such that the triangle + circle is one the spanner side.
	;
	(setq
		blockName (strcat _OCS_ "KPM-" "KONSOLL-PAA-MAST-FOR-STREVER")
		description "KL KRAFTAVLASTING, KONSOLL PAA MAST FOR STREVER"
		x	1.500
		y	1.500
		p6  '( 0.000 -0.205)
		p7  '(-0.500 -0.750)
		p8  '( 0.500 -0.750)
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawCircleAtPoint layDef_Zero p5 r5 _noWipeout_)
	(DrawLine layDef_Zero p6 p7)
	(DrawLine layDef_Zero p6 p8)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)		



(defun CONSOLE-ON-OCS-POLE-FOR-WTB-AND-SPANNER ( / blockName description x y r1 p1 p6 p7 p8 )
	; Console for transmission of forces on OCS pole.
	; Connects a spanner and a wire tensioning device to an OCS mast, typically when terminating a CW in a narrow yard.
	; This symbol has no equivalent in the Bane NOR symbol library.
	;
	;     +-------------+
	;     |             |
	;     |             | 
	;     |     _._     | 	
	;     |    / 6 \    | 		; The circle accomodates one wire tensioning device (balancer or fixed).
	;     |   | /1\ |   |		; The triangle accomodates a spanner.
	;     |    /___\    |
	;     +---7-----8---+		; The symbol shall be rotated by RC in runtime such that the triangle + circle is on the spanner side.
	;
	(setq
		blockName (strcat _OCS_ "KPM-" "KONSOLL-PAA-MAST-FOR-AVSPENNING-OG-STREVER")
		description "KL KRAFTAVLASTING, KONSOLL PAA MAST FOR AVSPENNING OG STREVER"
		x	1.500
		y	1.500
		r1	0.250
		p1  '(0.000 -0.500)
		p6  '( 0.000 -0.205)
		p7  '(-0.500 -0.750)
		p8  '( 0.500 -0.750)
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawCircleAtPoint layDef_Zero p1 r1 _noWipeout_)
	(DrawLine layDef_Zero p6 p7)
	(DrawLine layDef_Zero p6 p8)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)		
