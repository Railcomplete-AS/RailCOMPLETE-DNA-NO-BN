;=========================================================================================================================
;
; Ocs High Voltage Wire Connector.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Electrical connection (between contact wires and / or catenary wires)
; Floating crossing in switches.

(defun OCS-HIGH-VOLTAGE-WIRE-CONNECTOR ( / )
	; Implemented for all administrations:

	; Specific to this administration:
	(TraceLevel3 "KL-STROEMBRO-H")		(KL-STROEMBRO "H") ; "C-Pacman" moving right C
	(TraceLevel3 "KL-STROEMBRO-V")		(KL-STROEMBRO "V") ; "C-Pacman" moving left  )
	(TraceLevel3 "KL-SVEVENDE-KRYSS")	(KL-SVEVENDE-KRYSS) ; Floating crossing (where wires cross in a turnout/point/switch)
)



(defun KL-STROEMBRO ( dir / blockName description r startAngle endAngle arrowLength arrowWidth p1 p2 p3 p4 p5 )
	; See EH.705051 'Strømbro i avspenningsfelt'. Strømbro mellom to ledninger i et vekslingsfelt.
	; Symbol: A "C" with an arrow at each tip, which shall touch two catenary / contact wire alignments exactly.
	; Insertion direction is "both" in one of the wires (own Alignment).
	; Insertion point is the lowest tip. The "gap" always heads towards the location where the wires apparently cross.
	; Target alignment is the other contact wire (target alignment).
	; Acts as a permanent and good electrical connection object.
	;
	;    _____
	;   /     \
	;  |      \4/
	;  |       1 
	;  |   .
	;  2       3 
	;  |      /5\
	;   \_____/
	;
	(setq
		blockName	(strcat _OCS_ "FOR-" "FORBINDELSE-STROEMBRO" "-" dir)
		description	(strcat "KL FORBINDELSE, STROEMBRO " dir)
		r 2.5
		startAngle	25.0
		endAngle	(- 25.0)
		arrowLength	1.5
		arrowWidth	0.75
		p1	(strcat (rtos r) "<" (rtos startAngle)) 			; Start point
		p2	(strcat (rtos r) "<" (rtos (+ 180.0 startAngle))) 	; Middle point
		p3	(strcat (rtos r) "<" (rtos endAngle)) 				; End point
	
		p4	(strcat (rtos r) "<" (rtos (+ startAngle 34.377))) ; Amounts to 360*(arrowLength/(2*pi*r))
		p5	(strcat (rtos r) "<" (rtos (- endAngle 34.377)))
	)
	(SetLayer layDef_Zero)
	; Opening towards the right:
	(command _ARC_ p1 p2 p3 _ENTER_)
	(command
		_POLYLINE_ ; upper arrow
			p4
			_setPolylineWidth_ arrowWidth _lwZero_		; Start width -> end width
			p1
			_setPolylineWidth_ _lwZero_ _lwZero_		; Reset polyline width after use
			_ENTER_
	)
	(command
		_POLYLINE_ ; lower arrow
			p5
			_setPolylineWidth_ arrowWidth _zero_
			p3
			_setPolylineWidth_ _zero_ _zero_
			_ENTER_
	)
	(command _MOVE_ _selectAll_ _ENTER_ p1 _origin_)
	(if (= dir "V") (MirrorAboutYaxis _eraseMirrorSource_))
	(AddDescriptionBelowOrigin description (HalfOf r))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun KL-SVEVENDE-KRYSS ( / blockName description x y  )
	; EH.707403 'Svevende kontakttrådkryss for 12 mm2 kontakttråd'. 2500mm tråd med hengetrådklemme i hver ende.
	; Strømper på bærelinene om nødvendig.
	; Symbol: A 90 degrees "X" at the apparent intersection between two contact wire alignments.
	; Default insertion direction is "both" in one of the wires (own Alignment).
	; Target alignment is the other contact wire (target alignment).
	; Acts as a sporadical and bad electrical connection object.
	;
	; TL   TR
	;   \ /
	;    .
	;   / \
	; BL   BR
	;
	(setq
		blockName	(strcat _OCS_ "SVE-" "FORBINDELSE-SVEVENDE-KRYSS")
		description "KL SVEVENDE KRYSS"
		x	(* (sqrt 0.5) 3.0)
		y	(* (sqrt 0.5) 3.0)
	)
	(DrawStAndrewCross layDef_Zero x y)
	(AddDescriptionBelowOrigin description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
