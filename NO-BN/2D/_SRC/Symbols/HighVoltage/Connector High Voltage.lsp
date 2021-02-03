;=========================================================================================================================
;
; Connector High Voltage.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Electrical connection (between contact wires and / or catenary wires)

(defun C:CONNECTOR-HIGH-VOLTAGE ( / )
	(STROEMBRO "H") ; "C-Pacman" moving right C
	(STROEMBRO "V") ; "C-Pacman" moving left  )
	(SVEVENDE-KRYSS)
)



(defun STROEMBRO ( dir / blockName description r startAngle endAngle arrowLength arrowWidth )
	; See EH.705051 'Strømbro i avspenningsfelt'. Strømbro mellom to ledninger i et vekslingsfelt.
	; Symbol: A "C" with an arrow at each tip, which shall touch two catenary / contact wire alignments exactly.
	; Insertion direction is "both" in one of the wires (own Alignment).
	; Insertion point is the lowest tip. The "gap" always heads towards the location where the wires apparently cross.
	; Target alignment is the other contact wire (target alignment).
	; Acts as a permanent and good electrical connection object.
	;
	;    _____
	;   /     \
	;  |       V 
	;  |   .
	;  |       ^
	;   \_____/
	;
	(setq
		blockName (strcat "NO-BN-2D-JBTKL-FORBINDELSE-STROEMBRO" "-" dir)
		description (strcat "KL FORBINDELSE, STROEMBRO " dir)
		r 2.5
		startAngle 25.0
		endAngle (- 25.0)
		arrowLength 1.5
		arrowWidth 0.75
	)
	(setLayer layer_Zero)
	; Opening towards the right:
	(command
		_ARC_ 
			(strcat (rtos r) "<" (rtos startAngle)) 			; Start point
			(strcat (rtos r) "<" (rtos (+ 180.0 startAngle))) 	; Middle point
			(strcat (rtos r) "<" (rtos endAngle)) 				; End point
		_POLYLINE_ ; upper arrow
			(strcat (rtos r) "<" (rtos startAngle))
			_setPolylineWidth_ _zero_ arrowWidth						; Start width - end width
			(strcat "@" (rtos arrowLength) "<" (rtos (+ startAngle 105.0)))
			_setPolylineWidth_ _zero_ _zero_ ;reset polyline width after use
			_ENTER_
		_POLYLINE_ ; lower arrow
			(strcat (rtos r) "<" (rtos endAngle))
			_setPolylineWidth_ _zero_ arrowWidth
			(strcat "@" (rtos arrowLength) "<" (rtos (- endAngle 105.0)))
			_setPolylineWidth_ _zero_ _zero_ ;reset polyline width after use
			_ENTER_
	)
	(command
		_MOVE_ _selectAll_ _ENTER_
			(strcat (rtos r) "<" (rtos endAngle))
			_origo_
	)
	(if (= dir "V")
		(command _MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _eraseMirrorSource_)
	)
	(addDescriptionBelowOrigo description r)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun SVEVENDE-KRYSS ( / blockName radius )
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
		blockName "NO-BN-2D-JBTKL-FORBINDELSE-SVEVENDE-KRYSS"
		description "KL SVEVENDE KRYSS"
		x	(* (sqrt 0.5) 3.0)
		y	(* (sqrt 0.5) 3.0)
	)
	(drawStAndrewCross layer_Zero x y)
	(addDescriptionBelowOrigo description (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)


