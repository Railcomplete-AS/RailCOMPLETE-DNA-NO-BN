;=========================================================================================================================
;
; Connector Alignment Splice.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Symbols showing the topology of alignments (tracks, wires, cables, ducts, roads etc)

; Concerns schematic symbols that scale with paperspace drawing scale.

(defun C:CONNECTOR-SPLICE ( / )
	; Connects an alignment to another alignment when it has been recognized as a valid extension of the first alignment.
	; The symbols look like this: [>>], [><], [<>], and [<<]. The arrows represent each alignment's direction of increasing mileages.
	(setCadSystemDefaults)  
	(CONNECTOR-SPLICE 0 0) ; [>>]
	(CONNECTOR-SPLICE 0 1) ; [><]
	(CONNECTOR-SPLICE 1 0) ; [<>]
	(CONNECTOR-SPLICE 1 1) ; [<<]
)



(defun CONNECTOR-SPLICE ( leftArrow rightArrow / blockName x y leftArrowStart leftArrowMid rightArrowStart rightArrowMid )
	(setq
		blockName (strcat "NO-BN-2D-JBTOB-CONNECTOR-SPLICE-" (rtos leftArrow 2 0) "-" (rtos rightArrow 2 0))
		x (* _half_ 1.5)
		y (* _half_ 1.0)
	)
	(command
		_LINE_ (list (- x) (- y)) (list (- x) y) _ENTER_
		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
		_LINE_ (list (- x) (/ y 2.0)) (list x (/ y 2.0)) _ENTER_
		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _xAxis_ _keepMirrorSource_
	)
	(if (= leftArrow 0)
		(setq 
			leftArrowStart (- (* 2.0 (/ x 3.0)))
			leftArrowMid (- (/ x 3.0))
		)
		;else:
		(setq 
			leftArrowStart (- (/ x 3.0))
			leftArrowMid (- (* 2.0 (/ x 3.0)))
		)
	)
	(if (= rightArrow 0)
		(setq 
			rightArrowStart (/ x 3.0)
			rightArrowMid (* 2.0 (/ x 3.0))
		)
		(setq 
			rightArrowStart (* 2.0 (/ x 3.0))
			rightArrowMid (/ x 3.0)
		)
	)
	(command
		_LINE_
			(list leftArrowStart (/ y 2.0))
			(list leftArrowMid 0)
			(list leftArrowStart (- (/ y 2.0))) 
			_ENTER_
		_LINE_
			(list rightArrowStart (/ y 2.0))
			(list rightArrowMid 0)
			(list rightArrowStart (- (/ y 2.0))) 
			_ENTER_
	)
	(addDescriptionBelowOrigo description y)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
