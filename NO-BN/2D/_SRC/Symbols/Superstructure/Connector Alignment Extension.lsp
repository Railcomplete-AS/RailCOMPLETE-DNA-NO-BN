;=========================================================================================================================
;
; Connector Alignment Extension.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Symbols showing the topology of alignments (tracks, wires, cables, ducts, roads etc)

; Concerns schematic symbols that scale with paperspace drawing scale.

(defun C:CONNECTOR-ALIGNMENT-EXTENSION ( / )
	; Connects an alignment to another alignment when it has been recognized as a valid extension of the first alignment.
	; The symbols look like this: [>>], [><], [<>], and [<<]. The arrows represent each alignment's direction of increasing mileages.
	(setCadSystemDefaults)  
	(CONNECTOR-ALIGNMENT-EXTENSION 0 0) ; [>>]
	(CONNECTOR-ALIGNMENT-EXTENSION 0 1) ; [><]
	(CONNECTOR-ALIGNMENT-EXTENSION 1 0) ; [<>]
	(CONNECTOR-ALIGNMENT-EXTENSION 1 1) ; [<<]
)



(defun CONNECTOR-ALIGNMENT-EXTENSION ( leftArrow rightArrow / blockName x y leftArrowStart leftArrowMid rightArrowStart rightArrowMid )
	(setq
		blockName (strcat "NO-BN-2D-JBTOB-CONNECTOR-ALIGNMENT-EXTENSION-" (rtos leftArrow 2 0) "-" (rtos rightArrow 2 0))
		x (/ 1.5 2.0)
		y (/ 1.0 2.0)
	)
	(command
		"._LINE" (list (- x) (- y)) (list (- x) y) ""
		"._MIRROR" "_LAST" "" (list 0 0) (list 0 1) "_NO"
		"._LINE" (list (- x) (/ y 2.0)) (list x (/ y 2.0)) ""
		"._MIRROR" "_LAST" "" (list 0 0) (list 1 0) "_NO"
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
		"._LINE"
			(list leftArrowStart (/ y 2.0))
			(list leftArrowMid 0)
			(list leftArrowStart (- (/ y 2.0))) 
			""
		"._LINE"
			(list rightArrowStart (/ y 2.0))
			(list rightArrowMid 0)
			(list rightArrowStart (- (/ y 2.0))) 
			""
	)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _one_ blockName)
)
