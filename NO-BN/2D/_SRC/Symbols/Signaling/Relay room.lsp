;=========================================================================================================================
;
; Relay room.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Relay room (interlocking's technical equipment room)

(defun C:RELAY-ROOM ()
	(RELEROM)
)

(defun RELEROM ( / blockName x y )
	(setq 
		blockName "NO-BN-2D-JBTSI-TEKNISK-BEBYGGELSE-RELEROM"
		x 12.0
		y 9.0
	)
	; Schematic symbol
	(command _RECTANGLE_ (list (/ x -2) 0) (list (/ x 2) y))
	(createSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)