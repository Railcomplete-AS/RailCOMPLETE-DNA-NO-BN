;=========================================================================================================================
;
; Insertion point.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Graphical symbol (circle with cross) to better see where the insertion point for a 2D symbol is. 

(defun C:INSERTION-POINT ( / radius blockName )
	(setq 
		radius 0.05
		blockName "NO-BN-2D-INSERTION-POINT" ;2D symbol block insertion point
	)
	(setLayer layer_InsertionPoint)
	(command 
		"._CIRCLE" "0,0" radius
		"._LINE" (list (- radius) 0) (list radius 0) ""
		"._LINE" (list 0 (- radius)) (list 0 radius) ""
	)
	(createSchematicBlockFromCurrentGraphics blockName)
)
