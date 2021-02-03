;=========================================================================================================================
;
; Insertion point.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
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
;		_CIRCLE_ _origo_ radius
;		_LINE_ (list (- radius) 0) (list radius 0) _ENTER_
;		_LINE_ (list 0 (- radius)) (list 0 radius) _ENTER_
		_POINT_ _origo_ _ENTER_
	)
	(createSchematicBlockFromCurrentGraphics blockName)
)
