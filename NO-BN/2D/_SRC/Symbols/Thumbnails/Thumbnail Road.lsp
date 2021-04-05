;=========================================================================================================================
;
; Thumbnail Road.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for road alignment selection

(defun C:THUMBNAIL-ROAD ( / )
	(ROAD)
)



(defun ROAD ( / blockName ) 
	; A piece of two-lane road seen in perspective in the direction of driving.
	(setq blockName "NO-BN-2D-JBTUB-THUMBNAIL-ROAD")
	(command 
		_LINE_ _origo_ (list 0 1) _ENTER_
		_LINE_ (list 0 1.4) (list 0 2) _ENTER_
		_LINE_ (list 0 1.4) (list 0 2) _ENTER_
		_LINE_ (list 0 2.4) (list 0 2.8) _ENTER_
		_LINE_ (list 0 3.125) (list 0 3.375) _ENTER_
		_LINE_ (list 1.5 0) (list 0.5 3.375) _ENTER_
		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
)
