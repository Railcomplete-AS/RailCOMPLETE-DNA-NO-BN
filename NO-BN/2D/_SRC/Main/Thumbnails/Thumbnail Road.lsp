;=========================================================================================================================
;
; Thumbnail Road.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail for road alignment selection

(defun THUMBNAIL-ROAD ( / )
	(THUMBNAIL-ROAD)
)



(defun THUMBNAIL-ROAD ( / blockName ) 
	; A piece of two-lane road seen in perspective in the direction of driving.
	(setq blockName (strcat _RC_ thumbnailInfix "-BILVEI"			))
	(command 
		_LINE_ _origin_ (list 0 1) _ENTER_
		_LINE_ (list 0 1.4) (list 0 2) _ENTER_
		_LINE_ (list 0 1.4) (list 0 2) _ENTER_
		_LINE_ (list 0 2.4) (list 0 2.8) _ENTER_
		_LINE_ (list 0 3.125) (list 0 3.375) _ENTER_
		_LINE_ (list 1.5 0) (list 0.5 3.375) _ENTER_
		_MIRROR_ _lastSelection_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
