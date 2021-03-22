;=========================================================================================================================
;
; Thumbnail Closed Cable Conduit.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for concrete-encapsulated tubes (alignment objects)

(defun C:THUMBNAIL-CLOSED-CABLE-CONDUIT ( / )
	(ALIGNMENT-ROERPAKKE)
)



(defun ALIGNMENT-ROERPAKKE (/ blockName x y r )
	(setq 
		blockName "NO-BN-2D-JBTUB-THUMBNAIL-ROERPAKKE"
		y 0.155
		x 0.45
		r 0.055
	)
	(command 
		_POLYLINE_ 
			(list (/ x 2.0) 0)
			(list (/ x 2.0) (* y 0.9))
			(list 0.1954 y)
			(list 0 y)
			_ENTER_
		_MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
		_CIRCLE_ (list 0 0.07) r
		_CIRCLE_ (list 0.14 0.07) r
		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
		_MIRROR_ _selectAll_ _ENTER_ _origo_ _xAxis_ _keepMirrorSource_
		;
		_SCALE_ _selectAll_ _ENTER_ _origo_ _ten_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
)
