;=========================================================================================================================
;
; Thumbnail Flexible Tube.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for flexible tube (cable conduit) alignment selection

(defun C:THUMBNAIL-FLEXIBLE-TUBE ( / )
	(ALIGNMENT-TREKKEROER)
)



(defun ALIGNMENT-TREKKEROER ( / blockName r1 r2)
	; Flexible tubing. Three rounds of coiled-up flexible tube.
	(setq 
		blockName "NO-BN-2D-JBTUB-THUMBNAIL-TREKKEROER"
		r1 0.5
		r2 0.45
	)
	(command
		_POLYLINE_
			_origo_ (list 1 0) 
			_setPolylineArcMode_
			_setPolylineArcCenter_ (list 1.0 r1)
			_setPolylineArcAngle_ _angle180_ _setPolylineArcRadius_ r2
			_setPolylineArcAngle_ _angle180_ _angleMinus90_ _setPolylineArcRadius_ r1
			_setPolylineArcAngle_ _angle180_ _angle90_ _setPolylineArcRadius_ r2
			_setPolylineArcAngle_ _angle180_ _angleMinus90_ _setPolylineArcRadius_ r1
			_setPolylineArcAngle_ _angle180_ _angle90_ _setPolylineArcRadius_ r2
			_setPolylineArcAngle_ 22  -168
			_ENTER_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
)
