;=========================================================================================================================
;
; Thumbnail Cable Duct Flexible Tube.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail for flexible tube (cable conduit) alignment selection

(defun THUMBNAIL-CABLE-DUCT-FLEXIBLE-TUBE ( / blockName r1 r2)
	; Flexible tubing. Three rounds of coiled-up flexible tube.
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _RC_ thumbnailInfix "-CABLE-DUCT-FLEXIBLE-TUBE"			)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _RC_ thumbnailInfix "-KABELFOERING-TREKKEROER"			)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _RC_ thumbnailInfix "-CONDUIT-DE-CABLE-TUYAU-FLEXIBLE"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _RC_ thumbnailInfix "-KABELVERLEGUNG-FLEXIBLER-SLAUCH"	)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _RC_ thumbnailInfix "-CABLE-DUCT-FLEXIBLE-TUBE"			)))
	)
	(setq 
		r1 0.5
		r2 0.45
	)
	(command
		_POLYLINE_
			_origin_ (list 1 0) 
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
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
