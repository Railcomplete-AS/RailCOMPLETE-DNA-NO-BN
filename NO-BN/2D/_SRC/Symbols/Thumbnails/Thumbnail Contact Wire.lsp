;=========================================================================================================================
;
; Thumbnail Contact Wire.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for contact wire sweeped profile alignment selection (for 3D generation)

(defun C:THUMBNAIL-CONTACT-WIRE ( / )
	(ALIGNMENT-KONTAKTLEDNING)
)

(defun ALIGNMENT-KONTAKTLEDNING ( / blockName radiusWire radiusSmall radiusLarge ) 
	(setq
		blockName (strcat "NO-BN-2D-JBTKL-THUMBNAIL-KONTAKTLEDNING")
		radiusWire 6.0
		radiusSmall 0.3
		radiusLarge 0.4
	)
	(command
		_POLYLINE_
			(list 0 (- radiusWire)) 
			_setPolylineArcMode_
			_setPolylineArcCenter_ _origo_
			_setPolylineArcAngle_ "93"
			_setPolylineArcRadius_ radiusSmall
			_setPolylineArcAngle_ "64" "122"
			_setPolylineLineMode_
			"@3.075<154"
			_setPolylineArcMode_
			_setPolylineArcRadius_ radiusLarge 
			_setPolylineArcAngle_ "-104" "102"
			_setPolylineLineMode_
			"@2.092<50"
			_setPolylineArcMode_
			_setPolylineArcRadius_ radiusSmall 
			_setPolylineArcAngle_ "84" "91.80051135"
			_setPolylineArcCenter_ _origo_ (list 0 radiusWire) 
			_ENTER_
	)
	(command _MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_)
  	(drawHatchFromPoint 0.4 _origo_ 0 0)
	(command _SCALE_ _selectAll_ _ENTER_ _origo_ _fifth_)
	(createSchematicBlockFromCurrentGraphics blockName)
	blockName
)
