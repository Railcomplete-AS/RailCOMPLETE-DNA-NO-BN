;=========================================================================================================================
;
; Thumbnail Cable Duct Concrete Encased.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail for concrete-encased semi-flexible tubes (alignment objects)
; Norw.: 'Rørpakke'

(defun THUMBNAIL-CABLE-DUCT-CONCRETE-ENCASED ( / blockName x y r )
	(setq blockName (strcat _RC_ thumbnailInfix "-KABELFOERING-OMSTOEPTE-TREKKEROER"		))
	(setq 
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
		_MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
		_CIRCLE_ (list 0 0.07) r
		_CIRCLE_ (list 0.14 0.07) r
		_MIRROR_ _lastSelection_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_
		_MIRROR_ _selectAll_ _ENTER_ _origin_ _xAxis_ _keepMirrorSource_
		;
		_SCALE_ _selectAll_ _ENTER_ _origin_ _ten_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
