;=========================================================================================================================
;
; Thumbnail Earthing Wire.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail for earthing wire alignment selection

(defun THUMBNAIL-EARTHING-WIRE ( / )
	(THUMBNAIL-EARTHING-WIRE)
)


(defun THUMBNAIL-EARTHING-WIRE ( / blockName ) 
	(setq blockName (strcat _RC_ thumbnailInfix "-JORDLEDER"	))
	; - - - jl - - -
	(command 
		; - - -
		_LINE_ (list 0 10) (list 5 10) _ENTER_		
		_LINE_ (list 10 10) (list 15 10) _ENTER_
		_LINE_ (list 20 10) (list 25 10) _ENTER_
	
		; j
		_LINE_ (list 30 15) (list 30 12) _ENTER_
		_POLYLINE_ 
			(list 28 10)
			(list 31 10)
			(list 31 2)
			(list 29 0)
			(list 27 0)
			_ENTER_
	
		; l
		_POLYLINE_ 
			(list 34 15)
			(list 36 15)
			(list 36 4)
			(list 34 4)
			(list 38 4)
			_ENTER_

		; - - -
		_LINE_ (list 40 10) (list 45 10) _ENTER_
		_LINE_ (list 50 10) (list 55 10) _ENTER_
		_LINE_ (list 60 10) (list 65 10) _ENTER_
	)
	(command 
		; Scale down and move:
		_SCALE_ _selectAll_ _ENTER_ _origin_ _tenth_
		_MOVE_ _selectAll_ _ENTER_ (list 2.5 0.5) _origin_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
