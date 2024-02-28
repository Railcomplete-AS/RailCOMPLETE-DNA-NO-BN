;=========================================================================================================================
;
; Thumbnail Area.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail for area selection (interlocking areas, contract parcels, paperspace layout frames etc)

(defun THUMBNAIL-AREA ( / blockName x y )
	(setq blockName (strcat _RC_ thumbnailInfix "-OMRAADE"	))
	(setq
		x 12
		y 9
	)
	(command 
		_LINE_ (list (* x -0.5) 0) (list (* x -0.3) 0) _ENTER_
		_LINE_ (list (* x -0.5) 0) (list (* x -0.5) (* y 0.2)) _ENTER_
		
		_LINE_ (list (* x 0.5) 0) (list (* x 0.3) 0) _ENTER_
		_LINE_ (list (* x 0.5) 0) (list (* x 0.5) (* y 0.2)) _ENTER_
		
		_LINE_ (list (* x -0.5) y) (list (* x -0.3) y) _ENTER_
		_LINE_ (list (* x -0.5) y) (list (* x -0.5) (* y 0.8)) _ENTER_
		
		_LINE_ (list (* x 0.5) y) (list (* x 0.3) y) _ENTER_
		_LINE_ (list (* x 0.5) y) (list (* x 0.5) (* y 0.8)) _ENTER_
		
		_LINE_ (list (* x 0.1) 0) (list (* x -0.1) 0) _ENTER_
		_LINE_ (list (* x 0.1) y) (list (* x -0.1) y) _ENTER_
		
		_LINE_ (list (* x -0.5) (* y 0.4)) (list (* x -0.5) (* y 0.6)) _ENTER_
		_LINE_ (list (* x 0.5) (* y 0.4)) (list (* x 0.5) (* y 0.6)) _ENTER_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
