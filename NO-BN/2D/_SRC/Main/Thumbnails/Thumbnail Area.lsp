;=========================================================================================================================
;
; Thumbnail Area.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail for area selection (interlocking areas, contract parcels, paperspace layout frames etc)

(defun THUMBNAIL-AREA ( / blockName x y )
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _RC_ thumbnailInfix "-AREA"		))) 
		((= _ADM_ _NOBN_) (setq blockName (strcat _RC_ thumbnailInfix "-OMRAADE"	))) 
		((= _ADM_ _FRSR_) (setq blockName (strcat _RC_ thumbnailInfix "-REGION"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _RC_ thumbnailInfix "-REGION"	)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _RC_ thumbnailInfix "-AREA"		))) 
	)
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
