;=========================================================================================================================
;
; Thumbnail Area.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for area selection (interlocking areas, contract parcels, paperspace layout frames etc)

(defun C:THUMBNAIL-AREA ( / )
	(THUMBNAIL-AREA)
)

(defun THUMBNAIL-AREA ( / blockName x y )
	(setq 
		blockName "NO-BN-2D-JBTFE-THUMBNAIL-AREA"
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
)
