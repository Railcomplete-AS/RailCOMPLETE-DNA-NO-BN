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
	(AREA-GENERELL)
)

(defun AREA-GENERELL (/ blockName x y)
	(setq x 12
		  y 9
		  blockName "NO-BN-2D-JBTFE-THUMBNAIL-OMRAADE-GENERELL"
		  )
	(command "._Line" (list (* x -0.5) 0) (list (* x -0.3) 0) ""
			 "._Line" (list (* x -0.5) 0) (list (* x -0.5) (* y 0.2)) ""
			 
			 "._Line" (list (* x 0.5) 0) (list (* x 0.3) 0) ""
			 "._Line" (list (* x 0.5) 0) (list (* x 0.5) (* y 0.2)) ""
			 
			 "._Line" (list (* x -0.5) y) (list (* x -0.3) y) ""
			 "._Line" (list (* x -0.5) y) (list (* x -0.5) (* y 0.8)) ""
			 
			 "._Line" (list (* x 0.5) y) (list (* x 0.3) y) ""
			 "._Line" (list (* x 0.5) y) (list (* x 0.5) (* y 0.8)) ""
			 
			 "._Line" (list (* x 0.1) 0) (list (* x -0.1) 0) ""
			 "._Line" (list (* x 0.1) y) (list (* x -0.1) y) ""
			 
			 "._Line" (list (* x -0.5) (* y 0.4)) (list (* x -0.5) (* y 0.6)) ""
			 "._Line" (list (* x 0.5) (* y 0.4)) (list (* x 0.5) (* y 0.6)) ""
			)
	(createSchematicBlockFromCurrentGraphics blockName)
	blockName
)
