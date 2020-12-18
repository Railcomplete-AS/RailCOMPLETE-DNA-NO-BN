;=========================================================================================================================
;
; Thumbnail Rail Profile.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Thumbnail for sweeped rail profile alignment selection (for 3D generation)

(defun C:THUMBNAIL-RAIL-PROFILE ( / )
	(ALIGNMENT-SKINNE)
)



(defun ALIGNMENT-SKINNE ( / blockName ) 
	(setq
		blockName (strcat "NO-BN-2D-JBTOB-THUMBNAIL-SKINNE")
	)
	(command
		"._PLINE" 
			(list 0 0) ; Top-of-rail
			(list 0.60  0.00)
			(list 1.10 -0.10)
			(list 1.30 -0.25)
			(list 1.45 -0.50)
			(list 1.50 -1.50)
			(list 0.75 -1.75)
			(list 0.60 -1.85)
			(list 0.45 -2.20)
			(list 0.35 -2.80)
			(list 0.35 -4.75)
			(list 0.40 -5.30)
			(list 0.60 -5.85)
			(list 2.00 -6.35)
			(list 2.85 -6.40)
			(list 3.00 -6.50)
			(list 3.00 -6.80)
			(list 2.95 -6.85)
			(list 0.00 -6.85)
			""
		"._MIRROR" "_ALL" "" "0,0" "0,1" "_NO"
		"._MOVE" "_ALL" "" (list 0 0) (list 0 3.5)
	)
	(command
		"._SCALE" "_ALL" "" (list 0 0) 1 ; Already drawn to a suitable scale (and all thumbnails auto-scale anyway...)
	)
	(createSchematicBlockFromCurrentGraphics blockName)
	blockName
)
