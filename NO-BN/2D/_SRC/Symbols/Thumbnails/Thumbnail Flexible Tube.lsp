;=========================================================================================================================
;
; Thumbnail Flexible Tube.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for flexible tube (cable conduit) alignment selection

(defun C:THUMBNAIL-FLEXIBLE-TUBE ( / )
	(ALIGNMENT-TREKKEROER)
)



(defun ALIGNMENT-TREKKEROER (/ blockName r1 r2) 
	(setq 
		blockName "NO-BN-2D-JBTUB-THUMBNAIL-TREKKEROER"
		r1 0.5
		r2 0.45
	)
	(command
		"._PLINE"
			(list 0 0) (list 1 0) 
			"_ARC" "_CE" (list 1.0 r1)
			"A" "180" "R" r2
			"A" "180" "-90" "R" r1
			"A" "180" "90" "R" r2
			"A" "180" "-90" "R" r1
			"A" "180" "90" "R" r2
			"A" "22" "-168" 
			""
	)
	(createSchematicBlockFromCurrentGraphics blockName)
	blockName
)

