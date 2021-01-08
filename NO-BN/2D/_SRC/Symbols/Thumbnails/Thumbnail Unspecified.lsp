;=========================================================================================================================
;
; Thumbnail Unspecified.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for unspecified object selection (in use for abstract objects, or when 2D symbol has not yet been produced)

(defun C:THUMBNAIL-UNSPECIFIED ( / )
	(THUMBNAIL-UNSPECIFIED)
)



(defun THUMBNAIL-UNSPECIFIED (/ blockName x y) 
	(setq 
		blockName (strcat "NO-BN-2D-JBTFE-THUMBNAIL-UNSPECIFIED")
	)
	(command 
		"._RECTANGLE" "0,-3" "4,1"
		"._CIRCLE" "-1,-1" "3"
		"._POLYGON" "3" "EDGE" "-3,0" "3,0"
	)
	(createSchematicBlockFromCurrentGraphics blockName)
	blockName
)

