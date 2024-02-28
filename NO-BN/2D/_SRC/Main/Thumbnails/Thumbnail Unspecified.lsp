;=========================================================================================================================
;
; Thumbnail Unspecified.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail for unspecified object selection (in use for abstract objects, or when 2D symbol has not yet been produced)

(defun THUMBNAIL-UNSPECIFIED ( / )
	(THUMBNAIL-UNSPECIFIED)
)



(defun THUMBNAIL-UNSPECIFIED ( / blockName ) 
	; A rectangle, a circle and a triangle - to be used as proxy symbols for anything that lacks a suitable symbol.
	(setq blockName (strcat _RC_ thumbnailInfix "-USPESIFISERT"	))
	(command 
		_RECTANGLE_ "0,-3" "4,1"
		_CIRCLE_ "-1,-1" "3"
		_POLYGON_ "3" _specifyEdgeOfPolygon_ "-3,0" "3,0"
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
