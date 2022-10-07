;=========================================================================================================================
;
; Thumbnail Unspecified.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
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
	(cond
		((= _ADM_ _XXGL_) (setq blockName (strcat _RC_ thumbnailInfix "-UNSPECIFIED"	)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _RC_ thumbnailInfix "-USPESIFISERT"	)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _RC_ thumbnailInfix "-NON-SPECIFIE"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _RC_ thumbnailInfix "-UNSPEZIFISCH"	)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _RC_ thumbnailInfix "-UNSPECIFIED"	)))
	)
	(command 
		_RECTANGLE_ "0,-3" "4,1"
		_CIRCLE_ "-1,-1" "3"
		_POLYGON_ "3" _specifyEdgeOfPolygon_ "-3,0" "3,0"
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
