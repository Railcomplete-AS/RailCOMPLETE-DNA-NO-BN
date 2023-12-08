;=========================================================================================================================
;
; Thumbnail Table.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail for pre-defined table object selection

(defun THUMBNAIL-TABLE ( / )
	(THUMBNAIL-TABLE)
)



(defun THUMBNAIL-TABLE (/ blockName x y) 
	(setq blockName (strcat _RC_ thumbnailInfix "-TABELL"	))
	(setq 
		x (/ 5.0 2)
		y (/ 3.0 2)
	)
	(command _RECTANGLE_ (list (- x) (- y)) (list x y)
		_LINE_ (list (- x) (* (/ y 3) 2)) (list x (* (/ y 3) 2)) _ENTER_
		_LINE_ (list (+ (- x) (* (/ x 5) 2)) y) (list (+ (- x) (* (/ x 5) 2)) (- y)) _ENTER_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
