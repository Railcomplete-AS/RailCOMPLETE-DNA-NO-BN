;=========================================================================================================================
;
; Thumbnail Table.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for pre-defined table object selection

(defun C:THUMBNAIL-TABLE ( / )
	(TABLE-GENERELL)
)



(defun TABLE-GENERELL (/ blockName x y) 
	(setq blockName (strcat "NO-BN-2D-JBTFE-THUMBNAIL-TABELL-GENERELL")
		x (/ 5.0 2)
		y (/ 3.0 2)
	)
	(command _RECTANGLE_ (list (- x) (- y)) (list x y)
		_LINE_ (list (- x) (* (/ y 3) 2)) (list x (* (/ y 3) 2)) _ENTER_
		_LINE_ (list (+ (- x) (* (/ x 5) 2)) y) (list (+ (- x) (* (/ x 5) 2)) (- y)) _ENTER_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
)
