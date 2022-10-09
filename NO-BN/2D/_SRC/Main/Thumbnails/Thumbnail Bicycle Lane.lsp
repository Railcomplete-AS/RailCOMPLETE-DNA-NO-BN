;=========================================================================================================================
;
; Thumbnail Bicycle Lane.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail for bicycle lane alignment selection

(defun THUMBNAIL-BICYCLE-LANE ( / blockName ) 
	(setq blockName (strcat _RC_ thumbnailInfix "-SYKKELSTI"		))
	(command _POLYLINE_ _origin_ (list 6 10) (list 19 10) (list 11 0) _closedPolyline_
		_LINE_ (list 11 0) (list 5.25 11.5) _ENTER_
		_LINE_ (list 4.25 11.5) (list 7.25 11.5) _ENTER_
		_LINE_ (list 24 0) (list 18.25 11.5) _ENTER_
		_LINE_ (list 18.25 11.5) (list 20.5 12.25) _ENTER_
		_CIRCLE_ _origin_ "6.5"
		_CIRCLE_ (list 24 0) "6.5"
		_MOVE_ _selectAll_ _ENTER_ (list 11 0) _origin_
		_SCALE_ _selectAll_ _ENTER_ _origin_ _fifth_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
