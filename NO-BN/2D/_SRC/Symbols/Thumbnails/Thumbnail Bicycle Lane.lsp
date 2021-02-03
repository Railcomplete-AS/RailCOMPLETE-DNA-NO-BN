;=========================================================================================================================
;
; Thumbnail Bicycle Lane.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for bicycle lane alignment selection

(defun C:THUMBNAIL-BICYCLE-LANE ( / )
	(ALIGNMENT-SYKKELVEI)
)



(defun ALIGNMENT-SYKKELVEI ( / blockName ) 
	(setq blockName "NO-BN-2D-JBTUB-THUMBNAIL-SYKKELVEI")
	(command _POLYLINE_ _origo_ (list 6 10) (list 19 10) (list 11 0) _closedPolyline_
		_LINE_ (list 11 0) (list 5.25 11.5) _ENTER_
		_LINE_ (list 4.25 11.5) (list 7.25 11.5) _ENTER_
		_LINE_ (list 24 0) (list 18.25 11.5) _ENTER_
		_LINE_ (list 18.25 11.5) (list 20.5 12.25) _ENTER_
		_CIRCLE_ _origo_ "6.5"
		_CIRCLE_ (list 24 0) "6.5"
		_MOVE_ _selectAll_ _ENTER_ (list 11 0) _origo_
		_SCALE_ _selectAll_ _ENTER_ _origo_ _fifth_
	)
	(createSchematicBlockFromCurrentGraphics blockName)
	blockName
)


