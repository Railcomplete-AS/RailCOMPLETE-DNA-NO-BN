;=========================================================================================================================
;
; Thumbnail Bicycle Lane.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for bicycle lane alignment selection

(defun C:THUMBNAIL-BICYCLE-LANE ( / )
	(ALIGNMENT-SYKKELVEI)
)



(defun ALIGNMENT-SYKKELVEI ( / blockName ) 
	(setq blockName "NO-BN-2D-JBTUB-THUMBNAIL-SYKKELVEI")
	(command "._PLINE" (list 0 0) (list 6 10) (list 19 10) (list 11 0) "_CLOSE"
		"._LINE" (list 11 0) (list 5.25 11.5) ""
		"._LINE" (list 4.25 11.5) (list 7.25 11.5) ""
		"._LINE" (list 24 0) (list 18.25 11.5) ""
		"._LINE" (list 18.25 11.5) (list 20.5 12.25) ""
		"._CIRCLE" (list 0 0) "6.5"
		"._CIRCLE" (list 24 0) "6.5"
		"._MOVE" "_ALL" "" (list 11 0) (list 0 0)
		"._SCALE" "_ALL" "" (list 0 0) "0.2"
	)
	(createSchematicBlockFromCurrentGraphics blockName)
	blockName
)


