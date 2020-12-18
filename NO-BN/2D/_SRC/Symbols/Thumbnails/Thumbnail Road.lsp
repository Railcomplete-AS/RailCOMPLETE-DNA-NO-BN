;=========================================================================================================================
;
; Thumbnail Road.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Thumbnail for road alignment selection

(defun C:THUMBNAIL-ROAD ( / )
	(BILVEI)
)



(defun BILVEI ( / blockName ) 
  (setq blockName "NO-BN-2D-JBTUB-THUMBNAIL-BILVEI")
	(command "._LINE" (list 0 0) (list 0 1) ""
		"._LINE" (list 0 1.4) (list 0 2) ""
		"._LINE" (list 0 1.4) (list 0 2) ""
		"._LINE" (list 0 2.4) (list 0 2.8) ""
		"._LINE" (list 0 3.125) (list 0 3.375) ""
		"._LINE" (list 1.5 0) (list 0.5 3.375) ""
		"._MIRROR" "_LAST" "" (list 0 0) (list 0 1) "_NO"
		)
  (createSchematicBlockFromCurrentGraphics blockName)
  blockName
  )