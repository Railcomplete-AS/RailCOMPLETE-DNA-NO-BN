;=========================================================================================================================
;
; Thumbnail Closed Cable Conduit.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Thumbnail for concrete-encapsulated tubes (alignment objects)

(defun C:THUMBNAIL-CLOSED-CABLE-CONDUIT ( / )
	(ALIGNMENT-ROERPAKKE)
)



(defun ALIGNMENT-ROERPAKKE (/ blockName x y r) 
  (setq blockName "NO-BN-2D-JBTUB-THUMBNAIL-ROERPAKKE"
		y 0.155
		x 0.45
		r 0.055
	)
	(command 
		"._PLINE" 
			(list (/ x 2.0) 0)
			(list (/ x 2.0) (* y 0.9))
			(list 0.1954 y)
			(list 0 y)
			""
		"._MIRROR" "_ALL" "" "0,0" "0,1" "_NO"
		"._CIRCLE" (list 0 0.07) r
		"._CIRCLE" (list 0.14 0.07) r
		"._MIRROR" "_LAST" "" "0,0" "0,1" "_NO"
		"._MIRROR" "_ALL" "" "0,0" "1,0" "_NO"
		;
		"._SCALE" "_ALL" "" (list 0 0) "10"
	)
	(createSchematicBlockFromCurrentGraphics blockName)
	blockName
)

