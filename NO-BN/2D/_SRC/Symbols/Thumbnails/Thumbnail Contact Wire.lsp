;=========================================================================================================================
;
; Thumbnail Contact Wire.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for contact wire sweeped profile alignment selection (for 3D generation)

(defun C:THUMBNAIL-CONTACT-WIRE ( / )
	(ALIGNMENT-KONTAKTLEDNING)
)

(defun ALIGNMENT-KONTAKTLEDNING ( / blockName radiusWire radiusSmall radiusLarge ) 
	(setq
		blockName (strcat "NO-BN-2D-JBTKL-THUMBNAIL-KONTAKTLEDNING")
		radiusWire 6.0
		radiusSmall 0.3
		radiusLarge 0.4
	)
	(command
		"._PLINE"
			(list 0 (- radiusWire)) 
			"_ARC" "_CE" "0,0" "_ANGLE" "93"
			"_RADIUS" radiusSmall "_ANGLE" "64" "122"
			"_LINE" "@3.075<154"
			"_ARC" "_RADIUS" radiusLarge "_ANGLE" "-104" "102"
			"_LINE" "@2.092<50"
			"_ARC" "_RADIUS" radiusSmall "_ANGLE" "84" "91.80051135"
				"_CE" "0,0" (list 0 radiusWire) 
			""
	)
	(command "._MIRROR" "_ALL" "" "0,0" "0,1" "_NO")
  	(drawHatchFromPoint 0.4 (list 0 0) 0 0)
	(command "._SCALE" "_ALL" "" (list 0 0) "0.2")
	(createSchematicBlockFromCurrentGraphics blockName)
	blockName
)
