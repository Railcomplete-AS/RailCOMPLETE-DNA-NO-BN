;=========================================================================================================================
;
; OCS Mast.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Overhead Catenary System mast

(defun C:OCS-MAST ( / )

	(subSubStep "BJELKEMAST")	(BJELKEMAST)
	(subSubStep "GMBMAST")		(GMBMAST)
	(subSubStep "GITTERMAST-B")	(GITTERMAST-B)
	(subSubStep "GITTERMAST-H")	(GITTERMAST-H)
	(subSubStep "BETONGMAST")	(BETONGMAST)
	(subSubStep "TREMAST")		(TREMAST)
)



(defun BJELKEMAST ( / blockName description x y w1 w2 r )
	(setq
		blockName "NO-BN-2D-JBTKL-MAST-BJELKEMAST"
		description "KL BJELKEMAST TYPE HEB"
		x (/ 3.0 2) ;Halvbredde
		y (/ 3.0 2) ;Halvhøyde
		w1 0.09 	;Halvbredde "livet"
		w2 0.20     ;hel tykkelse "tverr-beina"
		r 0.15		;bøyradius i "innerhjørner" på HEB bjelken
	)
	; Draw the first quadrant of an "I" shaped beam, then mirror+mirror:
	(setLayer layer_Zero)
	(command
		"._PLINE"
			(list w1 0)
			(list w1 (- y (+ w2 r)))
			"_ARC"
			(list (+ w1 r) (- y w2))
			"_LINE" 
			(list x (- y w2))
			(list x y)
			(list 0 y)
			""
		"._MIRROR" "_LAST" "" (list 0 0) (list 0 1) "_NO"
		"._MIRROR" "_ALL" "" (list 0 0) (list 1 0) "_NO"
	)
	(drawHatchFromPoint _denseHatch__ (list 0 r) (/ 3.14 4) 0)
	(addDescriptionBelowOrigo description y)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun GMBMAST ( / blockName description x y r center_x center_y )
	(setq
		blockName "NO-BN-2D-JBTKL-MAST-GMBMAST"
		description "KL BJELKEMAST TYPE HEB/GMB"
		x 0.85 ;bredde
		y 3.00 ;høyde
		center_x 1.713
		center_y 1.520
	)
	(setLayer layer_Zero)
	(command
		"._PLINE" 
			(list 0 (/ y 2))
			(list 0 0)
			(list x 0)
			(list x y)
			"_ARC" "_CE" (list center_x center_y)
			(list 0 (/ y 2))
			""
	)
	(moveLeft (halfOf x))
	(addDescriptionBelowOrigo description 0)
	(moveDown (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun GITTERMAST-B ( / blockName description x y )
	(setq 
		blockName "NO-BN-2D-JBTKL-MAST-GITTERMAST-B"
		description "KL GITTERMAST TYPE B2/B3/B4/B5/B6"
		x 1.05
		y 3.0
	)
	(drawBox layer_Zero x y _noWipeout_)
	(addDescriptionBelowOrigo description (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun GITTERMAST-H ( / blockName description x y )
	(setq
		blockName "NO-BN-2D-JBTKL-MAST-GITTERMAST-H"
		description "KL GITTERMAST TYPE H3/H5/H6"
		x 3.0
		y 3.0
	)
	(drawBox layer_Zero x y _noWipeout_)
	(addDescriptionBelowOrigo description (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun BETONGMAST ( / blockName description x y )
	(setq
		blockName "NO-BN-2D-JBTKL-MAST-BETONGMAST"
		description "KL BETONGMAST"
		x 3.0
		y 3.0
	)
	(drawBox layer_Zero x y _noWipeout_)
	(drawHatch _mediumHatch_)
	(addDescriptionBelowOrigo description (halfOf y))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun TREMAST ( / blockName description r )
	(setq 
		blockName "NO-BN-2D-JBTKL-MAST-TREMAST"
		description "KL TREMAST"
		r	1.5
	)
	(drawCircle layer_Zero r _noWipeout_)
	(addDescriptionBelowOrigo description r)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)
