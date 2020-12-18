;=========================================================================================================================
;
; Reflective marker.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-11-15 CLFEY Release 2021.1
;
;=========================================================================================================================

; Reflective marker (on signal mast)

(defun C:REFLECTIVE-MARKER ( / )
	(REFLEKS_HEL)
	(REFLEKS_HALV)
)



(defun REFLEKS_HEL ( / blockName r )
	(setq
		blockName "NO-BN-2D-SKILT-REFLEKS-HEL"
		description (strcat "REFLEKS P" _uAA_ " FORSIDEN OG BAKSIDEN AV INNKJ" _uOE_ "RHOVEDSIGNAL OG BLOKKSIGNAL")
		r 1.5
	)
	; Schematic symbol
	(command
		"._POLYGON" 6 "0,0" "_INSCRIBED" r
		"._LINE" (list (- r) 0) (list r 0) ""
		"._LINE" (list (- (/ r 2)) (* r (/ (sqrt 3) 2))) (list (/ r 2) (- (* r (/ (sqrt 3) 2)))) ""
		"._LINE" (list (/ r 2) (* r (/ (sqrt 3) 2))) (list (- (/ r 2)) (- (* r (/ (sqrt 3) 2)))) ""
		"._MOVE" "_ALL" "" "0,0" (list 0 (* r (/ (sqrt 3) 2)))
	)
	(addDescriptionBelowOrigo description r)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols:
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun REFLEKS_HALV ( / blockName r )
	(setq
		blockName "NO-BN-2D-SKILT-REFLEKS-HALV"
		description (strcat "REFLEKS P" _uAA_ " FORSIDEN AV INDRE HOVEDSIGNAL I INNKJ" _uOE_ "RTOGVEI")
		r 1.5
	)
	; Schematic symbol
	(command ; half of hexagon (three triangles forming half of a 6-cell hexagon)
		"._LINE"
			(list (- r) 0) (list r 0)
			(list (/ r 2) (- (* r (/ (sqrt 3) 2))))
			(list (- (/ r 2)) (- (* r (/ (sqrt 3) 2))))
			(list (- r) 0) 
			""
		"._LINE" "0,0" (list (- (/ r 2)) (- (* r (/ (sqrt 3) 2)))) ""
		"._LINE" "0,0"(list (/ r 2) (- (* r (/ (sqrt 3) 2)))) ""
		"._MOVE" "_ALL" "" "0,0" (list 0 (* r (/ (sqrt 3) 2)))
	)
	(addDescriptionBelowOrigo description r)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols:
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)
	