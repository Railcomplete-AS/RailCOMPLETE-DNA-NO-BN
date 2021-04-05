;=========================================================================================================================
;
; Reflective marker.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Reflective marker (on signal mast)

(defun C:REFLECTIVE-MARKER ( / )
	(REFLEKS_HEL)
	(REFLEKS_HALV)
)



(defun REFLEKS_HEL ( / blockName r )
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-REFLEKS-HEL"
		description (strcat "REFLEKS P" _uARING_ " FORSIDEN OG BAKSIDEN AV INNKJ" _uOSLASH_ "RHOVEDSIGNAL OG BLOKKSIGNAL")
		r 1.5
	)
	; Schematic symbol
	(command
		"._POLYGON" 6 _origo_ "_INSCRIBED" r
		_LINE_ (list (- r) 0) (list r 0) _ENTER_
		_LINE_ (list (- (/ r 2)) (* r (/ (sqrt 3) 2))) (list (/ r 2) (- (* r (/ (sqrt 3) 2)))) _ENTER_
		_LINE_ (list (/ r 2) (* r (/ (sqrt 3) 2))) (list (- (/ r 2)) (- (* r (/ (sqrt 3) 2)))) _ENTER_
		_MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 (* r (/ (sqrt 3) 2)))
	)
	(AddDescriptionBelowOrigo description r)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun REFLEKS_HALV ( / blockName r )
	(setq
		blockName "NO-BN-2D-SKILT-KJOERENDE-REFLEKS-HALV"
		description (strcat "REFLEKS P" _uARING_ " FORSIDEN AV INDRE HOVEDSIGNAL I INNKJ" _uOSLASH_ "RTOGVEI")
		r 1.5
	)
	; Schematic symbol
	(command ; half of hexagon (three triangles forming half of a 6-cell hexagon)
		_LINE_
			(list (- r) 0) (list r 0)
			(list (/ r 2) (- (* r (/ (sqrt 3) 2))))
			(list (- (/ r 2)) (- (* r (/ (sqrt 3) 2))))
			(list (- r) 0) 
			_ENTER_
		_LINE_ _origo_ (list (- (/ r 2)) (- (* r (/ (sqrt 3) 2)))) _ENTER_
		_LINE_ _origo_(list (/ r 2) (- (* r (/ (sqrt 3) 2)))) _ENTER_
		_MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 (* r (/ (sqrt 3) 2)))
	)
	(AddDescriptionBelowOrigo description r)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
	