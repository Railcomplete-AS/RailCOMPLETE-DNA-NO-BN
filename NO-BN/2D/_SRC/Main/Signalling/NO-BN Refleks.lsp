;=========================================================================================================================
;
; NO-BN Refleks.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Reflective marker (on signal mast)

(defun NOBN-REFLEKS ( / )
	(NOBN-REFLEKS-HEL)
	(NOBN-REFLEKS-HALV)
)



(defun NOBN-REFLEKS-HEL ( / blockName description r )
	(setq
		blockName 	(strcat _BNP_ "SKK-" "SKILT-KJOERENDE-REFLEKS-HEL")
		description (strcat "REFLEKS P" _uARING_ " FORSIDEN OG BAKSIDEN AV INNKJ" _uOSLASH_ "RHOVEDSIGNAL OG BLOKKSIGNAL")
		r 1.5
	)
	; Schematic symbol
	(command
		"._POLYGON" 6 _origin_ "_INSCRIBED" r
		_LINE_ (list (- r) 0) (list r 0) _ENTER_
		_LINE_ (list (- (/ r 2)) (* r (/ (sqrt 3) 2))) (list (/ r 2) (- (* r (/ (sqrt 3) 2)))) _ENTER_
		_LINE_ (list (/ r 2) (* r (/ (sqrt 3) 2))) (list (- (/ r 2)) (- (* r (/ (sqrt 3) 2)))) _ENTER_
		_MOVE_ _selectAll_ _ENTER_ _origin_ (list 0 (* r (/ (sqrt 3) 2)))
	)
	(AddDescriptionBelowOrigin description r)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-REFLEKS-HALV ( / blockName description r )
	(setq
		blockName (strcat _BNP_ "SKK-" "SKILT-KJOERENDE-REFLEKS-HALV")
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
		_LINE_ _origin_ (list (- (/ r 2)) (- (* r (/ (sqrt 3) 2)))) _ENTER_
		_LINE_ _origin_(list (/ r 2) (- (* r (/ (sqrt 3) 2)))) _ENTER_
		_MOVE_ _selectAll_ _ENTER_ _origin_ (list 0 (* r (/ (sqrt 3) 2)))
	)
	(AddDescriptionBelowOrigin description r)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
	