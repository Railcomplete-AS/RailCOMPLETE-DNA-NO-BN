;=========================================================================================================================
;
; Thumbnail Earthing Wire.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for earthing wire alignment selection

(defun C:THUMBNAIL-EARTHING-WIRE ( / )
	(ALIGNMENT-JORDLEDER)
)


(defun ALIGNMENT-JORDLEDER ( / blockName ) 
	; Dashed line with the letter 'jl' (jordline = earthing wire)
	(setq blockName "NO-BN-2D-JBTKL-THUMBNAIL-JORDLEDER")
	(command 
		_LINE_ (list 0 10) (list 5 10) _ENTER_		
		_LINE_ (list 10 10) (list 15 10) _ENTER_
		_LINE_ (list 20 10) (list 25 10) _ENTER_

		; (Gap here, for 'jl' letters)
		_LINE_ (list 40 10) (list 45 10) _ENTER_
		_LINE_ (list 50 10) (list 55 10) _ENTER_
		_LINE_ (list 60 10) (list 65 10) _ENTER_

		; 'jl' letters:
		_LINE_ (list 30 15) (list 30 12) _ENTER_
		_POLYLINE_ 
			(list 28 10)
			(list 31 10)
			(list 31 2)
			(list 29 0)
			(list 27 0)
			_ENTER_

		_POLYLINE_ 
			(list 34 15)
			(list 36 15)
			(list 36 4)
			(list 34 4)
			(list 38 4)
			_ENTER_

		; Scale down and move:
		_SCALE_ _selectAll_ _ENTER_ _origo_ _tenth_
		_MOVE_ _selectAll_ _ENTER_ (list 2.5 0.5) _origo_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
)
