;=========================================================================================================================
;
; Thumbnail Escape Route.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail for escape route alignment selection (from tunnels, e.g. side-tunnels where distances shall be measured and depicted)

(defun THUMBNAIL-ESCAPE-ROUTE ( / )
	(THUMBNAIL-ESCAPE-ROUTE)
)



(defun THUMBNAIL-ESCAPE-ROUTE ( / blockName ) 
	; Pictogram featuring a running adult and an escape route doorway passage.
	(setq blockName (strcat _RC_ thumbnailInfix "-ROEMNINGSVEI"		))
	(command 
		; PERSON:
		_POLYLINE_ 
			(list 23 41) ;neck
			(list 28 37) ;right shoulder
			(list 28 32) ;right elbow
			(list 32 32) ;right arm
			(list 32 28) ;right arm
			(list 24 28) ;right elbow
			(list 24 35) ;right armpit
			(list 19 25) ;right waist
			(list 26 11) ;right knee
			(list 21 0) ;right foot
			(list 17 2) ;right foot
			(list 21 11) ;right knee
			(list 15 22) ;crotch
			(list 15 12) ;left knee
			(list 0 12) ;left foot
			(list 0 16) ;left foot
			(list 10 16) ;left knee
			(list 10 27) ;left waist
			(list 15 37) ;left armpit
			(list 11 37) ;left elbow
			(list 7 31) ;left arm
			(list 3 33) ;left arm
			(list 9 41) ;left elbow
			_closedPolyline_

		_CIRCLE_ (list 22 46) "5" ;head

		; ARROW:
		_POLYLINE_ 
			(list 41 27)
			(list 57 27)
			(list 53 31)
			(list 57 31)
			(list 63 25)
			(list 57 19)
			(list 53 19)
			(list 57 23)
			(list 41 23)
			_closedPolyline_

		; DOORWAY:
		_POLYLINE_ 
			(list 66 50)
			(list 90 50)
			(list 90 0)
			(list 66 0)
			_closedPolyline_

		; Scale down and move:
		_SCALE_ _selectAll_ _ENTER_ _origin_ _tenth_
		_MOVE_ _selectAll_ _ENTER_ (list 5.0 2.5) _origin_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
