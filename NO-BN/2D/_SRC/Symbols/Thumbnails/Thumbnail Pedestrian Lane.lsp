;=========================================================================================================================
;
; Thumbnail Pedestrian Lane.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for pedestrian lane alignment selection

(defun C:THUMBNAIL-PEDESTRIAN-LANE ( / )
	(ALIGNMENT-GANGVEI)
)



(defun ALIGNMENT-GANGVEI ( / blockName ) 
; Start drawing child, then draw adult, then scale down and reposition.
; Polyline starts at center neck and continues clockwise.
	(setq blockName "NO-BN-2D-JBTUB-THUMBNAIL-GANGVEI")
	(command 
		; CHILD:
		"._PLINE" 
		(list 10 27) ;neck
		(list 15 27) ;right shoulder
		(list 20 17) ;right arm
		(list 16 15) ;right arm
		(list 14 22) ;right armpit
		(list 15 14) ;right waist
		(list 21 1) ;right foot
		(list 17 0) ;right foot
		(list 12 11) ;crotch
		(list 10 11) ;crotch
		(list 10 6) ;left knee
		(list 5 1) ;left foot
		(list 2 3) ;left foot
		(list 6 7) ;left knee
		(list 7 22) ;left armpit
		(list 3 15) ;left arm
		(list 0 17) ;left arm
		(list 5 27) ;left shoulder
		"_CLOSE"
		"._CIRCLE" (list 10 33) "4" ;head

		; ADULT:
		"._PLINE" 
		(list 36 39) ;neck
		(list 43 39) ;right shoulder
		(list 50 22) ;right arm
		(list 45 21) ;right arm
		(list 41 31) ;right armpit
		(list 42 20) ;right waist
		(list 51 2) ;right foot
		(list 46 0) ;right foot
		(list 39 16) ;crotch
		(list 35 16) ;crotch
		(list 35 9) ;left knee
		(list 29 1) ;left foot
		(list 24 4) ;left foot
		(list 30 11) ;left knee
		(list 31 31) ;left armpit
		(list 27 22) ;left arm
		(list 22 24) ;left arm
		(list 29 39) ;left shoulder
		"_CLOSE"
		"._CIRCLE" (list 36 47) "6" ;head

		; Scale down and move:
		"._SCALE" "_ALL" "" (list 0 0) "0.1"
		"._MOVE" "_ALL" "" (list 2.5 2.5) (list 0 0)
	)
	(createSchematicBlockFromCurrentGraphics blockName)
	blockName
)

