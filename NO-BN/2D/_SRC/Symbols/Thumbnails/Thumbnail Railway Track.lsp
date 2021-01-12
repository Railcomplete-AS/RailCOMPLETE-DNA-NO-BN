;=========================================================================================================================
;
; Thumbnail Railway Track.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for railway track alignment selection (two sweeped rails / two rails and sleeper as repeated objects)

(defun C:THUMBNAIL-TRACK ( / )
	(ALIGNMENT-SPOR)
)

(defun ALIGNMENT-SPOR (/ blockName trackLength railFoot railHead sleeperWidth sleeperLength sleeperGap) 
	(setq
		blockName (strcat "NO-BN-2D-JBTOB-THUMBNAIL-SPOR")
		trackLength 1800
		railFoot 150
		railHead 72
		sleeperWidth 260
		sleeperLength (/ 2600 2) ;mirror to get full length
		sleeperSeparation 600
		sleeperGap (- sleeperSeparation SleeperWidth)
		trackLengthBeforeFirstSleeper (/ (- trackLength (* sleeperWidth 3) (* sleeperGap 2)) 2)
		centerTrack (- (/ 1435 2) (- (/ railFoot 2) (/ railHead 2 )))
	)
	(command
		"._RECTANGLE" (list 0 centerTrack) (list trackLength (+ centerTrack railFoot))
		"._LINE" (list 0 (+ centerTrack (/ (- railFoot railHead) 2))) (list trackLength (+ centerTrack (/ (- railFoot railHead) 2))) ""
		"._LINE" (list 0 (+ centerTrack (/ (- railFoot railHead) 2) railHead)) (list trackLength (+ centerTrack (/ (- railFoot railHead) 2) railHead)) ""
		"._LINE" (list trackLengthBeforeFirstSleeper 0) (list trackLengthBeforeFirstSleeper centerTrack) ""
		"._LINE" 
			(list trackLengthBeforeFirstSleeper (+ centerTrack railFoot)) (list trackLengthBeforeFirstSleeper sleeperLength)
			(list (+ trackLengthBeforeFirstSleeper sleeperWidth) sleeperLength) (list (+ trackLengthBeforeFirstSleeper sleeperWidth) (+ centerTrack railFoot))
			""
		"._LINE" (list (+ trackLengthBeforeFirstSleeper sleeperWidth) 0) (list (+ trackLengthBeforeFirstSleeper sleeperWidth) centerTrack) ""
		"._LINE" (list (+ trackLengthBeforeFirstSleeper sleeperWidth sleeperGap) 0) (list (+ trackLengthBeforeFirstSleeper sleeperWidth sleeperGap) centerTrack) ""
		"._LINE"
			(list (+ trackLengthBeforeFirstSleeper sleeperWidth sleeperGap) (+ centerTrack railFoot))
			(list (+ trackLengthBeforeFirstSleeper sleeperWidth sleeperGap) sleeperLength) (list (+ trackLengthBeforeFirstSleeper (* sleeperWidth 1.5) sleeperGap) sleeperLength)
			""
		"._MIRROR" "_ALL" "" (list (+ trackLengthBeforeFirstSleeper (* sleeperWidth 1.5) sleeperGap) 0) (list (+ trackLengthBeforeFirstSleeper (* sleeperWidth 1.5) sleeperGap) sleeperLength) "_NO"
		"._MIRROR" "_ALL" "" "0,0" "1,0" "_NO"
		"._MOVE" "_ALL" "" (list (+ trackLengthBeforeFirstSleeper (* sleeperWidth 1.5) sleeperGap) 0) (list 0 0) 
	)
	(command
		"._SCALE" "_ALL" "" (list 0 0) 0.001 ; convert millimeter to meter
		"._SCALE" "_ALL" "" (list 0 0) 2 ; suitable scale for alignment thumbnails - ca 4-5 meter (...and they auto-scale in RailCOMPLETE)
	)
	(createSchematicBlockFromCurrentGraphics blockName)
	blockName
)