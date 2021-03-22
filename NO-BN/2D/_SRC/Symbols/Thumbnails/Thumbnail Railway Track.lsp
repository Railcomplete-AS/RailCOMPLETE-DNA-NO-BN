;=========================================================================================================================
;
; Thumbnail Railway Track.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for railway track alignment selection (two sweeped rails / two rails and sleeper as repeated objects)

(defun C:THUMBNAIL-TRACK ( / )
	(ALIGNMENT-SPOR)
)

(defun ALIGNMENT-SPOR ( / blockName trackLength railFoot railHead sleeperWidth sleeperLength sleeperSeparation sleeperGap trackLengthBeforeFirstSleeper centerTrack ) 
	; A piece of track viewed from above, featuring the two rails and five sleepers.
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
		centerTrack (- (/ _normalGauge_ 2) (- (/ railFoot 2) (/ railHead 2 )))
	)
	(command
		_RECTANGLE_ (list 0 centerTrack) (list trackLength (+ centerTrack railFoot))
		_LINE_ (list 0 (+ centerTrack (/ (- railFoot railHead) 2))) (list trackLength (+ centerTrack (/ (- railFoot railHead) 2))) _ENTER_
		_LINE_ (list 0 (+ centerTrack (/ (- railFoot railHead) 2) railHead)) (list trackLength (+ centerTrack (/ (- railFoot railHead) 2) railHead)) _ENTER_
		_LINE_ (list trackLengthBeforeFirstSleeper 0) (list trackLengthBeforeFirstSleeper centerTrack) _ENTER_
		_LINE_ 
			(list trackLengthBeforeFirstSleeper (+ centerTrack railFoot)) (list trackLengthBeforeFirstSleeper sleeperLength)
			(list (+ trackLengthBeforeFirstSleeper sleeperWidth) sleeperLength) (list (+ trackLengthBeforeFirstSleeper sleeperWidth) (+ centerTrack railFoot))
			_ENTER_
		_LINE_ (list (+ trackLengthBeforeFirstSleeper sleeperWidth) 0) (list (+ trackLengthBeforeFirstSleeper sleeperWidth) centerTrack) _ENTER_
		_LINE_ (list (+ trackLengthBeforeFirstSleeper sleeperWidth sleeperGap) 0) (list (+ trackLengthBeforeFirstSleeper sleeperWidth sleeperGap) centerTrack) _ENTER_
		_LINE_
			(list (+ trackLengthBeforeFirstSleeper sleeperWidth sleeperGap) (+ centerTrack railFoot))
			(list (+ trackLengthBeforeFirstSleeper sleeperWidth sleeperGap) sleeperLength) (list (+ trackLengthBeforeFirstSleeper (* sleeperWidth 1.5) sleeperGap) sleeperLength)
			_ENTER_
		_MIRROR_ _selectAll_ _ENTER_ 
			(list (+ trackLengthBeforeFirstSleeper (* sleeperWidth 1.5) sleeperGap) 0) 
			(list (+ trackLengthBeforeFirstSleeper (* sleeperWidth 1.5) sleeperGap) sleeperLength)
			_keepMirrorSource_
		_MIRROR_ _selectAll_ _ENTER_ _origo_ _xAxis_ _keepMirrorSource_
		_MOVE_ _selectAll_ _ENTER_ (list (+ trackLengthBeforeFirstSleeper (* sleeperWidth 1.5) sleeperGap) 0) _origo_ 
	)
	(command
		_SCALE_ _selectAll_ _ENTER_ _origo_ 0.001 ; convert millimeter to meter
		_SCALE_ _selectAll_ _ENTER_ _origo_ _two_ ; suitable scale for alignment thumbnails - ca 4-5 meter (...and they auto-scale in RailCOMPLETE)
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
)
