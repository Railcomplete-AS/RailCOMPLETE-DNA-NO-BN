;=========================================================================================================================
;
; Thumbnail Railway Track.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail for railway track alignment (two sweeped rails / two rails and sleeper as repeated objects)

(defun THUMBNAIL-RAILWAY-TRACK ( / )
	(THUMBNAIL-RAILWAY-TRACK)
)

(defun THUMBNAIL-RAILWAY-TRACK ( / blockName trackLength railFoot railHead sleeperWidth sleeperLength sleeperSeparation sleeperGap trackLengthBeforeFirstSleeper centerTrack ) 
	; A piece of track viewed from above, featuring the two rails and five sleepers.
	(setq blockName (strcat _RC_ thumbnailInfix "-JERNBANESPOR"		))
	(setq
		trackLength 1800
		railFoot 150
		railHead 72
		sleeperWidth 260
		sleeperLength (/ 2600 2) ;mirror to get full length
		sleeperSeparation 600
		sleeperGap (- sleeperSeparation SleeperWidth)
		trackLengthBeforeFirstSleeper (/ (- trackLength (* sleeperWidth 3) (* sleeperGap 2)) 2)
		centerTrack (- (/ (* 1000 _normalGauge_) 2) (- (/ railFoot 2) (/ railHead 2 )))
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
		_MIRROR_ _selectAll_ _ENTER_ _origin_ _xAxis_ _keepMirrorSource_
		_MOVE_ _selectAll_ _ENTER_ (list (+ trackLengthBeforeFirstSleeper (* sleeperWidth 1.5) sleeperGap) 0) _origin_ 
	)
	(command
		_SCALE_ _selectAll_ _ENTER_ _origin_ 0.001 ; convert millimeter to meter
		_SCALE_ _selectAll_ _ENTER_ _origin_ _two_ ; suitable scale for alignment thumbnails - ca 4-5 meter (...and they auto-scale in RailCOMPLETE)
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
