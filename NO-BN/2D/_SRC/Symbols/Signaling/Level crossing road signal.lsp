;=========================================================================================================================
;
; Level crossing road signal.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Road signal at level crossing

; Veisignalene er ikke reglet i TRV for jernbane ()SJT TFF), men i Veitrafikkens regelverk.

(defun C:LEVEL-CROSSING-ROAD-SIGNAL ( / variation yokeMounted )
	(foreach variation '("VS1" "VS2")			; 'VS1'= single mast, 'VS2' = forked mast (double signal)
		(foreach yokeMounted '(nil T)			; 'nil' = upright mast, T = yoke mounting
			(VEISIGNAL variation yokeMounted )	; Single or double road signal (for level crossing), free-standing mast or yoke mpunted
		)
	)
)



(defun VEISIGNAL ( variation yokeMounted / )
	(setq blockName (strcat "NO-BN-2D-JBTSI-SIGNAL-" variation (if (= yokeMounted nil) "" "-AAK")))
	(thawAllLayers)

	; Schematic symbol
	; Schematic 1-line signaling / schematic plan ('skjematisk plan') view
	(setLayer layer_View_SchematicPlan)
	(drawLevelCrossingSignalTowardsRoad variation yokeMounted "LARGE" "WIDE")
	
	; Schematic 1-line signaling / cable plan ('plan og kabelplan') view
	(setLayer layer_View_CablePlan)
	(drawLevelCrossingSignalTowardsRoad variation yokeMounted "SMALL" "WIDE")
	
	; Schematic 2-line signaling / track insulation plan OR high voltage / return current plan view
	(setLayer layer_View_TrackIsolationPlan)
	(drawLevelCrossingSignalTowardsRoad variation yokeMounted "LARGE" "WIDE")
	
	; Create schematic symbol
	(thawAllLayers)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols
	; Geo 1-line signaling / schematic plan ('skjematisk plan') view
	(setLayer layer_View_SchematicPlan)
	(drawLevelCrossingSignalTowardsRoad variation yokeMounted "LARGE" "NARROW")
	
	; Geo 1-line signaling / cable plan ('plan og kabelplan') view
	(setLayer layer_View_CablePlan)
	(drawLevelCrossingSignalTowardsRoad variation yokeMounted "SMALL" "NARROW")
	
	; Geo 2-line signaling / track insulation plan OR high voltage / return current plan view
	(setLayer layer_View_TrackIsolationPlan)
	(drawLevelCrossingSignalTowardsRoad variation yokeMounted "LARGE" "NARROW")
	
	; Create geo symbols
	(thawAllLayers)
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _twoThirds_ blockName)
)



;==========================
; draw...X...() functions
;==========================
(defun drawLevelCrossingSignalTowardsRoad ( variation yokeMounted size width / lowerPole upperPole topOfPole lat r x y )
	; Drawn upright
	; Argument 'width' is ignored for 'VS1' signal
	(setq
		lowerPole	4.0
		upperPole	(cond ((= size "SMALL") 1.5) ((= size "LARGE") 2.0))
		topOfPole	(+ lowerPole upperPole)
		yokePole	2.0
		lat			(cond ; lateral displacement of arms from centre
						((= size "LARGE") (if (= width "WIDE") 3.5 2.5))
						((= size "SMALL") (if (= width "WIDE") 2.5 1.85))
					)
		r 			(cond ((= size "LARGE") (getLargeLanternRadius)) ((= size "SMALL") (getSmallLanternRadius))) ; lantern size
		x 			(cond ((= width "WIDE") (* 2.0 r)) ((= width "NARROW") (* 0.75 2.0 r))) ; horizontal length of diagonal 1:2 line, shorten diagonals by 3/4 if narrow version
		y  			(cond ((= width "WIDE") (* 1.0 r)) ((= width "NARROW") (* 0.75 1.0 r))) ; horizontal length of diagonal 1:2 line, shorten diagonals by 3/4 if narrow version
	)
	(freezeAllLayersExceptCurrentLayer)
	(cond
		((= variation "VS1")
			(drawLantern (list 0 (- r)) r)	; lower lantern, below origo
			(drawLantern (list 0 (+ r)) r)	; upper lantern, above origo
			(command
				; St Andrew cross at origo
				"._LINE" (list (- x) (- y))   (list x (+ y)) ""
				"._LINE" (list (- x) (+ y))   (list x (- y)) ""
			)
			(if yokeMounted
				(progn
					(command "._MOVE" "_ALL" "" "0,0" (list 0 (* -2 r)))	; shift down by one lantern
					(drawVerticalPole yokePole)								; add yoke pole
					(command "._MOVE" "_ALL" "" "0,0" (list 0 (- yokePole)))	; shift down by one yoke pole
				)
			;else - upright mast
				(progn
					(command "._MOVE" "_ALL" "" "0,0" (list 0 (+ (* 2 r) topOfPole))) ; shift up by one lantern + pole
					(drawVerticalPole topOfPole)
					(drawHorizontalHsBase)
				)
			)
		)
		((= variation "VS2")
			; Draw forked part, based at x=0
			(command "._LINE" (list (- lat) 0) (list (+ lat) 0) "") ; horizontal line between arms
			(command "._LINE" (list (- lat) 0) (list (- lat) upperPole) "") ; left upright arm
			(command "._LINE" (list (+ lat) 0) (list (+ lat) upperPole) "")	; right upright arm
			(drawLantern (list (- lat) (+ upperPole      r )) r)
			(drawLantern (list (- lat) (+ upperPole (* 3 r))) r)
			(drawLantern (list (+ lat) (+ upperPole      r)) r)
			(drawLantern (list (+ lat) (+ upperPole (* 3 r))) r)
			(command 
				; Left arm's St Andrew cross
				"._LINE" (list (+ (- lat) (- x)) (+ (+ upperPole (* 2 r)) (- y)))   (list (+ (- lat) (+ x)) (+ (+ upperPole (* 2 r)) (+ y))) "" ; Left '/'
				"._LINE" (list (+ (- lat) (- x)) (+ (+ upperPole (* 2 r)) (+ y)))   (list (+ (- lat) (+ x)) (+ (+ upperPole (* 2 r)) (- y))) "" ; Left '\'
				; Right arm's St Andrew cross
				"._LINE" (list (+ (+ lat) (- x)) (+ (+ upperPole (* 2 r)) (- y)))   (list (+ (+ lat) (+ x)) (+ (+ upperPole (* 2 r)) (+ y))) "" ; Right '/'
				"._LINE" (list (+ (+ lat) (- x)) (+ (+ upperPole (* 2 r)) (+ y)))   (list (+ (+ lat) (+ x)) (+ (+ upperPole (* 2 r)) (- y))) "" ; Right '\'
			)
			(if yokeMounted
				(progn
					(command "._MIRROR" "_ALL" "" "0,0" "1,0" "_YES")		; flip down around x=0
					(drawVerticalPole yokePole)								; add yoke pole
					(command "._MOVE" "_ALL" "" (list 0 yokePole) "0,0")	; shift down by one yoke pole
				)
			;else - upright mast
				(progn
					(command "._MOVE" "_ALL" "" "0,0" (list 0 lowerPole)) ; shift up by one lantern + lower pole
					(drawVerticalPole lowerPole)
					(drawHorizontalHsBase)
				)
			)
		)
	)
	(setLayer layer_Zero)
)
