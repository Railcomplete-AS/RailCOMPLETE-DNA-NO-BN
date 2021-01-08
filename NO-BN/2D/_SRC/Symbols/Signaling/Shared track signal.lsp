;=========================================================================================================================
;
; Shared track signal.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; Train track signal (departure signal per track instead of individual exit signals for each track - many tracks can share the same exit signal)

; Både frittstående, vegghengt og plassert på annen mast eller vegg (men som eget objekt, uten base ('fot'))

(defun C:SHARED-TRACK-SIGNAL ( / )
    (TOGSPORSIGNAL-1 nil)		; 1-begreps Togsporsignal på egen mast
    (TOGSPORSIGNAL-1 "AAK")		; 1-begreps Togsporsignal i åk
    (TOGSPORSIGNAL-2 nil)		; 2-begreps Togsporsignal på egen mast
    (TOGSPORSIGNAL-2 "AAK")		; 2-begreps Togsporsignal i åk
    (TOGSPORSIGNAL-3 "VSIDE")	; 1-begreps Togsporsignal, montert på vegg eller på siden av annet signal, lanterner til venstre for yokeMountedarm
    (TOGSPORSIGNAL-3 "HSIDE") 	; 1-begreps Togsporsignal, montert på vegg eller på siden av annet signal, lanterner til høyre for yokeMountedarm
    (TOGSPORSIGNAL-4 "VSIDE")	; 2-begreps Togsporsignal, montert på vegg eller på siden av annet signal, lanterner til venstre for yokeMountedarm
    (TOGSPORSIGNAL-4 "HSIDE")	; 2-begreps Togsporsignal, montert på vegg eller på siden av annet signal, lanterner til høyre for yokeMountedarm
)



(defun TOGSPORSIGNAL-1 ( yokeMounted / blockName )
	; Upright mast with 2 lanterns
	(setq 
		blockName "NO-BN-2D-JBTSI-SIGNAL-TOGSPORSIGNAL-1"
	)

	; Schematic symbol
	(drawSingleUprightTrackSignal yokeMounted)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols
	(createGeoBlockInAllPaperScalesFromBlock blockName _twoThirds_ blockName) ; Use 2/3 of scaled schematic symbol as 1:500 cale etc
)



(defun TOGSPORSIGNAL-2 ( yokeMounted / blockName schematicSidewaysSeparation geoSidewaysSeparation )
	; Upright 'forked' mast with 2x2 lanterns
	(setq 
		blockName "NO-BN-2D-JBTSI-SIGNAL-TOGSPORSIGNAL-2"
		schematicSidewaysSeparation 3.5		; Spacing sideways between forked signal arms in schematic symbol
		geoSidewaysSeparation 2.5			; Spacing sideways between forked signal arms in geo symbols (to mitigate that close signals overlap)
	)

	; Schematic symbol
	(drawForkedUprightTrackSignal yokeMounted schematicSidewaysSeparation)
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols
	(drawForkedUprightTrackSignal yokeMounted geoSidewaysSeparation)		; A narrower version than the schematic symbol
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _twoThirds_ blockName)
)



(defun TOGSPORSIGNAL-3 ( side / blockName )
	; Wall-mounted (no base), upright knee mast with 2 lanterns
	(setq 
		blockName (strcat "NO-BN-2D-JBTSI-SIGNAL-TOGSPORSIGNAL-3-" side)
	)

	; Schematic symbol
	(drawSingleWallmountedTrackSignal side)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols
	(createGeoBlockInAllPaperScalesFromBlock blockName _twoThirds_ blockName) ; Use 2/3 of scaled schematic symbol as 1:500 cale etc
)



(defun TOGSPORSIGNAL-4 ( side / blockName schematicSidewaysSeparation geoSidewaysSeparation )
	; Wall-mounted (no base), forked knee mast with 2x2 lanterns
	(setq 
		blockName (strcat "NO-BN-2D-JBTSI-SIGNAL-TOGSPORSIGNAL-4-" side)
		schematicSidewaysSeparation 3.5		; Spacing sideways between forked signal arms in schematic symbol
		geoSidewaysSeparation 2.5			; Spacing sideways between forked signal arms in geo symbols (to mitigate that close signals overlap)
	)
	; Schematic symbol
	(drawForkedWallmountedTrackSignal schematicSidewaysSeparation side)
	(createSchematicBlockFromCurrentGraphics blockName)

	; Geo symbols
	(drawForkedWallmountedTrackSignal geoSidewaysSeparation side)			; A narrower version than the schematic symbol
	(createGeoBlockInAllPaperScalesFromCurrentGraphics _twoThirds_ blockName)
)



;==========================
; draw...X...() functions
;==========================
(defun drawSingleUprightTrackSignal ( yokeMounted / r lowerPole upperPole )
	; Drawn lying, then rotated CCW.
	(setq 
		r (getSmallLanternRadius) ; Togsporsignal lantern radius
		lowerPole 2.5
		upperPole 2.0
	)
	(command "._CIRCLE" (list r 0) r)
	(command "._CIRCLE" (list (* 3 r) 0) r)
	(if yokeMounted
		(progn
			(setq blockName (strcat blockName "-AAK"))
			(command "._MOVE" "_ALL" "" "_Displacement" (list (- (+ (* 4 r) upperPole)) 0))
			(drawLyingPole 0 (- upperPole))
		)
		(progn
			(command "._MOVE" "_ALL" "" "_Displacement" (list (+ lowerPole upperPole) 0))
			(drawLyingPole 0 (+ lowerPole upperPole))
			(drawLyingTsBase)
		)
	)
	(command "._ROTATE" "_ALL" "" "0,0" "90")
)



(defun drawForkedUprightTrackSignal ( yokeMounted sidewaysSeparation / r lowerPole upperPole lat )
	; Drawn lying, then rotated CCW (upright) or CW (yoke mounting).
	(setq 
		r (getSmallLanternRadius)					; Size relative to main signal lanterns
		lowerPole 2.5
		upperPole 2.0
		lat (/ sidewaysSeparation 2.0)		; Lateral displacement of forked arms (from center pole)
	)
	(command
		"._LINE" ; lower pole + left part of split mast
			(list 0 0)
			(list lowerPole 0)
			(list lowerPole lat)
			(list (+ lowerPole upperPole) lat)
			""
		"._LINE" ; right part of split mast
			(list lowerPole 0)
			(list lowerPole (- lat))
			(list (+ lowerPole upperPole) (- lat))
			""
	)
	(command "._CIRCLE" (list (+ lowerPole upperPole r) lat) r)				; lower left lantern
	(command "._CIRCLE" (list (+ lowerPole upperPole (* 3 r)) lat) r)		; upper left lantern
	(command "._CIRCLE" (list (+ lowerPole upperPole r) (- lat)) r)			; lower right lantern
	(command "._CIRCLE" (list (+ lowerPole upperPole (* 3 r)) (- lat)) r)	; upper right lantern
	(if yokeMounted
		(progn ; rotate CW
			(setq blockName (strcat blockName "-AAK"))
			(command "._ROTATE" "_ALL" "" "0,0" "-90")
		)
		(progn ; add base, rotate CCW
			(drawLyingHsBase)
			(command "._ROTATE" "_ALL" "" "0,0" "90")
		)
	)
)



(defun drawSingleWallmountedTrackSignal ( side / r lowerPole upperPole knee )
	; Drawn upright
	(setq 
		r (getSmallLanternRadius)  ; Size relative to main signal lanterns
		knee (* 1.75 (getSmallLanternRadius))  ; Lateral displacement, horizontal part of 'knee'
		lowerPole 2.5
		upperPole 2.0
	)
	(command
		"._LINE"
			(list 0 0)
			(list knee 0)
			(list knee upperPole)
			""
	)
	(command "._CIRCLE" (list knee (+ upperPole r)) r)			; lower lantern
	(command "._CIRCLE" (list knee (+ upperPole (* 3 r))) r)	; upper lantern
	(if (= side "VSIDE")	
		(command "._MIRROR" "_ALL" "" "0,0" "0,1" "_YES")
	)
)



(defun drawForkedWallmountedTrackSignal ( sidewaysSeparation side / r lowerPole upperPole knee lat )
	; Drawn upright
	(setq 
		r (getSmallLanternRadius)					; Size relative to main signal lanterns
		lowerPole 2.5
		upperPole 2.0
		knee (* 1.75 (getSmallLanternRadius))  		; Lateral displacement, horizontal part of 'knee'
		lat (/ sidewaysSeparation 2.0)		; Lateral displacement of forked arms (from center pole)
	)
	(command
		"._LINE" ; Lower pole + left part of split mast
			(list 0 0)
			(list lowerPole 0)
			(list lowerPole lat)
			(list (+ lowerPole upperPole) lat)
			""
		"._LINE" ; right part of split mast
			(list lowerPole 0)
			(list lowerPole (- lat))
			(list (+ lowerPole upperPole) (- lat))
			""
	)
	(command "._CIRCLE" (list (+ lowerPole upperPole r) lat) r)				; lower left lantern
	(command "._CIRCLE" (list (+ lowerPole upperPole (* 3 r)) lat) r)		; upper left lantern
	(command "._CIRCLE" (list (+ lowerPole upperPole r) (- lat)) r)			; lower right lantern
	(command "._CIRCLE" (list (+ lowerPole upperPole (* 3 r)) (- lat)) r)	; upper right lantern
	(command "._MOVE" "_ALL" "" "_Displacement" (list 0 (+ lat knee)))		; Move "left"
	(command "._LINE" "0,0" (list 0 (+ knee lat)) "")  						; Add horizontal "knee"
	(if (= side "HSIDE")
		(command "._MIRROR" "_ALL" "" "0,0" "1,0" "_YES")
	)
	(command "._ROTATE" "_ALL" "" "0,0" "90")
)
