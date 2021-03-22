;=========================================================================================================================
;
; Shared track signal.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Train track signal (departure signal per track instead of individual exit signals for each track - many tracks can share the same exit signal)

; Både frittstående, vegghengt og plassert på annen mast eller vegg (men som eget objekt, uten base ('fot'))

(defun C:SHARED-TRACK-SIGNAL ( / )
	(TraceLevel3 "TOGSPORSIGNAL-1")
		(TOGSPORSIGNAL-1 nil)		; 1-begreps Togsporsignal på egen mast
		(TOGSPORSIGNAL-1 "AAK")		; 1-begreps Togsporsignal i åk

	(TraceLevel3 "TOGSPORSIGNAL-2")
		(TOGSPORSIGNAL-2 nil)		; 2-begreps Togsporsignal på egen mast
		(TOGSPORSIGNAL-2 "AAK")		; 2-begreps Togsporsignal i åk

	(TraceLevel3 "TOGSPORSIGNAL-3")
		(TOGSPORSIGNAL-3 "VSIDE")	; 1-begreps Togsporsignal, montert på vegg eller på siden av annet signal, lanterner til venstre for yokeMountedarm
		(TOGSPORSIGNAL-3 "HSIDE") 	; 1-begreps Togsporsignal, montert på vegg eller på siden av annet signal, lanterner til høyre for yokeMountedarm

	(TraceLevel3 "TOGSPORSIGNAL-4")
		(TOGSPORSIGNAL-4 "VSIDE")	; 2-begreps Togsporsignal, montert på vegg eller på siden av annet signal, lanterner til venstre for yokeMountedarm
		(TOGSPORSIGNAL-4 "HSIDE")	; 2-begreps Togsporsignal, montert på vegg eller på siden av annet signal, lanterner til høyre for yokeMountedarm
)



(defun TOGSPORSIGNAL-1 ( yokeMounted / blockName )
	; Upright mast with 2 lanterns
	(setq 
		blockName "NO-BN-2D-JBTSI-SIGNAL-TOGSPORSIGNAL-1"
	)

	; Schematic symbol
	(DrawSingleUprightTrackSignal yokeMounted)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawSingleUprightTrackSignal yokeMounted)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



(defun TOGSPORSIGNAL-2 ( yokeMounted / blockName schematicSidewaysSeparation geoSidewaysSeparation )
	; Upright 'forked' mast with 2x2 lanterns
	(setq 
		blockName "NO-BN-2D-JBTSI-SIGNAL-TOGSPORSIGNAL-2"
		schematicSidewaysSeparation 3.5		; Spacing sideways between forked signal arms in schematic symbol
		geoSidewaysSeparation 2.5			; Spacing sideways between forked signal arms in geo symbols (to mitigate that close signals overlap)
	)

	; Schematic symbol
	(DrawForkedUprightTrackSignal yokeMounted schematicSidewaysSeparation)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	
	; Annotative symbol
	(DrawForkedUprightTrackSignal yokeMounted geoSidewaysSeparation)		; A narrower version than the schematic symbol
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



(defun TOGSPORSIGNAL-3 ( side / blockName )
	; Wall-mounted (no base), upright knee mast with 2 lanterns
	(setq 
		blockName (strcat "NO-BN-2D-JBTSI-SIGNAL-TOGSPORSIGNAL-3-" side)
		description (strcat "TOGSPORSIGNAL P" _uAA_ " KNE, TO ARMER")
	)

	; Schematic symbol
	(DrawSingleWallmountedTrackSignal side)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



(defun TOGSPORSIGNAL-4 ( side / blockName schematicSidewaysSeparation geoSidewaysSeparation )
	; Wall-mounted (no base), forked knee mast with 2x2 lanterns
	(setq 
		blockName (strcat "NO-BN-2D-JBTSI-SIGNAL-TOGSPORSIGNAL-4-" side)
		schematicSidewaysSeparation 3.5		; Spacing sideways between forked signal arms in schematic symbol
		geoSidewaysSeparation 2.5			; Spacing sideways between forked signal arms in geo symbols (to mitigate that close signals overlap)
	)
	; Schematic symbol
	(DrawForkedWallmountedTrackSignal schematicSidewaysSeparation side)
	(CreateSchematicBlockFromCurrentGraphics blockName)

	; Annotative symbol
	(DrawForkedWallmountedTrackSignal geoSidewaysSeparation side)			; A narrower version than the schematic symbol
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



;==========================
; Draw...X...() functions
;==========================
(defun DrawSingleUprightTrackSignal ( yokeMounted / r lowerPole upperPole )
	; Drawn lying, then rotated CCW.
	(setq 
		r (GetSmallLanternRadius) ; Togsporsignal lantern radius
		lowerPole 2.5
		upperPole 2.0
	)
	(command _CIRCLE_ (list r 0) r)
	(command _CIRCLE_ (list (* 3 r) 0) r)
	(if yokeMounted
		(progn
			(setq blockName (strcat blockName "-AAK"))
			(command _MOVE_ _selectAll_ _ENTER_ _setMoveDisplacement_ (list (- (+ (* 4 r) upperPole)) 0))
			(DrawLyingPole 0 (- upperPole))
		)
		(progn
			(command _MOVE_ _selectAll_ _ENTER_ _setMoveDisplacement_ (list (+ lowerPole upperPole) 0))
			(DrawLyingPole 0 (+ lowerPole upperPole))
			(DrawLyingTsBase)
		)
	)
	(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angle90_)
)



(defun DrawForkedUprightTrackSignal ( yokeMounted sidewaysSeparation / r lowerPole upperPole lat )
	; Drawn lying, then rotated CCW (upright) or CW (yoke mounting).
	(setq 
		r (GetSmallLanternRadius)					; Size relative to main signal lanterns
		lowerPole 2.5
		upperPole 2.0
		lat (/ sidewaysSeparation 2.0)		; Lateral displacement of forked arms (from center pole)
	)
	(command
		_LINE_ ; lower pole + left part of split mast
			_origo_
			(list lowerPole 0)
			(list lowerPole lat)
			(list (+ lowerPole upperPole) lat)
			_ENTER_
		_LINE_ ; right part of split mast
			(list lowerPole 0)
			(list lowerPole (- lat))
			(list (+ lowerPole upperPole) (- lat))
			_ENTER_
	)
	(command _CIRCLE_ (list (+ lowerPole upperPole r) lat) r)				; lower left lantern
	(command _CIRCLE_ (list (+ lowerPole upperPole (* 3 r)) lat) r)		; upper left lantern
	(command _CIRCLE_ (list (+ lowerPole upperPole r) (- lat)) r)			; lower right lantern
	(command _CIRCLE_ (list (+ lowerPole upperPole (* 3 r)) (- lat)) r)	; upper right lantern
	(if yokeMounted
		(progn ; rotate CW
			(setq blockName (strcat blockName "-AAK"))
			(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angleMinus90_)
		)
		(progn ; add base, rotate CCW
			(DrawLyingHsBase)
			(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angle90_)
		)
	)
)



(defun DrawSingleWallmountedTrackSignal ( side / r lowerPole upperPole knee )
	; Drawn upright
	(setq 
		r (GetSmallLanternRadius)  ; Size relative to main signal lanterns
		knee (* 1.75 (GetSmallLanternRadius))  ; Lateral displacement, horizontal part of 'knee'
		lowerPole 2.5
		upperPole 2.0
	)
	(command
		_LINE_
			_origo_
			(list knee 0)
			(list knee upperPole)
			_ENTER_
	)
	(command _CIRCLE_ (list knee (+ upperPole r)) r)			; lower lantern
	(command _CIRCLE_ (list knee (+ upperPole (* 3 r))) r)	; upper lantern
	(if (= side "VSIDE")	
		(command _MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _eraseMirrorSource_)
	)
)



(defun DrawForkedWallmountedTrackSignal ( sidewaysSeparation side / r lowerPole upperPole knee lat )
	; Drawn upright
	(setq 
		r (GetSmallLanternRadius)					; Size relative to main signal lanterns
		lowerPole 2.5
		upperPole 2.0
		knee (* 1.75 (GetSmallLanternRadius))  		; Lateral displacement, horizontal part of 'knee'
		lat (/ sidewaysSeparation 2.0)		; Lateral displacement of forked arms (from center pole)
	)
	(command
		_LINE_ ; Lower pole + left part of split mast
			_origo_
			(list lowerPole 0)
			(list lowerPole lat)
			(list (+ lowerPole upperPole) lat)
			_ENTER_
		_LINE_ ; right part of split mast
			(list lowerPole 0)
			(list lowerPole (- lat))
			(list (+ lowerPole upperPole) (- lat))
			_ENTER_
	)
	(command _CIRCLE_ (list (+ lowerPole upperPole r) lat) r)				; lower left lantern
	(command _CIRCLE_ (list (+ lowerPole upperPole (* 3 r)) lat) r)		; upper left lantern
	(command _CIRCLE_ (list (+ lowerPole upperPole r) (- lat)) r)			; lower right lantern
	(command _CIRCLE_ (list (+ lowerPole upperPole (* 3 r)) (- lat)) r)	; upper right lantern
	(command _MOVE_ _selectAll_ _ENTER_ "_Displacement" (list 0 (+ lat knee)))		; Move "left"
	(command _LINE_ _origo_ (list 0 (+ knee lat)) _ENTER_)  						; Add horizontal "knee"
	(if (= side "HSIDE")
		(command _MIRROR_ _selectAll_ _ENTER_ _origo_ _xAxis_ _eraseMirrorSource_)
	)
	(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angle90_)
)
