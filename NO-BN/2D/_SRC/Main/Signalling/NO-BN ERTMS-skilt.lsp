;=========================================================================================================================
;
; NO-BN ERTMS-skilt.lsp
;
; Copyright (c) 2015-2026 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-09-02 CLFEY Removed E34 and E35 markerboards. Added E35 to the .LSP file for Combined Signals.
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
; 2024-04-16 SVNOE Added E37B. Clarified E106 vs E107 and E108, todo: update symbol graphics accordingly.
; 2026-01-16 CLFEY Fjernet skilt ERTMS-LEVEL-TRANSITION og ERTMS-LEVEL-ZERO. Disse fanges opp som "E37" og
;                  "E37B" av rutine (GetBoardAndPoleNames) som kalles fra 21_BoardsAndPoles.lsp.
; 2026-03-10 WIWIJ Added E38 (Avalanche area) and E39 (Frostport).

; TODO 2026-01-16 CLFEY rename alle ERTMS boards according to same conventsions as oprdinary signals. Use ORV numbers.

;=========================================================================================================================

; ERTMS marker boards and operational state boards


(defun NOBN-ERTMS-SKILT ( / )
	;
	; See trv \ Bane NOR document "ERTMS Programme Design requirements - Signs and boards" Doc.no. 1000001649_00E dated 2020-04-28.
	; Symbols: See example files from Bane NOR (drawings).
	;
	; Location board and stop board: See Bane NOR Signal-Combinations.
	;
	(SetCadSystemDefaults)
		
	; ERTMS signals
	; E36 Veisikringsanlegg - Level Crossing
	(TraceLevel2 "ERTMS-LEVEL-CROSSING")
	(NOBN-ERTMS-LEVEL-CROSSING "ANNOUNCE" nil)
	(NOBN-ERTMS-LEVEL-CROSSING "ANNOUNCE" "PORTAL")
	(NOBN-ERTMS-LEVEL-CROSSING nil nil)
	(NOBN-ERTMS-LEVEL-CROSSING nil "PORTAL")

	; ERTMS boards
	; E38 Skredomraade - Avalanche area
	(TraceLevel2 "E38")						(E38)

	; E39 Frostport
	(TraceLevel2 "E39")						(E39)

	; E106A - Stop for shunting, location
	; E106B - Stop for shunting, announcement
	(TraceLevel2 "E106A")					(E106 "LOCATION"		nil		)
	(TraceLevel2 "E106A portalmounted")		(E106 "LOCATION" 		"PORTAL")
	(TraceLevel2 "E106B")					(E106 "ANNOUNCEMENT"	nil		)
	(TraceLevel2 "E106B portalmounted")		(E106 "ANNOUNCEMENT"	"PORTAL")

	; E107
	(TraceLevel2 "ERTMS-INTERLOCKED-AREA")
	(NOBN-ERTMS-INTERLOCKED-AREA "END" nil)
	(NOBN-ERTMS-INTERLOCKED-AREA "END" "PORTAL")

	; E108
	(TraceLevel2 "ERTMS-INTERLOCKED-AREA")
	(NOBN-ERTMS-INTERLOCKED-AREA "BEGIN" nil)
	(NOBN-ERTMS-INTERLOCKED-AREA "BEGIN" "PORTAL")
)



(defun NOBN-ERTMS-LEVEL-CROSSING ( locationOrAnnouncement mounting / blockName pole portalPole x y )
	(setq
		blockName (strcat _SIG_ "MSS-" "SKILT-ERTMS-" "LEVEL-CROSSING")
		pole 6.0
		portalPole 2.0
		x 6.0 ; surrounding box
		y x
	)
	(if (= locationOrAnnouncement "ANNOUNCE")
		(setq description "SKILT ERTMS VARSEL OM VEISIKRINGSANLEGG")
		(setq description "SKILT ERTMS VEISIKRINGSANLEGG")
	)
	(if locationOrAnnouncement
		(setq blockName (strcat blockName "-" locationOrAnnouncement))
	)
	(if mounting
		(setq blockName (strcat blockName "-" mounting))
	)
	
	; Surrounding box:
	(DrawBox layDef_Zero x y _noWipeout_)
	
	; Level Crossing symbol:
	(command
		_RECTANGLE_ (list (* -0.3 x) (* -0.4 y)) (list (* 0.30 x) (* -0.35 y)) 	; 'base'
		_RECTANGLE_ (list (* -0.05 x) (* -0.35 y)) (list (* 0.05 x) (* 0.40 y))	; 'pole'
		_RECTANGLE_ (list (* -0.30 x) (* 0.24 y)) (list (* 0.30 x) (* 0.26 y))	; 'arm1'...
		_ROTATE_ _lastSelection_ _ENTER_ (list 0 (* 0.25 y)) _angle30_							; rotate arm1 by 30 deg CW
		_MIRROR_ _lastSelection_ _ENTER_ (list 0 (* 0.25 y)) (list 1 (* 0.25 y)) _keepMirrorSource_	; mirror to make arm2
	)

	; Add captions below portal mount / above mast mount;
	(if (= mounting "PORTAL")
		(if (= distantSignal "ANNOUNCE")
			(AddTextAtPointWithJustification layDef_Zero _th100_ (list 0 (* -0.55 y)) "PLO-Fs" _topCenter_) ; below surrounding box
			(AddTextAtPointWithJustification layDef_Zero _th100_ (list 0 (* -0.55 y)) "PLO" _topCenter_) ; below surrounding box
		)
	;else
		(if (= distantSignal "ANNOUNCE")
			(AddTextAtPointWithJustification layDef_Zero _th100_ (list 0 (* 0.55 y)) "PLO-Fs" _BottomCenter_) ; above surrounding box
			(AddTextAtPointWithJustification layDef_Zero _th100_ (list 0 (* 0.55 y)) "PLO" _BottomCenter_) ; above surrounding box
		)
	)

	; Epilogue:
	(if (= mounting "PORTAL")
		(progn
			(command
				_MOVE_ _selectAll_ _ENTER_ (list 0 (+ (/ y 2) portalPole)) _origin_ ; move down by half of surrounding box plus short pole
				_LINE_ _origin_ (list 0 (- portalPole)) _ENTER_ ; add suspension pole from portal
			)
		)
		(progn
			(command _MOVE_ _selectAll_ _ENTER_ _origin_ (list 0 (+ (/ y 2) pole))) ; move up by half of surrounding box plus main pole
			(command _ROTATE_ _selectAll_ _ENTER_ _origin_ _angleMinus90_) ; rotate CW 90 deg because NOBN_DrawLyingPole() and NOBN_DrawLyingHsBase() functions expect that orientation...
			(NOBN_DrawLyingPole 0 pole)
			(NOBN_DrawLyingHsBase)
			(command _ROTATE_ _selectAll_ _ENTER_ _origin_ _angle90_) ; rotate back to upright orientation
		)
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun E38 ( / blockName description x y r )
	; Avalanche area (Skredomraade)
	;
	; TL-----TR
	; |       |
	; | ( R ) | ; Letter 'R' in a circle
	; |       |
	; BL--.--BR
	;
	(setq
		blockName (strcat _SIG_ "MSS-SKILT-ERTMS-AVALANCHE-AREA")
		description "SKILT E38 SKREDOMRAADE"
		x 4.5
		y 4.5
		r 1.5
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(DrawCircle layDef_Zero r _noWipeout_)
	(AddTextAtPoint layDef_Zero (* 0.5 x) _origin_ "R") ; Letter size is 50% of side
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun E39 ( / blockName description x y r )
	; Frostport
	;
	; TL-----TR
	; |       |
	; | (FP)  | ; Letters 'FP' in a circle
	; |       |
	; BL--.--BR
	;
	(setq
		blockName (strcat _SIG_ "MSS-SKILT-ERTMS-FROSTPORT")
		description "SKILT E39 FROSTPORT"
		x 4.5
		y 4.5
		r 1.5
	)
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
	(DrawCircle layDef_Zero r _noWipeout_)
	(AddTextAtPoint layDef_Zero (* 0.4 x) _origin_ "FP") ; Slightly smaller height to fit two characters
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	description ; Used if table is created
)



(defun E106 ( locationOrAnnouncement mounting / blockName pole portalPole x y txtHeight )
;
; +----------+
; | 2----1\  |
; | |    6 5 | 	; Medium hatch = "yellow" = location 106A
; | |(8)(9)| | 	; No hatch = "white" = announcement 106B
; | |      | |
; | 3------4 |
; +----------+
;
	(if (= locationOrAnnouncement "LOCATION")
		(progn
			(setq blockName "NO-BN-2D-JBTSA_MSS-SKILT-KJOERENDE-SIGNAL-E106A-STOPP-FOR-SKIFT")
			(setq description "SKILT E106A STOPP FOR SKIFT")
		)
		(progn
			(setq blockName "NO-BN-2D-JBTSA_MSS-SKILT-KJOERENDE-SIGNAL-E106B-VARSEL-OM-STOPP-FOR-SKIFT")
			(setq description "SKILT E106B VARSEL OM STOPP FOR SKIFT")
		)
	)
	(setq
		pole 6.0
		portalPole 2.0
		x 6.0 ; surrounding box
		y x
		r (* 0.100 x)

		p1 (list (* x  0.100) (* y  0.350))	; Dwarf signal outline
		p2 (list (* x -0.350) (* y  0.350))
		p3 (list (* x -0.350) (* y -0.400))
		p4 (list (* x  0.350) (* y -0.400))
		p5 (list (* x  0.350) (* y  0.100))
		
		p6 (list (* x  0.100) (* y  0.100))	; Center of arc p1-p5

		p7 (list (* x -0.175) (* y  0.075)) ; Left 'eye'
		p8 (list (* x  0.175) (* y  0.075)) ; Right 'eye
	)
	(if mounting
		(setq blockName (strcat blockName "-" mounting))
	)

	(DrawBox layDef_Zero x y _noWipeout_)
	
	; Draw dark 'eyes'
	(DrawCircleAtPoint layDef_Zero p7 r _noWipeout_)
	(DrawHatchAtPoint _denseHatch_ p7 _angleZero_ _offsetZero_)
	(DrawCircleAtPoint layDef_Zero p8 r _noWipeout_)
	(DrawHatchAtPoint _denseHatch_ p8 _angleZero_ _offsetZero_)

	(if (= locationOrAnnouncement "LOCATION")
		(DrawHatchAtPoint _sparseHatch_ _origin_ _angleZero_ _offsetZero_)
	)

	; Dwarf signal outline
	(command _POLYLINE_ p1 p2 p3 p4 p5 _openPolyline_)
	(DrawArcByCenter layDef_Zero p6 p5 p1)

	; Epilogue:
	(if (= mounting "PORTAL")
		(progn
			(command
				_MOVE_ _selectAll_ _ENTER_ (list 0 (+ (/ y 2) portalPole)) _origin_ ; move down by half of surrounding box plus short pole
				_LINE_ _origin_ (list 0 (- portalPole)) _ENTER_ ; add suspension pole from portal
			)
		)
		(progn
			(command _MOVE_ _selectAll_ _ENTER_ _origin_ (list 0 (+ (/ y 2) pole))) ; move up by half of surrounding box plus main pole
			(command _ROTATE_ _selectAll_ _ENTER_ _origin_ _angleMinus90_) ; rotate CW 90 deg because NOBN_DrawLyingPole() and NOBN_DrawLyingHsBase() functions expect that orientation...
			(NOBN_DrawLyingPole 0 pole)
			(NOBN_DrawLyingHsBase)
			(command _ROTATE_ _selectAll_ _ENTER_ _origin_ _angle90_) ; rotate back to upright orientation
		)
	)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun NOBN-ERTMS-INTERLOCKED-AREA ( beginOrEnd mounting / blockName pole portalPole x y yoffs ptul ptll ptlr ptlm ptbm )
	(setq
		blockName (strcat _SIG_ "MSS-" "SKILT-ERTMS-" "INTERLOCKED-AREA-" beginOrEnd)
		pole 6.0
		portalPole 2.0
		x 6.0 ; surrounding box
		y x
	)
	(if mounting
		(setq blockName (strcat blockName "-" mounting))
	)
	
	; Surrounding box:
	(command _RECTANGLE_ (list (/ x -2) (/ y -2)) (list (/ x 2) (/ y 2)))

	;Draw a shunting signal symbol (for European style right-side driving)
	(command
		(setq 
			yoffs (/ y 10) ; The dwarf signal house shape will be displaced up to make space for a short pole
			ptul (list (/ x -4) (+ yoffs (/ y 4))) ; Point upper left corner
			ptll (list (/ x -4) (+ yoffs (/ y -4))) ; Point lower left corner
			ptlr (list (/ x 4) (+ yoffs (/ y -4))) ; Point lower right corner
			ptlm (list 0 (+ yoffs (/ y -4))) ; Point lower midpoint
			ptbm (list 0 (+ yoffs (/ y -2))) ; Point bottom of pole midpoint
		)
		_POLYLINE_ 
			ptul ptll ptlr 
			 _setPolylineArcMode_ 
			 _setPolylineArcDirection_ _angle90_
			 ptul
			 _ENTER_
		_POLYLINE_ ptlm ptbm _ENTER_
	)
	(if (= beginOrEnd "END")
		(command ; Add three inclined 'slash' lines:
			_LINE_ (list (/ x -2) (/ y -2)) (list (/ x 2) (/ y 2)) _ENTER_ ; Diagonal
			_LINE_ (list (/ x -2) (/ y -3)) (list (/ x 3) (/ y 2)) _ENTER_ ; Above diagonal
			_LINE_ (list (/ x -3) (/ y -2)) (list (/ x 2) (/ y 3)) _ENTER_ ; Below diagonal
		)
	)

	; Epilogue:
	(if (= mounting "PORTAL")
		(progn
			(command
				_MOVE_ _selectAll_ _ENTER_ (list 0 (+ (/ y 2) portalPole)) _origin_ ; move down by half of surrounding box plus short pole
				_LINE_ _origin_ (list 0 (- portalPole)) _ENTER_ ; add suspension pole from portal
			)
		)
		(progn
			(command _MOVE_ _selectAll_ _ENTER_ _origin_ (list 0 (+ (/ y 2) pole))) ; move up by half of surrounding box plus main pole
			(command _ROTATE_ _selectAll_ _ENTER_ _origin_ _angleMinus90_) ; rotate CW 90 deg because NOBN_DrawLyingPole() and NOBN_DrawLyingHsBase() functions expect that orientation...
			(NOBN_DrawLyingPole 0 pole)
			(NOBN_DrawLyingHsBase)
			(command _ROTATE_ _selectAll_ _ENTER_ _origin_ _angle90_) ; rotate back to upright orientation
		)
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
