;=========================================================================================================================
;
; NO-BN ERTMS-skilt.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-09-02 CLFEY Removed E34 and E35 markerboards. Added E35 to the .LSP file for Combined Signals.
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
; TODO list:
; 2020-09-13 CLFEY Clarify which ERTMS 2D-symbols should be present - and create those.
;
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
	(NOBN-ERTMS-LEVEL-CROSSING "ANNOUNCE" "YOKE")
	(NOBN-ERTMS-LEVEL-CROSSING nil nil)
	(NOBN-ERTMS-LEVEL-CROSSING nil "YOKE")

	; ERTMS boards
	; E??
	(TraceLevel2 "ERTMS-SHUNTING-AREA")
	(NOBN-ERTMS-SHUNTING-AREA "BEGIN" nil)
	(NOBN-ERTMS-SHUNTING-AREA "BEGIN" "YOKE")
	(NOBN-ERTMS-SHUNTING-AREA "END" nil)
	(NOBN-ERTMS-SHUNTING-AREA "END" "YOKE")

	; E??
	; ( there is no "BEGIN" here)
	(TraceLevel2 "ERTMS-SUPERVISED-AREA")
	(NOBN-ERTMS-SUPERVISED-AREA "END" nil)
	(NOBN-ERTMS-SUPERVISED-AREA "END" "YOKE")

	; E37 Systemovergang
	(TraceLevel2 "ERTMS-LEVEL-TRANSITION")
	(NOBN-ERTMS-LEVEL-TRANSITION)
)



;=======================
; ERTMS signals
;=======================
; 2020-09-02 CLFEY Note: This function is deprecated. See Combined Signals LISP file.



(defun NOBN-ERTMS-LEVEL-CROSSING ( distantSignal mounting / blockName pole yokePole x y )
	(setq
		blockName (strcat _SIG_ "MSS-" "SKILT-ERTMS-" "LEVEL-CROSSING")
		pole 6.0
		yokePole 2.0
		x 6.0 ; surrounding box
		y x
	)
	(if distantSignal
		(setq blockName (strcat blockName "-" distantSignal))
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

	; Add captions below yoke mount / above mast mount;
	(if (= mounting "YOKE")
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
	(if (= mounting "YOKE")
		(progn
			(command
				_MOVE_ _selectAll_ _ENTER_ (list 0 (+ (/ y 2) yokePole)) _origin_ ; move down by half of surrounding box plus short pole
				_LINE_ _origin_ (list 0 (- yokePole)) _ENTER_ ; add suspension pole from yoke
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



(defun NOBN-ERTMS-SHUNTING-AREA ( beginOrEnd mounting / blockName pole yokePole x y txtHeight )
	(setq
		blockName (strcat _SIG_ "MSS-" "SKILT-ERTMS-" "SHUNTING-AREA-" beginOrEnd)
		pole 6.0
		yokePole 2.0
		x 6.0 ; surrounding box
		y x
		txtHeight (* pole 0.60)
	)
	(if mounting
		(setq blockName (strcat blockName "-" mounting))
	)
	
	; Surrounding box:
	(DrawBox layDef_Zero x y _noWipeout_)
	
	; Text 'SH' for 'shunting':
	(AddTextAtPoint layDef_Zero txtHeight _origin_ "SH")

	(if (= beginOrEnd "END")
		; Add three inclined 'slash' lines (symbol is drawn upright here)
		(command
			_LINE_ (list (/ x -2) (/ y -2)) (list (/ x 2) (/ y 2)) _ENTER_ ; Diagonal
			_LINE_ (list (/ x -2) (/ y -3)) (list (/ x 3) (/ y 2)) _ENTER_ ; Above diagonal
			_LINE_ (list (/ x -3) (/ y -2)) (list (/ x 2) (/ y 3)) _ENTER_ ; Below diagonal
		)
	)

	; Epilogue:
	(if (= mounting "YOKE")
		(progn
			(command
				_MOVE_ _selectAll_ _ENTER_ (list 0 (+ (/ y 2) yokePole)) _origin_ ; move down by half of surrounding box plus short pole
				_LINE_ _origin_ (list 0 (- yokePole)) _ENTER_ ; add suspension pole from yoke
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



(defun NOBN-ERTMS-SUPERVISED-AREA ( beginOrEnd mounting / blockName pole yokePole x y yoffs ptul ptll ptlr ptlm ptbm )
	(setq
		blockName (strcat _SIG_ "MSS-" "SKILT-ERTMS-" "SUPERVISED-AREA-" beginOrEnd)
		pole 6.0
		yokePole 2.0
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
	(if (= mounting "YOKE")
		(progn
			(command
				_MOVE_ _selectAll_ _ENTER_ (list 0 (+ (/ y 2) yokePole)) _origin_ ; move down by half of surrounding box plus short pole
				_LINE_ _origin_ (list 0 (- yokePole)) _ENTER_ ; add suspension pole from yoke
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



(defun NOBN-ERTMS-LEVEL-TRANSITION ( / blockName x y txtHeight )
	; NB In example Bane NOR drawing ERP-S0-S-00005_00B_002, this symbol had a short mast pole (1.5) and just a line as a base (3).
	; We decided to treat this one as a board - i.e RC will provide the tail, be it in schematic or in geo mode.
	; We also decided to reduce it to same size as other ERTMS signals, 6 by 6, and remove the circle inside the signal.
	; See https://orv.banenor.no/orv/doku.php?id=tjn:Kapittel_8 section 8,76 "Signal for systemovergang på strekning med ERTMS".
	(setq
		blockName (strcat _SIG_ "MSS-" "SKILT-ERTMS-" "LEVEL-TRANSITION")
		x 6.0 ; surrounding box (not 9 as in Bane NOR's example found in an old drawing)
		y x
		txtHeight (* x 0.30)
	)

	; Surrounding box:
	(DrawBox layDef_Zero x y _noWipeout_)
	(AddTextAtPoint layDef_Zero  _th180_ (list 0 (* 0.25 y)) "LT")
	(AddTextAtPoint layDef_Zero  _th180_ (list 0 (* -0.25 y)) "ETCS")
	
	; Epilogue:
	(MoveUp (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
