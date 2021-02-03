;=========================================================================================================================
;
; Ertms boards.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2020-09-02 CLFEY Removed E34 and E35 markerboards. Added E35 to the .LSP file for Combined Signals.
; 2021-02-10 CLFEY Release 2021.a
;
; TODO list:
; 2020-09-13 CLFEY Clarify which ERTMS 2D-symbols should be present - and create those.
;
;=========================================================================================================================

; ERTMS marker boards and operational state boards

(defun C:ERTMS-BOARDS ( / )
	;
	; See trv \ Bane NOR document "ERTMS Programme Design requirements - Signs and boards" Doc.no. 1000001649_00E dated 2020-04-28.
	; 
	; Symbols: See example files from Bane NOR (drawings).
	; TODO: 2020-08-04 CLFEY - missing boards, see ORV. Move these routines over to "Boards and poles" Lisp routine, one file per board (group).
	;
	(setCadSystemDefaults)
		
	; ERTMS signals
	; E36 Veisikringsanlegg - Level Crossing
	(subStep "ERTMS-LEVEL-CROSSING")
	(ERTMS-LEVEL-CROSSING "FORSIGNAL" nil)
	(ERTMS-LEVEL-CROSSING "FORSIGNAL" "AAK")
	(ERTMS-LEVEL-CROSSING nil nil)
	(ERTMS-LEVEL-CROSSING nil "AAK")

	; ERTMS boards
	; E??
	(subStep "ERTMS-SHUNTING-AREA")
	(ERTMS-SHUNTING-AREA "BEGIN" nil)
	(ERTMS-SHUNTING-AREA "BEGIN" "AAK")
	(ERTMS-SHUNTING-AREA "END" nil)
	(ERTMS-SHUNTING-AREA "END" "AAK")

	; E??
	; ( there is no "BEGIN" here)
	(subStep "ERTMS-SUPERVISED-AREA")
	(ERTMS-SUPERVISED-AREA "END" nil)
	(ERTMS-SUPERVISED-AREA "END" "AAK")

	; E37 Systemovergang
	(subStep "ERTMS-LEVEL-TRANSITION")
	(ERTMS-LEVEL-TRANSITION)
)



;=======================
; ERTMS signals
;=======================
; 2020-09-02 CLFEY Note: This function is deprecated. See Combined Signals LISP file.
;(defun ERTMS ( variation mounting dir / blockName pole yokePole x y )	;<-- defines a function with name ERTMS, lists arguments and local variables of function. 
;	; Input parameters to the left of /, local variables to the right.
;	; Local variables are deleted after function call. 
;	; NB: If an identifier is not declared as local, but is used in (setq xyzzy ...) then 'xyzzy' will be globally accessible afterwards.
;	; All lines below this one are function bodies, that perform operations on the arguments.
;	(setq
;		blockName (strcat "NO-BN-2D-JBTSI-SIGNAL-ERTMS-" variation)	;<-- assigns value to the local variable blockName. 
;		pole 6.0
;		yokePole 2.0
;		x 6.0 ; surrounding box
;		y x
;	)
;	(if dir				;<-- checks if direction is assigned, and appends the value to the name. Only false if dir=nil.
;		(setq blockName (strcat blockName "-" dir))
;	)
;	(if mounting			;<-- checks if 'mounting' is different from nil, if so we append 'mounting' (being "AAK" (yoke) here) to the block name.
;		(setq blockName (strcat blockName "-" mounting))	
;	)
;
;	; 'command' executes AutoCAD commands. This is where the actual graphics for the sign is constructed. First line constructs a rectangle with a corner in (0,0) and (2,2)
;	; draws multiple polylines by calling the autoCAD command PLINE. This is where the arrow is constructed, pointing up, centered at origo.
;	; _ENTER_ gives 'enter', which ends the polyline. This has drawn the "bottom half" of the arrow.
;	; At the end, we mirror the arrow halves. _LAST + _ENTER_ selects the last object drawn, in this case the polyline. 
;	; The two points define the mirror axis. _keepMirrorSource_ mirrors without deleting the base object being mirrored.
;
;	 ; Surrounding box:
;	(command _RECTANGLE_ (list (/ x -2) (/ y -2)) (list (/ x 2) (/ y 2)))
;
;	 ; Arrow pointing straight up:
;	 (setq	
;	 (command
;		_POLYLINE_ 
;			(list 0 (* 0.45 y))
;			(list (* 0.45 x) 0)
;			(list (* 0.10 x) 0)
;			(list (* 0.10 x) (* -0.45 y))
;			(list 0 (* -0.45 y))
;			_ENTER_					
;		_MIRROR_ _lastSelection_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_
;	)
;	(if (= variation "E35-STOPPSKILT")
;		(drawHatchFromPoint 0.02 (list (* 0.45 x) (* 0.45 y)) 0 0)
;	)
;
;	(cond 
;		((= dir "HSIDE")			 					;<-- checks if direction is "VSIDE" (left) and rotates into right orientation
;			(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angle90_) ; rotate box with arrow 90 deg CW, now arrow points towards the left
;		)
;		((= dir "VSIDE")			 					;<-- checks if direction is "VSIDE" (left) and rotates into right orientation
;			(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angleMinus90_) ; rotate box with arrow 90 deg CCW, now arrow points towards the right
;		)
;		((= dir "OVER")
;			(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angle180_) ; rotate box with arrow 180 deg, now arrow points down
;		)
;	)
;
;	; Epilogue:
;	(if (= mounting "AAK")
;		(progn
;			(command
;				_MOVE_ _selectAll_ _ENTER_ (list 0 (+ (/ y 2) yokePole)) _origo_ ; move down by half of surrounding box plus short pole
;				_LINE_ _origo_ (list 0 (- yokePole)) _ENTER_ ; add suspension pole from yoke
;			)
;		)
;		(progn
;			(command _MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 (+ (/ y 2) pole))) ; move up by half of surrounding box plus main pole
;			(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angleMinus90_) ; rotate CW 90 deg because drawLyingPole() and drawLyingHsBase() functions expect that orientation...
;			(drawLyingPole 0 pole)
;			(drawLyingHsBase)
;			(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angle90_) ; rotate back to upright orientation
;		)
;	)
;	(createSchematicBlockFromCurrentGraphics blockName)
;	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
;)



(defun ERTMS-LEVEL-CROSSING ( distantSignal mounting / blockName pole yokePole x y )
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-ERTMS-PLANOVERGANG"
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
	(command _RECTANGLE_ (list (/ x -2) (/ y -2)) (list (/ x 2) (/ y 2)))
	
	; Level Crossing symbol:
	(command
		_RECTANGLE_ (list (* -0.3 x) (* -0.4 y)) (list (* 0.30 x) (* -0.35 y)) 	; 'base'
		_RECTANGLE_ (list (* -0.05 x) (* -0.35 y)) (list (* 0.05 x) (* 0.40 y))	; 'pole'
		_RECTANGLE_ (list (* -0.30 x) (* 0.24 y)) (list (* 0.30 x) (* 0.26 y))	; 'arm1'...
		_ROTATE_ _lastSelection_ _ENTER_ (list 0 (* 0.25 y)) _angle30_							; rotate arm1 by 30 deg CW
		_MIRROR_ _lastSelection_ _ENTER_ (list 0 (* 0.25 y)) (list 1 (* 0.25 y)) _keepMirrorSource_	; mirror to make arm2
	)

	; Add captions below yoke mount / above mast mount;
	(if (= mounting "AAK")
		(if (= distantSignal "FORSIGNAL")
			(addText "PLO-Fs" (list 0 (* -0.55 y)) _th100_ _angleZero_ _rcTextStyle_ _topCenter_) ; below surrounding box
			(addText "PLO" (list 0 (* -0.55 y)) _th100_ _angleZero_ _rcTextStyle_ _topCenter_) ; below surrounding box
		)
	;else
		(if (= distantSignal "FORSIGNAL")
			(addText "PLO-Fs" (list 0 (* 0.55 y)) _th100_ _angleZero_ _rcTextStyle_ _BottomCenter_) ; above surrounding box
			(addText "PLO" (list 0 (* 0.55 y)) _th100_ _angleZero_ _rcTextStyle_ _BottomCenter_) ; above surrounding box
		)
	)

	; Epilogue:
	(if (= mounting "AAK")
		(progn
			(command
				_MOVE_ _selectAll_ _ENTER_ (list 0 (+ (/ y 2) yokePole)) _origo_ ; move down by half of surrounding box plus short pole
				_LINE_ _origo_ (list 0 (- yokePole)) _ENTER_ ; add suspension pole from yoke
			)
		)
		(progn
			(command _MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 (+ (/ y 2) pole))) ; move up by half of surrounding box plus main pole
			(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angleMinus90_) ; rotate CW 90 deg because drawLyingPole() and drawLyingHsBase() functions expect that orientation...
			(drawLyingPole 0 pole)
			(drawLyingHsBase)
			(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angle90_) ; rotate back to upright orientation
		)
	)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun ERTMS-SHUNTING-AREA ( beginOrEnd mounting / blockName pole yokePole x y txtHeight )
	(setq
		blockName (strcat "NO-BN-2D-JBTSI-SIGNAL-ERTMS-SHUNTING-AREA-" beginOrEnd)
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
	(command _RECTANGLE_ (list (/ x -2) (/ y -2)) (list (/ x 2) (/ y 2)))
	
	; Text 'SH' for 'shunting':
	(addMText "SH" _origo_ txtHeight 3 0 _rcTextStyle_ _middleCenter_)

	(if (= beginOrEnd "END")
		; Add three inclined 'slash' lines (symbol is drawn upright here)
		(command
			_LINE_ (list (/ x -2) (/ y -2)) (list (/ x 2) (/ y 2)) _ENTER_ ; Diagonal
			_LINE_ (list (/ x -2) (/ y -3)) (list (/ x 3) (/ y 2)) _ENTER_ ; Above diagonal
			_LINE_ (list (/ x -3) (/ y -2)) (list (/ x 2) (/ y 3)) _ENTER_ ; Below diagonal
		)
	)

	; Epilogue:
	(if (= mounting "AAK")
		(progn
			(command
				_MOVE_ _selectAll_ _ENTER_ (list 0 (+ (/ y 2) yokePole)) _origo_ ; move down by half of surrounding box plus short pole
				_LINE_ _origo_ (list 0 (- yokePole)) _ENTER_ ; add suspension pole from yoke
			)
		)
		(progn
			(command _MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 (+ (/ y 2) pole))) ; move up by half of surrounding box plus main pole
			(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angleMinus90_) ; rotate CW 90 deg because drawLyingPole() and drawLyingHsBase() functions expect that orientation...
			(drawLyingPole 0 pole)
			(drawLyingHsBase)
			(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angle90_) ; rotate back to upright orientation
		)
	)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun ERTMS-SUPERVISED-AREA ( beginOrEnd mounting / blockName pole yokePole x y yoffs ptul ptll ptlr ptlm ptbm )
	(setq
		blockName (strcat "NO-BN-2D-JBTSI-SIGNAL-ERTMS-SUPERVISED-AREA-" beginOrEnd)
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
	(if (= mounting "AAK")
		(progn
			(command
				_MOVE_ _selectAll_ _ENTER_ (list 0 (+ (/ y 2) yokePole)) _origo_ ; move down by half of surrounding box plus short pole
				_LINE_ _origo_ (list 0 (- yokePole)) _ENTER_ ; add suspension pole from yoke
			)
		)
		(progn
			(command _MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 (+ (/ y 2) pole))) ; move up by half of surrounding box plus main pole
			(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angleMinus90_) ; rotate CW 90 deg because drawLyingPole() and drawLyingHsBase() functions expect that orientation...
			(drawLyingPole 0 pole)
			(drawLyingHsBase)
			(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angle90_) ; rotate back to upright orientation
		)
	)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



;==========================
; ERTMS boards (not really signals)
;==========================
(defun ERTMS-LEVEL-TRANSITION ( / blockName x y txtHeight )
	; NB In example Bane NOR drawing ERP-S0-S-00005_00B_002, this symbol had a short mast pole (1.5) and just a line as a base (3).
	; We decided to treat this one as a board - i.e RC will provide the tail, be it in schematic or in geo mode.
	; We also decided to reduce it to same size as other ERTMS signals, 6 by 6, and remove the circle inside the signal.
	; See https://orv.banenor.no/orv/doku.php?id=tjn:Kapittel_8 section 8,76 "Signal for systemovergang på strekning med ERTMS".
	(setq
		blockName (strcat "NO-BN-2D-SKILT-KJOERENDE-ERTMS-LEVEL-TRANSITION")
		x 6.0 ; surrounding box (not 9 as in Bane NOR's example found in an old drawing)
		y x
		txtHeight (* x 0.30)
	)

	; Surrounding box:
	(command _RECTANGLE_ (list (/ x -2) (/ y -2)) (list (/ x 2) (/ y 2)))

	; Add "LT ETCS" inside a circle:
	;intentionally omitted: 		(command _CIRCLE_ _origo_ (* (/ 8.0 9.0) x) 
	(addText "LT" (list 0 (* 0.25 y)) 1.8 0 _rcTextStyle_ _middleCenter_)
	(addText "ETCS" (list 0 (* -0.25 y)) 1.8 0 _rcTextStyle_ _middleCenter_)
	
	; Epilogue:
	(command _MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 (/ y 2))) ; move up by half of surrounding box, board insertion point is middle lower side
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

