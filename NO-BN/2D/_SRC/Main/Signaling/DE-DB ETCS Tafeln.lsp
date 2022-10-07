;=========================================================================================================================
;
; DE-DB ETCS Tafeln.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; ERTMS marker boards and operational state boards

(defun DEDB-ETCS-TAFELN ( / )

	(SetCadSystemDefaults)

	(TraceLevel2 "DEDB-ETCS-HALT-TAFEL")		(DEDB-ETCS-HALT-TAFEL)

	; ETCS various other boards
	;(TraceLevel2 "ERTMS-LEVEL-TRANSITION")		(DEDB-ETCS-LEVEL-TRANSITION)
	; TODO all the rest...
)



;=======================
; ERTMS signals
;=======================
(defun DEDB-ETCS-HALT-TAFEL ( / blockName description x y mountingMethod dir thisBlockName thisDescription )
	(setq
		blockName	(strcat _SIG_ "EHT-" "ETCS-HALT-TAFEL")
		description	(strcat              "ETCS HALT-TAFEL")
		x	6.000
		y	6.000
	)
	(foreach mountingMethod '("MAST" "NIEDRIG" "BRUECKE" "LINKSSEITIG" "RECHTSSEITIG" "DACH")
		(TraceLevel3 (strcat "DEDB-ETCS-HALT-TAFEL / " mountingMethod))
		(foreach dir (list _right_ _left_ _down_)
			(setq
				dirText			(cond ((= dir _left_) "NACH_LINKS") ((= dir _right_) "NACH_RECHTS") ((= dir _down_) "NACH_UNTEN"))
				thisBlockName	(strcat blockName "-" dirText "-" mountingMethod)
				thisDescription	(strcat description ", " dirText  ", " mountingMethod)
			)
			(DEDB_DrawMB dir)
			(cond 
				((= mountingMethod "MAST")			(MoveUp   (+ (HalfOf y) (DEDB_GetKsNormalSupportHeight)))													(DEDB_DrawNormalSupportForKs))
				((= mountingMethod "NIEDRIG")		(MoveUp   (+ (HalfOf y) (DEDB_GetKslLowSupportHeight)))														(DEDB_DrawSupportForGroundlevelBoardAndNonKs))
				((= mountingMethod "BRUECKE")		(MoveDown (+ (HalfOf y) (DEDB_GetKsYokeSuspensionMastHeight)))												(DEDB_DrawKsYokeSuspensionMast))
				((= mountingMethod "LINKSSEITIG")	(MoveDown (+ (HalfOf y) (DEDB_GetMbWallSupportHeight)))			(MoveRight (DEDB_GetMbWallSupportWidth))	(DEDB_DrawMbWallSupportLeftsided))
				((= mountingMethod "RECHTSSEITIG")	(MoveDown (+ (HalfOf y) (DEDB_GetMbWallSupportHeight)))			(MoveLeft  (DEDB_GetMbWallSupportWidth))	(DEDB_DrawMbWallSupportRightsided))
				((= mountingMethod "DACH")			(MoveDown (+ (HalfOf y) (DEDB_GetKsCeilingSupportHeight)))													(DEDB_DrawKsCeilingSupport))
				(T alert ("*** Error in DEDB-ETCS-HALT-TAFEL"))
			)
			(AddDescriptionBelowOrigin thisDescription _descriptionTextHeight_) 
			(CreateSchematicBlockFromCurrentGraphics thisBlockName)
			(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
		);foreach dir
	);foreach mountingMethod
)



; Draw helpers
;----------------------------------------------------------------

(defun DEDB_DrawMB ( dir / x y p11 p12 p21 p22 p23 p24 p31 )
	; Signal Ne14 ETCS-Halt-Tafel
	; dir = [ _left_ | _right_ | _down_ ]
	;
	; +---------------------+
	; |         p31         |
	; |         / \         |
	; |       /     \       |
	; |     /         \     |
	; |   /             \   |
	; | p21-p22  .  p23-p24 | ; Arrow will be rotated +/-90 or 180 DD, and enclosed in a square box.
	; |       |     |       |
	; |       |     |       |
	; |       |     |       |
	; |       p11-p12       | y
	; +---------------------+
	;                     x
	(setq
		x	6.000
		y	6.000
		p11	(list (* -0.170 x) (* -0.475 y))
		p12	(list (*  0.170 x) (* -0.475 y))
		p21	(list (* -0.475 x) (*  0.050 y))
		p22	(list (* -0.170 x) (*  0.050 y))
		p23	(list (*  0.170 x) (*  0.050 y))
		p24	(list (*  0.475 x) (*  0.050 y))
		p31	(list (*  0.000 x) (*  0.475 y))
	)
	; Surrounding box:
	(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout )

	; Draw arrow pointing straight up, then rotate it:
	(command _POLYLINE_ p11 p12 p23 p24 p31 p21 p22 p11 _closedPolyline_)
	(DrawHatchAtPoint _solidHatch_ (AddVectors p31 _slightlyAbove_) _angleZero_ _offsetZero_)
	(cond 
		((= dir _left_)		(RotateLeft _angle90_))		; Arrow now points to the left
		((= dir _right_)	(RotateRight _angle90_))	; Arrow now points to the right
		((= dir _down_)		(RotateLeft _angle180_))	; Arrow now points down
	)
)
