;=========================================================================================================================
;
;
; DE-DB Schachbretttafel.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================



(defun DEDB-SCHACHBRETT-TAFEL ( / blockName description x y p1 p2 )
	; DEDB: Schachbrett-Tafel
	;
	; +---+---+
	; |   |   |
	; |   |2  |
	; +-------+  y
	; |  1|   |
	; |   |   |
	; +---+---+
	;     | x
	;     | 
	; 	  |	Low or ordinary support for board and auxiliary signal
	;   --.-- 
	;
	(setq
		blockName 	(strcat _SIG_ "SBT-" "SCHACHBRETT-TAFEL")
		description (strcat 		     "SCHACHBRETT-TAFEL")
		x	3.000
		y	6.000
		p1 	(list -0.100 -0.100)
		p2	(list  0.100  0.100)
	)
	(defun LocalGraphics ( / )
		(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
		(DrawLine layDef_Zero (PosML x y) (PosMR x y))
		(DrawLine layDef_Zero (PosBC x y) (PosTC x y))
		(DrawHatchAtPoint _denseHatch_ p1 _angleZero_ _offsetZero_)
		(DrawHatchAtPoint _denseHatch_ p2 _angleZero_ _offsetZero_)
		(MoveUp (HalfOf y))
		(MoveUp (DEDB_GetBoardNormalSupportHeight))
	)
	(LocalGraphics)
	(DEDB_DrawSupportForBoardAndNonKs)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	(LocalGraphics)
	(DEDB_DrawSupportForGroundlevelBoardAndNonKs)
	(AddDescriptionBelowOrigin (strcat description "-NIEDRIG") 0)
	(CreateSchematicBlockFromCurrentGraphics (strcat blockName "-NIEDRIG"))
	(AddGraphicsFromScaledSchematicBlock (strcat blockName "-NIEDRIG") _one_)
	(CreateAnnotativeBlockFromCurrentGraphics (strcat blockName "-NIEDRIG"))
)
