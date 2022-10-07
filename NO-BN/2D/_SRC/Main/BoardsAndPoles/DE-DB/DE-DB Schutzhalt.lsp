;=========================================================================================================================
; 
;
; DE-DB Schutzhalt.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================



(defun DEDB-SCHUTZHALT ( / blockName description x1 y1 x2 y2 )
	; DEDB: Schutzhalt
	;
	;   +---------+
	;   | +-----+ | 
	;   | |     | |	Outer box: x1*y1
	;   | +-----+ |	Inner box: x2*y2
	;   +----+----+  
	;        |	  
	;        |	  
	;        |		Low or ordinary support for board and auxiliary signal
	;      --.-- 
	;
	(setq 
		blockName 	(strcat _SIG_ "SZH-" "SCHUTZHALT")
		description (strcat 			 "SCHUTZHALT")
		x1	4
		y1	3
		x2	3
		y2	2
	)
	(defun LocalGraphics ( / )
		(DrawBox layDef_Zero x1 y1 layDef_BoardOrPole_Wipeout)
		(DrawBox layDef_Zero x2 y2 _noWipeout_)
		(MoveUp (HalfOf y1))
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
