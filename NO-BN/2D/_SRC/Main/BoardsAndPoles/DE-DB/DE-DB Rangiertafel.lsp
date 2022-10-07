;=========================================================================================================================
;
; DE-DB Rangiertafel.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================



(defun DEDB-RANGIERTAFEL ( / blockName description p1 p2 p3 p4 p5 p6 )
	;
	;     _3_
	;    /   \
	;   |     |
	;   1--+--2   
	;      |
	;      |
	; 	   |	Low or ordinary support for board and auxiliary signal
	;    --.-- 
	;
	(setq 
		blockName 	(strcat _SIG_ "RAT-" 	"RANGIERTAFEL"		)
		description (strcat 				"RANGIERTAFEL"	 	)
		p1 (list -1.500 0.000)
		p2 (list  1.500 0.000)
		p3 (list  0.000 1.500)
	)
	(defun LocalGraphics ( / )
		(DrawLine layDef_Zero p1 p2)
		(DrawArc layDef_Zero p1 p3 p2)
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
