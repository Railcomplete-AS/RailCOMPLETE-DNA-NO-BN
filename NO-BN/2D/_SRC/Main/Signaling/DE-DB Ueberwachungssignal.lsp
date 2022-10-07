;=========================================================================================================================
; 
;
; DE-DB Ueberwachungssignal.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================



(defun DEDB-UEBERWACHUNGSSIGNAL ( / mountingMethod config thisBlockName thisDescription )
;
; Deutsche Bahn signal combos - Supervision signal for level crossing (Bü).
; Ref 1: Richtlinien =  ???
; Ref 2: 819.9002 Symbole für sicherungstechnische Pläne, V2.0, 2021-05-01
;
; TODO: Many combinations are possible. For now, just ONE has been made.
;
	(DEDB_DrawUeberwachungssignal)	; Just one variation for now...
)



;====================================================================================
; Draw signal items
;====================================================================================
(defun DEDB_DrawUeberwachungssignal ( / x y p1 p2 p3 r1 r2 )
	; DEDB: Überwachungssignal (level crossing signal)
	;
	;   +---------+
	;   |  _____  | 	Box: x*y
	;   | /     \ |		
	;   ||   3   ||		Center at p3 relative to bottom of frame, radius r2
	;   ||       ||
	;   | \_____/ |
	;   |         |
	;   |         |
	;   |  _   _  |
	;   | (_) (_) |		Centers at p1 and p2 relative to center bottom of frame, radius r1
	;   |  1   2  |
	;   |         |
	;   +----+----+  
	;        |	 
	;        |	  
	;        |		Low or ordinary support for board and auxiliary signal
	;      --.-- 
	;
	(setq 
		blockName 	(strcat _SIG_ "UWS-" "UEBERWACHUNGSSIGNAL")
		description (strcat 			 "UEBERWACHUNGSSIGNAL")
		x	2.5
		y	5
		p1	(list -0.600 1.300)
		p2	(list  0.600 1.300)
		p3	(list  0.000 3.600)
		r1	0.450
		r2	0.900
	)
	(defun LocalGraphics ( / )
		(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
		(MoveUp (HalfOf y))
		(DrawCircleAtPos layDef_Zero p1 r1 _noWipeout_)
		(DrawCircleAtPos layDef_Zero p2 r1 _noWipeout_)
		(DrawCircleAtPos layDef_Zero p3 r2 _noWipeout_)
		(MoveUp (DEDB_GetBoardNormalSupportHeight))
	)
	
	(LocalGraphics)
	(DEDB_DrawSupportForBoardAndNonKs)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)

	; TODO: Delete low mast version if not needed by DB:
	;(LocalGraphics)
	;(DEDB_DrawSupportForGroundlevelBoardAndNonKs)
	;(AddDescriptionBelowOrigin (strcat description ", NIEDRIG") 0)
	;(CreateSchematicBlockFromCurrentGraphics (strcat blockName "-NIEDRIG"))
	;(AddGraphicsFromScaledSchematicBlock (strcat blockName "-NIEDRIG") _one_)
	;(CreateAnnotativeBlockFromCurrentGraphics (strcat blockName "-NIEDRIG"))
)
