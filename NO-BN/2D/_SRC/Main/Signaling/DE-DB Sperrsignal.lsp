;=========================================================================================================================
;
; DE-DB Sperrsignal.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================



; TODO EIHOV: Use x1 y1 and x2 y2 draw large rectangle with wipeout, draw second without wipeout inside first. Hatch, move up, add support / low support.
(defun DEDB-SPERRSIGNAL ( / blockName description x y y2 r p1 kennlicht niedrig thisBlockName thisDescription )
	; DEDB: Sperrsignal
	; Ril.819.9002 S.16 
	;
	;          (1)		Kennlicht
	;   +-------+
	;   |       | y1
	;   +-------+
	;   |*******| y2	Solid hatch
	;   +-------+
	;   |       |
	;   +---+---+   
	;       | x
	;       | 
	;       | 
	; 	  --+--		Low or ordinary support for board and auxiliary signal
	;     --.-- 
	;
	(setq 
		blockName 	(strcat _SIG_ "SPS-" 	"SPERRSIGNAL"		)
		description (strcat 	      		"SPERRSIGNAL"	 	)
		x	3.000
		y	3.000
		y2	0.500
		r	0.625
		p1 	(list 1.250 (+ r (HalfOf y)))
	)
	(foreach kennlicht '(nil T)
		(foreach niedrig '(nil T)
			(DrawBox layDef_Zero x y layDef_BoardOrPole_Wipeout)
			(DrawBox layDef_Zero x y2 _noWipeout_)
			(DrawHatchAtPoint _solidHatch_ _origin_ _angleZero_ _offsetZero_)
			(if kennlicht (DrawCircleAtPos layDef_Zero p1 r _noWipeout_))
			(MoveUp (HalfOf y))
			(MoveUp (DEDB_GetBoardNormalSupportHeight))
			(setq 
				thisBlockName	(strcat blockName	(if kennlicht "-KENNLICHT" "") (if niedrig "-NIEDRIG" ""))
				thisDescription	(strcat description	(if kennlicht ", KENNLICHT" "") (if niedrig ", NIEDRIG" ""))
			)
			(if niedrig
				(DEDB_DrawSupportForGroundlevelBoardAndNonKs)
			;else
				(DEDB_DrawSupportForBoardAndNonKs)
			)
			(AddDescriptionBelowOrigin thisDescription 0)
			(CreateSchematicBlockFromCurrentGraphics thisBlockName)
			(AddGraphicsFromScaledSchematicBlock thisBlockName _one_)
			(CreateAnnotativeBlockFromCurrentGraphics thisBlockName)
		)
	)
)
