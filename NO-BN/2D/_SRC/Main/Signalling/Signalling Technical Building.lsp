;=========================================================================================================================
;
; Signalling Technical building.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Relay room (interlocking's technical equipment room)

(defun TECHNICAL-BUILDING ( / )
	(setq blockName (strcat _SIG_ "TER-" "TEKNISK-BEBYGGELSE-RELEROM"	))
	(setq description (strcat "TEKNISK BEBYGGELSE RELEROM"				))

	(TraceLevel3 "NOBN-TEKNISK-BEBYGGELSE-RELEROM")			(NOBN-TEKNISK-BEBYGGELSE-RELEROM)
)



;================== NOBN functions ==================================================================
(defun NOBN-TEKNISK-BEBYGGELSE-RELEROM ( / thisBlockName thisDescription x y hx hy vx vy r p1 p2 p3 p4 p5 p6 p7 p8 orientation )
	;
	; +-------------------+    +-------------------+    +-------------------+    +-------------------+    +-------------------+
	; |                   |    |                || |    |    [====3====]    |    | ||                |    |                   |
	; |                   |    |                || |    |        (4)        |    | ||                |    |                   |
	; |         .         |    |         .  (1) p2 |    |         .         |    | p5 (6)  .         |    |         .         |
	; |                   |    |                || |    |                   |    | ||                |    |        (7)        |
	; |                   |    |                || |    |                   |    | ||                |    |    [====8====]    |
	; +-------------------+    +-------------------+    +-------------------+    +-------------------+    +-------------------+ 
	;  Relerom u/betj.plass     Relerom m/betj.pl.1      Relerom m/betj.pl.2       Relerom m/betj.pl.4     Relerom m/betj.pl.3 
	;
    (setq 
		x	12.0
		y	9.0

		hx	8.0	; Horizontal bar = dispatcher's wall-board
		hy	1.0
		vx	1.0	; Vertical bar = dispatcher's wall-board
		vy	8.0
		r	1.0	; Filled circle = dispatcher's chair

		; Dispatcher's workplace, in quadrant 1-2-3-4:
		p1 (list (*  0.40 x) (*  0.00 y))	
		p2 (list (*  0.20 x) (*  0.00 y))

		p3 (list (*  0.00 x) (*  0.40 y))
		p4 (list (*  0.00 x) (*  0.15 y))

		p5 (list (* -0.40 x) (*  0.00 y))
		p6 (list (* -0.20 x) (*  0.00 y))

		p7 (list (*  0.00 x) (* -0.40 y))
		p8 (list (*  0.00 x) (* -0.15 y))
	)
	(foreach orientation '(0 1 2 3 4)
		(DrawBox layDef_Zero x y _noWipeout_)
		(setq
			thisBlockName	(strcat blockName "-" (itoa orientation))
			thisDescription	(strcat description "-" (itoa orientation))
		)
		(cond
			((= orientation 0)
				; No workplace
			)
			((= orientation 1)
				(DrawBoxAtPos layDef_Zero p1 vx vy _noWipeout_)
				(DrawHatch _solidHatch_)
				(DrawCircleAtPos layDef_Zero p2 r _noWipeout_)
				(DrawHatch _solidHatch_)
			)
			((= orientation 2)
				(DrawBoxAtPos layDef_Zero p3 hx hy _noWipeout_)
				(DrawHatch _solidHatch_)
				(DrawCircleAtPos layDef_Zero p4 r _noWipeout_)
				(DrawHatch _solidHatch_)
			)
			((= orientation 3)
				(DrawBoxAtPos layDef_Zero p5 vx vy _noWipeout_)
				(DrawHatch _solidHatch_)
				(DrawCircleAtPos layDef_Zero p6 r _noWipeout_)
				(DrawHatch _solidHatch_)
			)
			((= orientation 4)
				(DrawBoxAtPos layDef_Zero p7 hx hy _noWipeout_)
				(DrawHatch _solidHatch_)
				(DrawCircleAtPos layDef_Zero p8 r _noWipeout_)
				(DrawHatch _solidHatch_)
			)
		)
		(AddDescriptionBelowOrigin thisDescription (HalfOf y))
		(CreateSchematicBlockFromCurrentGraphics thisBlockName)
		(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
	)
)
