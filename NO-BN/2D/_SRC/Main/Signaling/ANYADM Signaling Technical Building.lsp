;=========================================================================================================================
;
; ANYADM Signaling Technical building.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Relay room (interlocking's technical equipment room)

(defun ANYADM-TECHNICAL-BUILDING ( / )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _SIG_ "TEB-" "TECHNICAL-BUILDING"			)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _SIG_ "TER-" "TEKNISK-BEBYGGELSE-RELEROM"	)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _SIG_ "BTT-" "BATIMENT-TECHNIQUE"			)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _SIG_ "GEB-" "GEBAEUDE"					)))
	)
	(cond
		((= _ADM_ _XXGL_) (setq description (strcat "TECHNICAL BUILDING"			)))
		((= _ADM_ _NOBN_) (setq description (strcat "TEKNISK BEBYGGELSE RELEROM"	)))
		((= _ADM_ _FRSR_) (setq description (strcat "BATIMENT TECHNIQUE"			)))
		((= _ADM_ _DEDB_) (setq description (strcat "GEBAEUDE"						)))
	)
	(cond 
		((= _ADM_ _XXGL_) 
			; XX-GL actions:
		)
		((= _ADM_ _NOBN_)
			(TraceLevel3 "NOBN-TEKNISK-BEBYGGELSE-RELEROM")			(NOBN-TEKNISK-BEBYGGELSE-RELEROM)
		)
		((= _ADM_ _FRSR_) 
			; FR-SR actions:
		)
		((= _ADM_ _DEDB_) 
			(TraceLevel3 "DEDB-SCHALTHAUS")							(DEDB-SCHALTHAUS)
			(TraceLevel3 "DEDB-BETONSCHALTHAUS")					(DEDB-BETONSCHALTHAUS)
			(TraceLevel3 "DEDB-GEBAEUDE")							(DEDB-GEBAEUDE)
		)
	)
	; Cleanup temp globals:
	(setq blockName nil description nil)
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


;================== DEDB functions ==================================================================
(defun DEDB-SCHALTHAUS ( / thisBlockName thisDescription x y xi yi )
	; Ril.819.9002_V02 S.23
	;
	; +-------------------+
	; | +---------------+ |
	; | |   \       /   | |
	; | |       .       | |
	; | |   /       \   | |
	; | +---------------+ | 
	; +-------------------+ 
	;    Schalthaus	(wie Kabelschrank + Viereck rundum)
	;
    (setq 
		thisBlockName	(strcat blockName "-SCHALTHAUS")
		thisDescription (strcat description ", SCHALTHAUS")
		x	6
		y	4
		xi	5
		yi	3
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawBox layDef_Zero xi yi _noWipeout_)
	(DrawStAndrewCross layDef_Zero xi yi)
	(AddDescriptionBelowOrigin thisDescription (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics thisBlockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
)



(defun DEDB-BETONSCHALTHAUS ( / thisBlockName thisDescription x y )
	; Ril.819.9002_V02 S.30
	;
	; +-------------------+
	; |                   | 
	; |                   |
	; |         .     *   |
	; |                   |
	; |                   | 
	; +-------------------+ 
	;    Beton-Schalthaus			
	;
    (setq 
		thisBlockName	(strcat blockName "-BETONSCHALTHAUS")
		thisDescription (strcat description ", BETONSCHALTHAUS")
		x	7.5
		y	4.5
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawHatch _sparseHatch_)
	(AddDescriptionBelowOrigin thisDescription (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics thisBlockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
)



(defun DEDB-GEBAEUDE ( / thisBlockName thisDescription x y xi yi hx hy vx vy r p1 p2 p3 p4 p5 p6 p7 p8 constructionHeight equipmentType dispatchingType orientation )
	; Ril.819.9002_V02 S.3
	; With 4 positions for dispatcher's workplace orientation in building
	;
	; +-------------------+   +-------------------+   +-------------------+    +-------------------+    +-------------------+    +-------------------+
	; |                   |   | +---------------+ |   |                || |    |    [====3====]    |    | ||  Low or high   |    |   Low or high     |
	; |                   |   | |               | |   |                || |    |        (4)        |    | ||  /// or XXX    |    |   /// or XXX      |
	; |         .         |   | |       .       | |   |         .  (1) p2 |    |         .         |    | p5 (6)  .         |    |         .         |
	; |                   |   | |               | |   |  /// or XXX    || |    | /// or XXX        |    | ||                |    |        (7)        |
	; |                   |   | +---------------+ |   |  Low or high   || |    | Low or high       |    | ||                |    |    [====8====]    |    
	; +-------------------+   +-------------------+   +-------------------+    +-------------------+    +-------------------+    +-------------------+     
	; Gebaüde, niedr. B.höhe  Gebaüde, hohe Bauhöhe    Stellwerk
	;
	;  /// 	= Single-line hatch (inclined lines) - Mechanisches Stellwerk
	;  XXX 	= Double-line hatch (losanges) - Elektronisches Stellwerk
	;  [==] = Dispatcher's panel (solid hatch)
	;  ( ) 	= Emergency dispatcher's workplace
	;      	= No dispatcher's workplace
	;  (*) 	= Regular dispatcher's workplace (solid hatch)
	;
    (setq 
		x	13	; Outer box
		y	6	
		xi	12	; Inner box
		yi	5	
		
		hx	5.0	; Horizontal bar = dispatcher's wall-board
		hy	0.8
		vx	0.8	; Vertical bar = dispatcher's wall-board
		vy	4.0
		r	0.6 ; Filled circle = dispatcher's chair

		; Dispatcher's workplace, in quadrant 1-2-3-4:
		p1 (list (*  0.40 xi) (*  0.00 yi))	
		p2 (list (*  0.25 xi) (*  0.00 yi))

		p3 (list (*  0.00 xi) (*  0.25 yi))
		p4 (list (*  0.00 xi) (* -0.10 yi))

		p5 (list (* -0.40 xi) (*  0.00 yi))
		p6 (list (* -0.25 xi) (*  0.00 yi))

		p7 (list (*  0.00 xi) (* -0.25 yi))
		p8 (list (*  0.00 xi) (*  0.10 yi))
	)
;;;;;;;;;;; (setq constructionHeight "HOCH" equipmentType "ELEKTRONISCHES-STELLWERK" dispatchingType "MIT-BEDIENPLATZ" orientation 3 )
	(foreach constructionHeight '("NIEDRIG" "HOCH")  ; Low building, floor is <= 2m above Top-Of-Rail / Above 2.0m over TOR
		(foreach equipmentType '("ALLGEMEIN" "MECHANISCHES-STELLWERK" "ELEKTRONISCHES-STELLWERK")
			(traceLevel3 (strcat "..." constructionHeight " / " equipmentType))
			(cond 
				((= equipmentType "ALLGEMEIN")
					(setq
						thisBlockName	(strcat blockName "-" constructionHeight "-" equipmentType)
						thisDescription	(strcat description ", " constructionHeight ", " equipmentType)
					)
					(DrawBox layDef_Zero x y _noWipeout_)  ; Outer box
					(if (= constructionHeight "HOCH") (DrawBox layDef_Zero xi yi _noWipeout_)) ; Smaller inner box
					(AddDescriptionBelowOrigin thisDescription (HalfOf y))
					(CreateSchematicBlockFromCurrentGraphics thisBlockName)
					(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
				)
				((or (= equipmentType "MECHANISCHES-STELLWERK") (= equipmentType "ELEKTRONISCHES-STELLWERK"))
					; Add hatch for mechanic or electronic interlocking, then add dispatcher's screends an workplace
					(foreach dispatchingType '("OHNE-BEDIENPLATZ" "NOT-BEDIENPLATZ" "MIT-BEDIENPLATZ")
						(foreach orientation '(1 2 3 4)  ; Orientation of dispatcher's screens (1=right 2=top 3=left 4=bottom)
							(setq
								thisBlockName	(strcat blockName "-" constructionHeight "-" equipmentType "-" dispatchingType "-" (itoa orientation))
								thisDescription	(strcat description ", " constructionHeight ", " equipmentType ", " dispatchingType ", " (itoa orientation))
							)
							(DrawBox layDef_Zero x y _noWipeout_)  ; Outer box
							(if (= constructionHeight "HOCH") (DrawBox layDef_Zero xi yi _noWipeout_)) ; Smaller inner box
							(cond 
								((= equipmentType "MECHANISCHES-STELLWERK")
									; Single hatch '///'
									(DrawHatch _lightHatch_)
								)
								((= equipmentType "ELEKTRONISCHES-STELLWERK")
									; Criss-cross hatch 'XXX'
									(DrawHatch _lightHatch_)
									(MirrorAboutYaxis _keepMirrorSource_)
								)
							)
							(cond
								((= orientation 1)
									(DrawBoxAtPos layDef_Zero p1 vx vy layDef_Zero)	; Dispatcher's panel (or computer screens(s))
									(DrawHatch _solidHatch_)
									(if (or (= dispatchingType "NOT-BEDIENPLATZ") (= dispatchingType "MIT-BEDIENPLATZ")) (DrawCircleAtPos layDef_Zero p2 r layDef_Zero))
									(if (= dispatchingType "MIT-BEDIENPLATZ") (DrawHatch _solidHatch_))
								)
								((= orientation 2)
									(DrawBoxAtPos layDef_Zero p3 hx hy layDef_Zero)
									(DrawHatch _solidHatch_)
									(if (or (= dispatchingType "NOT-BEDIENPLATZ") (= dispatchingType "MIT-BEDIENPLATZ")) (DrawCircleAtPos layDef_Zero p4 r layDef_Zero))
									(if (= dispatchingType "MIT-BEDIENPLATZ") (DrawHatch _solidHatch_))
								)
								((= orientation 3)
									(DrawBoxAtPos layDef_Zero p5 vx vy layDef_Zero)
									(DrawHatch _solidHatch_)
									(if (or (= dispatchingType "NOT-BEDIENPLATZ") (= dispatchingType "MIT-BEDIENPLATZ")) (DrawCircleAtPos layDef_Zero p6 r layDef_Zero))
									(if (= dispatchingType "MIT-BEDIENPLATZ") (DrawHatch _solidHatch_))
								)
								((= orientation 4)
									(DrawBoxAtPos layDef_Zero p7 hx hy layDef_Zero)
									(DrawHatch _solidHatch_)
									(if (or (= dispatchingType "NOT-BEDIENPLATZ") (= dispatchingType "MIT-BEDIENPLATZ")) (DrawCircleAtPos layDef_Zero p8 r layDef_Zero))
									(if (= dispatchingType "MIT-BEDIENPLATZ") (DrawHatch _solidHatch_))
								)
							);cond orientation
							(AddDescriptionBelowOrigin thisDescription (HalfOf y))
							(CreateSchematicBlockFromCurrentGraphics thisBlockName)
							(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
						);foreach orientation
					);foreach dispatchingType
				)
			);cond equipmentType
		);foreach equipmentType
	);foreach constructionHeight
)
