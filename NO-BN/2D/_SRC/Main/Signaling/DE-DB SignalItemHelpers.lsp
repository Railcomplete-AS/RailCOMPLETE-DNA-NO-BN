;=========================================================================================================================
;
; DE-DB SignalItemHelpers.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================



(defun DEDB_DrawVo ( / p1 p2 p3 )
	; DEDB: Vorsichtsignal
	;
	; 1   2
	;  \ /
	;   3
	;   .
	;
	(setq 
		p1 (list -1.500 3.100)
		p2 (list  1.500 3.100)
		p3 (list  0.000 0.502)
	)
	(DrawLine layDef_Zero p1 p3)
	(DrawLine layDef_Zero p3 p2)
)



(defun DEDB_DrawEr ( / p1 p2 p3 )
	; DEDB: Ersatzsignal
	; 
	;   3
	;  / \
	; 1   2
	;   .
	;
	(setq 
		p1 (list -1.500 0.502)
		p2 (list  1.500 0.502)
		p3 (list  0.000 3.100)
	)
	(DrawLine layDef_Zero p1 p3)
	(DrawLine layDef_Zero p3 p2)
)



(defun DEDB_DrawGa ( freeStanding luminous / p1 p2 p3 p4 att1 a1 )
	; DEDB: Geschwindigkeitsanzeiger
	; 
	;   3
	;  /4\	a1		a1 = Speed value(s) attribute
	; 1---2			p4 = seed for hatched solid triangle
	;   .
	;
	(setq 
		p1 (list -1.500 0.502)
		p2 (list  1.500 0.502)
		p3 (list  0.000 3.100)
		p4 (list  0.000 2.000)
		att1 '("GA_WERT" "Geschwindigkeit" "4")
		a1 (list  3.000 1.500)
	)
	(DrawLine layDef_Zero p1 p3)
	(DrawLine layDef_Zero p3 p2)
	(DrawLine layDef_Zero p2 p1)
	(if luminous
		(DrawHatchAtPoint _denseHatch_ p4 _angleZero_ _offsetZero_)
	)
	(AddAtt (GetAttributeTag att1) (GetAttributePrompt att1) (GetAttributeDefaultValue att1) a1 _th180_ _angle90_ _rcTextStyle_ _middleCenter_)
	(if freeStanding
		(progn
			(MoveUp (- (DEDB_GetBoardNormalSupportHeight) (yCoord p1)))
			(DEDB_DrawSupportForBoardAndNonKs)
		)
	;else nothing
	)
)



(defun DEDB_DrawHp ( bedienTyp zu / p0 rZu p1 p2 p3 p4 p5 p6 )
	; DEDB; Hauptsignal
	;
	;    (0)__5__		p0 = Zusatzlicht (if present)
	;      /  |  \  
	;     3   |   4
	;     |   |   |
	;     |   |   |
	;     |   |6  |		p6 is used as seed for hatch if bedienTyp is 1 or 2
	;     1___.___2
	; 
	(setq 
		p0	(list -1.840 3.563)		; (R+r)*(sin,cos) at angle 30 degrees seems suitable
		rZu	0.625
		p1	(list -1.500 0.000)
		p2	(list  1.500 0.000)
		p3	(list -1.500 2.500)
		p4	(list  1.500 2.500)
		p5	(list  0.000 4.000)
		p6	(list  0.100 1.500)		; slightly to the right of the vertical middle line
	)
	(DrawLine layDef_Zero p1 p3)
	(DrawArc layDef_Zero p3 p5 p4)
	(DrawLine layDef_Zero p4 p2)
	(DrawLine layDef_Zero p2 p1)
	(if (= zu 1) (DrawCircleAtPos layDef_Zero p0 rZu _noWipeout_))
	(cond
		((= bedienTyp 0)
			; 0=Zugbedient, shall be left empty
		)
		((= bedienTyp 1)
			; 1=Zug- oder Stellwerksbedient, left part shall be empty, right half hatched
			(DrawLine layDef_Zero _origin_ p5)
			(DrawHatchAtPoint _denseHatch_ p6 _angleZero_ _offsetZero_)
		)
		((= bedienTyp 2)
			; 2=Stellwerksbedient, the whole 'bowl' shall be hatched
			(DrawHatchAtPoint _denseHatch_ p6 _angleZero_ _offsetZero_)
		)
		(T 
			(alert (strcat "*** DrawHsRa: Wrong bedienTyp [" (itoa bedienTyp) "]."))
		)
	)
)



(defun DEDB_DrawHpRa ( bedienTyp zu / p0 rZu p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 )
	; DEDB: Ks Hauptsignal + Rangieren (Abdrücken)
	;
	;    (0)__9__		p0 = Zusatzlicht (if present)
	;      /  |  \  
	;     7   |11 8		p11 is used as seed for hatch if bedienTyp is 1 or 2
	;     |   |   |
	;     5___10__6
	;     3___ ___4
	;     1___.___2
	;
	(setq 
		p0	(list -1.840 3.563)		; (R+r)*(sin,cos) at angle 30 degrees seems suitable
		rZu	0.625
		p1	(list -1.500 0.000)
		p2	(list  1.500 0.000)
		p3	(list -1.500 1.000)
		p4	(list  1.500 1.000)
		p5	(list -1.500 2.000)
		p6	(list  1.500 2.000)
		p7	(list -1.500 2.500)
		p8	(list  1.500 2.500)
		p9	(list  0.000 4.000)
		p10 (list  0.000 2.000)
		p11	(list  0.100 2.500)		; slightly to the right of the vertical middle line
	)
	(DrawLine layDef_Zero p8 p2)
	(DrawLine layDef_Zero p2 p1)
	(DrawLine layDef_Zero p1 p7)
	(DrawArc layDef_Zero p7 p9 p8)
	(DrawLine layDef_Zero p3 p4)
	(DrawLine layDef_Zero p5 p6)
	(if (= zu 1) (DrawCircleAtPos layDef_Zero p0 rZu _noWipeout_))
	(cond
		((= bedienTyp 0)
			; 0=Zugbedient, 'bowl' shall be left empty
		)
		((= bedienTyp 1)
			; 1=Zug- oder Stellwerksbedient, left 'bowl' shall be empty, right half hatched
			(DrawLine layDef_Zero p10 p9)
			(DrawHatchAtPoint _denseHatch_ p11 _angleZero_ _offsetZero_)
		)
		((= bedienTyp 2)
			; 2=Stellwerksbedient, the whole 'bowl' shall be hatched
			(DrawHatchAtPoint _denseHatch_ p11 _angleZero_ _offsetZero_)
		)
		(T 
			(alert (strcat "*** Wrong bedienTyp [" (itoa bedienTyp) "]."))
		)
	)
)



(defun DEDB_DrawHpVr ( bedienTyp zu / p0 rZu p1 p2 p3 p4 p5 p6 )
	; DEDB: Ks Hauptsignal + Vorsignal = Mehrabschnittsignal
	;
	;    (0)__5__		p0 = Zusatzlicht (if present)
	;      /  |  \  
	;     3   |   4
	;     |   |   |
	;     |   |   |
	;     |   |   |
	;     |   |   |
	;     |   |   |
	;     1   |6  2		p6 is used as seed for hatch if bedienTyp is 1 or 2
	;      \__.__/		Origin = at middle bottom
	; 
	(setq 
		p0	(list -1.840 6.563)		; (R+r)*(sin,cos) at angle 30 degrees seems suitable
		rZu	0.625
		p1	(list -1.500 1.500)
		p2	(list  1.500 1.500)
		p3	(list -1.500 5.500)
		p4	(list  1.500 5.500)
		p5	(list  0.000 7.000)
		p6	(list  0.100 1.500)		; slightly to the right of the vertical middle line
	)
	(DrawLine layDef_Zero p1 p3)
	(DrawArc layDef_Zero p3 p5 p4)
	(DrawLine layDef_Zero p4 p2)
	(DrawArc layDef_Zero p2 _origin_ p1)
	(if (= zu 1) (DrawCircleAtPos layDef_Zero p0 rZu _noWipeout_))
	(cond
		((= bedienTyp 0)
			; 0=Zugbedient, shall be left empty
		)
		((= bedienTyp 1)
			; 1=Zug- oder Stellwerksbedient, left part shall be empty, right half hatched
			(DrawLine layDef_Zero _origin_ p5)
			(DrawHatchAtPoint _denseHatch_ p6 _angleZero_ _offsetZero_)
		)
		((= bedienTyp 2)
			; 2=Stellwerksbedient, the whole 'bowl' shall be hatched
			(DrawHatchAtPoint _denseHatch_ p6 _angleZero_ _offsetZero_)
		)
		(T 
			(alert (strcat "*** Wrong bedienTyp [" (itoa bedienTyp) "]."))
		)
	)
)



(defun DEDB_DrawHpRaVr ( bedienTyp zu / p0 rZu p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 )
	; DEDB: Ks Hauptsignal + Rangieren (abdrücken) + Vorsignal = Mehrabschnittsignal + Abdrücken
	;
	;    (0)__11_		p0 = Zusatzlicht (if present)
	;      /  |  \  	
	;     9   |15 10	p15 is used as seed for hatch if bedienTyp is 1 or 2
	;     |   |   |
	;     |   |   |
	;     7___13__8
	;     5_______6
	;     3___12__4
	;     1   |14 2		p14 is used as seed for hatch if bedienTyp is 1 or 2
	;      \__.__/		Origin = at middle bottom
	;
	(setq 
		p0	(list -1.840 6.563)		; (R+r)*(sin,cos) at angle 30 degrees seems suitable
		rZu	0.625
		p1	(list -1.500 1.500)
		p2	(list  1.500 1.500)
		p3	(list -1.500 1.500)
		p4	(list  1.500 1.500)
		p5	(list -1.500 2.500)
		p6	(list  1.500 2.500)
		p7	(list -1.500 3.500)
		p8	(list  1.500 3.500)
		p9	(list -1.500 5.500)
		p10	(list  1.500 5.500)
		p11	(list  0.000 7.000)
		p12	(list  0.000 1.500)
		p13	(list  0.000 3.500)
		p14	(list  0.100 1.000)		; slightly to the right of the vertical middle line
		p15	(list  0.100 6.000)		; slightly to the right of the vertical middle line
	)
	(DrawLine layDef_Zero p1 p9)
	(DrawArc layDef_Zero p9 p11 p10)
	(DrawLine layDef_Zero p10 p2)
	(DrawArc layDef_Zero p1 _origin_ p2)
	(DrawLine layDef_Zero p3 p4)
	(DrawLine layDef_Zero p5 p6)
	(DrawLine layDef_Zero p7 p8)
	(if (= zu 1) (DrawCircleAtPos layDef_Zero p0 rZu _noWipeout_))
	(cond
		((= bedienTyp 0)
			; 0=Zugbedient, shall be left empty
		)
		((= bedienTyp 1)
			; 1=Zug- oder Stellwerksbedient, left part shall be empty, right half hatched
			(DrawLine layDef_Zero _origin_ p12)
			(DrawLine layDef_Zero p13 p11)
			(DrawHatchAtPoint _denseHatch_ p14 _angleZero_ _offsetZero_)
			(DrawHatchAtPoint _denseHatch_ p15 _angleZero_ _offsetZero_)
		)
		((= bedienTyp 2)
			; 2=Stellwerksbedient, the whole 'bowl' shall be hatched
			(DrawHatchAtPoint _denseHatch_ p14 _angleZero_ _offsetZero_)
			(DrawHatchAtPoint _denseHatch_ p15 _angleZero_ _offsetZero_)
		)
		(T 
			(alert (strcat "*** Wrong bedienTyp [" (itoa bedienTyp) "]."))
		)
	)
)



(defun DEDB_DrawVr ( bedienTyp zu / p1 rZu p2 p3 p4 p5 p6 )
	; DEDB; Ks Vorsignal
	;
	;  (0)3___5___4		p0 = Zusatzlicht (if present)
	;     |   |   |
	;     1   |6  2		p6 is used as seed for hatch if bedienTyp is 1 or 2
	;      \__.__/		Origin = at middle bottom
	;
	(setq 
		p0	(list -2.125 3.000)
		rZu	0.625
		p1	(list -1.500 1.500)
		p2	(list  1.500 1.500)
		p3	(list -1.500 3.000)
		p4	(list  1.500 3.000)
		p5	(list  0.000 3.000)
		p6	(list  0.100 1.500)		; slightly to the right of the vertical middle line
	)
	(DrawLine layDef_Zero p1 p3)
	(DrawLine layDef_Zero p3 p4)
	(DrawLine layDef_Zero p4 p2)
	(DrawArc layDef_Zero p2 _origin_ p1)
	(if (= zu 1) (DrawCircleAtPos layDef_Zero p0 rZu _noWipeout_))
	(cond
		((= bedienTyp 0)
			; 0=Zugbedient, shall be left empty
		)
		((= bedienTyp 1)
			; 1=Zug- oder Stellwerksbedient, left part shall be empty, right half hatched
			(DrawLine layDef_Zero _origin_ p5)
			(DrawHatchAtPoint _denseHatch_ p6 _angleZero_ _offsetZero_)
		)
		((= bedienTyp 2)
			; 2=Stellwerksbedient, the whole 'bowl' shall be hatched
			(DrawHatchAtPoint _denseHatch_ p6 _angleZero_ _offsetZero_)
		)
		(T 
			(alert (strcat "*** Wrong bedienTyp [" (itoa bedienTyp) "]."))
		)
	)
)



(defun DEDB_DrawGv ( freeStanding luminous / p1 p2 p3 p4 p5 att1 a1 )
	; DEDB: Geschwindichkeitvorsanzeiger
	; Zs3 = Speed change execution, Zs3v = Speed change announcement
	; Free standing as reflective board or as luminous signal.
	; Mounted on a Ks signal's mast as reflective board or as luminous signal.
	;
	;   5
	; 1-4-2			p3 = seed for hatched solid triangle
	;  \3/	a1		a1 = Speed value(s) attribute
	;   .
	;   
	;
	(setq 
		p1 (list -1.500 2.598)
		p2 (list  1.500 2.598)
		p3 (list  0.000 1.500)
		p4 (list  0.000 2.598)
		p5 (list  0.000 3.100)
		att1 '("GV_WERT" "Geschwindigkeit" "4")
		a1 (list  3.000 1.500)
	)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p2 _origin_)
	(DrawLine layDef_Zero _origin_ p1)
	(if luminous
		(DrawHatchAtPoint _denseHatch_ p3 _angleZero_ _offsetZero_)
	)
	(AddAtt (GetAttributeTag att1) (GetAttributePrompt att1) (GetAttributeDefaultValue att1) a1 _th180_ _angle90_ _rcTextStyle_ _middleCenter_)
	(if freeStanding
		(progn
			(MoveUp (DEDB_GetBoardNormalSupportHeight))
			(DEDB_DrawSupportForBoardAndNonKs)
		)
	;else
		(DrawLine layDef_Zero p4 p5) ; Mounted on a common mast below other signals, draw the missing mast segment above this symbol
	)
)



(defun DEDB_DrawRi ( freeStanding announcement / x1 y1 x2 y2 p1 p2 att1 a1 )
	; DEDB: Richtungsanzeiger/Richtungsvoranzeiger - Zs2 / Zs2v
	; It is always luminous.
	; Physical / symbolic: 
	;	Execution signal: Zs2 = white luminous digits on black bakground / Solid black square
	;	Announcement signal; Zs2v = yellow luminous digits on black background / Fat framed squared, empty inside
	; Can be dark or showing one letter at a time from a predefined set of letters, each letter corresponding to a line or a next station name.
	; The letters are shown besides the symbols as e.g. "R,L" for "Rechtsdorf" / "Linksdorf". More letters may apply.
	;
	;     3					Mast: Only if not freeStanding (i.e. part of Ks-signal). If freeStanding: push up and add support mast.
	; +---2---+		
	; | +---+ |				x wide, y high x1,y1 = outer box, x2,y2 = inner box
	; | | 0 |1|	a1			Fat square border if the board is an announcment signal (on Ks distant signal, or freestanding)
	; | +---+ |				The whole outer square is hatched solid if the board is an 'execution' board (with Ks main signal or freestanding)
	; +---.---+				a1 = Direction(s) attribute
	;
	;	Note: Since this graphics is ADDED to the existing Ks-graphics, we cannot freely use MoveUp etc.
	;
	(setq 
		x1	3
		y1	3
		x2	2.4
		y2	2.4
		p0	(list 0.000 1.500)
		p1	(list 1.400 1.500)	; Hatch seed
		p2	(list 0.000 3.000)
		p3	(list 0.000 3.100)
		att1 '("RI_WERT" "Richtung" "A,B,C")
		a1 (list  3.000 1.500)
	)
	(DrawBoxAtPos layDef_Zero p0 x1 y1 layDef_BoardOrPole_Wipeout)
	(if announcement
		(DrawBoxAtPos layDef_Zero p0 x2 y2 _noWipeout_)
	)
	(DrawHatchAtPoint _denseHatch_ p1 _angleZero_ _offsetZero_)
	(AddAtt (GetAttributeTag att1) (GetAttributePrompt att1) (GetAttributeDefaultValue att1) a1 _th180_ _angle90_ _rcTextStyle_ _middleCenter_)
	(if freeStanding
		(progn
			(MoveUp (DEDB_GetBoardNormalSupportHeight))
			(DEDB_DrawSupportForBoardAndNonKs)
		)
	;else
		(DrawLine layDef_Zero p2 p3) ; Mounted on a common mast below other signals, draw the missing mast segment above this symbol
	)
)



(defun DEDB_DrawGg ( freeStanding luminous / x y p1 p2 p3 )
	; DEDB: Gegengleisanzeiger - Zs6
	; It is either a formsignal (not luminous board) or luminous.
	; Physical / symbolic: 
	; Zs6 = A reflective or luminous 'snake' from bottom right to top left / A square with a forward diagonal. Hatched top left if luminous.
	;
	;     3			Mast: Only if not freeStanding (i.e. part of Ks-signal). If freeStanding: push up and add support mast.
	; +---2---5
	; |   1 / |		x wide, y high
	; |   0   |		Hatched upper left part if luminous signal.
	; | /     |
	; 4---.---+	
	;
	;	Note: Since this graphics is ADDED to the existing Ks-graphics, we cannot freely use MoveUp etc.
	;
	(setq 
		x	3
		y	3
		p0	(list  0.000 1.500) ; Box center
		p1	(list  0.000 2.500)	; Hatch seed
		p2	(list  0.000 3.000)
		p3	(list  0.000 3.100)
		p4	(list -1.500 0.000)
		p5	(list  1.500 3.000)
	)
	(DrawBoxAtPos layDef_Zero p0 x y layDef_BoardOrPole_Wipeout)
	(DrawLine layDef_Zero p4 p5)
	(if luminous 
		(DrawHatchAtPoint _denseHatch_ p1 _angleZero_ _offsetZero_)
	)
	(if freeStanding
		(progn
			(MoveUp (DEDB_GetBoardNormalSupportHeight))
			(DEDB_DrawSupportForBoardAndNonKs)
		)
	;else
		(DrawLine layDef_Zero p2 p3) ; Mounted on a common mast below other signals, draw mast segment above this symbol
	)
)



(defun DEDB_DrawMt ( / x y p1 p2 p3 q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 )
	; DEDB: Müntlichkeitstafel - Zs12
	; Signal showing Stop can be passed after an oral (Müntlich) message has been received.
	; It is aways a formsignal (not luminous board).
	; It is always mounted on the main signal's mast.
	; Physical / symbolic: 
	; Zs6 = A reflective or luminous 'snake' from bottom right to top left / A square with a forward diagonal. Hatched top left if luminous.
	;
	;     3			Mast: Only if not freeStanding (i.e. part of Ks-signal). If freeStanding: push up and add support mast.
	; +---2---+
	; |  /|/| |		x wide, y high
	; | | 1 | |		A formsignal (not luminous) with a "handwritten" 'M' (so it can't be mistaken for being a Richtungsanzeiger).
	; |/  | |/|		The points p1... form a polyline 'M'.
	; +---.---+
	;
	;	Note: Since this graphics is ADDED to the existing Ks-graphics, we cannot freely use MoveUp etc.
	;
	(setq 
		x	3
		y	3
		p1	(list  0.000 1.500)		; Center of box at point
		p2	(list  0.000 3.000)	; Add short mast after scaling everything down to 3x3
		p3	(list  0.000 3.100)
		q1	'(-1.250 1.000)
		q2	'(-1.500 0.750)
		q3	'(-1.500 0.500)
		q4	'(-1.250 0.250)
		q5	'(-1.000 0.250)
		q6	'(-0.750 0.750)
		q7	'(-0.500 1.750)
		q8	'(-0.250 2.250)
		q9	'( 0.250 2.750)
		q10 '( 0.000 1.750)
		q11 '(-0.250 0.250)
		q12 '( 0.000 0.750)
		q13 '( 0.250 1.750)
		q14 '( 0.500 2.250)
		q15 '( 1.000 2.750)
		q16 '( 0.750 1.750)
		q17 '( 0.500 0.250)
		q18 '( 0.750 0.250)
		q19 '( 1.000 0.500)
		q20 '( 1.250 1.000)
	)
	(DrawBoxAtPos layDef_Zero p1 x y layDef_BoardOrPole_Wipeout)
	(command _POLYLINE_	q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 _closedPolyline)
	(DrawLine layDef_Zero p2 p3) ; Mounted on a common mast below other signals, draw mast segment above this symbol
	(if freeStanding
		(progn
			(MoveUp (DEDB_GetBoardNormalSupportHeight))
			(DEDB_DrawSupportForBoardAndNonKs)
		)
	;else
		(DrawLine layDef_Zero p2 p3) ; Mounted on a common mast below other signals, draw the missing mast segment above this symbol
	)
)



(defun DEDB_DrawSg ( luminous / x y p1 p2 p3 p4 p5 p6 p7 q1 q2 q3 q4 q5 q6 q7 q8 )
	; DEDB: Stumpfgleis - Zs13
	; The train route ends in a dead-end track or a shortened track.
	; It is either a formsignal (not luminous board) or luminous.
	; It can not be freeStanding. 
	; Physical / symbolic: 
	; Zs13 = Formsignal: A reflective yellow +90 degrees rotated 'T' on black background, or as luminous signal, a yellow rotated 'T' on black bakground.
	;
	; As form signal:
	;     7			Mast: Only if not reeStanding (i.e. part of Ks-signal). If freeStanding: push up and add support mast.
	; +---6---+
	; | 3     |		x wide, y high
	; | 2-0-4 |		Hatched upper left part if luminous signal.
	; | 1    5|		p5 = hatch seed point when white-on-black
	; +---.---+
	;
	;	Note: Since this graphics is ADDED to the existing Ks-graphics, we cannot freely use MoveUp etc.
	;
	; As luminous signal:
	; Same shape as form signal, drawn as unhatched on hatched background with p5 as seed point, using q1..q8 to draw the 'T' shape.
	;
	(setq 
		x	3
		y	3
		p0	'( 0.000  1.500)	; Center of box
		p6	(list 0.000 3.000)	; Add short mast after scaling everything down to 3x3
		p7	(list 0.000 3.100)
		
		; 'T' points when black on white, after moving the box up:
		p1	'( -0.800  0.400)
		p2	'( -0.800  1.500)
		p3	'( -0.800  2.600)
		p4	'(  0.800  1.500)
		p5	'(  1.400  0.100)
		
		; 'T' points q1..q20 when white on black
		q1	'(-0.900 0.400)
		q2	'(-0.900 2.600)
		q3	'(-0.500 2.600)
		q4	'(-0.500 1.700)
		q5	'( 0.900 1.700)
		q6	'( 0.900 1.300)
		q7	'(-0.500 1.300)
		q8	'(-0.500 0.400)
	)
	(DrawBoxAtPos layDef_Zero p0 x y layDef_BoardOrPole_Wipeout)
	(if luminous
		(progn
			; 'T' outline
			(command _POLYLINE_	q1 q2 q3 q4 q5 q6 q7 q8 _closedPolyline_)
			(DrawHatchAtPoint _denseHatch_ p5 _angleZero_ _offsetZero_)
		)
	;else
		(progn
			; 'T' thin line
			(DrawLine layDef_Zero p1 p3)
			(DrawLine layDef_Zero p2 p4)
		)
	)
	(DrawLine layDef_Zero p6 p7)	; Never freeStanding - add line above
)



; Ks-Signals, boards and auxiliary signals mast / support heights
;----------------------------------------------------------------
(defun DEDB_GetKsMainSignalHeight			( / tmp ) (setq tmp 4.000))
(defun DEDB_GetKsDistantSignalHeight		( / tmp ) (setq tmp 3.000))
(defun DEDB_GetKsCombinedSignalHeight		( / tmp ) (setq tmp 7.000))
(defun DEDB_GetKsAuxiliaryItemHeight		( / tmp ) (setq tmp 3.100))
(defun DEDB_GetBoardShortSupportHeight		( / tmp ) (setq tmp 1.000))
(defun DEDB_GetBoardNormalSupportHeight		( / tmp ) (setq tmp 4.500))
(defun DEDB_GetKslLowSupportHeight			( / tmp ) (setq tmp 4.400))
(defun DEDB_GetKsNormalSupportHeight		( / tmp ) (setq tmp 7.500))
(defun DEDB_GetKsYokeSuspensionMastHeight	( / tmp ) (setq tmp 2.000))
(defun DEDB_GetKsWallSupportHeight			( / tmp ) (setq tmp 1.000))
(defun DEDB_GetKsWallSupportWidth			( / tmp ) (setq tmp 2.000))
(defun DEDB_GetKsCeilingSupportHeight		( / tmp ) (setq tmp 2.000))
(defun DEDB_GetMbWallSupportWidth			( / tmp ) (setq tmp 3.500))
(defun DEDB_GetMbWallSupportHeight			( / tmp ) (setq tmp 1.500))



; Ks-Signals, boards and auxiliary signals mast / support
;---------------------------------------------------------
(defun DEDB_DrawShortSupportForBoard ( / p1 p2 p3 )
	; DEDB: Short support for board (Vorsignalbaken etc)
	;
	;     3
	;     | 		
	;   1-.-2
	;
	(setq 
		p1 (list -0.750 0.000)
		p2 (list  0.750 0.000)
		p3 (list  0.000 (DEDB_GetBoardShortSupportHeight))
	)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero _origin_ p3)
)



(defun DEDB_DrawSupportForBoardAndNonKs ( / p1 p2 p3 )
	; DEDB: Ordinary support for board or for auxiliary signal (not Ks-signals)
	;
	;     3
	;     |
	;     |
	;     | 		
	;   1-.-2
	;
	(setq 
		p1 (list -0.750 0.000)
		p2 (list  0.750 0.000)
		p3 (list  0.000 (DEDB_GetBoardNormalSupportHeight))
	)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero _origin_ p3)
)



(defun DEDB_DrawSupportForGroundlevelBoardAndNonKs ( / p1 p2 p3 p4 p5 )
	; DEDB: Support for near-ground mounted board or auxiliary signal (not Ks-signals)
	;
	;     3
	;     |
	;     | 
	;  4--+--5
	;  1--.--2
	;
	(setq 
		p1 (list -0.750 0.000)
		p2 (list  0.750 0.000)
		p3 (list  0.000 (DEDB_GetBoardNormalSupportHeight))
		p4 (list -0.750 0.750) ; Extra line = low board/signal
		p5 (list  0.750 0.750) ; Extra line = low board/signal
	)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero _origin_ p3)
	(DrawLine layDef_Zero p4 p5)
)




(defun DEDB_DrawNormalSupportForKs ( / p1 p2 p3 )
	; DEDB: Ordinary support for Ks-signal, whenever there is nothing under the lower of main and distant signal
	;
	;     3
	;     |
	;     |
	;     |
	;     |
	;     | 		
	;  1--.--2
	;
	(setq 
		p1 (list -1.000 0.000)
		p2 (list  1.000 0.000)
		p3 (list  0.000 (DEDB_GetKsNormalSupportHeight))
	)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero _origin_ p3)
)



(defun DEDB_DrawLowSupportForKs ( / p1 p2 p3 )
	; Support for Ks-signal whenever the symbol has something under the lower of main and distant signal
	;
	;     3
	;     |
	;     |
	;     | 		
	;  1--.--2
	;
	(setq 
		p1 (list -1.000 0.000)
		p2 (list  1.000 0.000)
		p3 (list  0.000 (DEDB_GetKslLowSupportHeight))
	)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero _origin_ p3)
)



(defun DEDB_DrawKsYokeSuspensionMast ( / p1 )
	; Support for Ks-signal whenever the symbol has something under the lower of main and distant signal
	;
	;     .
	;     |
	;     1			; Short mast from yoke down to top-of-signal / board
	;
	(setq 
		p1 (list  0.000 (- (DEDB_GetKsYokeSuspensionMastHeight)))
	)
	(DrawLine layDef_Zero _origin_ p1)
)



(defun DEDB_DrawKsWallSupportLeftsided ( / x y p1 p2 p3 )
	; Support for Ks-signal whenever the symbol has something under the lower of main and distant signal
	;
	;         +-+ 
	;         |*|       Solid hatch
	;      y  |.1---2
	;         |*|   |
	;         +-+   |
	;          x  . 3 .
	;            :(sig :
	;            :here):
	;            :     :
	;
	(setq
		x	0.5
		y	1.5
		p1	(list (HalfOf x) 0.000)
		p2	(list (DEDB_GetKsWallSupportWidth) 0.000)
		p3	(list (DEDB_GetKsWallSupportWidth) (- (DEDB_GetKsWallSupportHeight)))
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p2 p3)
	(DrawHatchAtPoint _denseHatch_ _origin_ _angleZero_ _offsetZero_)
)



(defun DEDB_DrawKsWallSupportRightsided ( / x y p1 p2 p3 )
	; Support for Ks-signal whenever the symbol has something under the lower of main and distant signal
	;
	;         +-+ 
	;         |*|		Solid hatch
	;     2---1.|  y
	;     |   |*|
	;     |   +-+
	;   . 3 .  x
	;  :(sig :
	;  :here):
	;  :     :
	;
	(setq
		x	0.5
		y	1.5
		p1	(list (HalfOf x) 0.000)
		p2	(list (- (DEDB_GetKsWallSupportWidth)) 0.000)
		p3	(list (- (DEDB_GetKsWallSupportWidth)) (- (DEDB_GetKsWallSupportHeight)))
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p2 p3)
	(DrawHatchAtPoint _denseHatch_ _origin_ _angleZero_ _offsetZero_)
)



(defun DEDB_DrawKsCeilingSupport ( / x y p1 p2 )
	; Support for Ks-signal whenever the symbol has something under the lower of main and distant signal
	;
	;  +-----+
	;  |**.**| y  Solid hatch
	;  +--1--+
	;     | x
	;   . 2 .
	;  :(sig :
	;  :here):
	;
	(setq
		x	1.5
		y	0.5
		p1	(list 0.000 (- (HalfOf y)))
		p2	(list 0.000 (- (DEDB_GetKsCeilingSupportHeight)))
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawLine layDef_Zero p1 p2)
	(DrawHatchAtPoint _denseHatch_ _origin_ _angleZero_ _offsetZero_)
)



(defun DEDB_DrawMbWallSupportLeftsided ( / x y p1 p2 p3 )
	; Support for Markerboard 
	; 
	;          +-+ 
	;          |*|          Solid hatch
	;       y  |.1------2
	;          |*|      |
	;          +-+      |
	;           x . . . 3 . . .
	;             : (MB here) :
	;             :           :
	(setq
		x	0.5
		y	1.5
		p1	(list (HalfOf x) 0.000)
		p2	(list (DEDB_GetMbWallSupportWidth) 0.000)
		p3	(list (DEDB_GetMbWallSupportWidth) (- (DEDB_GetMbWallSupportHeight)))
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p2 p3)
	(DrawHatchAtPoint _denseHatch_ _origin_ _angleZero_ _offsetZero_)
)



(defun DEDB_DrawMbWallSupportRightsided ( / x y p1 p2 p3 )
	; Support for Ks-signal whenever the symbol has something under the lower of main and distant signal
	;        
	;               +-+ 
	;               |*|		Solid hatch
	;        2------1.|  y
	;        |      |*|
	;        |      +-+
	;  . . . 3 . . . x
	;  : (MB here) :
	;  :           :
	;
	(setq
		x	0.5
		y	1.5
		p1	(list (- (HalfOf x)) 0.000)
		p2	(list (- (DEDB_GetMbWallSupportWidth)) 0.000)
		p3	(list (- (DEDB_GetMbWallSupportWidth)) (- (DEDB_GetMbWallSupportHeight)))
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p2 p3)
	(DrawHatchAtPoint _denseHatch_ _origin_ _angleZero_ _offsetZero_)
)
