;=========================================================================================================================
;
; 00_DrawHelpers.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Ratios
(defun HalfOf 		( x / )	(* x _half_))
(defun TwoThirdsOf	( x / )	(* x _twoThirds_))



; Pick from list
(defun FirstItemIn ( s / )	(nth 0 s))
(defun SecondItemIn ( s / )	(nth 1 s))
(defun ThirdItemIn ( s / )	(nth 2 s))
(defun FourthItemIn ( s / )	(nth 3 s))
(defun FifthItemIn ( s / )	(nth 4 s))



; Pick coordinate from vector
(defun xCoord ( p / ) (nth 0 p))
(defun yCoord ( p / ) (nth 1 p))



; Vector addition
(defun AddVectors ( point displacement / px py dx dy x y )
	; A position is a list of X- and Y coordinates: (list X Y) (which evaluates X and Y as expressions before returning)
	; Add displacement (dx,dy) to position (px,py), return position (px+dx,py+dy)
	(setq 
		px (car point)
		py (cadr point)
		dx (car displacement)
		dy (cadr displacement)
		x  (+ px dx)
		y  (+ py dy)
	)
	(list x y) ; Return value
)



; Positions, relative to centered box
(defun PointTL ( x y / ) (list (* -0.5 x) (*  0.5 y))) ; top left
(defun PointML ( x y / ) (list (* -0.5 x) (*  0.0 y))) ; middle right
(defun PointBL ( x y / ) (list (* -0.5 x) (* -0.5 y))) ; bottom left
(defun PointTC ( x y / ) (list (*  0.0 x) (*  0.5 y))) ; top center
(defun PointMC ( x y / ) (list (*  0.0 x) (*  0.0 y))) ; middle center = origin = "0,0"
(defun PointBC ( x y / ) (list (*  0.0 x) (* -0.5 y))) ; bottom center
(defun PointTR ( x y / ) (list (*  0.5 x) (*  0.5 y))) ; top right
(defun PointMR ( x y / ) (list (*  0.5 x) (*  0.0 y))) ; middle right
(defun PointBR ( x y / ) (list (*  0.5 x) (* -0.5 y))) ; bottom right



; Text positions above/below, relative to centered box
(defun PosAbove ( textHeight y / ) (list 0 (+ (*  0.5 y) (*  0.75 textHeight)))) ; above
(defun PointBelow ( textHeight y / ) (list 0 (+ (* -0.5 y) (* -0.75 textHeight)))) ; below



; Text positions inside, relative to centered box
(defun PosNR ( numberOfRows row y / )
	; Returns a position (list px py) for texts or text attributes in a box, at a given row number
	;
	; numberOfRows	= number of rows of equally sized text items
	; row			= row, counted from 1 = top row to bottom row = numberOfRows.
	; y				= the total height of the box (centered at origin).
	;
	;    TL-------TR
	;    |   11=.  |  --> numberOfRows = 1, row = 1, and origin '.'
	;    BL-------BR
	;
	;    TL-------TR
	;    |   21    |  --> numberOfRows = 2, row = 1
	;    |    .    |
	;    |   22    |  --> numberOfRows = 2, row = 2
	;    BL-------BR
	;     ...etc...
	;
	(list 0 (- (/ y 2.0) (* row (/ y (+ numberOfRows 1.0)))))
)
(defun Point11 ( y / ) (PosNR 1 1 y)) ; 1 row, middle = _origin_

(defun Point21 ( y / ) (PosNR 2 1 y)) ; 2 rows, top
(defun Point22 ( y / ) (PosNR 2 2 y)) ; 2 rows, bottom

(defun Point31 ( y / ) (PosNR 3 1 y)) ; 3 rows, top
(defun Point32 ( y / ) (PosNR 3 2 y)) ; 3 rows, middle = _origin_
(defun Point33 ( y / ) (PosNR 3 3 y)) ; 3 rows, bottom

(defun Point41 ( y / ) (PosNR 4 1 y)) ; 4 rows, top
(defun Point42 ( y / ) (PosNR 4 2 y)) ; 4 rows, above middle
(defun Point43 ( y / ) (PosNR 4 3 y)) ; 4 rows, below middle
(defun Point44 ( y / ) (PosNR 4 4 y)) ; 4 rows, bottom

(defun Point51 ( y / ) (PosNR 5 1 y)) ; 5 rows, top
(defun Point52 ( y / ) (PosNR 5 2 y)) ; 5 rows, above middle 
(defun Point53 ( y / ) (PosNR 5 3 y)) ; 5 rows, middle
(defun Point54 ( y / ) (PosNR 5 4 y)) ; 5 rows, below middle
(defun Point55 ( y / ) (PosNR 5 5 y)) ; 5 rows, bottom

(defun Point61 ( y / ) (PosNR 6 1 y)) ; 6 rows, top
(defun Point62 ( y / ) (PosNR 6 2 y)) ; 6 rows, below top
(defun Point63 ( y / ) (PosNR 6 3 y)) ; 6 rows, above middle 
(defun Point64 ( y / ) (PosNR 6 4 y)) ; 6 rows, below middle
(defun Point65 ( y / ) (PosNR 6 5 y)) ; 6 rows, above bottom
(defun Point66 ( y / ) (PosNR 6 6 y)) ; 6 rows, bottom


; Move all current graphics up/down/left/right
(defun MoveUp    ( dist / ) (command _MOVE_ _selectAll_ _ENTER_ _origin_ (list 0 dist)))
(defun MoveDown  ( dist / ) (command _MOVE_ _selectAll_ _ENTER_ _origin_ (list 0 (- dist))))
(defun MoveLeft  ( dist / ) (command _MOVE_ _selectAll_ _ENTER_ _origin_ (list (- dist) 0)))
(defun MoveRight ( dist / ) (command _MOVE_ _selectAll_ _ENTER_ _origin_ (list dist 0)))



; Geometrical graphics at specified layers, possibly with wipeout below
;----------------------------------------------------------------------------
(defun DrawLine (layDef pointA PointB / )
	; A line fram A to B
	;
	;     A-------B
	;
	(SetLayer layDef)
	(command _LINE_ pointA PointB _ENTER_)
	'DrawLine
)



(defun DrawArc (layDef pointA PointB posC / )
	; An arc from A through B to C 
	;
	;    __B__
	;   /     \
	;  A       C
	;
	(SetLayer layDef)
	(command _ARC_ pointA PointB posC)
	'DrawArc
)



(defun DrawArcByCenter (layDef pointCenter pointA PointB / )
	; An arc from A through B to C 
	;
	;    _____
	;   /     \
	;  A       B
	;      C		; C = center of circular arc
	;
	;
	(SetLayer layDef)
	(command _ARC_ 	_setArcCenter_ pointCenter pointA PointB _ENTER_)
	'DrawArc
)



(defun DrawStAndrewCrossAtPoint ( layDef point x y / p1 p2 p3 p4 )
	; The two diagonals in a x-by-y rectangle centered at point
	;	
	;   1   4
	;    \ /   
	;     .
	;    / \   
	;   3   2
	;
	(setq
		p1 (AddVectors (PointTL x y) point)
		p2 (AddVectors (PointBR x y) point)
		p3 (AddVectors (PointBL x y) point)
		p4 (AddVectors (PointTR x y) point)
	)
	(DrawLine layDef p1 p2) ; diagonal '\'
	(DrawLine layDef p3 p4) ; diagonal '/'
	'DrawStAndrewCrossAtPoint
)



(defun DrawStAndrewCross ( layDef x y / )
	; The two diagonals in a x-by-y rectangle centered at ORIGIN
	(DrawStAndrewCrossAtPoint layDef _origin_ x y)
	'DrawStAndrewCross
)



(defun DrawCircleAtPoint ( layDef point r layDef_Wipeout / )
	; A circle, centered at 'point'.
	; Wipeout is added if wipeoutlayer is non-nil.
	;        ___    
	;       /   \
	;      |  .  |       ; . = point
	;       \___/ 
	;
	(SetLayer layDef)
	(command _CIRCLE_ point r)
	(if layDef_Wipeout
		(progn
			(command _POLYGON_ 16 point _inscribedPolygon_ r)
			(AddWipeoutToLastClosedPolyline layDef_Wipeout _keepWipeoutSource_)
			(command _DRAWORDER_ _lastSelection_ _ENTER_ _aboveAllObjects_)
		)
	)
	'DrawCircleAtPoint
)



(defun DrawCircle ( layDef r layDef_Wipeout / )
	; A circle, centered at ORIGIN
	(DrawCircleAtPoint layDef _origin_ r layDef_Wipeout)
	'DrawCircle
)



(defun DrawBoxAtPoint ( layDef point x y layDef_Wipeout / )
	; A rectangular x-by-y area on specified layer, centered at 'point'.
	; Wipeout is added if wipeoutlayer is non-nil.
	;
	;    TL----------TC---------TR  ^
	;    |                       |  |
	;    ML          .          MR  y high   ; . = point
	;    |                       |  |
	;    BL----------BC---------BR  v
	;    <-------- x wide ------->
    ;
	(SetLayer layDef)
	(command _RECTANGLE_ (AddVectors (PointTL x y) point) (AddVectors (PointBR x y) point))
	(if layDef_Wipeout
		(progn
			(AddWipeoutToLastClosedPolyline layDef_Wipeout _keepWipeoutSource_)
			(command _DRAWORDER_ _lastSelection_ _ENTER_ _aboveAllObjects_)
		)
	)
	'DrawBoxAtPoint
)


(defun DrawBox ( layDef x y layDef_Wipeout / )
	; As DrawBoxAtPoint, centered at ORIGIN
	(DrawBoxAtPoint layDef _origin_ x y  layDef_Wipeout)
	'DrawBox
)
	
; ...emphasis at top/bottom/left/right line for box centered at origin
(defun DrawTopEmphasis    ( layDef x y / ) (DrawLine layDef (list (* -0.50 x) (*  0.45 y)) (list (*  0.50 x) (*  0.45 y)))) ; A thin band at the top of a box
(defun DrawBottomEmphasis ( layDef x y / ) (DrawLine layDef (list (* -0.50 x) (* -0.45 y)) (list (*  0.50 x) (* -0.45 y)))) ; A thin band at the bottom of a box
(defun DrawLeftEmphasis   ( layDef x y / ) (DrawLine layDef (list (* -0.45 x) (*  0.50 y)) (list (* -0.45 x) (* -0.50 y)))) ; A thin band at the left side of a box
(defun DrawRightEmphasis  ( layDef x y / ) (DrawLine layDef (list (*  0.45 x) (*  0.50 y)) (list (*  0.45 x) (* -0.50 y)))) ; A thin band at the right side of a box



;;;;;(defun DrawClosedShape ( layDef_mainLayer pointList hatchDef layDef_Wipeout / n )
;;;;;	; ==============TODO: 2020-08-11 CLFEY Under development
;;;;;	; Draw arbitrary shape, with hatch and wipeout if needed. 
;;;;;	; Shape must not cross itself.
;;;;;	;
;;;;;	; layDef_mainLayer is where the shape is drawn
;;;;;	; Pointlist is '(p1 p2 p3 .... pn) - cannot be nil
;;;;;	; HatchDef is '(hatchSize seedpoint) - or nil if no hatch needed
;;;;;	; layDef_Wipeout is nil if no wipeout is required
;;;;;	(setq
;;;;;		pA	(nth 0 pointList)
;;;;;		n 	(- (length pointList) 1) ; #segments
;;;;;	)
;;;;;	(if (< n 1) 
;;;;;		(alert "*** ERROR: DrawClosedShape() needs a list with at least 2 points.")
;;;;;		(progn
;;;;;			(SetLayer layDef_mainLayer)
;;;;;			(command "._POLYLINE" pA pB _ENTER_=
;;;;;			
;;;;;	(foreach pB (cdr pointList)
;;;;;		(command "._POLYLINE" pA pB _ENTER_=
;;;;;		; Hmmmmmmmmmmmmmm  JOIN benytter ikke SELECT, jeg får ikke pekt på det som skal legges til...	
;;;;;		)
;;;;;	)
;;;;;)



; Cabinet doors
;---------------
(defun DrawDoor ( layDef hingePos lockPos doorAngle maxAngle / doorWidth )
	; Input: Two positions plus open door angle and max door angle.
	; Door is drawn below the two points given.
	; Negative angles: Left hinged door below cabinet
	; Positive angles: Right hinged door below cabinet (wuth the hinge to the right of the lock)
	;
	;             TL-------------TR
	;             |               |
	;             |       .       |  ; . = origin
	;             |               |
	;       hinge BL-----BC------BR lock ; left-hinged large door is shown
	; \            \\            /  
	;   \           \\         /
	;     \----------\\------/ 
	;                 open
	(setq
		doorWidth (- (car lockPos) (car hingePos))
		openPos (list 
					(+ (car hingePos) (* (DDcos doorAngle) doorWidth))   
					(- (cadr hingePos) (* (DDsin doorAngle) doorWidth))
				)
	)
	(SetLayer layDef)
	(command
		_LINE_ hingePos openPos _ENTER_ ; The open door
		_ARC_ lockPos _setArcCenter_ hingePos _setArcAngle_ (- maxAngle)
	)
)
(defun DrawLeftDoor  ( layDef hingePos lockPos  / ) (DrawDoor layDef hingePos lockPos  60  160))
(defun DrawRightDoor ( layDef hingePos lockPos  / ) (DrawDoor layDef hingePos lockPos -60 -160)) ; reverse angles...



;==========================
; Proxy symbol - for objects that do not have any schematic representation / who need a simple-to-read symbol
;==========================
(defun DrawProxySymbol ( layDef text / )
	;
	; (text)  ; a circle with text
	; 
	(DrawCircle layDef _proxySymbolRadius_ _noWipeout_)
	(cond
		((= (strlen text) 1) (setq textHeight _oneLetterProxySymbolTextHeight_))
		((= (strlen text) 2) (setq textHeight _twoLetterProxySymbolTextHeight_))
		((= (strlen text) 3) (setq textHeight _threeLetterProxySymbolTextHeight_))
		(T 					 (setq textHeight _th050_)) ; Default linewidth, font height is 10x linewidth
	)
	(AddTextAtPoint layDef textHeight _origin_ text)
)



;==========================
; Add descriptive text to symbol
;==========================
(defun AddDescriptionBelowOrigin ( description distanceBelowOrigin / nLines )
	; Adds MTEXT in a box of standard width and text size, at x=0 and y=suitably below the given distance below origin.
	; The text is supposed to wrap inside a textbox with size _descriptionTextWidth_. We assume that character widths are on the average ca 0.7 of the text height.
	; "THE QUICK BROWN FOX JUMPED OVER THE LAZY DOG" is 44 characters, which in uppercase iso3098.shx textHeight 0.18 is 5.41 wide => 0.123/char => ratio 0.683.
	(setq 
		nLines	(+ 1 (/ (* (strlen description) 0.683 _descriptionTextHeight_) _descriptionTextBoxWidth_)) ; Round up
		point		(list 0 (- (+ distanceBelowOrigin (* nLines _descriptionTextHeight_))))
	)
	(AddMText
		layDef_Description
		_descriptionTextHeight_ 
		_descriptionTextBoxWidth_
		point
		description
	)
)
