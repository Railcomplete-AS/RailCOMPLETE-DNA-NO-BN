;=========================================================================================================================
;
; 00_DrawHelpers.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Ratios
(defun halfOf 		( x / )	(* x _half_))
(defun twoThirdsOf	( x / )	(* x _twoThirds_))



; Vector addition
(defun addVectors ( pos displacement / px py dx dy x y )
	; A position is a list of X- and Y coordinates: (list X Y) (which evaluates X and Y as expressions before returning)
	; Add displacement (dx,dy) to position (px,py), return position (px+dx,py+dy)
	(setq 
		px (car pos)
		py (cadr pos)
		dx (car displacement)
		dy (cadr displacement)
		x  (+ px dx)
		y  (+ py dy)
	)
	(list x y) ; Return value
)



; Positions, relative to centered box
(defun posTL ( x y / ) (list (* -0.5 x) (*  0.5 y))) ; top left
(defun posML ( x y / ) (list (* -0.5 x) (*  0.0 y))) ; middle right
(defun posBL ( x y / ) (list (* -0.5 x) (* -0.5 y))) ; bottom left
(defun posTC ( x y / ) (list (*  0.0 x) (*  0.5 y))) ; top center
(defun posMC ( x y / ) (list (*  0.0 x) (*  0.0 y))) ; middle center = origo = "0,0"
(defun posBC ( x y / ) (list (*  0.0 x) (* -0.5 y))) ; bottom cewnter
(defun posTR ( x y / ) (list (*  0.5 x) (*  0.5 y))) ; top right
(defun posMR ( x y / ) (list (*  0.5 x) (*  0.0 y))) ; middle right
(defun posBR ( x y / ) (list (*  0.5 x) (* -0.5 y))) ; bottom right



; Text positions above/below, relative to centered box
(defun posAbove ( textHeight y / ) (list 0 (+ (*  0.5 y) (*  0.75 textHeight)))) ; above
(defun posBelow ( textHeight y / ) (list 0 (+ (* -0.5 y) (* -0.75 textHeight)))) ; below



; Text positions inside, relative to centered box
(defun posNR ( numberOfRows row y / )
	; Returns a position (list px py) for texts or text attributes in a box, at a given row number
	;
	; numberOfRows	= number of rows of equally sized text items
	; row			= row, counted from 1 = top row to bottom row = numberOfRows.
	; y				= the total height of the box (centered at origo).
	;
	;    TL-------TR
	;    |   11=.  |  --> numberOfRows = 1, row = 1, and origo '.'
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
(defun pos11 ( y / ) (posNR 1 1 y)) ; 1 row, middle = _origo_

(defun pos21 ( y / ) (posNR 2 1 y)) ; 2 rows, top
(defun pos22 ( y / ) (posNR 2 2 y)) ; 2 rows, bottom

(defun pos31 ( y / ) (posNR 3 1 y)) ; 3 rows, top
(defun pos32 ( y / ) (posNR 3 2 y)) ; 3 rows, middle = _origo_
(defun pos33 ( y / ) (posNR 3 3 y)) ; 3 rows, bottom

(defun pos41 ( y / ) (posNR 4 1 y)) ; 4 rows, top
(defun pos42 ( y / ) (posNR 4 2 y)) ; 4 rows, above middle
(defun pos43 ( y / ) (posNR 4 3 y)) ; 4 rows, below middle
(defun pos44 ( y / ) (posNR 4 4 y)) ; 4 rows, bottom

(defun pos51 ( y / ) (posNR 5 1 y)) ; 5 rows, top
(defun pos52 ( y / ) (posNR 5 2 y)) ; 5 rows, above middle 
(defun pos53 ( y / ) (posNR 5 3 y)) ; 5 rows, middle
(defun pos54 ( y / ) (posNR 5 4 y)) ; 5 rows, below middle
(defun pos55 ( y / ) (posNR 5 5 y)) ; 5 rows, bottom

(defun pos61 ( y / ) (posNR 6 1 y)) ; 6 rows, top
(defun pos62 ( y / ) (posNR 6 2 y)) ; 6 rows, below top
(defun pos63 ( y / ) (posNR 6 3 y)) ; 6 rows, above middle 
(defun pos64 ( y / ) (posNR 6 4 y)) ; 6 rows, below middle
(defun pos65 ( y / ) (posNR 6 5 y)) ; 6 rows, above bottom
(defun pos66 ( y / ) (posNR 6 6 y)) ; 6 rows, bottom


; Move all current graphics up/down/left/right
(defun moveUp    ( dist / ) (command _MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 dist)))
(defun moveDown  ( dist / ) (command _MOVE_ _selectAll_ _ENTER_ _origo_ (list 0 (- dist))))
(defun moveLeft  ( dist / ) (command _MOVE_ _selectAll_ _ENTER_ _origo_ (list (- dist) 0)))
(defun moveRight ( dist / ) (command _MOVE_ _selectAll_ _ENTER_ _origo_ (list dist 0)))



; Geometrical graphics at specified layers, possibly with wipeout below
;----------------------------------------------------------------------------
(defun drawLine ( layDef posA posB / )
	; A line fram A to B
	;
	;     A-------B
	;
	(setLayer layDef)
	(command _LINE_ posA posB _ENTER_)
	'drawLine
)



(defun drawArc (layDef posA posB posC / )
	(setLayer layDef)
	(command _ARC_ posA posB posC)
	'drawArc
)



(defun drawStAndrewCrossAtPos ( layDef pos x y / p1 p2 p3 p4 )
	; The two diagonals in a x-by-y rectangle centered at pos
	;	
	;   1   4
	;    \ /   
	;     .
	;    / \   
	;   3   2
	;
	(setq
		p1 (addVectors (posTL x y) pos)
		p2 (addVectors (posBR x y) pos)
		p3 (addVectors (posBL x y) pos)
		p4 (addVectors (posTR x y) pos)
	)
	(drawLine layDef p1 p2) ; diagonal '\'
	(drawLine layDef p3 p4) ; diagonal '/'
	'drawStAndrewCrossAtPos
)



(defun drawStAndrewCross ( layDef x y / )
	; The two diagonals in a x-by-y rectangle centered at ORIGO
	(drawStAndrewCrossAtPos layDef _origo_ x y)
	'drawStAndrewCross
)



(defun drawCircleAtPos ( layDef pos r layDef_wipeoutLayer / )
	; A circle, centered at 'pos'.
	; Wipeout is added if wipeoutlayer is non-nil.
	;        ___    
	;       /   \
	;      |  .  |       ; . = pos
	;       \___/ 
	;
	(setLayer layDef)
	(command _CIRCLE_ pos r)
	(if layDef_wipeOutLayer
		(progn
			(setLayer layDef_wipeoutLayer)
			(command _WIPEOUT_ _createWipeoutFromPolyline_ _lastSelection_ _ENTER_ _keepWipeoutSource_)
			(setLayer layDef)
		)
	)
	'drawCircleAtPos
)



(defun drawCircle ( layDef r layDef_wipeoutLayer / )
	; A circle, centered at ORIGO
	(drawCircleAtPos layDef _origo_ r layDef_wipeoutLayer)
	'drawCircle
)



(defun drawBoxAtPos ( layDef pos x y layDef_wipeoutLayer / )
	; A rectangular x-by-y area on specified layer, centered at 'pos'.
	; Wipeout is added if wipeoutlayer is non-nil.
	;
	;    TL----------TC---------TR  ^
	;    |                       |  |
	;    ML          .          MR  y high   ; . = pos
	;    |                       |  |
	;    BL----------BC---------BR  v
	;    <-------- x wide ------->
    ;
	(setLayer layDef)
	(command _RECTANGLE_ (addVectors (posTL x y) pos) (addVectors (posBR x y) pos))
	(if layDef_wipeOutLayer
		(progn
			(setLayer layDef_wipeoutLayer)
			(command _WIPEOUT_ _createWipeoutFromPolyline_ _lastSelection_ _ENTER_ _keepWipeoutSource_)
			(setLayer layDef)
		)
	)
	'drawBoxAtPos
)


(defun drawBox ( layDef x y layDef_wipeoutLayer)
	; As drawBoxAtPos, centered at ORIGO
	(drawBoxAtPos layDef _origo_ x y  layDef_wipeoutLayer)
	'drawBox
)
	
; ...emphasis at top/bottom/left/right line for box centered at origo
(defun drawTopEmphasis    ( layDef x y / ) (drawLine layDef (list (* -0.50 x) (*  0.45 y)) (list (*  0.50 x) (*  0.45 y)))) ; A thin band at the top of a box
(defun drawBottomEmphasis ( layDef x y / ) (drawLine layDef (list (* -0.50 x) (* -0.45 y)) (list (*  0.50 x) (* -0.45 y)))) ; A thin band at the bottom of a box
(defun drawLeftEmphasis   ( layDef x y / ) (drawLine layDef (list (* -0.45 x) (*  0.50 y)) (list (* -0.45 x) (* -0.50 y)))) ; A thin band at the left side of a box
(defun drawRightEmphasis  ( layDef x y / ) (drawLine layDef (list (*  0.45 x) (*  0.50 y)) (list (*  0.45 x) (* -0.50 y)))) ; A thin band at the right side of a box



;;;;;(defun drawClosedShape ( layDef_mainLayer pointList hatchDef layDef_wipeoutLayer / n )
;;;;;	; ==============TODO: 2020-08-11 CLFEY Under development
;;;;;	; Draw arbitrary shape, with hatch and wipeout if needed. 
;;;;;	; Shape must not cross itself.
;;;;;	;
;;;;;	; layDef_mainLayer is where the shape is drawn
;;;;;	; Pointlist is '(p1 p2 p3 .... pn) - cannot be nil
;;;;;	; HatchDef is '(hatchSize seedpoint) - or nil if no hatch needed
;;;;;	; layDef_wipeoutLayer is nil if no wipeout is required
;;;;;	(setq
;;;;;		pA	(nth 0 pointList)
;;;;;		n 	(- (length pointList) 1) ; #segments
;;;;;	)
;;;;;	(if (< n 1) 
;;;;;		(alert "*** ERROR: drawClosedShape() needs a list with at least 2 points.")
;;;;;		(progn
;;;;;			(setLayer layDef_mainLayer)
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
(defun drawDoor ( layDef hingePos lockPos doorAngle maxAngle / doorWidth )
	; Input: Two positions plus open door angle and max door angle.
	; Door is drawn below the two points given.
	; Negative angles: Left hinged door below cabinet
	; Positive angles: Right hinged door below cabinet (wuth the hinge to the right of the lock)
	;
	;             TL-------------TR
	;             |               |
	;             |       .       |  ; . = origo
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
	(setLayer layDef)
	(command
		_LINE_ hingePos openPos _ENTER_ ; The open door
		_ARC_ lockPos _setArcCenter_ hingePos _setArcAngle_ (- maxAngle)
	)
)
(defun drawLeftDoor  ( layDef hingePos lockPos  / ) (drawDoor layDef hingePos lockPos  60  160))
(defun drawRightDoor ( layDef hingePos lockPos  / ) (drawDoor layDef hingePos lockPos -60 -160)) ; reverse angles...



;==========================
; Proxy symbol - for objects that do not have any schematic representation / who need a simple-to-read symbol
;==========================
(defun drawProxySymbol ( layDef text / )
	;
	; (text)  ; a circle with text
	; 
	(drawCircle layDef _proxySymbolRadius_ _noWipeout_)
	(cond
		((= (strlen text) 1) (setq textHeight _oneLetterProxySymbolTextHeight_))
		((= (strlen text) 2) (setq textHeight _twoLetterProxySymbolTextHeight_))
		((= (strlen text) 3) (setq textHeight _threeLetterProxySymbolTextHeight_))
		(T 					 (setq textHeight _th050_)) ; Default linewidth, font height is 10x linewidth
	)
	(addTextAtPos layDef textHeight _origo_ text)
)



;==========================
; Add descriptive text to symbol
;==========================
(defun addDescriptionBelowOrigo ( description distanceBelowOrigo / nLines )
	; Adds MTEXT in a box of standard width and text size, at x=0 and y=suitably below the given distance below origo.
	; The text is supposed to wrap inside a textbox with size _descriptionTextWidth_. We assume that character widths are on the average ca 0.7 of the text height.
	; "THE QUICK BROWN FOX JUMPED OVER THE LAZY DOG" is 44 characters, which in uppercase iso3098.shx textHeight 0.18 is 5.41 wide => 0.123/char => ratio 0.683.
	(setq 
		nLines	(+ 1 (/ (* (strlen description) 0.683 _descriptionTextHeight_) _descriptionTextBoxWidth_)) ; round up
		pos		(list 0 (- (+ distanceBelowOrigo (* nLines _descriptionTextHeight_))))
	)
	(addMText
		layDef_Description
		_descriptionTextHeight_ 
		_descriptionTextBoxWidth_
		pos
		description
	)
)
