;=========================================================================================================================
;
; BoardAndPoleHelpers.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; DRAWARC
; GETLOSANGESIDE
; DRAWLOSANGE
; DRAWLOSANGEWITHCIRCLE
; DRAWFATCIRCLE
; DRAWLOSANGEWITHLINING
; DRAWTRIANGLE
; DRAWRIGHTARROW
; DRAWLEFTARROW
; DRAWDOUBLEARROW
; DRAWFATRIGHTARROW
; DRAWFATLEFTARROW
; DRAWHOLLOWARROWATPOINT

(defun getLosangeSide ( / ) 
	(setq side 5.5)
)



(defun drawLosange ( / side )
	; Losange with wipeout
	;
	;       +
	;     /   \
	;   /       \  
	;  +    .    + 
	;   \       /
	;     \   /
	;       +
	;
	(setq	
		side   (getLosangeSide)
	)
	(drawBox layDef_Zero side side layDef_BoardOrPole_Wipeout)
	(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angle45_)
)



(defun drawLosangeWithCircle ( / side r )
	; Losange with circle
	;
	;       +
	;     /___\
	;   / /   \ \  
	;  + (  .  ) + 
	;   \ \___/ /
	;     \   /
	;       +
	;
	(setq	
		r	   (* 0.318 (getLosangeSide))
	)
	(drawLosange)
	(setLayer layDef_Zero)
	(drawCircle layDef_Zero r _noWipeout_)
)



(defun drawFatCircle ( r1 r2 )
	; Fat circle on white background 
	;
	;      === 
	;    //   \\ 
	;   ((  .  ))
	;    \\   // 
	;	   === 
	(setLayer layDef_Zero)
	(command _CIRCLE_ _origo_ r1)
	(command _CIRCLE_ _origo_ r2)
	(drawHatchFromPoint _solidHatch_ (list 0 (* 0.5 (+ r1 r2))) _angleZero_ _offsetZero_)
)



(defun drawLosangeWithLining ( / side )
	; Losange with wipeout and fat edge
	;
	;       +
	;     // \\
	;   //     \\  
	;  ++   .   ++
	;   \\     //
	;     \\ //
	;       +
	;
	(setq	
		side	(getLosangeSide)
		inner	(* 0.90 side)
	)
	(drawBox layDef_Zero side side layDef_BoardOrPole_Wipeout)
	(drawBox layDef_Zero inner inner _noWipeout_)
	(drawHatchFromPoint _solidHatch_ (list 0 (* 0.25 (+ side inner))) _angleZero_ _offsetZero_)	; Hatch the lining, the area between the two squares
	(command _ROTATE_ _selectAll_ _ENTER_ _origo_ _angle45_)
)



(defun drawTriangle ( side / ang )
	; Small triangle pointing down to origo
	;
	; +-------+
	;  \     /
	;   \   /
	;    \ /
	;     .
	;
	(setq ang (D->R 60))
	(setLayer layDef_Zero)
	(command 
		_POLYLINE_ 
			_origo_
			(list (* side (cos ang)) (* side (sin ang)))
			(list (- (* side (cos ang))) (* side (sin ang)))
			_closedPolyline_
	)
	(addWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _keepWipeoutSource_)
)



(defun drawRightArrow ( x y / p11 p21 p22 p23 p31 )
	; Simple right arrow
	;
	;           p31
	;           |  \
	;  p21---.--p22 p23
	;           |  /
	;           p11
	;
	(setq 
		p11 (list (*  0.167 x) (* -0.167 y))
		p21 (list (* -0.333 x) (*  0.000 y))
		p22 (list (*  0.167 x) (*  0.000 y))
		p23 (list (*  0.333 x) (*  0.000 y))
		p31 (list (*  0.167 x) (*  0.167 y))
	)
	(setLayer layDef_Zero)
	(command _POLYLINE_ p21 p22 p11 p23 p31 p22 _openPolyline_) ; Right arrow
	(drawHatch _solidHatch_)
)



(defun drawLeftArrow ( x y / p11 p21 p22 p23 p31 )
	; Simple left arrow
	;
	;    p31
	;   /  |
	; p21  p22--.----p23
	;   \  |
	;    p11
	;
	(setq 
		p11 (list (* -0.167 x) (* -0.167 y))
		p21 (list (* -0.333 x) (*  0.000 y))
		p22 (list (* -0.167 x) (*  0.000 y))
		p23 (list (*  0.333 x) (*  0.000 y))
		p31 (list (* -0.167 x) (*  0.167 y))
	)
	(setLayer layDef_Zero)
	(command _POLYLINE_ p23 p22 p11 p21 p31 p22 _openPolyline_) ; Right arrow
	(drawHatch _solidHatch_)
)



(defun drawDoubleArrow ( x y / q11 q21 q22 q31 p11 p21 p22 p23 p31 )
	; Double arrow
	;
	;    q31        p31
	;   /  |        |  \
	; q21  q22--..--p22 p23
	;   \  |        |  /
	;    q11        p11
	;
	(setq 
		q11 (list (* -0.167 x) (* -0.167 y))
		q21 (list (* -0.333 x) (*  0.000 y))
		q22 (list (* -0.167 x) (*  0.000 y))
		q31 (list (* -0.167 x) (*  0.167 y))
		p11 (list (*  0.167 x) (* -0.167 y))
		p22 (list (*  0.167 x) (*  0.000 y))
		p23 (list (*  0.333 x) (*  0.000 y))
		p31 (list (*  0.167 x) (*  0.167 y))
	)
	(setLayer layDef_Zero)
	(command _POLYLINE_ _origo_ q22 q11 q21 q31 q22 _openPolyline_) ; Left arrow
	(drawHatch _solidHatch_)
	(command _POLYLINE_ _origo_ p22 p11 p23 p31 p22 _openPolyline_) ; Right arrow
	(drawHatch _solidHatch_)
)



(defun drawFatRightArrow ( x y / p12 p21 p22 p33 p41 p42 p52 )
	; Fat Right arrow
	;
	;           p52\
	;  p41------p42 \
	;  |      .    p33
	;  p21------p22 /
	;           p12/
	;
	(setq 
		p12 (list (*  0.2 x) (* -0.16 y))
		p21 (list (* -0.4 x) (* -0.04 y))
		p22 (list (*  0.2 x) (* -0.04 y))
		p33 (list (*  0.4 x) (*  0.00 y))
		p41 (list (* -0.4 x) (*  0.04 y))
		p42 (list (*  0.2 x) (*  0.04 y))
		p52 (list (*  0.2 x) (*  0.16 y))
	)
	(setLayer layDef_Zero)
	(command _POLYLINE_ p21 p22 p12 p33 p52 p42 p41 _closedPolyline_) ; Right arrow
	(drawHatch _solidHatch_)
)



(defun drawFatLeftArrow ( x y / p12 p21 p22 p33 p41 p42 p52 )
	; Fat Left arrow
	;
	;    /p52
	;   / p42------p41
	;  p33    .      |
	;   \ p22------p21
	;    \p12
	;
	(setq 
		p12 (list (* -0.2 x) (* -0.16 y))
		p21 (list (*  0.4 x) (* -0.04 y))
		p22 (list (* -0.2 x) (* -0.04 y))
		p33 (list (* -0.4 x) (*  0.00 y))
		p41 (list (*  0.4 x) (*  0.04 y))
		p42 (list (* -0.2 x) (*  0.04 y))
		p52 (list (* -0.2 x) (*  0.16 y))
	)
	(setLayer layDef_Zero)
	(command _POLYLINE_ p21 p22 p12 p33 p52 p42 p41 _closedPolyline_) ; Left arrow
	(drawHatch _solidHatch_)
)



(defun drawHollowArrowAtpoint ( x y tip angle / ux uy p0 p1 p2 p3 p4 q1 q2 aq3 q4 )
	; x,y : Surrounding box dimensions
	; tip : Absolute coordinate of the arrow's right / left tip, meant to be inside box x*y
	; dir : 0 => Right arrow (East) / 90 = Up arrow (North) / 180  => Left arrow (West) / 270 = Down arrow (South)
	;
	; TL-------------------TR
	; |                     |
	; |          p2-- p1    |
	; |           \     \   |
	; |       p4---p3    \  | ; Right arrow shown (dir = 0)
	; |       |      tip=p0 | 
	; |       q4---q3    /  |
	; |           /     /   |
	; |          q2---q1    |
	; |                     |
	; |                     |
	; BL-------------------BR
	;
	; Debug test case, Right arrow, tip in quadrant 1: 
	; 		(setq x 10 y 10 tip '(0.0 0.0) dir   0) (DRAWHOLLOWARROW x y tip dir)
	; 		(setq x 10 y 10 tip '(0.0 0.0) dir  90) (DRAWHOLLOWARROW x y tip dir)
	; 		(setq x 10 y 10 tip '(0.0 0.0) dir 180) (DRAWHOLLOWARROW x y tip dir)
	; 		(setq x 10 y 10 tip '(0.0 0.0) dir 270) (DRAWHOLLOWARROW x y tip dir)
	;
	(setq 
		ux (* 0.03 x) ; horizontal drawing unit, relative to x
		uy (* 0.03 y) ; vertical drawing unit, relative to y
		;p0 (list (* (nth 0 tip) x) (* (nth 1 tip) y))
		p0 tip
		p1 (addVectors p0 (list (* -2 ux) (*  3 uy)))
		p2 (addVectors p1 (list (* -2 ux) (*  0 uy)))
		p3 (addVectors p2 (list (*  1 ux) (* -2 uy)))
		p4 (addVectors p3 (list (* -6 ux) (*  0 uy)))
		q1 (addVectors p0 (list (* -2 ux) (* -3 uy)))
		q2 (addVectors q1 (list (* -2 ux) (*  0 uy)))
		q3 (addVectors q2 (list (*  1 ux) (*  2 uy)))
		q4 (addVectors q3 (list (* -6 ux) (*  0 uy)))
	)
	(setLayer layDef_Zero)
	(drawBox layDef_Zero x y _noWipeout_)
	(command _POLYLINE_ p0 p1 p2 p3 p4 q4 q3 q2 q1 _closedPolyline_)
	(command _ROTATE_ _lastSelection_ _ENTER_ p0 angle)
)
