;=========================================================================================================================
;
; SignalItemHelpers.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
; 2020-09-02 CLFEY Added drawMb function
;
;=========================================================================================================================

; Useful functions providing an API which hides CAD system specifics. Usage: Draw signal parts.

; GETLARGELANTERNRADIUS
; GETMEDIUMLANTERNRADIUS
; GETSMALLLANTERNRADIUS
; DRAWLANTERN
; DRAWVERTICALPOLE
; DRAWHORIZONTALHSBASE
; DRAWLYINGPOLE
; DRAWLYINGHSBASE
; DRAWLYINGTSBASE
; DRAWMB
; DRAWTVS
; DRAWFKS
; DRAWFORMSIGNALWITHTEXT
; DRAWLS
; DRAWLHS
; DRAWZS
; DRAWMKS
; DRAWDS
; DRAWDSMKS

;==========================
; Lantern sizes
;==========================
(defun getLargeLanternRadius ( / r )
	; Hs, RepHs, Fs, Vs
	; Bane NOR hovedsignal, repeterhovedsignal, forsignal og veisignal (PLO mot vei) lanterne skjematisk symbol
	(setq r 1.5) ; Ø3
)

(defun getMediumLanternRadius( / r )
  	; Bane NOR middelkontroll signal lanterne skjematisk symbol
	(setq r 1.25) ; Ø2.5
)

(defun getSmallLanternRadius( / r )
	; Bane NOR togsporsignal lanterne skjematisk symbol
	(setq r 1.00) ; Ø2
)



;==========================
; draw...X...() functions
;==========================

;==========================
; Signal lanterns
;==========================
(defun drawLantern ( pos r / )
	(command "._CIRCLE" pos r)
)



;==========================
; Draw signal item
;==========================
(defun drawVerticalPole ( poleLength / )
	(command "._LINE" "0,0" (list 0 poleLength) "")
)	



(defun drawHorizontalHsBase ( / x y )
	(setq 
		x 3.0
		y 0.5
	)
	(command 
		"._RECTANGLE" 
			(list (/ x -2) (/ y -2)) 
			(list (/ x 2) (/ y 2))
	)
	(drawHatch _denseHatch_)
) 



(defun drawLyingPole ( poleEnd1 poleEnd2 / )
	; We here assume that the signal is drawn with its pole starting at (0,0) and extends to the right, lying along the X-axis.
	; In use by older drawX...X...() routines.
	(command "._LINE" (list poleEnd1 0) (list poleEnd2 0) "")
)



(defun drawLyingHsBase ( / x y )
	; We here assume that the signal is drawn with its pole starting at (0,0) and extends to the right, lying along the X-axis.
	; In use by older drawX...X...() routines.
	(setq 
		x 0.5
		y 3.0
	)
	(command 
		"._RECTANGLE" (list (- (/ x 2)) (- (/ y 2))) (list (/ x 2) (/ y 2))
	)
	(drawHatch _denseHatch_)
) 



(defun drawLyingTsBase ( / x y )
	; We here assume that the signal is drawn with its pole starting at (0,0) and extends to the right, lying along the X-axis.
	; In use by older drawX...X...() routines.
	; Ts = Togsporsignal has a smaller base.
	(setq 
		x 0.5
		y 2.0
	)
	(command 
		"._RECTANGLE" (list (- (/ x 2)) (- (/ y 2))) (list (/ x 2) (/ y 2))
	)
	(drawHatch _denseHatch_)
) 



(defun drawMb ( dir / x y )
	; Signal E35 Stop sign
	;
	;         p31
	;         / \
	;       /     \
	;     /         \
	;   /             \
	; p21-p22  .  p23-p24 
	;       |     | 
	;       |     | 
	;       |     | 
	;       p11-p12
	;
	(setq
		x	6.0
		y	6.0
		p11	(list (* -0.10 x) (* -0.45 y))
		p12	(list (*  0.10 x) (* -0.45 y))
		p21	(list (* -0.45 x) (*  0.00 y))
		p22	(list (* -0.10 x) (*  0.00 y))
		p23	(list (*  0.10 x) (*  0.00 y))
		p24	(list (*  0.45 x) (*  0.00 y))
		p31	(list (*  0.00 x) (*  0.45 y))
	)
	; Surrounding box:
	(drawBoxAtPos layer_Zero x y _origo_ layer_BoardOrPole_Wipeout )

	; Arrow pointing straight up:
	(command "._PLINE" p11 p12 p23 p24 p31 p21 p22 p11 _open_)
	(cond 
		((= dir _left_)	(command "._ROTATE" "_LAST" "" "0,0" "90")) ; Arrow now points to the left
		((= dir _right_)(command "._ROTATE" "_LAST" "" "0,0" "-90")); Arrow noe points to the right
		((= dir _down_)(command "._ROTATE" "_LAST" "" "0,0" "180")) ; Arrow now points down
	)
	(drawHatchFromPoint _denseHatch_ p1 0 0)
	(moveUp (/ y 2))
)



(defun drawTVs ( posX posY /  pt1 r1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 diff )
	; Signal 66 'Togvei Slutt'
	; Draw an upright 'S' shape with its lowest point located at (posX,posY)
	(setq 
		pt0 "0.00,1.25" ; center for first arc, to give directions
		pt1 (list (+ posX -1.0) (+ posY 0.5))
		r1 1.25
		pt2 (list (+ posX 1.0) (+ posY 0.5))
		r2 1.00
		pt3 (list (+ posX 0.75) (+ posY 1.75))
		r3 1.50
		pt4 (list (+ posX 0.00) (+ posY 2.00))
		r4 -1.50
		pt5 (list (+ posX -0.75) (+ posY 2.25))
		r5 -1.00
		pt6 (list (+ posX -1.0) (+ posY 3.5))
		r6 -1.25
		pt7 (list (+ posX 1.0) (+ posY 3.5))
	)
	(command 
		"._PLINE" pt1 "_ARC" "CE" pt0 pt2 pt3 pt4 pt5 pt6 pt7 ""
	)
)



(defun drawFKs ( posX posY / side s2 r )
	; Signal 32 'Forsiktig kjøring'
	(setq 
		side 3.0
		s2 (/ side 2)
		r 0.3125 ; five small circles in 3posX3 boposX
	)
	(command 
		"._RECTANGLE" (list (+ posX (- s2)) posY) (list (+ posX s2) (+ posY side)) ; boposX
		; five small circles:
		"._CIRCLE" (list (+ posX 0.0) (+ posY s2 -1.0)) r
		"._CIRCLE" (list (+ posX 0.0) (+ posY s2 1.0)) r  
		"._CIRCLE" (list (+ posX 0.0) (+ posY s2 0.0)) r
		"._CIRCLE" (list (- posX 1.0) (+ posY s2)) r
		"._CIRCLE" (list (+ posX 1.0) (+ posY s2)) r
		; Bottom center of box is now at (posX,posY)
	)
)



(defun drawFormSignalWithText ( text posX posY / side s2 centerPos )
	; Signal 
	(setq 
		side 3.0
		s2 (/ side 2)
		centerPos (list posX (+ posY s2))
	)
	(command "._RECTANGLE" (list (+ posX (- s2)) posY) (list (+ posX s2) (+ posY side)))
	(addText text centerPos 1.8 0 "iso" "_MC")
)



(defun drawLs ( posX posY / side s2 centerPos defaultText )
	; Signal 35B 'Linjesignal' (Hovedlinjesignal)
	(setq 
		side 3.0
		s2 (/ side 2)
		centerPos (list posX (+ posY s2))
		defaultText "A/D"
	)
	(command "._RECTANGLE" (list (+ posX (- s2)) posY) (list (+ posX s2) (+ posY side)))
	(addAtt "LS" "Hovedlinjesignal" defaultText centerPos 1.0 0 "iso" "_MC" (+ _multipleLines_ _lockPosition_))
)



(defun drawLHs ( posX posY / side s2 centerPos defaultSpeed )
	; Signal 68E 'Lysende hastighetssignal'
	(setq 
		side 3.0
		s2 (/ side 2)
		centerPos (list posX (+ posY s2))
		defaultSpeed "6/7"
	)
	(command "._RECTANGLE" (list (+ posX (- s2)) posY) (list (+ posX s2) (+ posY side)))
	(addAtt "LHS" "Lysende hastighetssignal" defaultSpeed centerPos 1.0 0 "iso" "_MC" (+ _multipleLines_ _lockPosition_))
)



(defun drawZs ( posX posY / side y1 y2 )
	; Signal 41/42 'Høyt skiftesignal'
	(setq
        x  3.0
		y  4.5
		y1 1.825
		y2 2.175
  	)
	(command 
		"._RECTANGLE" (list (+ posX (/ x -2)) posY) (list (+ posX (/ x 2)) (+ posY y)) ; Signal head rectangle, bottom line center in origo
		"._RECTANGLE" (list (+ posX (/ x -2)) (+ posY y1)) (list (+ posX (/ x 2)) (+ posY y2))  ; Area to hatch across signal head
	)
	(drawHatch _denseHatch_)
)



(defun drawMKs ( posX posY / side pt1 pt2 pt3 x y pt4 )
	; Signal 4C 'Middelkontrolllampe'
	(setq r (getMediumLanternRadius))
	(command "._CIRCLE" (list posX posY) r)
)



(defun drawDs ( / side pt1 pt2 pt3 x y pt4 )
	; Signal 43/44/45/46 'Dvergsignal'
	(setq
		side 4.0
		pt1 (list (- (/ side 2)) side) 	; upper left corner of centered Ds head
		pt2 (list (- (/ side 2)) 0) 	; lower left corner
		pt3 (list (+ (/ side 2)) 0) 	; lower right corner
		x (* side (cos (/ pi 4))) ; 45 deg
		y (* side (sin (/ pi 4))) ; 45 deg
		pt4 (list (- x (/ side 2)) y) ; diagonal point
	)
	(command ; Draw Ds head
		"._LINE" pt1 pt2 pt3 ""
		"._LINE" pt2 pt4 ""
		"._ARC" "C" pt2 pt3 "Angle" "90"
	)
)



(defun drawDsMKs ( / side pt1 pt2 pt3 x y pt4 r )
	(setq
		side 4.0
		r (getMediumLanternRadius)
		x (+ (/ side -2) (/ side (sqrt 2)) (/ r (sqrt 2.0))) 	; x = -s/2 + s/sqrt2 + r/sqrt2
		y (+ (/ side (sqrt 2.0)) (/ r (sqrt 2.0))) 				; y =        s/sqrt2 + r/sqrt2
	)
	(drawDs)
	(drawMKs x y) ; Lantern is now touching the dwarf's arc along the 45 deg diagonal
)
