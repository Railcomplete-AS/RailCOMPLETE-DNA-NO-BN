;
; oldDrawSignalParts.lsp
;
; 2020-08-02 CLFEY Deprecated code, is now re-written and included in main signal LISP routines.
;
;==========================
; Draw signal lanterns
; (without base or pole)
;==========================
(defun oldDrawFs ( mainPole / r lowerPos upperPos )
	; Signal 23/24/25 = Fs = Forsignal = Distant signal
	(setq
		r (getLargeLanternRadius)
		lowerPos (list (- mainPole (* 3 r)) (- r))
		upperPos (list (- mainPole r) (- r))
	)
	(drawHsLantern lowerPos)
	(drawHsLantern upperPos)
)



(defun oldDrawS68E ( mainPole / r x y pt1 pt2 pt3 )
	; Signal 68E = S68E = Lysende hastighetssignal = (luminous) speed signal
	(setq
		r (getLargeLanternRadius)
    	x (- mainPole (* 6 r))
		y (- (* 2 r))
		pt1 (list x y)
		pt2 (list (+ x (* 2 r)) (+ y (* 2 r)))
		pt3 (list (+ x r) (+ y r))
	)
	(command "._RECTANGLE" pt1 pt2)
	(addAtt "S68E" "Lysende hastighetssignal" 5 pt3 1.5 -90 "iso" "_MC" _lockPosition_)
)

(defun oldDrawS68E2 ( mainPole )
	; 
	(setq
		r (getLargeLanternRadius)
    	x (- mainPole (* 2 r))
		y 0
		pt1 (list x y)
		pt2 (list (+ x (* 2 r)) (* 2 r))
		pt3 (list (+ x r) (+ y r))
	)
	(command "._RECTANGLE" pt1 pt2)
	(addAtt "S68E" "Lysende hastighetssignal" 5 pt3 1.5 -90 "iso" "_MC" _lockPosition_)
)



(defun oldDrawFKs ( mainPole F / r FKsRadius FKsSide FKsDist r x y pt1 pt2 )
	; Signal FKs32 = Forsiktig kjøring = Cautious driving
	(setq r (getLargeLanternRadius))
	(if (= F 1)
		(setq topFKs (list (- mainPole (* 4 r)) (- r))) ; Placed below Fs (the distant signal)
	;else
		(setq topFKs (list mainPole (- r)))
	);endif
	(setq
		FKsRadius 0.3125 ; "Light bulbs"
		FKsSide 3.0 
		FKsDist 1.0
		x (- (car topFKs) (/ FKsSide 2))
		y (car (cdr topFKs))
		pt1 (list (+ x (/ FKsSide 2)) (+ y (/ FKsSide 2)))
		pt2 (list (- x (/ FKsSide 2)) (- y (/ FKsSide 2)))
	)
	(bulbFKs (list x y) FKsRadius)
	(bulbFKs (list (+ x FKsDist) y) FKsRadius)
	(bulbFKs (list (- x FKsDist) y) FKsRadius)
	(bulbFKs (list x (+ y FKsDist)) FKsRadius)
	(bulbFKs (list x (- y FKsDist)) FKsRadius)
	(command "._RECTANGLE" pt1 pt2)
)

(defun bulbFKs ( bulbPos bulbRadius / )
	(command "._CIRCLE" bulbPos bulbRadius)
)



(defun oldDrawHLs ( mainPole / r x y pt1 pt2 pt3 )
	; Signal 35 = HLs = Hovedlinjesignal = Line signal
	; The letter/letters will be added as TextAttribute elements from the RC object's definition in DNA.
	; Version without signal FKs at the same time:
	(setq
		r (getLargeLanternRadius)
    	x (- mainPole (* 6 r))
		y (- (* 2 r))
		pt1 (list x y)
		pt2 (list (+ x (* 2 r)) (+ y (* 2 r)))
		pt3 (list (+ x r) (+ y r))
	)
	(command "._RECTANGLE" pt1 pt2)
	(addAtt "HLS" "Hovedlinjesignal" "A/D" pt3 0.7 -90 "iso" "_MC" _lockPosition_)
	(princ)
)

(defun oldDrawHLs2 ( mainPole )
	; Version with signal FKs at the same time:
	(setq
		r (getLargeLanternRadius)
    	x (- mainPole (* 2 r))
		y 0
		pt1 (list x y)
		pt2 (list (+ x (* 2 r)) (* 2 r))
		pt3 (list (+ x r) (+ y r))
	)
	(command "._RECTANGLE" pt1 pt2)
	(addAtt "HLS" "Hovedlinjesignal" "A/D" pt3 0.7 -90 "iso" "_MC" _lockPosition_)
	(princ)
)



(defun oldDrawMKs ( mainPole / r topFKs pos )
	; Signal 4c = MK = Middelkontroll = Position lamp
	(setq
		r (getLargeLanternRadius)
		topFKs (list (- mainPole (* 4 r)) (- r)) ; Place left and below other signal
		pos (list (- mainPole r) (getMediumLanternRadius))
	)
	(command "._CIRCLE" pos (getMediumLanternRadius))
)



(defun oldDrawDs ( / side poleDs topOfDs pt1 pt2 pt3 xrad yrad pt4)
	; Signal 43/44/45/46 Ds = dvergsignal = (low, dwarf) shunting signal
	; This function requires that there exists a line which the BREAK command can act upon.
	; The function will remove for instance a part of a longer Hs mainPole before drawing Ds signal head.
	(setq
		side 4.0 ; "1_S" schematic scale
		poleDs 3.0 ; standard dist from origo to bottom of dwarf signal head symbol
		topOfDs (+ poleDs (* side (/ (sqrt 3.0) 2))) ; measured along centerline
		pt1 (list poleDs (- (/ side 2)))
		pt2 (list poleDs (/ side 2))
		pt3 (list (+ poleDs side) (/ side 2))
		xrad (* side (cos (* (- 0.25) pi)))
		yrad (* side (sin (* (- 0.25) pi)))
		pt4 (list (+ poleDs xrad) (+ (/ side 2) yrad))
	)
	(command
		"._BREAK" (list poleDs 0) (list topOfDs 0) ; remove part of underlying line, if any is there
		"._LINE" pt1 pt2 pt3 "" ; Add Ds head at standard pos ('mainPole' over base)
		"._LINE" pt2 pt4 "" ; Ds head
		"._ARC" "C" pt2 pt1 pt3 ; Ds head
	)
)



(defun oldDrawDsMKs ( / side poleDs topOfDs MKsRadius )
	; Note: drawLyingPole() must extend the line under the Ds head, in order for the following BREAK command in drawDs() to work properly.
	; Place a MK position lamp up/right on a Ds symbol
	(setq 
		side 4.0 ; "1_S" schematic scale
		poleDs 3.0 ; standard dist from origo to bottom of dwarf signal head symbol
		topOfDs (+ poleDs (* side (/ (sqrt 3.0) 2))) ; measured along centerline
		MKsRadius (getMediumLanternRadius)
	)
	(drawLyingPole poleDs topOfDs)
	(drawDs)
	(command 
		"._CIRCLE" ; place a small circle touching the dwarf's arc along the 45 deg diagonal
			(list 
				(+ poleDs (/ side (sqrt 2.0)) (/ MKsRadius (sqrt 2.0)))		 ; x = p + s/sqrt2 + r/sqrt2
				(- (/ side 2) (+ (/ side (sqrt 2.0)) (/ MKsRadius (sqrt 2.0))))  ; y = s - s/sqrt2 - r/sqrt2
			)
			MKsRadius
	)
)
