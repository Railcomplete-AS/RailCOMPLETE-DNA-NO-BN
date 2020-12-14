;
; Togsporsignal.lsp
;
; Både frittstående, vegghengt og plassert på annen mast (men som eget objekt)
;

(defun C:SIGNAL-TOGSPORSIGNAL ()
    (newBlock (drawTogsporSignal-1 nil))		; 1-begreps Togsporsignal på egen mast
    (newBlock (drawTogsporSignal-1 "AAK"))		; 1-begreps Togsporsignal i åk
    (newBlock (drawTogsporSignal-2 nil))		; 2-begreps Togsporsignal på egen mast
    (newBlock (drawTogsporSignal-2 "AAK"))		; 2-begreps Togsporsignal i åk
    (newBlock (drawTogsporSignal-3 "VSIDE"))	; 1-begreps Togsporsignal, montert på vegg eller på siden av annet signal, lanterner til venstre for festearm
    (newBlock (drawTogsporSignal-3 "HSIDE")) 	; 1-begreps Togsporsignal, montert på vegg eller på siden av annet signal, lanterner til høyre for festearm
    (newBlock (drawTogsporSignal-4 "VSIDE"))	; 2-begreps Togsporsignal, montert på vegg eller på siden av annet signal, lanterner til venstre for festearm
    (newBlock (drawTogsporSignal-4 "HSIDE"))	; 2-begreps Togsporsignal, montert på vegg eller på siden av annet signal, lanterner til høyre for festearm
)



(defun drawTogsporSignal-1 (feste / blockName pole1 r)
	(setq 
		blockName "NO-BN-2D-JBTSI-SIGNAL-36-TOGSPORSIGNAL-1"
		topOfPole 4.0
		r (getSradius)
	)
	(drawLantern (list r 0))
	(drawLantern (list (* 3 r) 0))
	(if (= feste "AAK")
		(progn
			(setq blockName (strcat blockName "-AAK"))
			(command "._MOVE" "ALL" "" "Displacement" (list (- (* -4 r) 2) 0))
			(drawPole 0 -2)
		)
		(progn
			(command "._MOVE" "ALL" "" "Displacement" (list topOfPole 0))
			(drawBase)
			(drawPole 0 topOfPole)
		)
	)
	(command "._ROTATE" "ALL" "" "0,0" "90")
	blockName
)



(defun drawTogsporSignal-2 (feste / blockName pole1 pole2 r Y)
	(setq 
		blockName "NO-BN-2D-JBTSI-SIGNAL-36-TOGSPORSIGNAL-2"
		pole1 2.5 ; Lower part of mast
		pole2 1.5 ; Higher part (after split) of mast
		r (getSradius)
		Y 2.5  ; Lateral displacement from mast's centerline to lantern's centerline
	)
	(command
		"._LINE" ; right part of split mast
			"0,0"
			(list pole1 0)
			(list pole1 Y)
			(list (+ pole1 pole2) Y)
			""
		"._LINE" ; left part of split mast
			(list pole1 0)
			(list pole1 (- Y))
			(list (+ pole1 pole2) (- Y))
			""
	)
	(drawLantern (list (+ pole1 pole2 r) Y))			; lower right lantern
	(drawLantern (list (+ pole1 pole2 (* 3 r)) Y))		; upper right lantern
	(drawLantern (list (+ pole1 pole2 r) (- Y)))		; lower left lantern
	(drawLantern (list (+ pole1 pole2 (* 3 r)) (- Y)))	; upper left lantern
	(if (= feste "AAK")
		(progn
			(setq blockName (strcat blockName "-AAK"))
			(command "._ROTATE" "ALL" "" "0,0" "-90")
		)
		(progn
			(drawBase)
			(command "._ROTATE" "ALL" "" "0,0" "90")
		)
	)
	blockName
)


(defun drawTogsporSignal-3 (side / blockName r)
	; NB Drawn upright, i.e. not rotated 90 degrees in the end of LISP routine
	(setq 
		blockName (strcat "NO-BN-2D-JBTSI-SIGNAL-36-TOGSPORSIGNAL-3-" side)
		pole1 2.5 ; Horizontal part of 'knee'
		pole2 1.5 ; Vertical part of 'knee'
		r (getSradius)
	)
	(command
		"._LINE"
		"0,0"
		(list pole1 0)
		(list pole1 pole2)
		""
	)
	(drawLantern (list pole1 (+ pole2 r)))		; lower lantern
	(drawLantern (list pole1 (+ pole2 (* 3 r))))	; upper lantern
	(if (= side "VSIDE")	
		(command "._MIRROR" "ALL" "" "0,0" "0,1" "Yes")
	)
	blockName
)


(defun drawTogsporSignal-4 (side / blockName r)
	(setq 
		blockName (strcat "NO-BN-2D-JBTSI-SIGNAL-36-TOGSPORSIGNAL-4-" side)
		pole1 2.5 ; Lower part of mast
		pole2 1.5 ; Higher part (after split) of mast
		pole3 5   ; Horizontal part of 'knee'
		r (getSradius)
		Y 2.5  ; Lateral displacement from mast's centerline to lantern's centerline
	)
	(command
		"._LINE" ; right part of split mast
		"0,0"
		(list pole1 0)
		(list pole1 Y)
		(list (+ pole1 pole2) Y)
		""
		"._LINE" ; left part of split mast
		(list pole1 0)
		(list pole1 (- Y))
		(list (+ pole1 pole2) (- Y))
		""
	)
	(drawLantern (list (+ pole1 pole2 r) Y))				; lower right lantern
	(drawLantern (list (+ pole1 pole2 (* 3 r)) Y))		; upper right lantern
	(drawLantern (list (+ pole1 pole2 r) (- Y)))			; lower left lantern
	(drawLantern (list (+ pole1 pole2 (* 3 r)) (- Y)))	; upper left lantern
	(command "._MOVE" "ALL" "" "Displacement" (list 0 pole3)) ; Move "left"
	(command "._LINE" "0,0" (list 0 pole3) "")  ; Add horizontal arm of "knee", lanterns to the left of knee
	(if (= side "HSIDE")
		(command "._MIRROR" "ALL" "" "0,0" "1,0" "Yes")
	)
	(command "._ROTATE" "ALL" "" "0,0" "90")
	blockName
)
