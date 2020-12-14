;
; VEISIGNAL.lsp
;
; Veisignalene er ikke reglet i TRV for jernbane ()SJT TFF), men i Veitrafikkens regelverk.
;
(defun C:VEISIGNAL ()
	(newBlock (VEISIGNAL-1)) ; Enkelt signal mot vei
	(newBlock (VEISIGNAL-2)) ; Dobbelt signal mot vei
)

(defun VEISIGNAL-1 (/ blockName topOfPole ang len r)
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-VEISIGNAL-1"
		topOfPole 4.0
		ang (D->R 26.5651)
		len 4.4721
		r (getSradius)
	)
	(drawBase)
	(drawPole topOfPole 0)
	(command "._ROTATE" "ALL" "" "0,0" "90")
	(drawLantern (list 0 (+ topOfPole r)))
	(drawLantern (list 0 (+ topOfPole (* 3 r))))
	(command 
		"._LINE"
			(polar (list 0 (+ topOfPole (* 2 r))) ang (/ len 2))
			(polar (list 0 (+ topOfPole (* 2 r))) ang (/ len -2))
			""
		"._LINE"
			(polar (list 0 (+ topOfPole (* 2 r))) (- pi ang) (/ len 2))
			(polar (list 0 (+ topOfPole (* 2 r))) (- pi ang) (/ len -2))
			""
	)
	blockName
)



(defun VEISIGNAL-2 (/ blockName arm X Y left right lower upper)
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-VEISIGNAL-2"
		arm 2.5      ; Lateral displacement of lanterns from main axis
		r (getSradius)
		Y (+ 2.5 1.5 (* 2 r))	; Center of cross
		left (* -2 r)
		right (* 2 r)
		lower (- Y r)   ; Upper end of crosses
		upper (+ Y r)    ; Lower end of crosses
	)
	(drawTogsporSignal-2 nil) ; Dobbelt Veibomsignal har samme symbol som 2-begreps Togsporsignal på egen mast, pluss et kryss mellom lanternene
	(command 
		"._LINE" (list (+ (- arm) left) lower) (list (+ (- arm) right) upper) ""
		"._LINE" (list (+ (- arm) left) upper) (list (+ (- arm) right) lower) ""
		"._LINE" (list (+ arm left) lower) (list (+ arm right) upper) ""
		"._LINE" (list (+ arm left) upper) (list (+ arm right) lower) ""
	)
	blockName
)
