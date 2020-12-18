;
; Isolert skjoet.lsp
;
; NB! Kan ikke ha æøå i filnavn som skal loades av LISP.
;
(defun C:ISOLERT-SKJOET (/) ; 1-strek isolert skjøt symboler
	(drawSkjoet 0 0 0 0)
	(drawSkjoet 0 0 0 1)
	(drawSkjoet 0 0 1 0)
	(drawSkjoet 0 0 1 1)
	(drawSkjoet 0 1 0 0)
	(drawSkjoet 0 1 0 1)
	(drawSkjoet 0 1 1 0)
	(drawSkjoet 0 1 1 1)
	(drawSkjoet 1 0 0 0)
	(drawSkjoet 1 0 0 1)
	(drawSkjoet 1 0 1 0)
	(drawSkjoet 1 0 1 1)
	(drawSkjoet 1 1 0 0)
	(drawSkjoet 1 1 0 1)
	(drawSkjoet 1 1 1 0)
	(drawSkjoet 1 1 1 1)
)

(defun drawSkjoet (q4 q3 q2 q1 / blockName sideLength)
	(setq sideLength 1.5)

	(if (and (= q4 0) (= q3 0) (= q2 0) (= q1 0))  ; q1 = Quadrant I, q2 = Quadrant II, q3 = Quadrant III, q4 = Quadrant IV (direction for signal current)
		; Sidestilt isolert skjøt symbol (innsettingspunktet snapper til en av skinnestrengene):  
		(progn
			(setq blockName "NO-BN-2D-JBTSI-TOGDETEKSJON-EN-STREK-ISOLERT-SKJOET")
			(command "._LINE" (list 0 (- sideLength)) (list 0 sideLength) ""
				"._MOVE" "ALL" "" (list 0 0) (list 0 (- 0.75))
				)
			(newBlock blockName)
		)
		; else: Sidestilt isolert skjøt symbol med angivelse av retning for signalstrømmen
		(progn
			(setq blockName (strcat "NO-BN-2D-JBTSI-TOGDETEKSJON-EN-STREK-ISOLERT-SKJOET-" (rtos q4 2 0) "-" (rtos q3 2 0) "-" (rtos q2 2 0) "-" (rtos q1 2 0)))
			(if (= q1 1)
				(command "._LINE" (list 0 sideLength) (list sideLength sideLength) "")
			)
			(if (= q2 1)
				(command "._LINE" (list 0 sideLength) (list (- sideLength) sideLength) "")
			)
			(if (= q3 1)
				(command "._LINE" (list 0 (- sideLength)) (list (- sideLength) (- sideLength)) "")
			)
			(if (= q4 1)
				(command "._LINE" (list 0 (- sideLength)) (list sideLength (- sideLength)) "")
			)
			(command "._MOVE" "ALL" "" (list 0 0) (list 0 (- 0.75)))
			(newBlock blockName)
		)
	)
 )
 