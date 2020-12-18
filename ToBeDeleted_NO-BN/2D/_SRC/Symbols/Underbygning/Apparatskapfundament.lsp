;
; Apparatskapfundament.lsp
;
(defun C:APPARATSKAP-FUNDAMENT ()
	(newLayer "JBTUB__FE_FUN_TYPENAVN" 62 "Fundamenttype")
	(newLayer "JBTUB__FE_FUN_LOKALISERING" 62 "Fundamentlokalisering")
	(BETONG-AS-FUNDAMENT "LANGSIDE")
	(BETONG-AS-FUNDAMENT "KORTSIDE")
)



(defun BETONG-AS-FUNDAMENT (spec / blockName s1 s2 h1 h2 len)
	(setq
		blockName (strcat "NO-BN-2D-JBTUB-FUNDAMENT-BETONG-APPARATSKAP-" spec)
		s1 (/ 1.4 2)
		s2 (/ 0.82 2)
		h1 (/ 0.76 2)
		h2 (/ 0.38 2)
	)
	(command
		"._RECTANGLE" (list (- s1) (- s2)) (list s1 s2)
		"._RECTANGLE" (list (- h1) (- h2)) (list h1 h2)
	)
	(if (= spec "LANGSIDE")
		(progn
			(setq len (/ (- s2 h2) 5))
				(command 
				"._LINE" (list h1 (- h2))
					(strcat "@0," (rtos (- len)))
					""
				"._LINE" (list h1 (- (- h2) (* 2 len)))
					(strcat "@0," (rtos (- len)))
					""
				"._LINE" (list h1 (- (- h2) (* 4 len)))
					(strcat "@0," (rtos (- len)))
					""
				"._LINE" (list (- h1) (- h2))
					(strcat "@0," (rtos (- len)))
					""
				"._LINE" (list (- h1) (- (- h2) (* 2 len)))
					(strcat "@0," (rtos (- len)))
					""
				"._LINE" (list (- h1) (- (- h2) (* 4 len)))
					(strcat "@0," (rtos (- len)))
					""
			)
			(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
			(addMText "BETONG APPARATSKAP - FUNDAMENT LANGSIDE 1400x820x540"
				"0,-0.5982" 0.18 2.8 0 "iso" "TC")
			(drawFoundationLocator "S" 2.5)
			(newBlock blockName)
		);else
		(progn
			(setq len (/ (- s1 h1) 5))
			(command
				"._LINE" (list h1 (- h2))
					(strcat "@" (rtos len) ",0")
					""
				"._LINE" (list (+ h1 (* 2 len)) (- h2))
					(strcat "@" (rtos len) ",0")
					""
				"._LINE" (list (+ h1 (* 4 len)) (- h2))
					(strcat "@" (rtos len) ",0")
					""
				"._LINE" (list h1 h2)
					(strcat "@" (rtos len) ",0")
					""
				"._LINE" (list (+ h1 (* 2 len)) h2)
					(strcat "@" (rtos len) ",0")
					""
				"._LINE" (list (+ h1 (* 4 len)) h2)
					(strcat "@" (rtos len) ",0")
					""
			)
			(command "._ROTATE" "ALL" "" "0,0" -90)
			(command "._LAYER" "SET" "JBTUB__FE_FUN_TYPENAVN" "" "._COLOR" "ByLayer")
			(addMText "BETONG APPARATSKAP - FUNDAMENT KORTSIDE 1400x820x540"
				"0,-1.0" 0.18 2.8 0 "iso" "TC")
			(drawFoundationLocator "S" 2.5)
			(newBlock blockName)
		)
	)
)




