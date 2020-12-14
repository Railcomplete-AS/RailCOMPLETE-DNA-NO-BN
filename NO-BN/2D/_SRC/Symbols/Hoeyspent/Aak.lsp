(defun C:AAK (/)
	(setq counter 10)
	(repeat 34
		(AAK counter)
		(setq counter (+ counter 1))
	)
  	(UTLIGGERAAK)
)

 (defun AAK (len / blockName)
	(setq 
		blockName (strcat "NO-BN-2D-JBTKL-AAK-" (rtos (* len 1000) 2 0))
	)
	(command "._LINE" (list 0 0) (list 0 (- len)) "")
	(newBlock blockName)
	blockName
	)

(defun UTLIGGERAAK (/ len blockName)
	(setq 
		blockName "NO-BN-2D-JBTKL-AAK-UTLIGGERAAK"
		len 6.0
	)
	(command "._LINE" (list 0 0) (list 0 (- len)) "")
	(newBlock blockName)
	blockName
)