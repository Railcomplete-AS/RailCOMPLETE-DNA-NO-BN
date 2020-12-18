;
; THUMBNAIL-Bilvei.lsp
;
(defun C:THUMBNAIL-BILVEI (/)
	(BILVEI)
)



(defun BILVEI (/ blockName) 
  (setq blockName "NO-BN-2D-JBTUB-THUMBNAIL-BILVEI")
	(command "._LINE" (list 0 0) (list 0 1) ""
		"._LINE" (list 0 1.4) (list 0 2) ""
		"._LINE" (list 0 1.4) (list 0 2) ""
		"._LINE" (list 0 2.4) (list 0 2.8) ""
		"._LINE" (list 0 3.125) (list 0 3.375) ""
		"._LINE" (list 1.5 0) (list 0.5 3.375) ""
		"._MIRROR" "L" "" (list 0 0) (list 0 1) "NO"
		)
  (newBlock blockName)
  blockName
  )