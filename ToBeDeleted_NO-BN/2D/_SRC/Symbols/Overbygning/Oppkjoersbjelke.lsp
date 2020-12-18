;
; Oppkjoersbjelke.lsp
;
(defun C:OPPKJOERSBJELKE ()
	(OPPKJOERSBJELKE)
)



(defun OPPKJOERSBJELKE (/ blockName fotX fotY bjelkeX bjelkeY)
	(setq
		blockName "NO-BN-2D-JBTOB-SPOROBJEKT-OPPKJOERSBJELKE"
		fotX (/ 0.16 2)
		fotY (/ 0.8 2)
		bjelkeX (/ 1.0 2)
		bjelkeY (/ 0.1 2)
		bjelke2X (* (/ (* 2 bjelkeX) 100) 28)
        distFot (* (/ (* 2 bjelkeX) 10) 3)
	)
	(command
		"._RECTANGLE" (list (- bjelkeX) (- bjelkeY)) (list bjelkeX bjelkeY)
		"._RECTANGLE" (list (- (+ bjelkeX bjelke2X)) (- bjelkeY)) (list (- bjelkeX) bjelkeY)
		"._MIRROR" "L" "" "0,0" "0,1" "N"
		"._RECTANGLE" (list (- (+ distFot fotX)) bjelkeY) (list (- (- distFot fotX)) fotY)
		"._ARRAY" "L" "" "R" "2" "2" (- (+ bjelkeY fotY)) (* 2 distFot)
	)
	(command "._ROTATE" "ALL" "" "0,0" "90")
	(newBlock blockName)
	blockName
)