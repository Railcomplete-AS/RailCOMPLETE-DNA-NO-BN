;
; Marker.lsp
;
; Marker for posistions and other stuff which needs a pin-needle symbol and a text in a drawing.
;
(defun C:MARKER (/ blockName description)
	(setq
		blockName "NO-BN-2D-JBTFE-MARKOER-KNAPPENAAL"
		description "Knappenål"
	)
	(command "._CIRCLE" "1,1" "0.4")
	(command "._LINE" "0,0" "0.7563,0.6828" "")
	(command "._LINE" "0,0" "0.6828,0.7563" "")
	(addAtt "FARGE" "Farge" "" "1,2" 0.9 0 "iso" "MC" 16)
	(newBlock blockName)
	description
)