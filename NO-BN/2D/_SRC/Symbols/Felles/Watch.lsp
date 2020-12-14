;
; Watch.lsp
;
; 'Watch' object symbol - used to display the result of a database search (a function) in a nice frame in CAD system model space.
; The jig symbol features a ring with an 'X', so the user sees a circle during the insertion jig process.
; After clicking the object in place, a permanent WATCH-OBJECT 2D symbol is inserted, without the ring. The ring is added by
; RailCOMPLETE as the generic "SymbolFrame" mechanism, which adapts to the amount of text shown as a text attribute 'X' with the symbol.
;
(defun C:WATCH (/)
	(WATCH-SYMBOL)
	(WATCH-JIGSYMBOL)
)



(defun WATCH-SYMBOL (/ blockName textHeight)
	(setq 
		blockName "NO-BN-2D-JBTFE-WATCH-SYMBOL"
		textHeight 1.8
	)
	(addAtt "W" "Watch Object" "W" (list 0 0) textHeight 0 "iso" "MC" 48)
	(newBlock blockName)
	blockName
	)

(defun WATCH-JIGSYMBOL (/ radius blockName textHeight)
	(setq 
		blockName "NO-BN-2D-JBTFE-WATCH-JIGSYMBOL"
		textHeight 1.8
		radius 2.75
	)
	(command "._CIRCLE" (list 0 0) radius)
	(addAtt "W" "Watch Object" "W" (list 0 0) textHeight 0 "iso" "MC" 48)
	(newBlock blockName)
	blockName
)