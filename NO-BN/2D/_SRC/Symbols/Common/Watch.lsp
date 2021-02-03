;=========================================================================================================================
;
; Watch.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; 'Watch' object symbol - used to display the result of a database search (a function) in a nice frame in CAD system model space.
; The jig symbol features a ring with an 'X', so the user sees a circle during the insertion jig process.
; After clicking the object in place, a permanent WATCH-OBJECT 2D symbol is inserted, without the ring. The ring is added by
; RailCOMPLETE as the generic "SymbolFrame" mechanism, which adapts to the amount of text shown as a text attribute 'X' with the symbol.

(defun C:WATCH ( )
	(WATCH-SYMBOL)
	(WATCH-JIGSYMBOL)
)



(defun WATCH-SYMBOL ( / blockName textHeight )
	(setq 
		blockName "NO-BN-2D-JBTFE-WATCH-SYMBOL"
		textHeight 1.8
	)
	(addAtt "W" "Watch Object" "W" _origo_ textHeight _angleZero_ _rcTextStyle_ _middleCenter_ (+ _multipleLines_ _lockPosition_))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun WATCH-JIGSYMBOL ( / radius blockName textHeight )
	(setq 
		blockName "NO-BN-2D-JBTFE-WATCH-JIGSYMBOL"
		textHeight 1.8
		radius 2.75
	)
	(command _CIRCLE_ _origo_ radius)
	(addAtt "W" "Watch Object" "W" _origo_ textHeight _angleZero_ _rcTextStyle_ _middleCenter_ (+ _multipleLines_ _lockPosition_))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)