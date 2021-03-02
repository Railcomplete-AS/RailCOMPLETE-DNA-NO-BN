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



(defun WATCH-SYMBOL ( / blockName att )
	(setq 
		blockName "NO-BN-2D-JBTFE-WATCH-SYMBOL"
		description "WATCH"
		att '("W" "Watch Object" "W")
	)
	(addTextAttributeAtPos layDef_Zero _th180_ _origo_ att)
	(addDescriptionBelowOrigo description _one_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun WATCH-JIGSYMBOL ( / blockName att radius )
	(setq 
		blockName "NO-BN-2D-JBTFE-WATCH-JIGSYMBOL"
		att '("W" "Watch Object" "W")
		radius 2.75
	)
	(command _CIRCLE_ _origo_ radius)
	(addTextAttributeAtPos layDef_Zero _th180_ _origo_ att)
	(createSchematicBlockFromCurrentGraphics blockName)
	; No need for annotative version, DNA doesn't use it anyway
)
