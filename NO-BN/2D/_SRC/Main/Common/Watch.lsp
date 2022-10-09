;=========================================================================================================================
;
; Watch.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; 'Watch' object symbol - used to display the result of a database search (a function) in a nice frame in CAD system model space.
; The jig symbol features a ring with an 'X', so the user sees a circle during the insertion jig process.
; After clicking the object in place, a permanent WATCH-OBJECT 2D symbol is inserted, without the ring. The ring is added by
; RailCOMPLETE as the generic "SymbolFrame" mechanism, which adapts to the amount of text shown as a text attribute 'X' with the symbol.

(defun WATCHES ( )
	(WATCH-JIGSYMBOL)
	(WATCH-SYMBOL)
)



(defun WATCH-JIGSYMBOL ( / blockName att r )
	(setq blockName (strcat _RC_ "" "WATCH-INNSETTING-SYMBOL"	))
	; (Jig symbols are just temporary and don't feature a description.)
	(setq att '("W" "Watch-objekt"	"X"	))
	(setq 
		r 2.75
	)
	(command _CIRCLE_ _origin_ r)
	(AddTextAttributeAtPos layDef_Zero _th180_ _origin_ att)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun WATCH-SYMBOL ( / blockName description att )
	(setq blockName (strcat _COM_ "WCH-" "WATCH"	))
	(setq description (strcat "WATCH"				))
	(setq att '("W" "Watch-objekt"	"W"				))

	(AddMultilineTextAttributeAtPos layDef_Zero _th180_ _origin_ att)
	(AddDescriptionBelowOrigin description _one_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
