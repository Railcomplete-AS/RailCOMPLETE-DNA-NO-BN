;=========================================================================================================================
;
; ANYADM Watch.lsp
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
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _RC_ "" "WATCH-INSERTION-SYMBOL"			)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _RC_ "" "WATCH-INNSETTING-SYMBOL"			)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _RC_ "" "SURVEILLANT-SYMBOLE-INSERTION"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _RC_ "" "WATCH-EINFUEGE-SYMBOL"			)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _RC_ "" "WATCH-INSERTION-SYMBOL"			)))
	)
	; (Jig symbols are just temporary and don't feature a description.)
	(cond 
		((= _ADM_ _XXGL_) (setq att '("W" "Watch Object"	"X"	)))
		((= _ADM_ _NOBN_) (setq att '("W" "Watch-objekt"	"X"	)))
		((= _ADM_ _FRSR_) (setq att '("W" "Surveillant"		"X"	)))
		((= _ADM_ _DEDB_) (setq att '("W" "Watch-Objekt"	"X"	)))
		((= _ADM_ _JPTX_) (setq att '("W" "Watch Object"	"X"	)))
	)
	(setq 
		r 2.75
	)
	(command _CIRCLE_ _origin_ r)
	(AddTextAttributeAtPos layDef_Zero _th180_ _origin_ att)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun WATCH-SYMBOL ( / blockName description att )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _COM_ "WCH-" "WATCH"			)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _COM_ "WCH-" "WATCH"			)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _COM_ "SUR-" "SURVEILLANT"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _COM_ "WCH-" "WATCH"			)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _COM_ "WCH-" "WATCH"			)))
	)
	(cond
		((= _ADM_ _XXGL_) (setq description (strcat "WATCH"			)))
		((= _ADM_ _NOBN_) (setq description (strcat "WATCH"			)))
		((= _ADM_ _FRSR_) (setq description (strcat "SURVEILLANT"	)))
		((= _ADM_ _DEDB_) (setq description (strcat "WATCH"			)))
		((= _ADM_ _JPTX_) (setq description (strcat "WATCH"			)))
	)
	(cond
		((= _ADM_ _XXGL_) (setq att '("W" "Watch Object"	"W"	)))
		((= _ADM_ _NOBN_) (setq att '("W" "Watch-objekt"	"W"	)))
		((= _ADM_ _FRSR_) (setq att '("W" "Surveillant"		"W"	)))
		((= _ADM_ _DEDB_) (setq att '("W" "Watch-Objekt"	"W"	)))
		((= _ADM_ _JPTX_) (setq att '("W" "Watch Object"	"W"	)))
	)
	(AddMultilineTextAttributeAtPos layDef_Zero _th180_ _origin_ att)
	(AddDescriptionBelowOrigin description _one_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
