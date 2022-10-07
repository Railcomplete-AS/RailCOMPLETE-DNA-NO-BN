;=========================================================================================================================
;
; ANYADM Section.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Generalized section symbol - used to associate an identifier (a name, a type etc) to a connected part of an aligment graph.
; The jig symbol features a ring with an 'X', so the user sees a circle during the insertion jig process.
; After clicking the object in place, a permanent SECTION 2D symbol is inserted, without the ring. The ring is added by
; RailCOMPLETE as the generic "SymbolFrame" mechanism, which adapts to the amount of text shown as a text attribute 'X' with the symbol.
;
; Note: Each 'Section' element of the DNA should have an associated 'Section Delimiter' element
; Example 1: Axle counters are delimiters, whereas axle counter section elements give names to the track vacancy detection sections.
; Example 2: Isolated joints are delimiters, associated with track circuits.
; Example 3: "Track Usage Border" elements are delimiters, associated with "Track Usage Section" elements, identifying parts of the track
; network as e.g. 'Shunting area', 'Parking area', 'Platform area' etc. The "RC-ShowSection" function assigns colors from a color palette
; to the selected sections when you select one or more section elements (declared as <ObjectType DataType="RailCOMPLETESection" Class="RailwayPlacedObject" etc).

; Note: Section symbols do not feature a 'description' (symbol name), since they will be used in many context for different purposes.


(defun SECTIONS ( )
	(SECTION-JIGSYMBOL)
	(SECTION-SYMBOL)
)



(defun SECTION-JIGSYMBOL ( / blockName att r )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _RC_ "" "SECTION-INSERTION-SYMBOL"	)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _RC_ "" "SEKSJON-INNSETTING-SYMBOL"	)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _RC_ "" "SECTION-SYMBOLE-INSERTION"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _RC_ "" "ABSCHNITT-EINFUEGE-SYMBOL"	)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _RC_ "" "SECTION-INSERTION-SYMBOL"	)))
	)
	; (Jig symbols are just temporary and don't feature a description.)
	(cond 
		((= _ADM_ _XXGL_) (setq att '("X" "Section Object" "X"	)))
		((= _ADM_ _NOBN_) (setq att '("X" "Seksjonsobjekt" "X"	)))
		((= _ADM_ _FRSR_) (setq att '("X" "Objet section" "X"	)))
		((= _ADM_ _DEDB_) (setq att '("X" "Abschnittsobjekt" "X"	)))
		((= _ADM_ _JPTX_) (setq att '("X" "Section Object" "X"	)))
	)
	(setq
		r 2.75
	)
	(command _CIRCLE_ _origin_ r)
	(AddTextAttributeAtPos layDef_Zero _th180_ _origin_ att)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	; No need for annotative version, DNA doesn't use it anyway
)



(defun SECTION-SYMBOL ( / blockName description att )
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _COM_ "SEC-" "SECTION"	)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _COM_ "SEK-" "SEKSJON"	)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _COM_ "SEC-" "SECTION"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _COM_ "ABS-" "ABSCHNITT"	)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _COM_ "SEC-" "SECTION"	)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "SECTION"	)))
		((= _ADM_ _NOBN_) (setq description (strcat "SEKSJON"	)))
		((= _ADM_ _FRSR_) (setq description (strcat "SECTION"	)))
		((= _ADM_ _DEDB_) (setq description (strcat "ABSCHNITT"	)))
		((= _ADM_ _JPTX_) (setq description (strcat "SECTION"	)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq att '("X" "Section Object" "X"	)))
		((= _ADM_ _NOBN_) (setq att '("X" "Seksjonsobjekt" "X"	)))
		((= _ADM_ _FRSR_) (setq att '("X" "Objet section" "X"	)))
		((= _ADM_ _DEDB_) (setq att '("X" "Abscnitt-Objekt" "X"	)))
		((= _ADM_ _JPTX_) (setq att '("X" "Section Object" "X"	)))
	)

	;(AddMultilineTextAttributeAtPos layDef_Zero _th180_ _origin_ att)
	; NB! We had trouble with scaling of section objects : Adding in scale 1:1, then changing to 
	; m:1, then changing back to 1:1 - and then the text (but not the frame) is 'stuck' at m:1 
	; scale.... And if I first change to m:1, then insert the very first section element, then the 
	; text will then be drawn in scale 1:1 (but the circle around is m_1). Weird!!!!!

	(AddTextAttributeAtPos layDef_Zero _th180_ _origin_ att)
	(AddDescriptionBelowOrigin description _one_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
