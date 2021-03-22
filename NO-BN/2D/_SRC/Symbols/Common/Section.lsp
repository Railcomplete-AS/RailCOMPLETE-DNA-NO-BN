;=========================================================================================================================
;
; Section.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
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


(defun C:SECTION ( )
	(SECTION-SYMBOL)
	(SECTION-JIGSYMBOL)
)



(defun SECTION-SYMBOL ( / blockName description att )
	(setq
		blockName "NO-BN-2D-JBTFE-SECTION-SYMBOL"
		description "SECTION"
		att '("X" "Section Object" "X")
	)
	(AddTextAttributeAtPos layDef_Zero _th180_ _origo_ att)
	(AddDescriptionBelowOrigo description _one_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun SECTION-JIGSYMBOL ( / blockName att r )
	(setq
		blockName "NO-BN-2D-JBTFE-SECTION-JIGSYMBOL"
		att '("X" "Section Object" "X")
		r 2.75
	)
	(command _CIRCLE_ _origo_ r)
	(AddTextAttributeAtPos layDef_Zero _th180_ _origo_ att)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	; No need for annotative version, DNA doesn't use it anyway
)
