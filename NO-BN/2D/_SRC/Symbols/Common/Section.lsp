;=========================================================================================================================
;
; Section.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
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

(defun C:SECTION ( )
	(SECTION-SYMBOL)
	(SECTION-JIGSYMBOL)
)



(defun SECTION-SYMBOL ( / blockName textHeight )
	(setq
		blockName "NO-BN-2D-JBTFE-SECTION-SYMBOL"
		textHeight 1.8
	)
	(addAtt "X" "Section Object" "X" (list 0 0) textHeight 0 "iso" "_MC" (+ _multipleLines_ _lockPosition_))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)



(defun SECTION-JIGSYMBOL ( / blockName textHeight radius )
	(setq
		blockName "NO-BN-2D-JBTFE-SECTION-JIGSYMBOL"
		textHeight 1.8
		radius 2.75
	)
	(command "._CIRCLE" (list 0 0) radius)
	(addAtt "X" "Section Object" "X" (list 0 0) textHeight 0 "iso" "_MC" (+ _multipleLines_ _lockPosition_))
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)
)
