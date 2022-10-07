;=========================================================================================================================
;
; ANYADM Annotation text.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Clearly readable symbol in addition to an annotation text's insertion point, to locate it easily in a 2D model.
; The graphics should be switched on/off using RC-ShowLayers. 

(defun ANNOTATION-TEXTS ( / blockName description p1 p2 p3 p4 p5 p6 p7 p8 )
	;
	;   4---------5
	;   |         |
	;   3--2   7--6
	;      |   |
	;      |   |
	;      |   |
	;      1-.-8
	;
	(setq
		p1	'(-0.5 0)
		p2	'(-0.5 2)
		p3	'(-1.5 2)
		p4	'(-1.5 3)
		p5	'( 1.5 3)
		p6	'( 1.5 2)
		p7	'( 0.5 2)
		p8	'( 0.5 0)
	)
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _COM_ "TXT-" "ANNOTATION-TEXT"	)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _COM_ "TXT-" "ANNOTERINGSTEKST"	)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _COM_ "TXT-" "TEXTE-ANNOTATIF"	)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _COM_ "TXT-" "MARKIERUNGSTEXT"	)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _COM_ "TXT-" "ANNOTATION-TEXT"	)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "ANNOTATION-TEXT"	)))
		((= _ADM_ _NOBN_) (setq description (strcat "ANNOTERINGSTEKST"	)))
		((= _ADM_ _FRSR_) (setq description (strcat "TEXTE-ANNOTATIF"	)))
		((= _ADM_ _DEDB_) (setq description (strcat "MARKIERUNGSTEXT"	)))
		((= _ADM_ _JPTX_) (setq description (strcat "ANNOTATION-TEXT"	)))
	)
	(SetLayer layDef_AnnotationTextLocator)
	(command _POLYLINE_ p1 p2 p3 p4 p5 p6 p7 p8 _closedPolyline_)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
