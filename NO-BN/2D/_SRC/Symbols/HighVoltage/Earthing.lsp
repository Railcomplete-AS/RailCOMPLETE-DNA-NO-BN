;=========================================================================================================================
;
; Earthing.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Earthing (general symbol, earthing busbar)

(defun C:EARTHING ( / )
	(JORDPOTENSIAL)
	(JORDINGSSKINNE)
)



(defun JORDPOTENSIAL ( / blockName description p0 p1 p2 p3 p4 p5 p6 p7 )
	; Generelt jordsymbol - kan benyttes som jordpotensial-markør, eller som kråkefotsymbol osv.
	; Ref. TRV KL skjemasymboler.
	;
	;      .
	;      |
	;      |
	; 1----0----2
	;   3-----4
	;     5,6
	;     (7)
	
	(setq
		blockName "NO-BN-2D-JBTKL-JORDING-JORDPOTENSIAL"
		description "JORDPOTENSIAL"
		p0 (list  0.0 -3.0)
		p1 (list -2.1 -3.0)
		p2 (list  2.1 -3.0)
		p3 (list -1.4 -3.7)
		p4 (list  1.4 -3.7)
		p5 (list -0.5 -4.4)
		p6 (list  0.5 -4.4)
		p7 (list  0.0 -5.0)
	)
	(DrawLine layDef_Zero _origo_ p0)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p3 p4)
	(DrawLine layDef_Zero p5 p6)
	(AddTextAtPos layDef_Description _descriptionTextHeight_ p7 description)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)


(defun JORDINGSSKINNE ( / blockName description x y d r p1 p2 p3 p4 )
	; For montasje under datagulv i bygninger, på vegg, i MANHOLE med mer. 
	; Benyttes hos Bane NOR i 50x10x290 (9xØ13 pluss et Ø5 hull for merking) eller 50x10x530 (17xØ13 pluss 1xØ5). Galvanisert (GMB utførelse) eller ren kobber (mest vanlig).
	; Monteres 40mm fra underlaget.
	; Se også "magebelte" rundt HEB-master, ref ECT tegning / JEMTLAND (Gjøvik) tegning for HEB260 mast (i RC_3D-katalog for EH-JSK).
	;
	;  TL------------------------------TR
	;  |  (1)   (2)   (.)   (3)   (4)   |  
	;  BL------------------------------BR
	;
	(setq
		blockName "NO-BN-2D-JBTKL-JORDING-JORDINGSSKINNE"
		description "JORDINGSSKINNE"
		x 6.0	; Schematic / annotative symbol measures
		y 2.0
		d 1.0
		r 0.35
		p1 (list (* -2 d) 0)
		p2 (list (* -1 d) 0)
		p3 (list (*  1 d) 0)
		p4 (list (*  2 d) 0)
	)
	
	; Schematic & geo symbols:
	(DrawBox layDef_Zero x y _noWipeout_)
	(DrawCircleAtPos layDef_Zero p1 r _noWipeout_)
	(DrawCircleAtPos layDef_Zero p2 r _noWipeout_)
	(DrawCircleAtPos layDef_Zero _origo_ r _noWipeout_)
	(DrawCircleAtPos layDef_Zero p3 r _noWipeout_)
	(DrawCircleAtPos layDef_Zero p4 r _noWipeout_)
	(AddDescriptionBelowOrigo description (HalfOf y))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
