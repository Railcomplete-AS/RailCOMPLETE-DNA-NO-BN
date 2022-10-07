;=========================================================================================================================
;
; DE-DB Vorsignalbake.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================



(defun DEDB-VORSIGNALBAKEN ( / blockName description p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 thisBlockName thisDescription)
  ; TODO: Draw just one line at a time and MoveUp and MoveDown, add DrawBox rectangle, MoveUp and end with DEDB_DrawShortSupportForBoard.
  ; DEDB: VORSIGNALBAKE
  ;
  ; 6-----7
  ; |     |
  ; |   / 23
  ; 22 /  |
  ; |   / 19
  ; 18 /  |
  ; |   / 15
  ; 14 /  |
  ; |   / 11
  ; 10 /  |
  ; |   / 9
  ; 8  /  |
  ; |   / 13
  ; 12 /  |
  ; |   / 17
  ; 16 /  |
  ; |   / 21
  ; 20 /  |
  ; |   / 25
  ; 24 /  |
  ; |     |
  ; 3--4--5   
  ;    |
  ;  1-.-2
  
	(setq 
		blockName 	(strcat _SIG_ "VSB-" 	"VORSIGNALBAKE"		)
		description (strcat 				"VORSIGNALBAKE"		)
		p1 (list -0.750 0.000)
		p2 (list  0.750 0.000)
		p3 (list -1.000 1.000)
		p4 (list  0.000 1.000)
		p5 (list  1.000 1.000)
		p6 (list -1.000 9.000)
		p7 (list  1.000 9.000)
		p8 (list -1.000 4.000)
		p9 (list  1.000 6.000)
		p10 (list -1.000 4.650)
		p11 (list  1.000 6.650)
		p12 (list -1.000 3.350)
		p13 (list  1.000 5.350) 
		p14 (list -1.000 5.300)
		p15 (list  1.000 7.300)
		p16 (list -1.000 2.700)
		p17 (list  1.000 4.700)
		p18 (list -1.000 5.950)
		p19 (list  1.000 7.950)
		p20 (list -1.000 2.050)
		p21 (list  1.000 4.050)
		p22 (list -1.000 6.600)
		p23 (list  1.000 8.600)
		p24 (list -1.000 1.400)
		p25 (list  1.000 3.400)
	)
  
	(defun localGraphics (/)
		(DrawLine layDef_Zero p1 p2)
		(DrawLine layDef_Zero _origin_ p4)
		(DrawLine layDef_Zero p3 p5)
		(DrawLine layDef_Zero p3 p6)
		(DrawLine layDef_Zero p5 p7)
		(DrawLine layDef_Zero p6 p7)
	)
  
	;1
	(setq thisBlockName(strcat blockName "-1"))
	(setq thisDescription (strcat description " 1"))
	(DrawLine layDef_Zero p8 p9)
	(localGraphics)
	(AddDescriptionBelowOrigin thisDescription 0) 
	(CreateSchematicBlockFromCurrentGraphics thisBlockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)

  
	;2
	(setq thisBlockName (strcat blockName "-2"))
	(setq thisDescription (strcat description " 2"))
	(DrawLine layDef_Zero p10 p11)
	(DrawLine layDef_Zero p12 p13)
	(localGraphics)  
	(AddDescriptionBelowOrigin thisDescription 0) 
	(CreateSchematicBlockFromCurrentGraphics thisBlockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
  
	;3
	(setq thisBlockName (strcat blockName "-3"))
	(setq thisDescription (strcat description " 3"))
	(DrawLine layDef_Zero p8 p9)
	(DrawLine layDef_Zero p14 p15)
	(DrawLine layDef_Zero p16 p17)
	(localGraphics)  
	(AddDescriptionBelowOrigin thisDescription 0) 
	(CreateSchematicBlockFromCurrentGraphics thisBlockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)

	;4
	(setq thisBlockName (strcat blockName "-4"))
	(setq thisDescription (strcat description " 4"))
	(DrawLine layDef_Zero p10 p11)
	(DrawLine layDef_Zero p12 p13)
	(DrawLine layDef_Zero p18 p19)
	(DrawLine layDef_Zero p20 p21)
	(localGraphics)  
	(AddDescriptionBelowOrigin thisDescription 0) 
	(CreateSchematicBlockFromCurrentGraphics thisBlockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
  
	;5
	(setq thisBlockName (strcat blockName "-5"))
	(setq thisDescription (strcat description " 5"))
	(DrawLine layDef_Zero p8 p9)
	(DrawLine layDef_Zero p14 p15)
	(DrawLine layDef_Zero p16 p17)
	(DrawLine layDef_Zero p22 p23)
	(DrawLine layDef_Zero p24 p25) 
	(localGraphics)  
	(AddDescriptionBelowOrigin thisDescription 0) 
	(CreateSchematicBlockFromCurrentGraphics thisBlockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
)
