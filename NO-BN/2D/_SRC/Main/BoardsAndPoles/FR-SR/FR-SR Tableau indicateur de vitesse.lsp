;=========================================================================================================================
;
; FR-SR Tableau indicateur de vitesse.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================



(defun FRSR-TABLEAU-INDICATEUR-DE-VITESSE ( / )
	;=========================================================================================================================
	;
	; Réf. OP 00580.
	; Réf. IN 1492 (2005, valable dès 2012) (replaces IN 2488).
	;
	;
	(TraceLevel3 "FRSR-TABLEAU-INDICATEUR-DE-VITESSE-PERMANENT-ORDINAIRE")		(FRSR-TABLEAU-INDICATEUR-DE-VITESSE-PERMANENT-ORDINAIRE)
)



(defun FRSR-TABLEAU-INDICATEUR-DE-VITESSE-PERMANENT-ORDINAIRE ( / blockName description )


	;
	;
	;
	;         (7)
	;          3                 3             +----3----+        +----3----+       +---------+       +---------+     
	;        /   \             /   \           |  /   \  |        |  /   \  |       |         |       |         |     
	;      /       \         /  a2   \         |/       \|        |/  a2   \|       |         |       |   a2    |     
	; a4  1   a1    2 a5 a4 1 4-----5 2 a5  a4 1   a1    2 a5  a4 1 4-----5 2 a5 a4 |   a1    | a5 a4 |q4----q5 | a5   
	;      \       /         \  a3   /         |\       /|        |\  a3   /|       |         |       |   a3    |     
	;        \   /             \   /           |  \   /  |        |  \   /  |       |         |       |         |     
	;          .                 .             +----.----+        +----.----+       +----.----+       +----.----+     
	;         (6)                                  x*y                x*y               qx*qy             qx*qy 
	;          8
	;         _9
	;        (_10) 
	;
	;       LOSANGE          LOSANGE        LOSANGE_CARRE    LOSANVE_CARRE     CARRE             CARRE
	;       UN_NOMBRE        DEUX_NOMBRES   UN_NOMBRE        DEUX_NOMBRES      UN_NOMBRE         DEUX_NOMBRES
	;
	; A noter:  Les points 6 et 7 servent pour les tableaux aux voyants blancs alternés.
	;           Le point 10 = moteur électrique pour tab. mobile commandé électriquement
	;	
	;
	(setq 
		blockName	(strcat _SIG_ "TIV-" "TIV-PERMANENT-ORDINAIRE"	)
		description (strcat "TABLEAU INDICATEUR DE VITESSE PERMANENT ORDINAIRE"		)
		x	8.0	
		y	8.0
		qx	5.0
		qy	5.0
		p0	'( 0.000 -4.000)	; All points to be modified by MoveUp
		p1	'(-4.000  0.000)
		p2	'( 4.000  0.000)
		p3	'( 0.000  4.000)
		p4	'(-3.000  0.000)
		p5	'( 3.000  0.000)
		q4	'(-2.000  0.000)	
		q5	'( 2.000  0.000)

		p6	'( 0.000 -4.500)
		p7	'( 0.000  4.500)
		r1	0.500				; Voyant blancs alternés

		p8	'( 0.000  0.000)	; Avec ou sans voyants blancs, après MoveUp
		p9	'( 0.000 -1.500)	; Avec ou sans voyants blancs, après MoveUp
		p10	'( 0.000 -2.250)	; Avec ou sans voyants blancs, après MoveUp
		r2	0.750				; Moteur électrique

		a1	'( 0.000  0.000)
		a2  '( 0.000  1.250)
		a3  '( 0.000 -1.25)
		a4  '(-5.000  0.000)
		a5  '(5.000  0.000)		
		attVitesse1	'("VITESSE_1" "Vitesse primaire"	"")
		attVitesse2	'("VITESSE_2" "Vitesse secondaire"	"")
		attAnnotationG '("ANNOTATION_G" "Annotation Gauche"	"")	; Position annotation sur a4 au dessus direction Aval 
		attAnnotationD '("ANNOTATION_D" "Annotation Droite"	"")	; Position annotation sur a5 au dessus direction Amont 		
	)
	(SetLayer layDef_Zero)
	(foreach variation '("LOSANGE" "LOSANGE_MOTEUR" "LOSANGE_VOYANTS" "LOSANGE_VOYANTS_MOTEUR" "LOSANGE_CARRE" "CARRE")
		(foreach nombres '("UN_NOMBRE" "DEUX_NOMBRES")
			(setq 
				thisBlockName	(strcat blockName "-" variation "-" nombres)
				thisDescription	(strcat description "-" variation "-" nombres)
			)
			(cond 
				((= variation "LOSANGE")
					(command _POLYLINE_ p0 p1 p3 p2 _closedPolyline_)
					(AddAtt (GetAttributeTag attAnnotationG) (GetAttributePrompt attAnnotationG) (GetAttributeDefaultValue attAnnotationG) a4 _th180_ _angleZero_ _rcTextStyle_ _BottomRight_)
					(AddAtt (GetAttributeTag attAnnotationD) (GetAttributePrompt attAnnotationD) (GetAttributeDefaultValue attAnnotationD) a5 _th180_ _angleZero_ _rcTextStyle_ _BottomLeft_)					
					(cond 
						((= nombres "UN_NOMBRE")
							(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a1 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
						((= nombres "DEUX_NOMBRES")
							(DrawLine layDef_Zero p4 p5)
							(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a2 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
							(AddAtt (GetAttributeTag attVitesse2) (GetAttributePrompt attVitesse2) (GetAttributeDefaultValue attVitesse2) a3 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
					)
					(MoveUp (HalfOf y))
				)
				((= variation "LOSANGE_MOTEUR")
					(command _POLYLINE_ p0 p1 p3 p2 _closedPolyline_)
					(AddAtt (GetAttributeTag attAnnotationG) (GetAttributePrompt attAnnotationG) (GetAttributeDefaultValue attAnnotationG) a4 _th180_ _angleZero_ _rcTextStyle_ _BottomRight_)
					(AddAtt (GetAttributeTag attAnnotationD) (GetAttributePrompt attAnnotationD) (GetAttributeDefaultValue attAnnotationD) a5 _th180_ _angleZero_ _rcTextStyle_ _BottomLeft_)						
					(cond 
						((= nombres "UN_NOMBRE")
							(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a1 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
						((= nombres "DEUX_NOMBRES")
							(DrawLine layDef_Zero p4 p5)
							(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a2 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
							(AddAtt (GetAttributeTag attVitesse2) (GetAttributePrompt attVitesse2) (GetAttributeDefaultValue attVitesse2) a3 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
					)
					(MoveUp (HalfOf y))
					(DrawLine layDef_Zero _origin_ p9)
					(DrawCircleAtPos layDef_Zero p10 r2 _noWipeout_)
					(MoveDown (yCoord p10)) ; p10.Y negative
					(MoveUp r2)
				)
				((= variation "LOSANGE_VOYANTS")
					(command _POLYLINE_ p0 p1 p3 p2 _closedPolyline_)
					(AddAtt (GetAttributeTag attAnnotationG) (GetAttributePrompt attAnnotationG) (GetAttributeDefaultValue attAnnotationG) a4 _th180_ _angleZero_ _rcTextStyle_ _BottomRight_)
					(AddAtt (GetAttributeTag attAnnotationD) (GetAttributePrompt attAnnotationD) (GetAttributeDefaultValue attAnnotationD) a5 _th180_ _angleZero_ _rcTextStyle_ _BottomLeft_)						
					(cond 
						((= nombres "UN_NOMBRE")
							(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a1 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
						((= nombres "DEUX_NOMBRES")
							(DrawLine layDef_Zero p4 p5)
							(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a2 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
							(AddAtt (GetAttributeTag attVitesse2) (GetAttributePrompt attVitesse2) (GetAttributeDefaultValue attVitesse2) a3 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
					)
					(DrawCircleAtPos layDef_Zero p6 r1 _noWipeout_)
					(DrawCircleAtPos layDef_Zero p7 r1 _noWipeout_)
					(MoveUp (HalfOf y))
				)
				((= variation "LOSANGE_VOYANTS_MOTEUR")
					(command _POLYLINE_ p0 p1 p3 p2 _closedPolyline_)
					(AddAtt (GetAttributeTag attAnnotationG) (GetAttributePrompt attAnnotationG) (GetAttributeDefaultValue attAnnotationG) a4 _th180_ _angleZero_ _rcTextStyle_ _BottomRight_)
					(AddAtt (GetAttributeTag attAnnotationD) (GetAttributePrompt attAnnotationD) (GetAttributeDefaultValue attAnnotationD) a5 _th180_ _angleZero_ _rcTextStyle_ _BottomLeft_)						
					(cond 
						((= nombres "UN_NOMBRE")
							(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a1 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
						((= nombres "DEUX_NOMBRES")
							(DrawLine layDef_Zero p4 p5)
							(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a2 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
							(AddAtt (GetAttributeTag attVitesse2) (GetAttributePrompt attVitesse2) (GetAttributeDefaultValue attVitesse2) a3 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
					)
					(DrawCircleAtPos layDef_Zero p6 r1 _noWipeout_)
					(DrawCircleAtPos layDef_Zero p7 r1 _noWipeout_)
					(MoveUp (HalfOf y))
					(MoveUp r1)
					(MoveUp r1)

					(DrawLine layDef_Zero _origin_ p9)
					(DrawCircleAtPos layDef_Zero p10 r2 _noWipeout_)
					(MoveDown (yCoord p9)) ; p9.Y negative
					(MoveUp r2)
					(MoveUp r2)
				)
				((= variation "LOSANGE_CARRE")
					(command _POLYLINE_ p0 p1 p3 p2 _closedPolyline_)
					(DrawBox layDef_Zero x y _noWipeout_)
					(AddAtt (GetAttributeTag attAnnotationG) (GetAttributePrompt attAnnotationG) (GetAttributeDefaultValue attAnnotationG) a4 _th180_ _angleZero_ _rcTextStyle_ _BottomRight_)
					(AddAtt (GetAttributeTag attAnnotationD) (GetAttributePrompt attAnnotationD) (GetAttributeDefaultValue attAnnotationD) a5 _th180_ _angleZero_ _rcTextStyle_ _BottomLeft_)						
					(cond 
						((= nombres "UN_NOMBRE")
							(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a1 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
						((= nombres "DEUX_NOMBRES")
							(DrawLine layDef_Zero p4 p5)
							(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a2 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
							(AddAtt (GetAttributeTag attVitesse2) (GetAttributePrompt attVitesse2) (GetAttributeDefaultValue attVitesse2) a3 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
					)
					(MoveUp (HalfOf y))
				)
				((= variation "CARRE")
					(DrawBox layDef_Zero qx qy _noWipeout_)
					(AddAtt (GetAttributeTag attAnnotationG) (GetAttributePrompt attAnnotationG) (GetAttributeDefaultValue attAnnotationG) a4 _th180_ _angleZero_ _rcTextStyle_ _BottomRight_)
					(AddAtt (GetAttributeTag attAnnotationD) (GetAttributePrompt attAnnotationD) (GetAttributeDefaultValue attAnnotationD) a5 _th180_ _angleZero_ _rcTextStyle_ _BottomLeft_)						
					(cond 
						((= nombres "UN_NOMBRE")
							(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a1 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
						((= nombres "DEUX_NOMBRES")
							(DrawLine layDef_Zero q4 q5)
							(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a2 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
							(AddAtt (GetAttributeTag attVitesse2) (GetAttributePrompt attVitesse2) (GetAttributeDefaultValue attVitesse2) a3 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
					)
					(MoveUp (HalfOf qy))
				)
			)	
			(AddDescriptionBelowOrigin thisDescription 0)
			(CreateSchematicBlockFromCurrentGraphics thisBlockName)
			(AddGraphicsFromScaledSchematicBlock thisBlockName _one_)
			(CreateAnnotativeBlockFromCurrentGraphics thisBlockName)
		);foreach
	);foreach

	;     2---------3      2---------3
	;     |         |      |    a2   |
	;   a4|    a1   |a5  a4| 6-----7 |a5
	;     1         4      1    a3   4
	;      \       /        \       /
	;       \__0__/          \__0__/
	;
	;     TYPE_B              TYPE_B
	;     UN_NOMBRE           DEUX_NOMBRES
	;
	
	(setq 
		blockName	(strcat _SIG_ "TIV-" "TIV-PERMANENT-ORDINAIRE-TYPE_B"	)
		description (strcat "TABLEAU INDICATEUR DE VITESSE PERMANENT ORDINAIRE TYPE B"		)
		x	5.0	
		y	6.0
		p0	'( 0.000 -3.500)    ; All points to be modified by MoveUp		
		p1	'(-2.500 -1.000)
		p2	'(-2.500  2.500)
		p3	'( 2.500  2.500)
		p4	'( 2.500 -1.000)
		p6	'(-2.000  0.000)	
		p7	'( 2.000  0.000)		
		a1	'( 0.000  0.000)
		a2  '( 0.000  1.250)
		a3  '( 0.000 -1.250)
		a4  '(-3.500  0.000)
		a5  '(3.500  0.000)		
		attVitesse1	'("VITESSE_1" "Vitesse primaire"	"")
		attVitesse2	'("VITESSE_2" "Vitesse secondaire"	"")
		attAnnotationG '("ANNOTATION_G" "Annotation Gauche"	"")	; Position annotation sur a4 au dessus direction Aval 
		attAnnotationD '("ANNOTATION_D" "Annotation Droite"	"")	; Position annotation sur a5 au dessus direction Amont 			
	)	
	(SetLayer layDef_Zero)
		(foreach nombres '("UN_NOMBRE" "DEUX_NOMBRES")
			(setq 
				thisBlockName	(strcat blockName "-" nombres)
				thisDescription	(strcat description "-" nombres)
			)
			(command 
				_POLYLINE_ p1 p2 p3 p4 _openPolyline_
				_ARC_ p1 p0 p4	
			)				
			(AddAtt (GetAttributeTag attAnnotationG) (GetAttributePrompt attAnnotationG) (GetAttributeDefaultValue attAnnotationG) a4 _th180_ _angleZero_ _rcTextStyle_ _BottomRight_)
			(AddAtt (GetAttributeTag attAnnotationD) (GetAttributePrompt attAnnotationD) (GetAttributeDefaultValue attAnnotationD) a5 _th180_ _angleZero_ _rcTextStyle_ _BottomLeft_)						
			(cond 
				((= nombres "UN_NOMBRE")
					(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a1 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
				((= nombres "DEUX_NOMBRES")
					(DrawLine layDef_Zero p6 p7)
					(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a2 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
					(AddAtt (GetAttributeTag attVitesse2) (GetAttributePrompt attVitesse2) (GetAttributeDefaultValue attVitesse2) a3 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
			)
			(MoveUp (HalfOf y))			
			(AddDescriptionBelowOrigin thisDescription 0)
			(CreateSchematicBlockFromCurrentGraphics thisBlockName)
			(AddGraphicsFromScaledSchematicBlock thisBlockName _one_)
			(CreateAnnotativeBlockFromCurrentGraphics thisBlockName)
		);foreach


	;        __5__            __5__
	;       /     \          /     \
	;      /       \        /       \
	;     1         4      1    a1   4
	;   a4|    a1   |a5  a4| 6-----7 |a5
	;     |         |      |    a2   |
	;     2----0----3      2----0----3
	;
	;     TYPE_C           TYPE_C
	;     UN_NOMBRE        DEUX_NOMBRES
	;

	(setq 
		blockName	(strcat _SIG_ "TIV-" "TIV-PERMANENT-ORDINAIRE-TYPE_C"	)
		description (strcat "TABLEAU INDICATEUR DE VITESSE PERMANENT ORDINAIRE TYPE C"		)
		x	5.0	
		y	5.5
		p0	'( 0.000 -2.500)    ; All points to be modified by MoveUp		
		p1	'(-2.500  1.000)
		p2	'(-2.500 -2.500)
		p3	'( 2.500 -2.500)
		p4	'( 2.500  1.000)
		p5	'( 0.000  3.500)
		p6	'(-2.000  0.000)	
		p7	'( 2.000  0.000)			
		a1	'( 0.000  0.000)
		a2  '( 0.000  1.250)
		a3  '( 0.000 -1.250)
		a4  '(-3.500  0.000)
		a5  '(3.500  0.000)		
		attVitesse1	'("VITESSE_1" "Vitesse primaire"	"")
		attVitesse2	'("VITESSE_2" "Vitesse secondaire"	"")
		attAnnotationG '("ANNOTATION_G" "Annotation Gauche"	"")	; Position annotation sur a4 au dessus direction Aval 
		attAnnotationD '("ANNOTATION_D" "Annotation Droite"	"")	; Position annotation sur a5 au dessus direction Amont 		
	)	
	(SetLayer layDef_Zero)
		(foreach nombres '("UN_NOMBRE" "DEUX_NOMBRES")
			(setq 
				thisBlockName	(strcat blockName "-" nombres)
				thisDescription	(strcat description "-" nombres)
			)
			(command 
				_POLYLINE_ p1 p2 p3 p4 _openPolyline_
				_ARC_ p1 p5 p4	
			)				
			(AddAtt (GetAttributeTag attAnnotationG) (GetAttributePrompt attAnnotationG) (GetAttributeDefaultValue attAnnotationG) a4 _th180_ _angleZero_ _rcTextStyle_ _BottomRight_)
			(AddAtt (GetAttributeTag attAnnotationD) (GetAttributePrompt attAnnotationD) (GetAttributeDefaultValue attAnnotationD) a5 _th180_ _angleZero_ _rcTextStyle_ _BottomLeft_)						
			(cond 
				((= nombres "UN_NOMBRE")
					(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a1 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
				((= nombres "DEUX_NOMBRES")
					(DrawLine layDef_Zero p6 p7)
					(AddAtt (GetAttributeTag attVitesse1) (GetAttributePrompt attVitesse1) (GetAttributeDefaultValue attVitesse1) a2 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
					(AddAtt (GetAttributeTag attVitesse2) (GetAttributePrompt attVitesse2) (GetAttributeDefaultValue attVitesse2) a3 _th200_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
			)
			(MoveUp (HalfOf y))			
			(AddDescriptionBelowOrigin thisDescription 0)
			(CreateSchematicBlockFromCurrentGraphics thisBlockName)
			(AddGraphicsFromScaledSchematicBlock thisBlockName _one_)
			(CreateAnnotativeBlockFromCurrentGraphics thisBlockName)
		);foreach

	;     +---------+       +---------+     
	;     |         |       |         |     
	;     |         |       |   a2    |     
	;  a4 |   a1    | a5 a4 |p1----p2 | a5   
	;     |         |       |   a3    |     
	;     |         |       |         |     
	;     +----.----+       +----.----+     
	;         x*y               x*y 
	;
	;     PANCARTE_ZR           PANCARTE_KM 
	;

	(setq 
		blockName	(strcat _SIG_ "TIV-" "TIV-PERMANENT-ORDINAIRE-PANCARTE"	)
		description (strcat "TABLEAU INDICATEUR DE VITESSE PERMANENT ORDINAIRE PANCARTE"		)
		x	5.0
		y	5.0
		p0	'( 0.000 -2.500)    ; All points to be modified by MoveUp	
		p1	'(-2.000  0.000)	
		p2	'( 2.000  0.000)	
		a1	'( 0.000  0.000)
		a2  '( 0.000  1.250)
		a3  '( 0.000 -1.250)
		a4  '(-3.500  0.000)
		a5  '(3.500  0.000)			
		attAnnotationG '("ANNOTATION_G" "Annotation Gauche"	"")	; Position annotation sur a4 au dessus direction Aval 
		attAnnotationD '("ANNOTATION_D" "Annotation Droite"	"")	; Position annotation sur a5 au dessus direction Amont 	
		attTexteMH '("TEXTE_1" "Texte Milieu ou Haut"	"")	; Position texte sur a1 ou a2 
		attTexteB '("TEXTE_2" "Texte Bas"	"")	; Position texte sur a3 			
	)
	(SetLayer layDef_Zero)
		(foreach typologie '("ZR" "KM")
			(setq 
				thisBlockName	(strcat blockName "-" typologie)
				thisDescription	(strcat description "-" typologie)
			)
			(DrawBox layDef_Zero x y _noWipeout_)		
			(AddAtt (GetAttributeTag attAnnotationG) (GetAttributePrompt attAnnotationG) (GetAttributeDefaultValue attAnnotationG) a4 _th180_ _angleZero_ _rcTextStyle_ _BottomRight_)
			(AddAtt (GetAttributeTag attAnnotationD) (GetAttributePrompt attAnnotationD) (GetAttributeDefaultValue attAnnotationD) a5 _th180_ _angleZero_ _rcTextStyle_ _BottomLeft_)						
			(cond 
				((= typologie "ZR")
					(AddAtt (GetAttributeTag attTexteMH) (GetAttributePrompt attTexteMH) (GetAttributeDefaultValue attTexteMH) a1 _th300_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
				((= typologie "KM")
					(DrawLine layDef_Zero p1 p2)
					(AddAtt (GetAttributeTag attTexteMH) (GetAttributePrompt attTexteMH) (GetAttributeDefaultValue attTexteMH) a2 _th300_ _angleZero_ _rcTextStyle_ _middleCenter_)
					(AddAtt (GetAttributeTag attTexteB) (GetAttributePrompt attTexteB) (GetAttributeDefaultValue attTexteB) a3 _th300_ _angleZero_ _rcTextStyle_ _middleCenter_)
						)
			)
			(MoveUp (HalfOf y))			
			(AddDescriptionBelowOrigin thisDescription 0)
			(CreateSchematicBlockFromCurrentGraphics thisBlockName)
			(AddGraphicsFromScaledSchematicBlock thisBlockName _one_)
			(CreateAnnotativeBlockFromCurrentGraphics thisBlockName)
		);foreach	
	
)


;;; crocodilé / non crocodilé
;;; annonce / exécution
;;; un nombre / deux nombres
;;; Reflectorisé / non reflectorisé / fixe éclairé par transparence / mobile lumineux
;;; lettre noir sur fond blanc / lettre blanc sur fond noir
;;; Formes: Losange | carré | type B | type C 
