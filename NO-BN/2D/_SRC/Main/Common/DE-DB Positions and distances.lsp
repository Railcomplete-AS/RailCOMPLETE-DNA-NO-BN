;=========================================================================================================================
; 
;
; DE-DB Positions and distances.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================



(defun DEDB-POSITIONS-AND-DISTANCES ( / )
	(DEDB-HEKTOMETERZEICHEN)
	(DEDB-KILOMETERZEICHEN)
	(DEDB-D-WEG-ABSTAND)
)
	

(defun DEDB-HEKTOMETERZEICHEN ( / blockName description r1 )
	; DEDB: Hektometerzeichen
	;      _____
	;     /     \  		
	;    |   .   |
	;     \_____/  
	;
	(setq 
		blockName 	(strcat _TRK_ "HMZ-" "HEKTOMETERZEICHEN")
		description (strcat 			 "HEKTOMETERZEICHEN")
		r1	0.750
	)
	(DrawCircle layDef_Zero r1 _noWipeout_)
	(AddDescriptionBelowOrigin description r1)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



(defun DEDB-KILOMETERZEICHEN ( / blockName description r1 r2 )
	; DEDB: Kilometerzeichen
	;     _______
	;    / _____ \
	;   / /     \ \		
	;  | |   .   | |
	;   \ \_____/ /
	;    \_______/ 
	;
	(setq 
		blockName 	(strcat _TRK_ "KMZ-" "KILOMETERZEICHEN")
		description (strcat 			 "KILOMETERZEICHEN")
		r1	0.750
		r2	1.250
	)
	(DrawCircle layDef_Zero r1 _noWipeout_)
	(DrawCircle layDef_Zero r2 _noWipeout_)
	(AddDescriptionBelowOrigin description r2)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



(defun DEDB-D-WEG-ABSTAND ( / blockName description a b c d e p1 p2 p3 p4 p5 p6 attDweg attSignalId attIstAbstand attSollAbstand )
	; DEDB: D-Weg Abstand
	; Use to annotate safety distance / overlap at home signals etc.
	; Let RailCOMPLETE draw the tail down to the track - place object at the desired kilometric position.
	; Note: RC should implement snap points at suitable distances X=0 Y=3, Y=6, Y=9 etc to make pretty insertion possible.
	;
	;      1 (th250)  2 (th250) 
	;                        b\
	;     a------------------ce*.		; Solid arrow from c to the origin
	;                        d/
	;      3 soll: 4 (th180)
	;      5 ist:  6 (th180)
	;
	(setq 
		blockName 	(strcat _SIG_ "DWA-" "D-WEG-ABSTAND"	)
		description (strcat 			 "D-WEG ABSTAND"	)
		a	(list -18.000  0.000)
		b	(list  -3.500  0.600)
		c	(list  -3.500  0.000)	; Start arrow
		d	(list  -3.500 -0.600)
		e	(list  -1.000  0.000)	; Hatch seed point
		p1	(list -17.500  2.500)	; DWEG attribute, overlap ID
		p2	(list -12.500  2.500)	; SIGNAL attribute, signal ID
		p3	(list -17.500 -2.000)	; soll:
		p4	(list -12.500 -2.000)	; SOLLWERT attribute
		p5	(list -17.500 -5.000)	; ist:
		p6	(list -12.500 -5.000)	; ISTWERT attribute
		attDweg			'("D_WEG" 		"Durchrutschweg"	"")
		attSignalId 	'("SIGNAL_ID" 	"Signal-ID"			"")
		attIstAbstand	'("IST" 		"Ist-Abstand"		"")
		attSollAbstand	'("SOLL" 		"Soll-Abstand"		"")
	)
	; Arrow line:
	(DrawLine layDef_Zero a c)
	(DrawLine layDef_Zero b d)
	(DrawLine layDef_Zero b _origin_)
	(DrawLine layDef_Zero d _origin_)
	(DrawHatchAtPoint _solidHatch_ e _angleZero_ _offsetZero_)
	
	; Texts and attributes:
	(SetLayer layDef_Zero)
	(AddAtt (GetAttributeTag attDweg)			(GetAttributePrompt attDweg)		(GetAttributeDefaultValue attDweg)			p1 _th250_ _angleZero_ _rcTextStyle_ _middleLeft_)
	(AddAtt (GetAttributeTag attSignalId)		(GetAttributePrompt attSignalId)	(GetAttributeDefaultValue attSignalId)		p2 _th250_ _angleZero_ _rcTextStyle_ _middleLeft_)
	(AddAtt (GetAttributeTag attSollAbstand)	(GetAttributePrompt attSollAbstand)	(GetAttributeDefaultValue attSollAbstand)	p4 _th180_ _angleZero_ _rcTextStyle_ _middleLeft_)
	(AddAtt (GetAttributeTag attIstAbstand)		(GetAttributePrompt attIstAbstand)	(GetAttributeDefaultValue attIstAbstand)	p6 _th180_ _angleZero_ _rcTextStyle_ _middleLeft_)
	(AddTextAtPosWithJustification layDef_Zero _th180_ p5 "ist:" _middleLeft_)
	(AddTextAtPosWithJustification layDef_Zero _th180_ p3 "soll:" _middleLeft_)

	; Create blocks:
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)
