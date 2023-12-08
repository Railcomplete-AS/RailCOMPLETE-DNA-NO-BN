;=========================================================================================================================
;
; Label.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Labels (with 1, 2 or 3 texts)
;
; V = vertical
; H = Horizontal
; L = Left
; R = Right
; B = Bottom
; T = Top
; n-m = Total number of text lines / Number of text lines below the division line
; T01, T02, T03 : Tag names for the CAD system text entities, addressable from RailCOMPLETE's DNA
;
; A/B/C/D = Variations:
;
; A
;       | 
;       | 
;       | 
;     1 |
;     0 |
;     T .
;
; B
;   ----------+
;   T01       |
;             .
;
; C 
;   T01
;   --------+ |
;   T02      \|
;             .
;
; D
;   T01
;   --------+ |
;   T02      \|
;   T03       |
;             .
;

(defun LABELS ( / )
	(setq blockNameInfix (strcat "ETI-" "ETIKETT"		))
	; (The description will be generated in each subroutine.)
	
	(TraceLevel3 "...LABEL A-1-0")
		(LABEL-A-1-0-VLB-018)
		(LABEL-A-1-0-VLT-018)
		(LABEL-A-1-0-VRB-018)
		(LABEL-A-1-0-VRT-018)
		
	(TraceLevel3 "...LABEL B-1-0")
		(LABEL-B-1-0-HLB-018)
		(LABEL-B-1-0-HLT-018)
		(LABEL-B-1-0-HRB-018)
		(LABEL-B-1-0-HRT-018)
		
	(TraceLevel3 "...LABEL C-1-0")
		(LABEL-C-1-0-HLB-025)
		(LABEL-C-1-0-HLT-025)
		(LABEL-C-1-0-HRB-025)
		(LABEL-C-1-0-HRT-025)
		
	(TraceLevel3 "...LABEL C-1-1")
		(LABEL-C-1-1-HLB-025-018)
		(LABEL-C-1-1-HLT-025-018)
		(LABEL-C-1-1-HRB-025-018)
		(LABEL-C-1-1-HRT-025-018)
		
	(TraceLevel3 "...LABEL C-1-2")
		(LABEL-C-1-2-HLB-025-018)
		(LABEL-C-1-2-HLT-025-018)
		(LABEL-C-1-2-HRB-025-018)
		(LABEL-C-1-2-HRT-025-018)
		
	(TraceLevel3 "...LABEL D-1-1")
		(LABEL-D-1-1-HLB-025-018)
		(LABEL-D-1-1-HLT-025-018)
		(LABEL-D-1-1-HRB-025-018)
		(LABEL-D-1-1-HRT-025-018)
		
	(TraceLevel3 "...LABEL D-1-2")
		(LABEL-D-1-2-HLB-025-018)
		(LABEL-D-1-2-HLT-025-018)
		(LABEL-D-1-2-HRB-025-018)
		(LABEL-D-1-2-HRT-025-018)
		
	; Cleanup temp globals
	(setq blockNameInfix nil)
)
	
	
	
(defun LABEL-A-1-0-VLB-018 ( / variation blockName description )
	(setq 
		variation	"A-1-0-VLB-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ _origin_ "0,10" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-0.6,0" _th180_ _angle90_ _rcTextStyle_ _bottomLeft_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-A-1-0-VLT-018 ( / blockName description )
	(setq 
		variation	"A-1-0-VLT-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ _origin_ "0,10" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-0.6,0" _th180_ _angle90_ _rcTextStyle_ _bottomLeft_)
	(AddDescriptionBelowOrigin description 1.0)
	(command _MOVE_ _selectAll_ _ENTER_ _setMoveDisplacement_ "0,-10") 
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-A-1-0-VRB-018 ( / blockName description )
	(setq 
		variation	"A-1-0-VRB-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ _origin_ "0,10" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-0.6,0" _th180_ _angle90_ _rcTextStyle_ _bottomLeft_)
	(AddDescriptionBelowOrigin description 1.0)
	(command _ROTATE_ _selectAll_ _ENTER_ "0,5" _angle180_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-A-1-0-VRT-018 ( / blockName description )
	(setq 
		variation	"A-1-0-VRT-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ _origin_ "0,10" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-0.6,0" _th180_ _angle90_ _rcTextStyle_ _bottomLeft_)
	(AddDescriptionBelowOrigin description 1.0)
	(command _ROTATE_ _selectAll_ _ENTER_ "0,5" _angle180_)
	(command _MOVE_ _selectAll_ _ENTER_ _setMoveDisplacement_ "0,-10") 
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-B-1-0-HLB-018 ( / blockName description )
	(setq
		variation	"B-1-0-HLB-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ _origin_ "0,3" "-10,3" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-10,2.4" _th180_ _angleZero_ _rcTextStyle_ _topLeft_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-B-1-0-HLT-018 ( / blockName description )
	(setq 
		variation	"B-1-0-HLT-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ "-10,0" _origin_ "0,3" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-10,0.6" _th180_ _angleZero_ _rcTextStyle_ _bottomLeft_)
	(AddDescriptionBelowOrigin description 1.0)
	(command _MOVE_ _selectAll_ _ENTER_ _origin_ "0,-3")
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-B-1-0-HRB-018 ( / blockName description )
	(setq 
		variation	"B-1-0-HRB-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ _origin_ "0,3" "10,3" _ENTER_)
	(AddAtt "T01" "T01" "T01" "10,2.4" _th180_ _angleZero_ _rcTextStyle_ _topRight_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-B-1-0-HRT-018 ( / blockName description )
	(setq 
		variation	"B-1-0-HRT-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ _origin_ "0,3" "10,3" _ENTER_)
	(AddAtt "T01" "T01" "T01" "10,2.4" _th180_ _angleZero_ _rcTextStyle_ _topRight_)
	(AddDescriptionBelowOrigin description 1.0)
	(command _MIRROR_ _selectAll_ _ENTER_ "0,1.5" "1,1.5" _eraseMirrorSource_)
	(command _MOVE_ _selectAll_ _ENTER_ _origin_ "0,-3")
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-C-1-0-HLB-025 ( / blockName description )
	(setq 
		variation	"C-1-0-HLB-025"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ "0,3" _origin_ "-3,3" "-21,3" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-21.5,3.75" _th250_ _angleZero_ _rcTextStyle_ _bottomLeft_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-C-1-0-HLT-025 ( / blockName description )
	(setq 
		variation	"C-1-0-HLT-025"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ "0,-3" _origin_ "-3,-3" "-21,-3" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-21.5,-2.25" _th250_ _angleZero_ _rcTextStyle_ _bottomLeft_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-C-1-0-HRB-025 ( / blockName description )
	(setq 
		variation	"C-1-0-HRB-025"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ "0,3" _origin_ "3,3" "21,3" _ENTER_)
	(AddAtt "T01" "T01" "T01" "21.5,3.75" _th250_ _angleZero_ _rcTextStyle_ _BottomRight_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-C-1-0-HRT-025 ( / blockName description )
	(setq 
		variation	"C-1-0-HRT-025"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ "0,-3" _origin_ "3,-3" "21,-3" _ENTER_)
	(AddAtt "T01" "T01" "T01" "21.5,-2.25" _th250_ _angleZero_ _rcTextStyle_ _BottomRight_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-C-1-1-HLB-025-018 ( / blockName description )
	(setq 
		variation	"C-1-1-HLB-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ "0,3" _origin_ "-3,3" "-21,3" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-12.5,3.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "-12.5,2.4" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-C-1-1-HLT-025-018 ( / blockName description )
	(setq 
		variation	"C-1-1-HLT-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ _origin_ "0,3" "-3,0" "-21,0" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-12.5,0.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "-12.5,-0.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(command _MOVE_ _selectAll_ _ENTER_ _origin_ "0,-3")
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-C-1-1-HRB-025-018 ( / blockName description )
	(setq 
		variation	"C-1-1-HRB-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ "0,3" _origin_ "3,3" "21,3" _ENTER_)
	(AddAtt "T01" "T01" "T01" "12.5,3.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "12.5,2.4" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-C-1-1-HRT-025-018 ( / blockName description )
	(setq 
		variation	"C-1-1-HRT-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ _origin_ "0,3" "3,0" "21,0" _ENTER_)
	(AddAtt "T01" "T01" "T01" "12.5,0.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "12.5,-0.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddDescriptionBelowOrigin description 1.0)
	(command _MOVE_ _selectAll_ _ENTER_ _origin_ "0,-3")
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-C-1-2-HLB-025-018 ( / blockName description )
	(setq 
		variation	"C-1-2-HLB-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ "0,3" _origin_ "-3,3" "-21,3" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-12.5,3.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "-12.5,2.4" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddAtt "T03" "T03" "T03" "-12.5,0.4" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-C-1-2-HLT-025-018 ( / blockName description )
	(setq 
		variation	"C-1-2-HLT-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ _origin_ "0,3" "-3,0" "-21,0" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-12.5,0,75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "-12.5,-0.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddAtt "T03" "T03" "T03" "-12.5,-2.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddDescriptionBelowOrigin description 1.0)
	(command _MOVE_ _selectAll_ _ENTER_ _origin_ "0,-3")
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-C-1-2-HRB-025-018 ( / blockName description )
	(setq 
		variation	"C-1-2-HRB-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ _origin_ "0,-3" "3,0" "21,0" _ENTER_)
	(AddAtt "T01" "T01" "T01" "12.5,0.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "12.5,-0.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddAtt "T03" "T03" "T03" "12.5,-2.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-C-1-2-HRT-025-018 ( / blockName description )
	(setq 
		variation	"C-1-2-HRT-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ _origin_ "0,3" "3,0" "21,0" _ENTER_)
	(AddAtt "T01" "T01" "T01" "12.5,0.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "12.5,-0.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddAtt "T03" "T03" "T03" "12.5,-2.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddDescriptionBelowOrigin description 1.0)
	(command _MOVE_ _selectAll_ _ENTER_ _origin_ "0,-3")
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-D-1-1-HLB-025-018 ( / blockName description )
	(setq 
		variation	"D-1-1-HLB-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ "0,6" _origin_ _ENTER_)
	(command _LINE_ "0,1.5" "-3,4.5" "-21,4.5" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-12.5,5.25" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "-12.5,3.9" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-D-1-1-HLT-025-018 ( / blockName description )
	(setq 
		variation	"D-1-1-HLT-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ "0,6" _origin_ _ENTER_)
	(command _LINE_ "0,4.5" "-3,1.5" "-21,1.5" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-12.5,2.25" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "-12.5,0.9" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddDescriptionBelowOrigin description 1.0)
	(command _MOVE_ _selectAll_ _ENTER_ _origin_ "0,-6")
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-D-1-1-HRB-025-018 ( / blockName description )
	(setq 
		variation	"D-1-1-HRB-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ "0,6" _origin_ _ENTER_)
	(command _LINE_ "0,1.5" "3,4.5" "21,4.5" _ENTER_)
	(AddAtt "T01" "T01" "T01" "12.5,5.25" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "12.5,3.9" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-D-1-1-HRT-025-018 ( / blockName description )
	(setq 
		variation	"D-1-1-HRT-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ "0,6" _origin_ _ENTER_)
	(command _LINE_ "0,4.5" "3,1.5" "21,1.5" _ENTER_)
	(AddAtt "T01" "T01" "T01" "12.5,2.25" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "12.5,0.9" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddDescriptionBelowOrigin description 1.0)
	(command _MOVE_ _selectAll_ _ENTER_ _origin_ "0,-6")
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-D-1-2-HLB-025-018 ( / blockName description )
	(setq 
		variation	"D-1-2-HLB-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ "0,6" _origin_ _ENTER_)
	(command _LINE_ "0,1.5" "-3,4.5" "-21,4.5" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-12.5,5.25" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "-12.5,3.9" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddAtt "T03" "T03" "T03" "-12.5,1.4" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-D-1-2-HLT-025-018 ( / blockName description )
	(setq 
		variation	"D-1-2-HLT-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ _origin_ "0,-6" _ENTER_)
	(command _LINE_ "0,-1.5" "-3,-4.5" "-21,-4.5" _ENTER_)
	(AddAtt "T01" "T01" "T01" "-12.5,-3.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "-12.5,-5.1" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddAtt "T03" "T03" "T03" "-12.5,-7.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-D-1-2-HRB-025-018 ( / blockName description )
	(setq 
		variation	"D-1-2-HRB-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ "0,6" _origin_ _ENTER_)
	(command _LINE_ "0,1.5" "3,4.5" "21,4.5" _ENTER_)
	(AddAtt "T01" "T01" "T01" "12.5,5.25" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "12.5,3.9" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddAtt "T03" "T03" "T03" "12.5,1.4" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun LABEL-D-1-2-HRT-025-018 ( / blockName description )
	(setq 
		variation	"D-1-2-HRT-025-018"
		blockName 	(strcat _COM_ blockNameInfix "-" variation)
		description	(strcat (substr blockNameInfix 5) " " variation)
	)
	(setLayer layDef_Zero)
	(command _LINE_ _origin_ "0,-6" _ENTER_)
	(command _LINE_ "0,-1.5" "3,-4.5" "21,-4.5" _ENTER_)
	(AddAtt "T01" "T01" "T01" "12.5,-3.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(AddAtt "T02" "T02" "T02" "12.5,-5.1" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddAtt "T03" "T03" "T03" "12.5,-7.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(AddDescriptionBelowOrigin description 1.0)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
