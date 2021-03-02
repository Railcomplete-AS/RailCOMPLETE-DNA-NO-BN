;=========================================================================================================================
;
; Label.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Labels (with 1, 2 or 3 texts)

(defun C:LABEL ( / )
	(subSubStep "...LABEL A-1-0")
	(NO-BN-2D-JBTFE-ETIKETT-A-1-0-VLB-018)
	(NO-BN-2D-JBTFE-ETIKETT-A-1-0-VLT-018)
	(NO-BN-2D-JBTFE-ETIKETT-A-1-0-VRB-018)
	(NO-BN-2D-JBTFE-ETIKETT-A-1-0-VRT-018)
	(subSubStep "...LABEL B-1-0")
	(NO-BN-2D-JBTFE-ETIKETT-B-1-0-HLB-018)
	(NO-BN-2D-JBTFE-ETIKETT-B-1-0-HLT-018)
	(NO-BN-2D-JBTFE-ETIKETT-B-1-0-HRB-018)
	(NO-BN-2D-JBTFE-ETIKETT-B-1-0-HRT-018)
	(subSubStep "...LABEL C-1-0")
	(NO-BN-2D-JBTFE-ETIKETT-C-1-0-HLB-025)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-0-HLT-025)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-0-HRB-025)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-0-HRT-025)
	(subSubStep "...LABEL C-1-1")
	(NO-BN-2D-JBTFE-ETIKETT-C-1-1-HLB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-1-HLT-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-1-HRB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-1-HRT-025-018)
	(subSubStep "...LABEL C-1-2")
	(NO-BN-2D-JBTFE-ETIKETT-C-1-2-HLB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-2-HLT-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-2-HRB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-2-HRT-025-018)
	(subSubStep "...LABEL D-1-1")
	(NO-BN-2D-JBTFE-ETIKETT-D-1-1-HLB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-D-1-1-HLT-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-D-1-1-HRB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-D-1-1-HRT-025-018)
	(subSubStep "...LABEL D-1-2")
	(NO-BN-2D-JBTFE-ETIKETT-D-1-2-HLB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-D-1-2-HLT-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-D-1-2-HRB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-D-1-2-HRT-025-018)
)
	
	
	
(defun NO-BN-2D-JBTFE-ETIKETT-A-1-0-VLB-018 ( / blockName )
	(setq 
		blockName	"NO-BN-2D-JBTFE-ETIKETT-A-1-0-VLB-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ _origo_ "0,10" _ENTER_)
	(addAtt "T01" "T01" "T01" "-0.6,0" _th180_ _angle90_ _rcTextStyle_ _bottomLeft_)
	(addDescriptionBelowOrigo description 1.0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-A-1-0-VLT-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-A-1-0-VLT-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ _origo_ "0,10" _ENTER_)
	(addAtt "T01" "T01" "T01" "-0.6,0" _th180_ _angle90_ _rcTextStyle_ _bottomLeft_)
	(command _MOVE_ _selectAll_ _ENTER_ _setMoveDisplacement_ "0,-10") 
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-A-1-0-VRB-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-A-1-0-VRB-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ _origo_ "0,10" _ENTER_)
	(addAtt "T01" "T01" "T01" "-0.6,0" _th180_ _angle90_ _rcTextStyle_ _bottomLeft_)
	(command _ROTATE_ _selectAll_ _ENTER_ "0,5" _angle180_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-A-1-0-VRT-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-A-1-0-VRT-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ _origo_ "0,10" _ENTER_)
	(addAtt "T01" "T01" "T01" "-0.6,0" _th180_ _angle90_ _rcTextStyle_ _bottomLeft_)
	(command _ROTATE_ _selectAll_ _ENTER_ "0,5" _angle180_)
	(command _MOVE_ _selectAll_ _ENTER_ _setMoveDisplacement_ "0,-10") 
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-B-1-0-HLB-018 ( / blockName )
	(setq
		blockName "NO-BN-2D-JBTFE-ETIKETT-B-1-0-HLB-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ _origo_ "0,3" "-10,3" _ENTER_)
	(addAtt "T01" "T01" "T01" "-10,2.4" _th180_ _angleZero_ _rcTextStyle_ _topLeft_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-B-1-0-HLT-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-B-1-0-HLT-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ "-10,0" _origo_ "0,3" _ENTER_)
	(addAtt "T01" "T01" "T01" "-10,0.6" _th180_ _angleZero_ _rcTextStyle_ _bottomLeft_)
	(command _MOVE_ _selectAll_ _ENTER_ _origo_ "0,-3")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-B-1-0-HRB-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-B-1-0-HRB-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ _origo_ "0,3" "10,3" _ENTER_)
	(addAtt "T01" "T01" "T01" "10,2.4" _th180_ _angleZero_ _rcTextStyle_ _topRight_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-B-1-0-HRT-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-B-1-0-HRT-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ _origo_ "0,3" "10,3" _ENTER_)
	(addAtt "T01" "T01" "T01" "10,2.4" _th180_ _angleZero_ _rcTextStyle_ _topRight_)
	(command _MIRROR_ _selectAll_ _ENTER_ "0,1.5" "1,1.5" _eraseMirrorSource_)
	(command _MOVE_ _selectAll_ _ENTER_ _origo_ "0,-3")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-0-HLB-025 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-0-HLB-025"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
(command _LINE_ "0,3" _origo_ "-3,3" "-21,3" _ENTER_)
	(addAtt "T01" "T01" "T01" "-21.5,3.75" _th250_ _angleZero_ _rcTextStyle_ _bottomLeft_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-0-HLT-025 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-0-HLT-025"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ "0,-3" _origo_ "-3,-3" "-21,-3" _ENTER_)
	(addAtt "T01" "T01" "T01" "-21.5,-2.25" _th250_ _angleZero_ _rcTextStyle_ _bottomLeft_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-0-HRB-025 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-0-HRB-025"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ "0,3" _origo_ "3,3" "21,3" _ENTER_)
	(addAtt "T01" "T01" "T01" "21.5,3.75" _th250_ _angleZero_ _rcTextStyle_ _BottomRight_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-0-HRT-025 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-0-HRT-025"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ "0,-3" _origo_ "3,-3" "21,-3" _ENTER_)
	(addAtt "T01" "T01" "T01" "21.5,-2.25" _th250_ _angleZero_ _rcTextStyle_ _BottomRight_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-1-HLB-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-1-HLB-025-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ "0,3" _origo_ "-3,3" "-21,3" _ENTER_)
	(addAtt "T01" "T01" "T01" "-12.5,3.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "-12.5,2.4" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-1-HLT-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-1-HLT-025-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ _origo_ "0,3" "-3,0" "-21,0" _ENTER_)
	(addAtt "T01" "T01" "T01" "-12.5,0.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "-12.5,-0.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(command _MOVE_ _selectAll_ _ENTER_ _origo_ "0,-3")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-1-HRB-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-1-HRB-025-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ "0,3" _origo_ "3,3" "21,3" _ENTER_)
	(addAtt "T01" "T01" "T01" "12.5,3.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "12.5,2.4" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-1-HRT-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-1-HRT-025-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ _origo_ "0,3" "3,0" "21,0" _ENTER_)
	(addAtt "T01" "T01" "T01" "12.5,0.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "12.5,-0.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(command _MOVE_ _selectAll_ _ENTER_ _origo_ "0,-3")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-2-HLB-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-2-HLB-025-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ "0,3" _origo_ "-3,3" "-21,3" _ENTER_)
	(addAtt "T01" "T01" "T01" "-12.5,3.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "-12.5,2.4" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(addAtt "T03" "T03" "T03" "-12.5,0.4" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-2-HLT-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-2-HLT-025-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ _origo_ "0,3" "-3,0" "-21,0" _ENTER_)
	(addAtt "T01" "T01" "T01" "-12.5,0,75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "-12.5,-0.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(addAtt "T03" "T03" "T03" "-12.5,-2.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(command _MOVE_ _selectAll_ _ENTER_ _origo_ "0,-3")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-2-HRB-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-2-HRB-025-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ _origo_ "0,-3" "3,0" "21,0" _ENTER_)
	(addAtt "T01" "T01" "T01" "12.5,0.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "12.5,-0.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(addAtt "T03" "T03" "T03" "12.5,-2.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-2-HRT-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-2-HRT-025-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ _origo_ "0,3" "3,0" "21,0" _ENTER_)
	(addAtt "T01" "T01" "T01" "12.5,0.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "12.5,-0.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(addAtt "T03" "T03" "T03" "12.5,-2.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(command _MOVE_ _selectAll_ _ENTER_ _origo_ "0,-3")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-1-HLB-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-1-HLB-025-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ "0,6" _origo_ _ENTER_)
	(command _LINE_ "0,1.5" "-3,4.5" "-21,4.5" _ENTER_)
	(addAtt "T01" "T01" "T01" "-12.5,5.25" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "-12.5,3.9" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-1-HLT-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-1-HLT-025-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ "0,6" _origo_ _ENTER_)
	(command _LINE_ "0,4.5" "-3,1.5" "-21,1.5" _ENTER_)
	(addAtt "T01" "T01" "T01" "-12.5,2.25" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "-12.5,0.9" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(command _MOVE_ _selectAll_ _ENTER_ _origo_ "0,-6")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-1-HRB-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-1-HRB-025-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ "0,6" _origo_ _ENTER_)
	(command _LINE_ "0,1.5" "3,4.5" "21,4.5" _ENTER_)
	(addAtt "T01" "T01" "T01" "12.5,5.25" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "12.5,3.9" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-1-HRT-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-1-HRT-025-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ "0,6" _origo_ _ENTER_)
	(command _LINE_ "0,4.5" "3,1.5" "21,1.5" _ENTER_)
	(addAtt "T01" "T01" "T01" "12.5,2.25" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "12.5,0.9" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(command _MOVE_ _selectAll_ _ENTER_ _origo_ "0,-6")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-2-HLB-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-2-HLB-025-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ "0,6" _origo_ _ENTER_)
	(command _LINE_ "0,1.5" "-3,4.5" "-21,4.5" _ENTER_)
	(addAtt "T01" "T01" "T01" "-12.5,5.25" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "-12.5,3.9" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(addAtt "T03" "T03" "T03" "-12.5,1.4" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-2-HLT-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-2-HLT-025-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ _origo_ "0,-6" _ENTER_)
	(command _LINE_ "0,-1.5" "-3,-4.5" "-21,-4.5" _ENTER_)
	(addAtt "T01" "T01" "T01" "-12.5,-3.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "-12.5,-5.1" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(addAtt "T03" "T03" "T03" "-12.5,-7.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-2-HRB-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-2-HRB-025-018"
		description (strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ "0,6" _origo_ _ENTER_)
	(command _LINE_ "0,1.5" "3,4.5" "21,4.5" _ENTER_)
	(addAtt "T01" "T01" "T01" "12.5,5.25" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "12.5,3.9" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(addAtt "T03" "T03" "T03" "12.5,1.4" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-2-HRT-025-018 ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-2-HRT-025-018"
		description	(strcat "ETIKETT " (substr blockname 24 13))
	)
	(command _LINE_ _origo_ "0,-6" _ENTER_)
	(command _LINE_ "0,-1.5" "3,-4.5" "21,-4.5" _ENTER_)
	(addAtt "T01" "T01" "T01" "12.5,-3.75" _th250_ _angleZero_ _rcTextStyle_ _BottomCenter_)
	(addAtt "T02" "T02" "T02" "12.5,-5.1" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(addAtt "T03" "T03" "T03" "12.5,-7.6" _th180_ _angleZero_ _rcTextStyle_ _topCenter_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
