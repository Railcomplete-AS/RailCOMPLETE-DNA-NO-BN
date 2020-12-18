;
; Label.lsp
;
(defun C:LABEL ()
	(NO-BN-2D-JBTFE-ETIKETT-A-1-0-VLB-018)
	(NO-BN-2D-JBTFE-ETIKETT-A-1-0-VLT-018)
	(NO-BN-2D-JBTFE-ETIKETT-A-1-0-VRB-018)
	(NO-BN-2D-JBTFE-ETIKETT-A-1-0-VRT-018)
	(NO-BN-2D-JBTFE-ETIKETT-B-1-0-HLB-018)
	(NO-BN-2D-JBTFE-ETIKETT-B-1-0-HLT-018)
	(NO-BN-2D-JBTFE-ETIKETT-B-1-0-HRB-018)
	(NO-BN-2D-JBTFE-ETIKETT-B-1-0-HRT-018)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-0-HLB-025)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-0-HLT-025)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-0-HRB-025)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-0-HRT-025)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-1-HLB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-1-HLT-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-1-HRB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-1-HRT-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-2-HLB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-2-HLT-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-2-HRB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-C-1-2-HRT-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-D-1-1-HLB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-D-1-1-HLT-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-D-1-1-HRB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-D-1-1-HRT-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-D-1-2-HLB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-D-1-2-HLT-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-D-1-2-HRB-025-018)
	(NO-BN-2D-JBTFE-ETIKETT-D-1-2-HRT-025-018)
)
	
(defun NO-BN-2D-JBTFE-ETIKETT-A-1-0-VLB-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-A-1-0-VLB-018")
	(command "._LINE" "0,0" "0,10" "")
	(addAtt "T01" "T01" "T01" "-0.6,0" 1.8 90 "iso" "BL" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-A-1-0-VLT-018 (/ blockName)	
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-A-1-0-VLT-018")
	(command "._LINE" "0,0" "0,10" "")
	(addAtt "T01" "T01" "T01" "-0.6,0" 1.8 90 "iso" "BL" 48)
	(command "._MOVE" "ALL" "" "Displacement" "0,-10") 
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-A-1-0-VRB-018 (/ blockName)	
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-A-1-0-VRB-018")
	(command "._LINE" "0,0" "0,10" "")
	(addAtt "T01" "T01" "T01" "-0.6,0" 1.8 90 "iso" "BL" 48)
	(command "._ROTATE" "ALL" "" "0,5" "180") 
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-A-1-0-VRT-018 (/ blockName)	
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-A-1-0-VRT-018")
	(command "._LINE" "0,0" "0,10" "")
	(addAtt "T01" "T01" "T01" "-0.6,0" 1.8 90 "iso" "BL" 48)
	(command "._ROTATE" "ALL" "" "0,5" "180")
	(command "._MOVE" "ALL" "" "Displacement" "0,-10") 
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-B-1-0-HLB-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-B-1-0-HLB-018")
	(command "._LINE" "0,0" "0,3" "-10,3" "")
	(addAtt "T01" "T01" "T01" "-10,2.4" 1.8 0 "iso" "TL" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-B-1-0-HLT-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-B-1-0-HLT-018")
	blockName
	(command "._LINE" "-10,0" "0,0" "0,3" "")
	(addAtt "T01" "T01" "T01" "-10,0.6" 1.8 0 "iso" "BL" 48)
	(command "._MOVE" "ALL" "" "0,0" "0,-3")
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-B-1-0-HRB-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-B-1-0-HRB-018")
	(command "._LINE" "0,0" "0,3" "10,3" "")
	(addAtt "T01" "T01" "T01" "10,2.4" 1.8 0 "iso" "TR" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-B-1-0-HRT-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-B-1-0-HRT-018")
	(command "._LINE" "0,0" "0,3" "10,3" "")
	(addAtt "T01" "T01" "T01" "10,2.4" 1.8 0 "iso" "TR" 48)
	(command "._MIRROR" "ALL" "" "0,1.5" "1,1.5" "Y")
	(command "._MOVE" "ALL" "" "0,0" "0,-3")
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-0-HLB-025 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-0-HLB-025")
	(command "._LINE" "0,3" "0,0" "-3,3" "-21,3" "")
	(addAtt "T01" "T01" "T01" "-21.5,3.75" 2.5 0 "iso" "BL" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-0-HLT-025 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-0-HLT-025")
	(command "._LINE" "0,-3" "0,0" "-3,-3" "-21,-3" "")
	(addAtt "T01" "T01" "T01" "-21.5,-2.25" 2.5 0 "iso" "BL" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-0-HRB-025 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-0-HRB-025")
	(command "._LINE" "0,3" "0,0" "3,3" "21,3" "")
	(addAtt "T01" "T01" "T01" "21.5,3.75" 2.5 0 "iso" "BR" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-0-HRT-025 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-0-HRT-025")
	(command "._LINE" "0,-3" "0,0" "3,-3" "21,-3" "")
	(addAtt "T01" "T01" "T01" "21.5,-2.25" 2.5 0 "iso" "BR" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-1-HLB-025-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-1-HLB-025-018")
	(command "._LINE" "0,3" "0,0" "-3,3" "-21,3" "")
	(addAtt "T01" "T01" "T01" "-12.5,3.75" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "-12.5,2.4" 1.8 0 "iso" "TC" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-1-HLT-025-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-1-HLT-025-018")
	(command "._LINE" "0,0" "0,3" "-3,0" "-21,0" "")
	(addAtt "T01" "T01" "T01" "-12.5,0.75" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "-12.5,-0.6" 1.8 0 "iso" "TC" 48)
	(command "._MOVE" "ALL" "" "0,0" "0,-3")
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-1-HRB-025-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-1-HRB-025-018")
	(command "._LINE" "0,3" "0,0" "3,3" "21,3" "")
	(addAtt "T01" "T01" "T01" "12.5,3.75" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "12.5,2.4" 1.8 0 "iso" "TC" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-1-HRT-025-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-1-HRT-025-018")
	(command "._LINE" "0,0" "0,3" "3,0" "21,0" "")
	(addAtt "T01" "T01" "T01" "12.5,0.75" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "12.5,-0.6" 1.8 0 "iso" "TC" 48)
	(command "._MOVE" "ALL" "" "0,0" "0,-3")
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-2-HLB-025-018 (/ blockName)
  (setq blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-2-HLB-025-018")
	(command "._LINE" "0,3" "0,0" "-3,3" "-21,3" "")
	(addAtt "T01" "T01" "T01" "-12.5,3.75" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "-12.5,2.4" 1.8 0 "iso" "TC" 48)
	(addAtt "T03" "T03" "T03" "-12.5,0.4" 1.8 0 "iso" "TC" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-2-HLT-025-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-2-HLT-025-018")
	(command "._LINE" "0,0" "0,3" "-3,0" "-21,0" "")
	(addAtt "T01" "T01" "T01" "-12.5,0,75" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "-12.5,-0.6" 1.8 0 "iso" "TC" 48)
	(addAtt "T03" "T03" "T03" "-12.5,-2.6" 1.8 0 "iso" "TC" 48)
	(command "._MOVE" "ALL" "" "0,0" "0,-3")
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-2-HRB-025-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-2-HRB-025-018")
	(command "._LINE" "0,0" "0,-3" "3,0" "21,0" "")
	(addAtt "T01" "T01" "T01" "12.5,0.75" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "12.5,-0.6" 1.8 0 "iso" "TC" 48)
	(addAtt "T03" "T03" "T03" "12.5,-2.6" 1.8 0 "iso" "TC" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-C-1-2-HRT-025-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-C-1-2-HRT-025-018")
	(command "._LINE" "0,0" "0,3" "3,0" "21,0" "")
	(addAtt "T01" "T01" "T01" "12.5,0.75" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "12.5,-0.6" 1.8 0 "iso" "TC" 48)
	(addAtt "T03" "T03" "T03" "12.5,-2.6" 1.8 0 "iso" "TC" 48)
	(command "._MOVE" "ALL" "" "0,0" "0,-3")
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-1-HLB-025-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-1-HLB-025-018")
	(command "._LINE" "0,6" "0,0" "")
	(command "._LINE" "0,1.5" "-3,4.5" "-21,4.5" "")
	(addAtt "T01" "T01" "T01" "-12.5,5.25" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "-12.5,3.9" 1.8 0 "iso" "TC" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-1-HLT-025-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-1-HLT-025-018")
	(command "._LINE" "0,6" "0,0" "")
	(command "._LINE" "0,4.5" "-3,1.5" "-21,1.5" "")
	(addAtt "T01" "T01" "T01" "-12.5,2.25" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "-12.5,0.9" 1.8 0 "iso" "TC" 48)
	(command "._MOVE" "ALL" "" "0,0" "0,-6")
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-1-HRB-025-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-1-HRB-025-018")
	(command "._LINE" "0,6" "0,0" "")
	(command "._LINE" "0,1.5" "3,4.5" "21,4.5" "")
	(addAtt "T01" "T01" "T01" "12.5,5.25" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "12.5,3.9" 1.8 0 "iso" "TC" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-1-HRT-025-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-1-HRT-025-018")
	(command "._LINE" "0,6" "0,0" "")
	(command "._LINE" "0,4.5" "3,1.5" "21,1.5" "")
	(addAtt "T01" "T01" "T01" "12.5,2.25" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "12.5,0.9" 1.8 0 "iso" "TC" 48)
	(command "._MOVE" "ALL" "" "0,0" "0,-6")
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-2-HLB-025-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-2-HLB-025-018")
	(command "._LINE" "0,6" "0,0" "")
	(command "._LINE" "0,1.5" "-3,4.5" "-21,4.5" "")
	(addAtt "T01" "T01" "T01" "-12.5,5.25" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "-12.5,3.9" 1.8 0 "iso" "TC" 48)
	(addAtt "T03" "T03" "T03" "-12.5,1.4" 1.8 0 "iso" "TC" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-2-HLT-025-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-2-HLT-025-018")
	(command "._LINE" "0,0" "0,-6" "")
	(command "._LINE" "0,-1.5" "-3,-4.5" "-21,-4.5" "")
	(addAtt "T01" "T01" "T01" "-12.5,-3.75" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "-12.5,-5.1" 1.8 0 "iso" "TC" 48)
	(addAtt "T03" "T03" "T03" "-12.5,-7.6" 1.8 0 "iso" "TC" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-2-HRB-025-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-2-HRB-025-018")
	(command "._LINE" "0,6" "0,0" "")
	(command "._LINE" "0,1.5" "3,4.5" "21,4.5" "")
	(addAtt "T01" "T01" "T01" "12.5,5.25" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "12.5,3.9" 1.8 0 "iso" "TC" 48)
	(addAtt "T03" "T03" "T03" "12.5,1.4" 1.8 0 "iso" "TC" 48)
	(newBlock blockName)
	blockName
)

(defun NO-BN-2D-JBTFE-ETIKETT-D-1-2-HRT-025-018 (/ blockName)
	(setq blockName "NO-BN-2D-JBTFE-ETIKETT-D-1-2-HRT-025-018")
	(command "._LINE" "0,0" "0,-6" "")
	(command "._LINE" "0,-1.5" "3,-4.5" "21,-4.5" "")
	(addAtt "T01" "T01" "T01" "12.5,-3.75" 2.5 0 "iso" "BC" 48)
	(addAtt "T02" "T02" "T02" "12.5,-5.1" 1.8 0 "iso" "TC" 48)
	(addAtt "T03" "T03" "T03" "12.5,-7.6" 1.8 0 "iso" "TC" 48)
	(newBlock blockName)
	blockName
)