;
; Thumbnail-Unspecified.lsp
;
(defun C:THUMBNAIL-UNSPECIFIED (/)
	(THUMBNAIL-UNSPECIFIED)
)



(defun THUMBNAIL-UNSPECIFIED (/ blockName x y) 
	(setq 
		blockName (strcat "NO-BN-2D-JBTFE-THUMBNAIL-UNSPECIFIED")
	)
	(command 
		"._RECTANGLE" "0,-3" "4,1"
		"._CIRCLE" "-1,-1" "3"
		"._POLYGON" "3" "EDGE" "-3,0" "3,0"
	)
	(newBlock blockName)
	blockName
)

