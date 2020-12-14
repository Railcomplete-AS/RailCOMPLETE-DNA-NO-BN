;
; THUMBNAIL-Roemningsvei.lsp
;
(defun C:THUMBNAIL-ROEMNINGSVEI (/)
	(ALIGNMENT-ROEMNINGSVEI)
)



(defun ALIGNMENT-ROEMNINGSVEI (/ blockName) 
	(setq blockName "NO-BN-2D-JBTUB-THUMBNAIL-ROEMNINGSVEI")
	(command 
		; PERSON:
		"._PLINE" 
		(list 23 41) ;neck
		(list 28 37) ;right shoulder
		(list 28 32) ;right elbow
		(list 32 32) ;right arm
		(list 32 28) ;right arm
		(list 24 28) ;right elbow
		(list 24 35) ;right armpit
		(list 19 25) ;right waist
		(list 26 11) ;right knee
		(list 21 0) ;right foot
		(list 17 2) ;right foot
		(list 21 11) ;right knee
		(list 15 22) ;crotch
		(list 15 12) ;left knee
		(list 0 12) ;left foot
		(list 0 16) ;left foot
		(list 10 16) ;left knee
		(list 10 27) ;left waist
		(list 15 37) ;left armpit
		(list 11 37) ;left elbow
		(list 7 31) ;left arm
		(list 3 33) ;left arm
		(list 9 41) ;left elbow
		"CLOSE"
		"._CIRCLE" (list 22 46) "5" ;head

		; ARROW:
		"._PLINE" 
		(list 41 27)
		(list 57 27)
		(list 53 31)
		(list 57 31)
		(list 63 25)
		(list 57 19)
		(list 53 19)
		(list 57 23)
		(list 41 23)
		"CLOSE"

		; DOORWAY:
		"._PLINE" 
		(list 66 50)
		(list 90 50)
		(list 90 0)
		(list 66 0)
		"CLOSE"

		; Scale down and move:
		"._SCALE" "All" "" (list 0 0) "0.1"
		"._MOVE" "All" "" (list 5.0 2.5) (list 0 0)
	)
	(newBlock blockName)
	blockName
)