;
; THUMBNAIL-Kontaktledningsoppheng.lsp
;
(defun C:THUMBNAIL-KONTAKTLEDNINGSOPPHENG ( )
	(ALIGNMENT-KONTAKTLEDNINGSOPPHENG)
)



(defun ALIGNMENT-KONTAKTLEDNINGSOPPHENG (/ blockName ) 
	(setq
		blockName (strcat "NO-BN-2D-JBTKL-THUMBNAIL-KONTAKTLEDNINGSOPPHENG")
		; spanLength 60
		; stitchWireLength 14
		; systemHeight (* 1.8 5)  ; System height is exaggerated 5-fold in Z direction
		; Contact wire, one spanlength:
		w1 (list 0 0)
		w2 (list 60 0)
		; Catenary, one spanlength:
		c1 (list 0 9)
		c2 (list 7 6)
		c3 (list 15 4)
		c4 (list 25 3)
		c5 (list 35 3)
		c6 (list 45 4)
		c7 (list 53 6)
		c8 (list 60 9)
		; Stitch wire, two halves:
		s11 (list 0 5) 
		s12 (list 5 5)
		s13 (list 7 6)
		s21 (list 53 6)
		s22 (list 55 5)
		s23 (list 60 5)
		; Droppers, 6 pcs:
		d11 (list 5 5)
		d12 (list 5 0)
		d21 (list 15 4)
		d22 (list 15 0)
		d31 (list 25 3)
		d32 (list 25 0)
		d41 (list 35 3)
		d42 (list 35 0)
		d51 (list 45 4)
		d52 (list 45 0)
		d61 (list 55 5)
		d62 (list 55 0)
	)
	; Draw one span, then mirror into two full span lengths:
	(command
		"._PLINE" w1 w2 ""
		"._PLINE" c1 c2 c3 c4 c5 c6 c7 c8 ""
		"._PLINE" s11 s12 s13 ""
		"._PLINE" s21 s22 s23 ""
		"._PLINE" d11 d12 ""
		"._PLINE" d21 d22 ""
		"._PLINE" d31 d32 ""
		"._PLINE" d41 d42 ""
		"._PLINE" d51 d52 ""
		"._PLINE" d61 d62 ""
		"._MIRROR" "ALL" "" "0,0" "0,1" "NO"
	)
	; Reduce to about 10 drawing units overall size (standard for alignment thumbnails, even if they will be auto-scaled in RailCOMPLETE):
	(command "._SCALE" "ALL" "" (list 0 0) "0.1")
	(newBlock blockName)
	blockName
)


