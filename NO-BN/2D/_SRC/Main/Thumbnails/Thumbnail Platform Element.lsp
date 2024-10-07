;=========================================================================================================================
;
; Thumbnail Platform Element.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Thumbnail for sweeped iterated platform element along the platform edge

(defun THUMBNAIL-PLATFORM-ELEMENT ( side / blockName ) 
	; Platform element along the platform edge
	;
	;    9---------------8
	;   10-11 6----------7
	;      |  |        
	;      |  |        
	;      |  |        
	;      |  |        
	;      |  |        
	;      |  |        
	;      |  5----------4
	;   13-12            3-2
	;   .------------------1
	;
	
	(setq blockName (strcat _RC_ thumbnailInfix "-PLATFORM-EDGE-" side		))
	(command
		_POLYLINE_ 
			_origin_ ; Lower left corner '.'
			(list 1350    0) ; p1
			(list 1350  150) ; p2
			(list 1125  150) ; p3
			(list 1125  350) ; p4
			(list  375  350) ; p5
			(list  375 1230) ; p6
			(list 1125 1230) ; p7
			(list 1125 1380) ; p8
			(list  125 1380) ; p9
			(list  125 1230) ; p10
			(list  225 1230) ; p11
			(list  225  150) ; p12
			(list    0  150) ; p13
			_origin_
			_ENTER_
		_SCALE_ _selectAll_ _ENTER_ _origin_ _tenth_
		_SCALE_ _selectAll_ _ENTER_ _origin_ _tenth_
	)
	(if (eq side "LEFT") (MirrorAboutYaxis _eraseMirrorSource_))
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
