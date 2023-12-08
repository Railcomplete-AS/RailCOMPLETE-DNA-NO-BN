;=========================================================================================================================
;
; Thumbnail Cable Duct Open.lsp
;
; (c) Copyright Railcomplete AS, Norway, NO916118503, 2015-2024. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; ELements which may be opened for inspection and cable laying. Often made of concrete with short concrete slabs as cover.
; Thumbnail for cable duct alignment selection

(defun THUMBNAIL-CABLE-DUCT-OPEN ( / )
	(THUMBNAIL-CABLE-DUCT-OPEN-1-CHAMBER-300)
	(THUMBNAIL-CABLE-DUCT-OPEN-1-CHAMBER-400)
	(THUMBNAIL-CABLE-DUCT-OPEN-2-CHAMBER-620)
	(THUMBNAIL-CABLE-DUCT-OPEN-3-CHAMBER-620)
)




; 1-løps skal ha 50 mm lokk, 2- og 3-løps har 70 mm lokk. Overkant lokk bygges normalt på SOK-10 cm. Innsettingspunkt for kanaler er senter underkant kanal.

(defun THUMBNAIL-CABLE-DUCT-OPEN-1-CHAMBER-300 ( / blockName ) 
	(setq blockName (strcat _RC_ thumbnailInfix "-KABELFOERING-KABELKANAL"	"-1-LOEPS-300"		))
	(command 
		_POLYLINE_ 
			_origin_ 
			(list -15 0)
			(list -15 30)
			(list -10 30)
			(list -10 7.5)
			(list -7.5 5)
			(list 7.5 5)
			(list 10 7.5)
			(list 10 30)
			(list 15 30)
			(list 15 0)
			_closedPolyline_
		_SCALE_ _selectAll_ _ENTER_ _origin_ _tenth_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
  
  
  
(defun THUMBNAIL-CABLE-DUCT-OPEN-1-CHAMBER-400 ( / blockName ) 
	(setq blockName (strcat _RC_ thumbnailInfix "-KABELFOERING-KABELKANAL"	"-1-LOEPS-400"		))
	(command 
		_POLYLINE_ 
			_origin_ 
			(list -20 0)
			(list -20 30)
			(list -15 30)
			(list -15 7.5)
			(list -12.5 5)
			(list 12.5 5)
			(list 15 7.5)
			(list 15 30)
			(list 20 30)
			(list 20 0)
			_closedPolyline_
		_SCALE_ _selectAll_ _ENTER_ _origin_ _tenth_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

  
  
(defun THUMBNAIL-CABLE-DUCT-OPEN-2-CHAMBER-620 ( / blockName ) 
	(setq blockName (strcat _RC_ thumbnailInfix "-KABELFOERING-KABELKANAL"	"-2-LOEPS-620"		))
	(command 
		_POLYLINE_ 
			_origin_ 
			(list -31 0)
			(list -30 28)
			(list -24.5 28)
			(list -23.5 7)
			(list -21.5 5)
			(list -5.5 5)
			(list -3.5 7)
			(list -2.5 28)
			(list 2.5 28)
			(list 3.5 7)
			(list 5.5 5)
			(list 21.5 5)
			(list 23.5 7)
			(list 24.5 28)
			(list 30 28)
			(list 31 0)
			_closedPolyline_
		_SCALE_ _selectAll_ _ENTER_ _origin_ _tenth_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

  
  
(defun THUMBNAIL-CABLE-DUCT-OPEN-3-CHAMBER-620 ( / blockName ) 
	(setq blockName (strcat _RC_ thumbnailInfix "-KABELFOERING-KABELKANAL"	"-3-LOEPS-620"		))
	(command 
		_POLYLINE_ 
			_origin_ 
			(list -31 0)
			(list -30 28)
			(list -24.5 28)
			(list -23.5 7)
			(list -21.5 5)
			(list -16.5 5)
			(list -14.5 7)
			(list -13.5 28)
			(list -8.5 28)
			(list -7.5 7)
			(list -5.5 5)
			(list 5.5 5)
			(list 7.5 7)
			(list 8.5 28)
			(list 13.5 28)
			(list 14.5 7)
			(list 16.5 5)
			(list 21.5 5)
			(list 23.5 7)
			(list 24.5 28)
			(list 30 28)
			(list 31 0)
			_closedPolyline_

		_SCALE_ _selectAll_ _ENTER_ _origin_ _tenth_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
