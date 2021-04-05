;=========================================================================================================================
;
; Thumbnail Cable Duct.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Thumbnail for cable duct alignment selection

(defun C:THUMBNAIL-CABLE-DUCT ( / )
	(THUMBNAIL-CABLE-DUCT-1-CHAMBER-300)
	(THUMBNAIL-CABLE-DUCT-1-CHAMBER-400)
	(THUMBNAIL-CABLE-DUCT-2-CHAMBER-620)
	(THUMBNAIL-CABLE-DUCT-3-CHAMBER-620)
)




; 1-løps skal ha 50 mm lokk, 2- og 3-løps har 70 mm lokk. Overkant lokk bygges normalt på SOK-10 cm. Innsettingspunkt for kanaler er senter underkant kanal.

(defun THUMBNAIL-CABLE-DUCT-1-CHAMBER-300 ( / blockName ) 
	(setq blockName "NO-BN-2D-JBTUB-THUMBNAIL-CABLE-DUCT-1-CHAMBER-300")
	(command 
		_POLYLINE_ 
			_origo_ 
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
		_SCALE_ _selectAll_ _ENTER_ _origo_ _tenth_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
)
  
  
  
(defun THUMBNAIL-CABLE-DUCT-1-CHAMBER-400 ( / blockName ) 
	(setq blockName "NO-BN-2D-JBTUB-THUMBNAIL-CABLE-DUCT-1-CHAMBER-400")
	(command 
		_POLYLINE_ 
			_origo_ 
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
		_SCALE_ _selectAll_ _ENTER_ _origo_ _tenth_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
)

  
  
(defun THUMBNAIL-CABLE-DUCT-2-CHAMBER-620 ( / blockName ) 
  (setq blockName "NO-BN-2D-JBTUB-THUMBNAIL-CABLE-DUCT-2-CHAMBER-620")
	(command 
		_POLYLINE_ 
			_origo_ 
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
		_SCALE_ _selectAll_ _ENTER_ _origo_ _tenth_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	blockName
)

  
  
(defun THUMBNAIL-CABLE-DUCT-3-CHAMBER-620 ( / blockName ) 
	(setq blockName "NO-BN-2D-JBTUB-THUMBNAIL-CABLE-DUCT-3-CHAMBER-620")
	(command 
		_POLYLINE_ 
			_origo_ 
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

		_SCALE_ _selectAll_ _ENTER_ _origo_ _tenth_
	)
	(CreateSchematicBlockFromCurrentGraphics blockName)
)
