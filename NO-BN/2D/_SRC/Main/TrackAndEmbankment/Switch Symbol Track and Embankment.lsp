;=========================================================================================================================
;
; Switch Symbol Track and Embankment.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
; TODO list:
; 2020-09-13 CLFEY Adjust areas forbidden for axle counter sensors to new rules for speeds above / bekow 120 km/h.
; 2020-09-13 CLFEY Include info on forbidden areas in all switch types
; 2020-09-13 CLFEY Include info on point machine placements in all switch types (to be added to the XML for DNA).
;
;=========================================================================================================================
; See Bane NOR, TRV, Overbygning, sporvekselsymboler

; Superstructure (track and embankment) discipline's graphics for inclusion into switch symbol

;-------------------------------------------------------------------------------------------------------------------------
;
; Guide to understanding the RailCOMPLETE switch object symbols
; =============================================================
;
; Switch objects consist of a tongue part, a middle part and a rear part.
;
; These parts' XY coordinates have been extracted from construction drawings and assembled in an Excel spreadsheet, look
; for files with name such as 'NO-BN switches.xlsx' in the RailCOMPLETE 2D symbol creation source code folders. The exact
; XY-plane geometry is captured to the millimeter.
;
; Immediately after the rear-of-switch ('Norw: 'bakkant sporveksel, BK'), there is a stretch of long sleepers. The long
; sleepers are in use until the two tracks' rails have separated enough that there is space enough for using individual
; short sleepers for each track. This is the 'short sleeper' area, after the 'long sleeper' area.
; We have added a dividing line through the short sleeper area, as a visual reminder that there are individual sleepers here.
;
; Not shown in current 2D symbols is how the rails are gradually canted from upright position in the switch to 1:20 inclination.
; Some administrations use 1:20 incliniation throughout all switches, some administrations use upright rails or canted rails,
; depending on the switch type. Bane NOR has both types.
;
;-------------------------------------------------------------------------------------------------------------------------


(defun SWITCH-SYMBOL-SUPERSTRUCTURE ( quadrant drawingNumber 
	/ 	blockName
		description
		switchParameters
		SwitchDiamondType  A B C D E F L R x RailType ang
		zeroPad
	)
	(setq 
		switchParameters (getSwitchParameters drawingNumber)
		SwitchDiamondType	(cadr (assoc "SwitchDiamondType" switchParameters))
		A			(/ (cadr (assoc "A" switchParameters)) 1000.0)
		B			(/ (cadr (assoc "B" switchParameters)) 1000.0)
		C			(/ (cadr (assoc "C" switchParameters)) 1000.0)
		D			(/ (cadr (assoc "D" switchParameters)) 1000.0)
		E			(/ (cadr (assoc "E" switchParameters)) 1000.0)
		F			(/ (cadr (assoc "F" switchParameters)) 1000.0)
		L			(/ (cadr (assoc "L" switchParameters)) 1000.0)
		R			(cadr (assoc "R" switchParameters))
		x			(cadr (assoc "x" switchParameters))
		RailType	(cadr (assoc "RailType" switchParameters))
		ang (R->D (atan (/ 1.0 x)))
	)
	(setq blockName1 (strcat _TRK_ "SPV-" "FORBINDELSE-SPORVEKSEL"		))
	(setq description1 (strcat "SPORVEKSEL, SPORFAG-SYMBOL"				))

	(if (< x 10)
		(setq zeroPad "0") ; Pad with leading zeros
	;else
		(setq zeroPad _emptyString_)
	)
	(setq 
		blockName	(strcat blockName1 "-"    zeroPad (rtos x 2 2) "-R" (rtos R 2 0) "-" RailType "-" SwitchDiamondType "-" (rtos quadrant 2 0))
		description	(strcat description1 ", " zeroPad (rtos x 2 2) "-R" (rtos R 2 0) "-" RailType "-" SwitchDiamondType "-" (rtos quadrant 2 0))
	)
	(SetLayer layDef_Zero)
 	(command
		_COLOR_ _ByBlock_
		_LINE_ _origin_ (list A 0) _ENTER_
		_LINE_ (list A 1) (list A -1) _ENTER_
		_LINE_ (list 0 1) (list 0 -1) _ENTER_
		_ARC_ (list L 0) _setArcCenter_ (list A 0) _setArcAngle_ (+ ang 1.5)
		_ARC_ (list L 0) _setArcCenter_ (list A 0) _setArcAngle_ -1.5
		_POLYLINE_
			(list A 0)
			(list (+ A C) 0)
			_setPolylineArcMode_
			_setPolylineArcCenter_ (list A 0) _setArcAngle_ ang 
			_setPolylineLineMode_
			_closedPolyline_
	)
	(DrawHatch _denseHatch_)
	(command 
		_POLYLINE_ 
			(list (+ A C) 0)
			_setPolylineArcMode_
			_setPolylineArcCenter_ (list A 0)
			_setPolylineArcAngle_ ang 
			_setPolylineLineMode_
			(strcat "@" (rtos D) "<" (rtos ang))
			_setPolylineArcMode_
			_setPolylineArcCenter_ (list A 0)
			_setPolylineArcAngle_ (- ang)
			_setPolylineLineMode_
			_closedPolyline_
	)
	(DrawHatch _sparseHatch_)

	; Deviating track / geometry axis
	(SetLayer layDef_Switch_Geometry)
	(command
		_POLYLINE_ 
			_origin_
			_setPolylineArcMode_
			_setPolylineArcDirection_ _xAxis_ (polar (list A 0) (D->R ang) C)
			_setPolylineLineMode_
			(strcat "@" (rtos D) "<" (rtos ang))
			_openPolyline_
		
		_POLYLINE_ 
			_origin_ (list L 0)
			_openPolyline_
	) 
  
	; Show long-sleepers area outside rear end of switch
	(SetLayer layDef_Switch_LongSleepers)
	(command 
		_POLYLINE_
			(list L 0)
			_setPolylineArcMode_
			_setPolylineArcCenter_ (list A 0) 
			_setPolylineArcAngle_ ang
			_setPolylineLineMode_
			(strcat "@" (rtos E) "<" (rtos ang))
			_setPolylineArcMode_
			_setPolylineArcCenter_ (list A 0)
			_setPolylineArcAngle_ (- ang)
			_setPolylineLineMode_
			_closedPolyline_
	)

	; Show short-sleepers area outside rear end of switch, after the long-sleeper area (if any)
	(SetLayer layDef_Switch_ShortSleepers)
	(command
		_POLYLINE_
			(list (+ L E) 0)
			_setPolylineArcMode_
			_setPolylineArcCenter_ (list A 0) _setArcAngle_ ang
			_setPolylineLineMode_
			(strcat "@" (rtos F) "<" (rtos ang))
			_setPolylineArcMode_
			_setPolylineArcCenter_ (list A 0) _setArcAngle_ (- ang) 
			_setPolylineLineMode_
			_closedPolyline_
    )
	; Add 'division line' to illustrate that there are now one set of sleepers for each switch leg:
	(command
		_LINE_ (list (+ A (* (+ B E) (DDcos (/ ang 2.0)))) (* (+ B E) (DDsin (/ ang 2.0)))) (strcat "@" (rtos F) "<" (rtos (/ ang 2.0))) _ENTER_
	)

	(MoveToQuadrant quadrant _selectAll_)
	(AddDescriptionBelowOrigin description _one_)
	(CreateMetricBlockFromCurrentGraphics blockName)
)
