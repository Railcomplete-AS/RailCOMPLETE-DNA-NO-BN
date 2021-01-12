;=========================================================================================================================
;
; Superstructure Switch Symbol.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
; TODO list:
; 2020-09-13 CLFEY Adjust areas forbidden for axle counter sensors to new rules for speeds above / bekow 120 km/h.
; 2020-09-13 CLFEY Include info on forbidden areas in all switch types
; 2020-09-13 CLFEY Include info on point machine placements in all switch types (to be added to the XML for DNA).
;
;=========================================================================================================================
; See Bane NOR, TRV, Overbygning, sporvekselsymboler

; Superstructure (track and embankment) discipline's graphics for inclusion into turnout symbol

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
; Immediately after the rear-of-switch ('Norw: 'bakkant sporveksel, BK'), there is a strech of long sleepers. The long
; sleepers are in use until the two track's rails have separated enough that there is space enough for using individual
; short sleepers for each track. This is the 'short sleeper' area, after the 'long sleeper' area.
; We have added a dividing line through the short sleeper area, as a visual reminder that there are individual sleepers here.
;
; Not shown in current 2D symbols is how the rails are gradually canted from upright position in the switch to 1:20 inclination.
; Some administrations use 1:20 incliniation throughout all switches, some administrations use upright rails or canted rails,
; depending on the switch type. Bane NOR has both types.
;
;-------------------------------------------------------------------------------------------------------------------------


(defun SWITCH-SYMBOL-SUPERSTRUCTURE (quadrant Drawing_Number 
	/ 	blockName
		switchParameters
		crossType  A B C D E F L R x railProfile ang radius
		str pt pts
	)
	(setq 
		switchParameters (getSwitchParameters Drawing_Number)
		crossType	(cadr (assoc "SwitchCrossing" switchParameters))
		A			(/ (cadr (assoc "A" switchParameters)) 1000.0)
		B			(/ (cadr (assoc "B" switchParameters)) 1000.0)
		C			(/ (cadr (assoc "C" switchParameters)) 1000.0)
		D			(/ (cadr (assoc "D" switchParameters)) 1000.0)
		E			(/ (cadr (assoc "E" switchParameters)) 1000.0)
		F			(/ (cadr (assoc "F" switchParameters)) 1000.0)
		L			(/ (cadr (assoc "L" switchParameters)) 1000.0)
		R			(cadr (assoc "R" switchParameters))
		x			(cadr (assoc "x" switchParameters))
		railProfile	(cadr (assoc "RailProfile" switchParameters))
		ang (R->D (atan (/ 1.0 x)))
		radius 0.75
	)
	(setq str "")
	(if (< x 10)
		(setq str "0")
	)
	(setq blockName (strcat "NO-BN-2D-JBTOB-CONNECTOR-SWITCH-" str (rtos x 2 2) "-R" (rtos R 2 0) "-H-" railProfile "-" crossType "-" (rtos quadrant 2 0)))
	(setLayer layer_Zero)
 	(command
		"._COLOR" "_ByBlock"
		"._LINE" "0,0" (list A 0) ""
		"._LINE" (list A 1) (list A -1) ""
		"._LINE" (list 0 1) (list 0 -1) ""
		"._ARC" (list L 0) "C" (list A 0) "Angle" (+ ang 1.5)
		"._ARC" (list L 0) "C" (list A 0) "Angle" -1.5
		"._PLINE"
			(list A 0)
			(list (+ A C) 0)
			"_ARC" "_CE" (list A 0) "Angle" ang 
			"Line" "_CLOSE"
	)
	(drawHatch _mediumHatch_)
	(command 
		"._PLINE" 
			(list (+ A C) 0)
			"_ARC" "_CE" (list A 0)
			"Angle" ang 
			"Line" (strcat "@" (rtos D) "<" (rtos ang))
			"_ARC" "_CE" (list A 0)
			"Angle" (- ang)
			"Line" "_CLOSE"
	)
	(drawHatch _sparseHatch_)

	; Deviating track / centreline
	(setLayer layer_Turnout_TrackCenterLines)
	(command
		"._PLINE" 
			"0,0"
			"_ARC" "Direction" "1,0" (polar (list A 0) (D->R ang) C)
			"Line" (strcat "@" (rtos D) "<" (rtos ang))
			""
		"._PLINE" "0,0" (list L 0) ""
	) 
  
	; Show long-sleepers area outside back end of switch
	(setLayer layer_Turnout_LongSleepers)
	(command 
		"._PLINE"
			(list L 0)
			"_ARC" "_CE" (list A 0) "Angle" ang
			"Line" (strcat "@" (rtos E) "<" (rtos ang))
			"_ARC" "_CE" (list A 0) "Angle" (- ang)
			"Line" 
			"_CLOSE"
	)

	; Show short-sleepers area outside back end of switch, after the long-sleeper area (if any)
	(setLayer layer_Turnout_ShortSleepers)
	(command
		"._PLINE"
			(list (+ L E) 0)
			"_ARC" "_CE" (list A 0) "Angle" ang
			"Line" (strcat "@" (rtos F) "<" (rtos ang))
			"_ARC" "_CE" (list A 0) "Angle" (- ang) 
			"Line" "_CLOSE"
    )
	; Add 'division line' to illustrate that there are now one set of sleepers for each turnout leg:
	(command
		"._LINE" (list (+ A (* (+ B E) (cos (D->R(/ ang 2.0))))) (* (+ B E) (sin (D->R (/ ang 2.0))))) (strcat "@" (rtos F) "<" (rtos (/ ang 2.0))) ""
	)
    
	; Forbidden area for axle counter sensors
	; TODO 2020-08-20 CLFEY: Adjust areas.
	; Split in two areas: One for XX cm separation => speeds less than 120 km/h allowed. Another (above 1.0 meter separation?) for speeds above 120 km/h. See new BN spec.
	(setLayer layer_Turnout_ForbiddenAreaForAxleCounterSensor)
	(setq pts (getAreaOne Drawing_Number))
	(command "._PLINE" (foreach pt pts (command pt)))
	(drawHatchOptions _sparseHatch_ 0 0.01 "ANSI37" "_LAST")
	(setq pts (getAreaTwo Drawing_Number))
	(command "._PLINE" (foreach pt pts (command pt)))
	(drawHatchOptions _sparseHatch_ 0 0.01 "ANSI37" "_LAST")

	(moveToQuadrant quadrant "_ALL")
	(createSchematicBlockFromCurrentGraphics blockName)
)
