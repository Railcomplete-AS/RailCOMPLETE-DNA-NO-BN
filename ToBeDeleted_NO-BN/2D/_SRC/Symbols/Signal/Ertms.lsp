; Comments added by Eirik Kjeken for educational purposes.
(defun C:ERTMS ()					; <-- defines a command function for autoCAD with name ERTMS and no input parameters
	(setCadSystemDefaults)			; Define ISO fonts etc
	(newBlock (E34 nil "HSIDE"))		; <-- constructs new blocks with the possible variations of the signs E34 and E35. 
	(newBlock (E34 nil "VSIDE"))		; The command ERTMS constructs all the blocks by calling the functions E34 and E35 with 
	(newBlock (E34 "AAK" "HSIDE"))	; all possible variations of input parameters. The rest of this file is used for defining the functions E34 and E35.
	(newBlock (E34 "AAK" "VSIDE"))
	(newBlock (E34 nil "OVER"))
	(newBlock (E34 "AAK" "OVER"))
	(newBlock (E35 nil "HSIDE"))
	(newBlock (E35 nil "VSIDE"))
	(newBlock (E35 "AAK" "HSIDE"))
	(newBlock (E35 "AAK" "VSIDE"))
	(newBlock (E35 nil "OVER"))
	(newBlock (E35 "AAK" "OVER"))
	(ERTMS-PLANOVERGANG "FORSIGNAL" nil)
	(ERTMS-PLANOVERGANG "FORSIGNAL" "AAK")
	(ERTMS-PLANOVERGANG nil nil)
	(ERTMS-PLANOVERGANG nil "AAK")
	(ERTMS-SHUNTING-AREA "BEGIN" nil)
	(ERTMS-SHUNTING-AREA "BEGIN" "AAK")
	(ERTMS-SHUNTING-AREA "END" nil)
	(ERTMS-SHUNTING-AREA "END" "AAK")
	(ERTMS-SUPERVISED-AREA "END" nil)
	(ERTMS-SUPERVISED-AREA "END" "AAK")
)



(defun E34 							;<-- defines a function with name E34
	(feste dir / blockName pole)	;<-- lists parameters and local variables of function. 
			; Input parameters to the left of /, local variables to the right.
			; Local variables are deleted after function call. 
			; NB: If an identifier is not declared as local, but is used in (setq xyzzy ...) then 'xyzzy' will be globally accessible afterwards.
			; All lines below this one are function bodies, that perform operations on the parameters.
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-ERTMS-E34-STEDSSKILT"			;<-- assigns values to the local variables blockname and pole. 
		pole 4
	)
	(if (= feste "AAK")			;<-- checks if the sign is of type "AAK", and appends "AAK" to the name if so.
		(setq blockName (strcat blockName "-" feste))	
	)
	(if dir				;<-- checks if direction is assigned, and appends the value to the name. Only false if dir=nil.
		(setq blockName (strcat blockName "-" dir))
	)
	(command
		"._RECTANG" "0,0" "2.0,2.0" ;<-- calls autoCAD commands. This is the where the actual graphics for the sign is constructed. First line constructs a rectangle with a corner in (0,0) and (2,2)
		"._PLINE"		       ;<-- draws multiple polylines by calling the autoCAD comman polyline. This is where the arrow is constructed.
			"0.01,0.9993"
			"0.9089,0.2914"
			"0.9067,0.7328"
			"1.8793,0.7329"
			(strcat "@0," (rtos (/ 0.5514 2.0))) ;<-- strcat combines two strings, in this case "@0" with the string representation of 0.5514/2 (rtos gives the sting representation of a number). the @ specifies that the coordinates are relative to the last specified coordinate.
			""					;<-- "" gives 'enter', which ends the polyline. This has drawn the "bottom half" of the arrow.
		"._MIRROR" "L" "" "0,0.9993" "1,0.9993" "No" ;<-- mirrors the arrow. L + "" selects the last object drawn, in this case the polyline. 
	)						;Next two points defines mirror axis. "No" mirrors without deleting the mirrored polyline.
	(command "._SCALE" "ALL" "" "0,0" 2)			;<-- scales all objects in drawing by 2, at the base point (0,0)
	(if (= dir "VSIDE")					;<-- checks if direction is "right", and mirrors the whole drawing around the line defined by
		(command "._MIRROR" "ALL" "" "2,0" "2,1" "Y")	;the points (2,0) and (2,1) (center line of the rectangle), then deletes the objects that are mirrored.
	)
	(if (= feste "AAK")					;<-- checks if sign type is "AAK"
		(progn						
			(if (= dir "OVER")
				(command 
					"._ROTATE" "ALL" "" "0,0" "90"
					"._MOVE" "ALL" "" "Displacement" (strcat "2.0," (rtos (- -2 4)))
					"._LINE" "0,0" (list 0 (- 2)) ""
				)
				(command
					"._MOVE" "ALL" "" "Displacement" (strcat "-2," (rtos (- -2 (* 2 2.0))))
					"._LINE" "0,0" (list 0 (- 2)) ""
				)
			);if
		)
		(progn
			(if (= dir "OVER")
				(command "._MOVE" "ALL" "" "Displacement" "0,-2.0")
			;else
				(command
					"._MOVE" "ALL" "" "Displacement" "-2,0"
					"._ROTATE" "ALL" "" "0,0" "-90"
				)
			);if
			(command
				"._MOVE" "ALL" "" "Displacement" (strcat (rtos pole) ",0")
			)
			(drawPole 0 pole)
			(drawBase)
			(command "._ROTATE" "ALL" "" "0,0" "90")
		)
	)
	blockName
)



(defun E35 (feste dir / blockName pole)
	(E34 feste dir)
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-ERTMS-E35-STOPPSKILT"
		pole 4
	)
	(if feste
		(setq blockName (strcat blockName "-" feste))
	)
	(if dir
		(setq blockName (strcat blockName "-" dir))
	)
	(if (= feste "AAK")
		(drawHatchSelectPoint 0.05 (list 0 -2.1) 0 0.01)
		(drawHatchSelectPoint 0.05 (list 0.1 (+ pole 0.1)) 0 0.01) ;yellow 1.2
	)
	blockName
)



(defun ERTMS-PLANOVERGANG (forsignal feste / blockName pole x y)
	(setq
		blockName "NO-BN-2D-JBTSI-SIGNAL-ERTMS-PLANOVERGANG"
		pole 4
		x 2.0 ;x og y for omsluttende rektangel
		y x
	)
	(if forsignal
		(setq blockName (strcat blockName "-" forsignal))
	)
	(if feste
		(setq blockName (strcat blockName "-" feste))
	)
	(command
		"._RECTANGLE" (list (/ x -2.0) 0) (list (/ x 2.0) y)
		"._RECTANGLE" (list (* x -0.3) (* y 0.1)) (list (* x 0.3) (* y 0.15))
		"._RECTANGLE" (list (* x -0.05) (* y 0.15)) (list (* x 0.05) (* y 0.9))
		"._RECTANGLE" (list (* x -0.3) (* y 0.75)) (list (* x 0.3) (* y 0.765))
		"._ROTATE" "L" "" (list 0 (* y 0.7575)) 30
		"._MIRROR" "L" "" (list 0 (* y 0.7575)) (list 1 (* y 0.7575)) "N"
	)
	(if (= feste "AAK")
		(progn
			(if (= forsignal "FORSIGNAL")
				(addText "PLO-Fs" (list 0 (/ y -10.0)) 1 0 "iso" "TC")
				(addText "PLO" (list 0 (/ y -10.0)) 1 0 "iso" "TC")
			)
			(command
				"._MOVE" "ALL" "" "0,0" (list 0 (- y))
				"._LINE" "0,0" (list 0 (* y 0.4)) ""
			)
		)
		(progn
			(if (= forsignal "FORSIGNAL")
				(addText "PLO-Fs" (list 0 (+ y (/ y 10.0))) 1 0 "iso" "BC")
				(addText "PLO" (list 0 (+ y (/ y 10.0))) 1 0 "iso" "BC")
			)
			(command
				"._ROTATE" "ALL" "" "0,0" -90
				"._MOVE" "ALL" "" "0,0" (list pole 0)
			)
			(drawBase)
			(drawPole 0 pole)
			(command "._ROTATE" "ALL" "" "0,0" "90")
		)
	)
	(newBlock blockName)
	blockName
)



(defun ERTMS-SHUNTING-AREA ( beginOrEnd feste / blockName pole x y txtHeight)
	(setq
		blockName (strcat "NO-BN-2D-JBTSI-SIGNAL-ERTMS-SHUNTING-AREA-" beginOrEnd)
		pole 4
		x 2.0 ;x og y for omsluttende rektangel
		y x
		txtHeight (* y 0.60)
	)
	(if feste
		(setq blockName (strcat blockName "-" feste))
	)
	; Surrounding box:
	(command
		"._RECTANGLE" (list (/ x -2.0) (/ y -2)) (list (/ x 2.0) (/ y 2))
	)
	; Text 'SH' for 'shunting':
	(addMText "SH" "0,0" txtHeight 3 0 "iso" "MC")
	(if (= beginOrEnd "END")
		(command ; Add three inclined 'slash' lines:
			"._LINE" (list (/ x -2) (/ y -2)) (list (/ x 2) (/ y 2)) "" ; Diagonal
			"._LINE" (list (/ x -2) (/ y -3)) (list (/ x 3) (/ y 2)) "" ; Above diagonal
			"._LINE" (list (/ x -3) (/ y -2)) (list (/ x 2) (/ y 3)) "" ; Below diagonal
		)
	)
	(if (= feste "AAK")
		(progn
			(command
				"._MOVE" "ALL" "" "0,0" (list 0 (- y))
				"._LINE" "0,0" (list 0 (/ y -2)) ""
			)
		);else:
		(progn
			(command
				"._ROTATE" "ALL" "" "0,0" -90 ; Rotate right so that drawBase and drawPole can work properly...
				"._MOVE" "ALL" "" "0,0" (list (+ pole (/ y 2)) 0)
			)
			(drawBase)
			(drawPole 0 pole)
			(command "._ROTATE" "ALL" "" "0,0" "90") ; Rotate left - back tp upright board and pole
		);endif
	)
	(newBlock blockName)
)



(defun ERTMS-SUPERVISED-AREA ( beginOrEnd feste / blockName pole x y yoffs ptul ptll ptlr ptlm ptbm)
	(setq
		blockName (strcat "NO-BN-2D-JBTSI-SIGNAL-ERTMS-SUPERVISED-AREA-" beginOrEnd)
		pole 4
		x 2.0 ;x og y for omsluttende rektangel
		y x
	)
	(if feste
		(setq blockName (strcat blockName "-" feste))
	)
	; Surrounding box:
	(command
		"._RECTANGLE" (list (/ x -2) (/ y -2)) (list (/ x 2) (/ y 2))
	)
	;Draw a shunting signal symbol (for European style right-side driving)
	(command
		(setq 
			yoffs (/ y 10) ; The dwarf signal house shape will be displaced up to make space for a short pole
			ptul (list (/ x -4) (+ yoffs (/ y 4))) ; Point upper left corner
			ptll (list (/ x -4) (+ yoffs (/ y -4))) ; Point lower left corner
			ptlr (list (/ x 4) (+ yoffs (/ y -4))) ; Point lower right corner
			ptlm (list 0 (+ yoffs (/ y -4))) ; Point lower midpoint
			ptbm (list 0 (+ yoffs (/ y -2))) ; Point bottom of pole midpoint
		)
		"._PLINE" ptul ptll ptlr "ARC" "DI" 90 ptul ""
		"._PLINE" ptlm ptbm ""
	)
	(if (= beginOrEnd "END")
		(command ; Add three inclined 'slash' lines:
			"._LINE" (list (/ x -2) (/ y -2)) (list (/ x 2) (/ y 2)) "" ; Diagonal
			"._LINE" (list (/ x -2) (/ y -3)) (list (/ x 3) (/ y 2)) "" ; Above diagonal
			"._LINE" (list (/ x -3) (/ y -2)) (list (/ x 2) (/ y 3)) "" ; Below diagonal
		)
	)
	(if (= feste "AAK")
		(progn
			(command
				"._MOVE" "ALL" "" "0,0" (list 0 (- y))
				"._LINE" "0,0" (list 0 (/ y -2)) ""
			)
		);else:
		(progn
			(command
				"._ROTATE" "ALL" "" "0,0" -90 ; Rotate right so that drawBase and drawPole can work properly...
				"._MOVE" "ALL" "" "0,0" (list (+ pole (/ y 2)) 0)			)
			(drawBase)
			(drawPole 0 pole)
			(command "._ROTATE" "ALL" "" "0,0" "90") ; Rotate left - back tp upright board and pole
		);endif
	)
	(newBlock blockName)
)

