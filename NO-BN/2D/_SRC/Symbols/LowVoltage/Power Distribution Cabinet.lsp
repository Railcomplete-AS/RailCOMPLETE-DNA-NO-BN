;=========================================================================================================================
;
; Power Distribution Cabinet.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; Power distribution xabinet

(defun C:POWER-DISTRIBUTION-CABINET ( / )
	
	(FORDELINGSSKAP)	; Fordelingsskap
	(GRUPPESKAP)		; Gruppeskap (=sporvekselvarmeskap)
	(TOGVARMEPOST)		; Skap for togvarme
)


 
(defun FORDELINGSSKAP ( / blockName x y halfWidth halfHeight )
	(setq 
		blockName "NO-BN-2D-JBTEL-SKAP-FORDELINGSSKAP"
		; TRV:  4.5 x 2.25
		x 2.0 	; Not drawn to scale
		y 1.0  	; Not drawn to scale
		halfWidth (/ x 2)
		halfHeight (/ y 2)
		frontlineHeight (/ y 9)
		pt1 (list (/ x 40) (+ (/ y 6) (/ y 18)))
		pt2 (list (/ x -40) (+ (/ y -6) (/ y 18)))
	)
	(command
		_RECTANGLE_ (list (- halfWidth) (- halfHeight)) (list halfWidth halfHeight)
		_LINE_ (list (- halfWidth)  (+ (- halfHeight) frontlineHeight)) (list halfWidth  (+ (- halfHeight) frontlineHeight)) _ENTER_
		_LINE_ (list (- halfWidth) (+ (- halfHeight) frontlineHeight)) pt1 pt2 (list halfWidth halfHeight) _ENTER_
	)
	(setLayerAndObjectColor layer_Description "_ByLayer")
	(addMText "Fordelings- skap" "0,-0.6" _descriptionTextHeight_ 1.5 0 _rcTextStyle_ _topCenter_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun GRUPPESKAP ( / blockName x y halfWidth halfHeight )
	(setq 
		blockName "NO-BN-2D-JBTEL-SKAP-GRUPPESKAP"
		; TRV:  4.5 x 2.25
		x 2.0 ; Not drawn to scale
		y 1.0 ; Not drawn to scale
		halfWidth (/ x 2)
		halfHeight (/ y 2)
		frontlineHeight (/ y 9)
		pt1 (list (/ x 40) (+ (/ y 6) (/ y 18)))
		pt2 (list (/ x -40) (+ (/ y -6) (/ y 18)))
	)
	(command
		_RECTANGLE_ (list (- halfWidth) (- halfHeight)) (list halfWidth halfHeight)
		_LINE_ (list (- halfWidth)  (+ (- halfHeight) frontlineHeight)) (list halfWidth  (+ (- halfHeight) frontlineHeight)) _ENTER_
		_LINE_ (list (- halfWidth) (+ (- halfHeight) frontlineHeight)) pt1 pt2 (list halfWidth halfHeight) _ENTER_
	)
	(setLayerAndObjectColor layer_Description "_ByLayer")
	(addMText "Gruppeskap" "0,-0.6" _descriptionTextHeight_ 1.5 0 _rcTextStyle_ _topCenter_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun TOGVARMEPOST ( / blockName halfWidth halfHeight )
	(setq 
		blockName "NO-BN-2D-JBTEL-SKAP-TOGVARMEPOST"
		; TRV:  4.5 x 2.25
		x 2.0 ; Not drawn to scale
		y 1.0 ; Not drawn to scale
		halfWidth (/ x 2)
		halfHeight (/ y 2)
	)
	(command
		_RECTANGLE_ (list (- halfWidth) (- halfHeight)) (list halfWidth halfHeight)
		_LINE_ (list (* x -0.5) (* y -0.5)) (list (* x 0.5) (* y 0.5)) _ENTER_
		_LINE_ (list (* x -0.5) (* y 0.5)) (list (* x 0.5) (* y -0.5)) _ENTER_
	)
	(setLayerAndObjectColor layer_Description "_ByLayer")
	(addMText "Togvarme- post" "0,-0.6" _descriptionTextHeight_ 1.5 0 _rcTextStyle_ _topCenter_)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)

