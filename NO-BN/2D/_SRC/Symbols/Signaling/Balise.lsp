;=========================================================================================================================
;
; Balise.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; ATC balise

(defun C:BALISE ( / ) 

	(BALISEGRUPPE)
	
	(BALISE 1) ; variation 1 = tom/fast = empty / fixed telegram
	(BALISE 2) ; variation 2 = tom/styrt = empty / controlled telegram
	(BALISE 3) ; variation 3 = fylt/fast = hatched / fixed telegram
	(BALISE 4) ; variation 4 = fylt/styrt = hatched / controlled telegram
)



(defun BALISE ( variation / blockName side h dh bx by A B C mx my paperScale dwgScale  )
	; NB This routine contains metric info which must be generated for each scale in turn.
	;https://trv.jbv.no/wiki/Signal/Bygging/ATC/Parallellbaliser_og_kodere#label-fig:Metallfritt område, og avstand til kryssende kabler
	;Metallfritt område
	;
	; Uses Global 'paperScaleList'.
	;
	(setq 
		conversionScale 0.5 ; reduce schematic graphics by 0.5 to achieve a suitable 1:500 scale

		; metal free area:
		bx 0.6	; large balise physical length [m]
		by 0.5 	; large balise physical width [m]
		A 0.5  		; Metal-free tolerance in track direction, low-mileage side, ref Bane NOR minibalise installation manual
		B 0.5  		; Metal-free tolerance track direction, hi-mileage side
		C 0.2  		; Metal-free tolerance across track direction
		mx (+ (/ bx 2) A)	; metal-free area along track direction, from centre of balise
		my (+ (/ by 2) C)	; metal-free area across track direction, from centre of balise
	)

	; Schematic symbol:
	(setq blockName (drawBalise variation)) ; Schematic size graphics - returns block name
	(createSchematicBlockFromCurrentGraphics blockName)
	
	; Geo symbols:
	(foreach paperScale paperScaleList
	
		(drawBalise variation) ; Schematic size graphics
		(command "._SCALE" "_ALL" "" "0,0" (/ (* conversionScale (atof paperScale)) 250.0))  ; convert graphics into 1:250 scale (default modeling scale)

		; Add metric snap lines:
		(setLayerAndObjectColor layer_Balise_3m_Separation "_ByLayer")
		(command 
			"._PLINE" (list 3 0) "_ARC" "_CE" (list 0 0) "A" 15 ""
			"._MIRROR" "_LAST" "" (list 0 0) (list 0 1) "_NO"
			"._MIRROR" "_LAST" "" (list 0 0) (list 1 0) "_NO"
			"._MIRROR" "_LAST" "" (list 0 0) (list 0 1) "_NO"
		)
		(addText "3" (list (- 2.9) (- 0.5)) 0.2 0 "iso" "_TL")
		(command "._MIRROR" "_LAST" "" (list 0 0) (list 0 1) "_NO")
		
		(setLayerAndObjectColor layer_Balise_8m_Separation "_ByLayer")
		(command 
			"._PLINE" (list 8 0) "_ARC" "_CE" (list 0 0) "A" 9 ""
			"._MIRROR" "_LAST" "" (list 0 0) (list 0 1) "_NO"
			"._MIRROR" "_LAST" "" (list 0 0) (list 1 0) "_NO"
			"._MIRROR" "_LAST" "" (list 0 0) (list 0 1) "_NO"
		)
		(addText "8" (list (- 7.9) (- 0.95)) 0.2 0 "iso" "_TL")
		(command "._MIRROR" "_LAST" "" (list 0 0) (list 0 1) "_NO")

		(setLayerAndObjectColor layer_Balise_12m_Separation "_ByLayer")
		(command 
			"._PLINE" (list 12 0) "_ARC" "_CE" (list 0 0) "A" 10 ""
			"._MIRROR" "_LAST" "" (list 0 0) (list 0 1) "_NO"
			"._MIRROR" "_LAST" "" (list 0 0) (list 1 0) "_NO"
			"._MIRROR" "_LAST" "" (list 0 0) (list 0 1) "_NO"
		)
		(addText "12" (list (- 11.8) (- 1.8)) 0.2 0 "iso" "_TL")
		(command "._MIRROR" "_LAST" "" (list 0 0) (list 0 1) "_NO")
		
		; Add metal-free area:
		(setLayerAndObjectColor layer_Balise_MetalFreeArea "_ByLayer")
		(command "._RECTANGLE" (list (- mx) (- my))   (list (+ mx) (+ my)))
		 ; As (createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)() but it does not iterate over all scales:
		(createGeoBlockInCurrentPaperScaleFromCurrentGraphics paperScale blockName)
	)
)


 		
(defun BALISEGRUPPE ( / blockName )
	(setq 
		blockName "NO-BN-2D-JBTSI-ATC-BALISEGRUPPE"
	)
	; Equilateral triangle, side = 6, dashed line:
	(setLayerAndObjectColor layer_Zero "_ByBlock")
	(command
		"._LINE" "0,1.5981" "-0.3,1.0785" ""
		"._LINE" "-0.6,0.5588" "-0.9,0.0392" ""
		"._LINE" "-1.2,-0.4804" "-1.5,-1" "-0.9,-1" ""
		"._LINE" "-0.3,-1" "0,-1" ""
		"._MIRROR" "_ALL" "" "0,0" "0,1" "_NO"
		"._MOVE" "_ALL" "" "0,0" "0,1"
	)
	
	; Twice as big as balise symbol (Bane NOR wants it that way):
	(command "._SCALE" "_ALL" "" "0,0" 2.0)
	
	; Store schematic block to BLOCK table, erase graphics from modelspace:
	(createSchematicBlockFromCurrentGraphics blockName) 
	
	; Reduce to 1/4 for scale 1:500 etc. Retrieve schematic block, rescale & store as scaled block(s).
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.25 blockName)
)	



;==========================
; draw...X...() functions
;==========================
(defun drawBalise ( variation / blockName side h dh )
	; Draw balise triangle in schematic size
	(setq 
		; Balise triangle:
		side 3.0 	; equilateral triangle symbol side
		h (* side (sin (D->R 60))) ; height of triangle, side * sqrt(3)/2
		dh 0.10  ; The height of the "controlled balise" bar above the triangle
	)
	
	(setLayer layer_Zero)
	(command 
		"._COLOR" "_ByBlock"
		"._PLINE"
			(list (/ side -2) 0)
			(list (/ side 2) 0)
			(strcat "@" (rtos side) "<120")
			"_CLOSE"
	)
	(cond 
		((= variation 1)
			(setq blockName "NO-BN-2D-JBTSI-ATC-BALISE-TOM-FAST")
		)
		((= variation 2)
			(setq blockName "NO-BN-2D-JBTSI-ATC-BALISE-TOM-STYRT")
			(command 
				"._RECTANGLE" 
					(list (/ side -2.0) h)
					(list (/ side 2.0) (+ h dh))
			)
			(drawHatch _denseHatch_)
		)
		((= variation 3)
			(drawHatchFromPoint 0.02 "0.1,0.1" 0 0.1)
			(setq blockName "NO-BN-2D-JBTSI-ATC-BALISE-FYLT-FAST")
		)
		((= variation 4)
			(drawHatchFromPoint 0.02 "0.1,0.1" 0 0.1)
			(setq blockName "NO-BN-2D-JBTSI-ATC-BALISE-FYLT-STYRT")
			(command 
				"._RECTANGLE"
					(list (/ side -2.0) h)
					(list (/ side 2.0) (+ h dh))
			)
			(drawHatch _denseHatch_)
		)
	)
	(addAtt "TEKSTOVER" "Tekst over" "" (list 0 (* 1.333 side)) 1.0 0 "iso" "_MC" _lockPosition_)
	(addAtt "TEKSTUNDER" "Tekst under" "" (list 0 (- (* 0.8 side))) 1.0 0 "iso" "_MC" _lockPosition_)
	blockName
)
