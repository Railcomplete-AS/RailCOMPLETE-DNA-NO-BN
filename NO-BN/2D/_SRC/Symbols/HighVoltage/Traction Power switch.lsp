;=========================================================================================================================
;
; Traction Power switch.lsp 
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================

; OCS in/out switches, to be mounted on top of OCS masts

(defun C:TRACTION-POWER-SWITCH ( / )

	(SKILLEBRYTER)
  	(LASTSKILLEBRYTER)
  	(JORDINGSBRYTER)
  	(SKINNEJORD)
)



(defun SKILLEBRYTER ( / blockName )
	;--------- manuelle brytere ---------
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-AAPEN")
	(drawSkillebryter "AAPEN" 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
  
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-LUKKET")
	(drawSkillebryter "LUKKET" 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
  
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-TOPOLET-AAPEN")
	(drawSkillebryter "AAPEN" 0)
	(drawSkillebryter "AAPEN" (- 6.0))
	(command "._LINE" (list 3.0 1.6629) (list 3.0 (+ (- 6.0) 1.6629)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
  
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-TOPOLET-LUKKET")
	(drawSkillebryter "LUKKET" 0)
	(drawSkillebryter "LUKKET" (- 6.0))
	(command "._LINE" (list 3.0 0) (list 3.0 (- 6.0)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
  
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-TREPOLET-AAPEN")
	(drawSkillebryter "AAPEN" 0)
	(drawSkillebryter "AAPEN" (- 6.0))
	(drawSkillebryter "AAPEN" (- 12.0))
	(command "._LINE" (list 3.0 1.6629) (list 3.0 (+ (- 12.0) 1.6629)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	(setq blockName "NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-TREPOLET-LUKKET")
	(drawSkillebryter "LUKKET" 0)
	(drawSkillebryter "LUKKET" (- 6.0))
	(drawSkillebryter "LUKKET" (- 12.0))
	(command "._LINE" (list 3.0 0) (list 3.0 (- 12.0)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	;--------- motoriserte brytere ---------
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-MOTORISERT-AAPEN")
	(drawMotor "AAPEN")
	(drawSkillebryter "AAPEN" 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
  
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-MOTORISERT-LUKKET")
	(drawMotor "LUKKET")
	(drawSkillebryter "LUKKET" 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	(setq blockName "NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-MOTORISERT-TOPOLET-AAPEN")
	(drawMotor "AAPEN")
	(drawSkillebryter "AAPEN" 0)
	(drawSkillebryter "AAPEN" (- 6.0))
	(command "._LINE" (list 3.0 1.6629) (list 3.0 (+ (- 6.0) 1.6629)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
	
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-MOTORISERT-TOPOLET-LUKKET")
	(drawMotor "LUKKET")
	(drawSkillebryter "LUKKET" 0)
	(drawSkillebryter "LUKKET" (- 6.0))
	(command "._LINE" (list 3.0 0) (list 3.0 (- 6.0)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	(setq blockName "NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-MOTORISERT-TREPOLET-AAPEN")
	(drawMotor "AAPEN")
	(drawSkillebryter "AAPEN" 0)
	(drawSkillebryter "AAPEN" (- 6.0))
	(drawSkillebryter "AAPEN" (- 12.0))
	(command "._LINE" (list 3.0 1.6629) (list 3.0 (+ (- 12.0) 1.6629)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
  
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-MOTORISERT-TREPOLET-LUKKET")
	(drawMotor "LUKKET")
	(drawSkillebryter "LUKKET" 0)
	(drawSkillebryter "LUKKET" (- 6.0))
	(drawSkillebryter "LUKKET" (- 12.0))
	(command "._LINE" (list 3.0 0) (list 3.0 (- 12.0)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
)
	
	
	
(defun LASTSKILLEBRYTER ( / blockName )
	;--------- manuelle brytere ---------
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-AAPEN")
	(drawLastSkillebryter "AAPEN" 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
	
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-LUKKET")
	(drawLastSkillebryter "LUKKET" 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
  
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-TOPOLET-AAPEN")
	(drawLastSkillebryter "AAPEN" 0)
	(drawLastSkillebryter "AAPEN" (- 6.0))
	(command "._LINE" (list 3.0 1.6629) (list 3.0 (+ (- 6.0) 1.6629)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
  
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-TOPOLET-LUKKET")
	(drawLastSkillebryter "LUKKET" 0)
	(drawLastSkillebryter "LUKKET" (- 6.0))
	(command "._LINE" (list 3.0 0) (list 3.0 (- 6.0)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
	
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-TREPOLET-AAPEN")
	(drawLastSkillebryter "AAPEN" 0)
	(drawLastSkillebryter "AAPEN" (- 6.0))
	(drawLastSkillebryter "AAPEN" (- 12.0))
	(command "._LINE" (list 3.0 1.6629) (list 3.0 (+ (- 12.0) 1.6629)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
  
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-TREPOLET-LUKKET")
	(drawLastSkillebryter "LUKKET" 0)
	(drawLastSkillebryter "LUKKET" (- 6.0))
	(drawLastSkillebryter "LUKKET" (- 12.0))
	(command "._LINE" (list 3.0 0) (list 3.0 (- 12.0)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
	
	;--------- motoriserte brytere ---------
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-MOTORISERT-AAPEN")
	(drawMotor "AAPEN")
	(drawLastSkillebryter "AAPEN" 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
	
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-MOTORISERT-LUKKET")
	(drawMotor "LUKKET")
	(drawLastSkillebryter "LUKKET" 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
	
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-MOTORISERT-TOPOLET-AAPEN")
	(drawMotor "AAPEN")
	(drawLastSkillebryter "AAPEN" 0)
	(drawLastSkillebryter "AAPEN" (- 6.0))
	(command "._LINE" (list 3.0 1.6629) (list 3.0 (+ (- 6.0) 1.6629)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	(setq blockName "NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-MOTORISERT-TOPOLET-LUKKET")
	(drawMotor "LUKKET")
	(drawLastSkillebryter "LUKKET" 0)
	(drawLastSkillebryter "LUKKET" (- 6.0))
	(command "._LINE" (list 3.0 0) (list 3.0 (- 6.0)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
  
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-MOTORISERT-TREPOLET-AAPEN")
	(drawMotor "AAPEN")
	(drawLastSkillebryter "AAPEN" 0)
	(drawLastSkillebryter "AAPEN" (- 6.0))
	(drawLastSkillebryter "AAPEN" (- 12.0))
	(command "._LINE" (list 3.0 1.6629) (list 3.0 (+ (- 12.0) 1.6629)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
	
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-MOTORISERT-TREPOLET-LUKKET")
	(drawMotor "LUKKET")
	(drawLastSkillebryter "LUKKET" 0)
	(drawLastSkillebryter "LUKKET" (- 6.0))
	(drawLastSkillebryter "LUKKET" (- 12.0))
	(command "._LINE" (list 3.0 0) (list 3.0 (- 12.0)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
)



(defun SKINNEJORD ( / blockName )
	(setq blockName "NO-BN-2D-JBTKL-JORDING-SKINNEJORD")
	(drawSkinneJord nil)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
)



(defun JORDINGSBRYTER ( / blockName )
	;--------- manuelle brytere ---------
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-AAPEN")
	(drawSkinneJord (list 5.5426 3.2))
	(drawSkillebryter "AAPEN" 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	(setq blockName "NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-LUKKET")
	(drawSkinneJord (list 5.5426 3.2))
	(drawSkillebryter "LUKKET" 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	(setq blockName "NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-TOPOLET-AAPEN")
	(drawSkinneJord (list 5.5426 3.2))
	(drawSkillebryter "AAPEN" 0)
	(drawSkillebryter "AAPEN" (- 6.0))
	(command "._LINE" (list 3.0 1.6629) (list 3.0 (+ (- 6.0) 1.6629)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	(setq blockName "NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-TOPOLET-LUKKET")
	(drawSkinneJord (list 5.5426 3.2))
	(drawSkillebryter "LUKKET" 0)
	(drawSkillebryter "LUKKET" (- 6.0))
	(command "._LINE" (list 3.0 0) (list 3.0 (- 6.0)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	(setq blockName "NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-TREPOLET-AAPEN")
	(drawSkinneJord (list 5.5426 3.2))
	(drawSkillebryter "AAPEN" 0)
	(drawSkillebryter "AAPEN" (- 6.0))
	(drawSkillebryter "AAPEN" (- 12.0))
	(command "._LINE" (list 3.0 1.6629) (list 3.0 (+ (- 12.0) 1.6629)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	(setq blockName "NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-TREPOLET-LUKKET")
	(drawSkinneJord (list 5.5426 3.2))
	(drawSkillebryter "LUKKET" 0)
	(drawSkillebryter "LUKKET" (- 6.0))
	(drawSkillebryter "LUKKET" (- 12.0))
	(command "._LINE" (list 3.0 0) (list 3.0 (- 12.0)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	;--------- motoriserte brytere ---------
	(setq blockName "NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-MOTORISERT-AAPEN")
	(drawSkinneJord (list 5.5426 3.2))
	(drawMotor "AAPEN")
	(drawSkillebryter "AAPEN" 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	(setq blockName "NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-MOTORISERT-LUKKET")
	(drawSkinneJord (list 5.5426 3.2))
	(drawMotor "LUKKET")
	(drawSkillebryter "LUKKET" 0)
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	(setq blockName "NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-MOTORISERT-TOPOLET-AAPEN")
	(drawSkinneJord (list 5.5426 3.2))
	(drawMotor "AAPEN")
	(drawSkillebryter "AAPEN" 0)
	(drawSkillebryter "AAPEN" (- 6.0))
	(command "._LINE" (list 3.0 1.6629) (list 3.0 (+ (- 6.0) 1.6629)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	(setq blockName "NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-MOTORISERT-TOPOLET-LUKKET")
	(drawSkinneJord (list 5.5426 3.2))
	(drawMotor "LUKKET")
	(drawSkillebryter "LUKKET" 0)
	(drawSkillebryter "LUKKET" (- 6.0))
	(command "._LINE" (list 3.0 0) (list 3.0 (- 6.0)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	(setq blockName "NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-MOTORISERT-TREPOLET-AAPEN")
	(drawSkinneJord (list 5.5426 3.2))
	(drawMotor "AAPEN")
	(drawSkillebryter "AAPEN" 0)
	(drawSkillebryter "AAPEN" (- 6.0))
	(drawSkillebryter "AAPEN" (- 12.0))
	(command "._LINE" (list 3.0 1.6629) (list 3.0 (+ (- 12.0) 1.6629)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale

	(setq blockName "NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-MOTORISERT-TREPOLET-LUKKET")
	(drawSkinneJord (list 5.5426 3.2))
	(drawMotor "LUKKET")
	(drawSkillebryter "LUKKET" 0)
	(drawSkillebryter "LUKKET" (- 6.0))
	(drawSkillebryter "LUKKET" (- 12.0))
	(command "._LINE" (list 3.0 0) (list 3.0 (- 12.0)) "")
	(createSchematicBlockFromCurrentGraphics blockName)
	(createGeoBlockInAllPaperScalesFromBlock blockName 0.5 blockName) ; 50% size is 1:500 scale
)



;==========================
; draw...X...() functions
;==========================
(defun drawSkillebryter ( variation y / len len2 len3 )
	(setq 
		len 6.0
		len2 6.4
		len3 2.4
	)
	(if (= variation "AAPEN")
		(command 
			"._LINE" (list 0 y) (list len2 y) ""
			"._ROTATE" "_LAST" "" (list 0 y) 30
		)
		(command "._LINE" (list 0 y) (list len y) "")
	)
	(command "._LINE" (list len (+ y (/ len3 2.0))) (list len (+ y (- (/ len3 2.0)))) "")
)



(defun drawLastSkilleBryter ( variation y / r len len2 len3 )
	(setq 
		r 0.8
		len 6.4
		len2 6.0
		len3 2.4
	)
	(if (= variation "AAPEN")
		(command
			"._LINE" (list 0 y) (list len y) ""
			"._ROTATE" "_LAST" "" (list 0 y) 30
		)
		(command "._LINE" (list 0 y) (list (- len2 (* r 2.0)) y) "")
	)
	(command 
		"._LINE" (list len2 (+ y (/ len3 2.0))) (list len2 (+ y (- (/ len3 2.0)))) ""
		"._CIRCLE" (list (- len2 r) y) r
	)
)



(defun drawMotor ( variation / dist r textHeight )
	(setq 
		dist 3.0
		r 1.65
		textHeight 2.0
	)
	(command "._CIRCLE" (list dist (+ dist r)) r)
	(addText "M" (list dist (+ dist r)) textHeight 0 "iso" "_MC")
	(if (= variation "AAPEN")
		(command "._LINE" (list dist dist) (list dist (- dist 1.2679)) "")
		(command "._LINE" (list dist dist) (list dist 0) "")
    )
)



(defun drawSkinneJord ( offset / len1 len2 r x y )
	(setq 
		len1 4.0
		len2 (/ 2.5 2.0)
		r 0.5
		x (/ 2.5 2.0)
		y 0.9
	)
	(command 
		"._LINE" (list 0 0) (list len1 0) ""
		"._LINE" (list len1 len2) (list len1 (- len2)) ""
		"._CIRCLE" (list len1 (+ len2 r)) r
		"._RECTANGLE" (list (- len1 x) (- len2)) (list (+ len1 x) (- (+ len2 y)))
	)
	(drawHatchFromPoint 0.02 (list len1 (- (+ len2 (/ y 2.0)))) 0 0)
	(if offset
		(command "._MOVE" "_ALL" "" (list 0 0) offset)
    )
)
