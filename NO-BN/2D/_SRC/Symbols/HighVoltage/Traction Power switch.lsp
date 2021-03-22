;=========================================================================================================================
;
; Traction Power switch.lsp 
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

; OCS in/out switches, to be mounted on top of OCS masts

(defun C:TRACTION-POWER-SWITCH ( / )
	(setq distBelow 0)

	(TraceLevel3 "SKILLEBRYTER")		(SKILLEBRYTER)
  	(TraceLevel3 "LASTSKILLEBRYTER")	(LASTSKILLEBRYTER)
  	(TraceLevel3 "JORDINGSBRYTER")		(JORDINGSBRYTER)
  	(TraceLevel3 "SKINNEJORD")			(SKINNEJORD)

	(setq distBelow nil) ; 'purge' after use - it has done its work as global parameter ...
)



(defun SKILLEBRYTER ( / blockName description )
	;--------- manuelle brytere ---------
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-AAPEN"
		description	(strcat "KL SKILLEBRYTER, " _uAA_ "PEN")
	)
	(DrawSkillebryter "AAPEN" 0)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-LUKKET"
		description	(strcat "KL SKILLEBRYTER, LUKKET")
	)
	(DrawSkillebryter "LUKKET" 0)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-TOPOLET-AAPEN"
		description	(strcat "KL SKILLEBRYTER, TOPOLET " _uAA_ "PEN")
	)
	(DrawSkillebryter "AAPEN" 0)
	(DrawSkillebryter "AAPEN" (- 6.0))
	(command _LINE_ (list 3.0 1.6629) (list 3.0 (+ (- 6.0) 1.6629)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-TOPOLET-LUKKET"
		description	(strcat "KL SKILLEBRYTER, TOPOLET LUKKET")
	)
	(DrawSkillebryter "LUKKET" 0)
	(DrawSkillebryter "LUKKET" (- 6.0))
	(command _LINE_ (list 3.0 0) (list 3.0 (- 6.0)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-TREPOLET-AAPEN"
		description	(strcat "KL SKILLEBRYTER, TREPOLET " _uAA_ "PEN")
	)
	(DrawSkillebryter "AAPEN" 0)
	(DrawSkillebryter "AAPEN" (- 6.0))
	(DrawSkillebryter "AAPEN" (- 12.0))
	(command _LINE_ (list 3.0 1.6629) (list 3.0 (+ (- 12.0) 1.6629)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-TREPOLET-LUKKET"
		description	(strcat "KL SKILLEBRYTER, TREPOLET LUKKET")
	)
	(DrawSkillebryter "LUKKET" 0)
	(DrawSkillebryter "LUKKET" (- 6.0))
	(DrawSkillebryter "LUKKET" (- 12.0))
	(command _LINE_ (list 3.0 0) (list 3.0 (- 12.0)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	;--------- motoriserte brytere ---------
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-MOTORISERT-AAPEN"
		description	(strcat "KL SKILLEBRYTER, MOTORISERT " _uAA_ "PEN")
	)
	(DrawMotor "AAPEN")
	(DrawSkillebryter "AAPEN" 0)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-MOTORISERT-LUKKET"
		description	(strcat "KL SKILLEBRYTER, MOTORISERT LUKKET")
	)
	(DrawMotor "LUKKET")
	(DrawSkillebryter "LUKKET" 0)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-MOTORISERT-TOPOLET-AAPEN"
		description	(strcat "KL SKILLEBRYTER, MOTORISERT TOPOLET " _uAA_ "PEN")
	)
	(DrawMotor "AAPEN")
	(DrawSkillebryter "AAPEN" 0)
	(DrawSkillebryter "AAPEN" (- 6.0))
	(command _LINE_ (list 3.0 1.6629) (list 3.0 (+ (- 6.0) 1.6629)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-MOTORISERT-TOPOLET-LUKKET"
		description	(strcat "KL SKILLEBRYTER, MOTORISERT TOPOLET LUKKET")
	)
	(DrawMotor "LUKKET")
	(DrawSkillebryter "LUKKET" 0)
	(DrawSkillebryter "LUKKET" (- 6.0))
	(command _LINE_ (list 3.0 0) (list 3.0 (- 6.0)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq 
		blockName	"NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-MOTORISERT-TREPOLET-AAPEN"
		description	(strcat "KL SKILLEBRYTER, MOTORISERT TREPOLET " _uAA_ "PEN")
	)
	(DrawMotor "AAPEN")
	(DrawSkillebryter "AAPEN" 0)
	(DrawSkillebryter "AAPEN" (- 6.0))
	(DrawSkillebryter "AAPEN" (- 12.0))
	(command _LINE_ (list 3.0 1.6629) (list 3.0 (+ (- 12.0) 1.6629)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-SKILLEBRYTER-MOTORISERT-TREPOLET-LUKKET"
		description	(strcat "KL SKILLEBRYTER, MOTORISERT TREPOLET LUKKET")
	)
	(DrawMotor "LUKKET")
	(DrawSkillebryter "LUKKET" 0)
	(DrawSkillebryter "LUKKET" (- 6.0))
	(DrawSkillebryter "LUKKET" (- 12.0))
	(command _LINE_ (list 3.0 0) (list 3.0 (- 12.0)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)
	
	
	
(defun LASTSKILLEBRYTER ( / blockName description )
	;--------- manuelle brytere ---------
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-AAPEN"
		description	(strcat "KL LASTSKILLEBRYTER, " _uAA_ "PEN")
	)
	(DrawLastSkilleBryter "AAPEN" 0)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-LUKKET"
		description	(strcat "KL LASTSKILLEBRYTER, LUKKET")
	)
	(DrawLastSkilleBryter "LUKKET" 0)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-TOPOLET-AAPEN"
		description	(strcat "KL LASTSKILLEBRYTER, TOPOLET " _uAA_ "PEN")
	)
	(DrawLastSkilleBryter "AAPEN" 0)
	(DrawLastSkilleBryter "AAPEN" (- 6.0))
	(command _LINE_ (list 3.0 1.6629) (list 3.0 (+ (- 6.0) 1.6629)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq 
		blockName	"NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-TOPOLET-LUKKET"
		description	(strcat "KL LASTSKILLEBRYTER, TOPOLET LUKKET")
	)
	(DrawLastSkilleBryter "LUKKET" 0)
	(DrawLastSkilleBryter "LUKKET" (- 6.0))
	(command _LINE_ (list 3.0 0) (list 3.0 (- 6.0)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-TREPOLET-AAPEN"
		description	(strcat "KL LASTSKILLEBRYTER, TREPOLET " _uAA_ "PEN")
	)
	(DrawLastSkilleBryter "AAPEN" 0)
	(DrawLastSkilleBryter "AAPEN" (- 6.0))
	(DrawLastSkilleBryter "AAPEN" (- 12.0))
	(command _LINE_ (list 3.0 1.6629) (list 3.0 (+ (- 12.0) 1.6629)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq 
		blockName "NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-TREPOLET-LUKKET"
		description	(strcat "KL LASTSKILLEBRYTER, TREPOLET LUKKET")
	)
	(DrawLastSkilleBryter "LUKKET" 0)
	(DrawLastSkilleBryter "LUKKET" (- 6.0))
	(DrawLastSkilleBryter "LUKKET" (- 12.0))
	(command _LINE_ (list 3.0 0) (list 3.0 (- 12.0)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	;--------- motoriserte brytere ---------
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-MOTORISERT-AAPEN"
		description	(strcat "KL LASTSKILLEBRYTER, MOTORISERT " _uAA_ "PEN")
	)
	(DrawMotor "AAPEN")
	(DrawLastSkilleBryter "AAPEN" 0)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-MOTORISERT-LUKKET"
		description	(strcat "KL LASTSKILLEBRYTER, MOTORISERT LUKKET")
	)
	(DrawMotor "LUKKET")
	(DrawLastSkilleBryter "LUKKET" 0)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-MOTORISERT-TOPOLET-AAPEN"
		description	(strcat "KL LASTSKILLEBRYTER, MOTORISERT TOPOLET " _uAA_ "PEN")
	)
	(DrawMotor "AAPEN")
	(DrawLastSkilleBryter "AAPEN" 0)
	(DrawLastSkilleBryter "AAPEN" (- 6.0))
	(command _LINE_ (list 3.0 1.6629) (list 3.0 (+ (- 6.0) 1.6629)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-MOTORISERT-TOPOLET-LUKKET"
		description	(strcat "KL LASTSKILLEBRYTER, MOTORISERT TOPOLET LUKKET")
	)
	(DrawMotor "LUKKET")
	(DrawLastSkilleBryter "LUKKET" 0)
	(DrawLastSkilleBryter "LUKKET" (- 6.0))
	(command _LINE_ (list 3.0 0) (list 3.0 (- 6.0)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
  
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-MOTORISERT-TREPOLET-AAPEN"
		description	(strcat "KL LASTSKILLEBRYTER, MOTORISERT TREPOLET " _uAA_ "PEN")
	)
	(DrawMotor "AAPEN")
	(DrawLastSkilleBryter "AAPEN" 0)
	(DrawLastSkilleBryter "AAPEN" (- 6.0))
	(DrawLastSkilleBryter "AAPEN" (- 12.0))
	(command _LINE_ (list 3.0 1.6629) (list 3.0 (+ (- 12.0) 1.6629)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
	
	(setq
		blockName "NO-BN-2D-JBTKL-BRYTER-LASTSKILLEBRYTER-MOTORISERT-TREPOLET-LUKKET"
		description	(strcat "KL LASTSKILLEBRYTER, MOTORISERT TREPOLET LUKKET")
	)
	(DrawMotor "LUKKET")
	(DrawLastSkilleBryter "LUKKET" 0)
	(DrawLastSkilleBryter "LUKKET" (- 6.0))
	(DrawLastSkilleBryter "LUKKET" (- 12.0))
	(command _LINE_ (list 3.0 0) (list 3.0 (- 12.0)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun SKINNEJORD ( / blockName description )
	(setq
		blockName	"NO-BN-2D-JBTKL-JORDING-SKINNEJORD"
		description	(strcat "KL JORDING, SKINNEJORD")
	)
	(DrawSkinneJord nil)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



(defun JORDINGSBRYTER ( / blockName description )
	;--------- manuelle brytere ---------
	(setq 
		blockName	"NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-AAPEN"
		description	(strcat "KL JORDINGSBRYTER, " _uAA_ "PEN")
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawSkillebryter "AAPEN" 0)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq 
		blockName	"NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-LUKKET"
		description	(strcat "KL JORDINGSBRYTER, LUKKET")
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawSkillebryter "LUKKET" 0)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq 
		blockName	"NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-TOPOLET-AAPEN"
		description	(strcat "KL JORDINGSBRYTER, TOPOLET " _uAA_ "PEN")
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawSkillebryter "AAPEN" 0)
	(DrawSkillebryter "AAPEN" (- 6.0))
	(command _LINE_ (list 3.0 1.6629) (list 3.0 (+ (- 6.0) 1.6629)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-TOPOLET-LUKKET"
		description	(strcat "KL JORDINGSBRYTER, TOPOLET LUKKET")
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawSkillebryter "LUKKET" 0)
	(DrawSkillebryter "LUKKET" (- 6.0))
	(command _LINE_ (list 3.0 0) (list 3.0 (- 6.0)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-TREPOLET-AAPEN"
		description	(strcat "KL JORDINGSBRYTER, TREPOLET " _uAA_ "PEN")
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawSkillebryter "AAPEN" 0)
	(DrawSkillebryter "AAPEN" (- 6.0))
	(DrawSkillebryter "AAPEN" (- 12.0))
	(command _LINE_ (list 3.0 1.6629) (list 3.0 (+ (- 12.0) 1.6629)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-TREPOLET-LUKKET"
		description	(strcat "KL JORDINGSBRYTER, TREPOLET LUKKET")
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawSkillebryter "LUKKET" 0)
	(DrawSkillebryter "LUKKET" (- 6.0))
	(DrawSkillebryter "LUKKET" (- 12.0))
	(command _LINE_ (list 3.0 0) (list 3.0 (- 12.0)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	;--------- motoriserte brytere ---------
	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-MOTORISERT-AAPEN"
		description	(strcat "KL JORDINGSBRYTER, MOTORISERT " _uAA_ "PEN")
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawMotor "AAPEN")
	(DrawSkillebryter "AAPEN" 0)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-MOTORISERT-LUKKET"
		description	(strcat "KL JORDINGSBRYTER, MOTORISERT LUKKET")
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawMotor "LUKKET")
	(DrawSkillebryter "LUKKET" 0)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-MOTORISERT-TOPOLET-AAPEN"
		description	(strcat "KL JORDINGSBRYTER, MOTORISERT TOPOLET " _uAA_ "PEN")
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawMotor "AAPEN")
	(DrawSkillebryter "AAPEN" 0)
	(DrawSkillebryter "AAPEN" (- 6.0))
	(command _LINE_ (list 3.0 1.6629) (list 3.0 (+ (- 6.0) 1.6629)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-MOTORISERT-TOPOLET-LUKKET"
		description	(strcat "KL JORDINGSBRYTER, MOTORISERT TOPOLET LUKKET")
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawMotor "LUKKET")
	(DrawSkillebryter "LUKKET" 0)
	(DrawSkillebryter "LUKKET" (- 6.0))
	(command _LINE_ (list 3.0 0) (list 3.0 (- 6.0)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-MOTORISERT-TREPOLET-AAPEN"
		description	(strcat "KL JORDINGSBRYTER, MOTORISERT TREPOLET " _uAA_ "PEN")
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawMotor "AAPEN")
	(DrawSkillebryter "AAPEN" 0)
	(DrawSkillebryter "AAPEN" (- 6.0))
	(DrawSkillebryter "AAPEN" (- 12.0))
	(command _LINE_ (list 3.0 1.6629) (list 3.0 (+ (- 12.0) 1.6629)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

	(setq
		blockName	"NO-BN-2D-JBTKL-BRYTER-JORDINGSBRYTER-MOTORISERT-TREPOLET-LUKKET"
		description	(strcat "KL JORDINGSBRYTER, MOTORISERT TREPOLET LUKKET")
	)
	(DrawSkinneJord (list 5.5426 3.2))
	(DrawMotor "LUKKET")
	(DrawSkillebryter "LUKKET" 0)
	(DrawSkillebryter "LUKKET" (- 6.0))
	(DrawSkillebryter "LUKKET" (- 12.0))
	(command _LINE_ (list 3.0 0) (list 3.0 (- 12.0)) _ENTER_)
	(AddDescriptionBelowOrigo description distBelow)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
)



;==========================
; Draw...X...() functions
;==========================
(defun DrawSkillebryter ( variation y / len len2 len3 )
	(setq 
		len 6.0
		len2 6.4
		len3 2.4
	)
	(SetLayer layDef_Zero)
	(if (= variation "AAPEN")
		(command 
			_LINE_ (list 0 y) (list len2 y) _ENTER_
			_ROTATE_ _lastSelection_ _ENTER_ (list 0 y) _angle30_
		)
		(command _LINE_ (list 0 y) (list len y) _ENTER_)
	)
	(command _LINE_ (list len (+ y (/ len3 2.0))) (list len (+ y (- (/ len3 2.0)))) _ENTER_)
)



(defun DrawLastSkilleBryter ( variation y / r len len2 len3 )
	(setq 
		r 0.8
		len 6.4
		len2 6.0
		len3 2.4
	)
	(SetLayer layDef_Zero)
	(if (= variation "AAPEN")
		(command
			_LINE_ (list 0 y) (list len y) _ENTER_
			_ROTATE_ _lastSelection_ _ENTER_ (list 0 y) _angle30_
		)
		(command _LINE_ (list 0 y) (list (- len2 (* r 2.0)) y) _ENTER_)
	)
	(command 
		_LINE_ (list len2 (+ y (/ len3 2.0))) (list len2 (+ y (- (/ len3 2.0)))) _ENTER_
		_CIRCLE_ (list (- len2 r) y) r
	)
)



(defun DrawMotor ( variation / dist r p1 p2 p3 p4 )
	(setq 
		dist 3.0
		r 1.65
		p1	(list dist (+ dist r))
		p2	(list dist dist)
		p3	(list dist (- dist 1.2679))
		p4	(list dist 0)
	)
	(DrawCircleAtPos layDef_Zero p1 r _noWipeout_)
	(AddTextAtPos layDef_Zero _th180_ p1 "M")
	(if (= variation "AAPEN")
		(command _LINE_ p2 p3 _ENTER_)
	;else
		(command _LINE_ p2 p4 _ENTER_)
    )
)



(defun DrawSkinneJord ( offset / len1 len2 r x y )
	(setq 
		len1 4.0
		len2 (/ 2.5 2.0)
		r 0.5
		x (/ 2.5 2.0)
		y 0.9
	)
	(SetLayer layDef_Zero)
	(command 
		_LINE_ _origo_ (list len1 0) _ENTER_
		_LINE_ (list len1 len2) (list len1 (- len2)) _ENTER_
		_CIRCLE_ (list len1 (+ len2 r)) r
		_RECTANGLE_ (list (- len1 x) (- len2)) (list (+ len1 x) (- (+ len2 y)))
	)
	(DrawHatchFromPoint _solidHatch_ (list len1 (- (+ len2 (HalfOf y)))) _angleZero_ _offsetZero_)
	(if offset (MoveUp offset))
)
