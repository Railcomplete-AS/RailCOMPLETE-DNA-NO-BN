;=========================================================================================================================
;
; ANYADM Annotation Chainbreak.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
; See https://trv.banenor.no/wiki/Skilt/Plassering_av_skilt_langs_sporet

; Chain break symbol, scalable inside RailCOMPLETE (as you zoom in and out)

(defun ANYADM-ANNOTATION-CHAINBREAK ( / blockName description )
	;
	;                Jump
	;      +------------------------+
	;     +------------------------+
	;    /       symbolText       /		
	;   +------------------------+
	;  +------------.-----------+
	;         In_Km   Out_Km
	;         In_m    Out_m
	;
	(cond 
		((= _ADM_ _XXGL_) (setq blockName (strcat _RC_ "" "CHAINBREAK"				)))
		((= _ADM_ _NOBN_) (setq blockName (strcat _RC_ "" "KJEDEBRUDD"				)))
		((= _ADM_ _FRSR_) (setq blockName (strcat _RC_ "" "ANOMALIE"				)))
		((= _ADM_ _DEDB_) (setq blockName (strcat _RC_ "" "KILOMETRIERUNGSSPRUNG"	)))
		((= _ADM_ _JPTX_) (setq blockName (strcat _RC_ "" "CHAINBREAK"				)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq description (strcat "CHAIN BREAK"			)))
		((= _ADM_ _NOBN_) (setq description (strcat "KJEDEBRUDD"			)))
		((= _ADM_ _FRSR_) (setq description (strcat "ANOMALIE"				)))
		((= _ADM_ _DEDB_) (setq description (strcat "KILOMETRIERUNGSSPRUNG"	)))
		((= _ADM_ _JPTX_) (setq description (strcat "CHAIN BREAK"			)))
	)
	(cond 
		((= _ADM_ _XXGL_) (setq symbolText (strcat "CHAIN BREAK"			)))
		((= _ADM_ _NOBN_) (setq symbolText (strcat "KJEDEBRUDD"				)))
		((= _ADM_ _FRSR_) (setq symbolText (strcat "ANOMALIE"				)))
		((= _ADM_ _DEDB_) (setq symbolText (strcat "SPRUNG"					)))
		((= _ADM_ _JPTX_) (setq symbolText (strcat "CHAIN BREAK"			)))
	)
	
	(command _POLYLINE_ "-11,0" "9,0" "11,4" "-9,4" _closedPolyline_)
	(AddWipeoutToLastClosedPolyline layDef_BoardOrPole_Wipeout _keepWipeoutSource_)
	(SetLayer layDef_Zero)
	(command _LINE_ "-10.876,0.248" "9.124,0.248" _ENTER_)
	(command _LINE_ "-10.752,0.4961" "9.248,0.4961" _ENTER_)
	(command _LINE_ "-9.248,3.5039" "10.752,3.5039" _ENTER_)
	(command _LINE_ "-9.124,3.752" "10.876,3.752" _ENTER_) 
	(cond 
		((= _ADM_ _XXGL_)
			(AddAtt "INCOMING_KM"	"Incoming km:"		"15"	"-1.0,-2.75" _th180_ _angleZero_ _rcTextStyle_ _middleRight_)
			(AddAtt "INCOMING_M"	"Incoming m:"		"422"	"-1.0,-5.50" _th180_ _angleZero_ _rcTextStyle_ _middleRight_)
			(AddAtt "OUTGOING_KM"	"Outgoing km:"		"15"	"1.0,-2.75"  _th180_ _angleZero_ _rcTextStyle_ _middleLeft_)
			(AddAtt "OUTGOING_M"	"Outgoing m:"		"450"	"1.0,-5.50"  _th180_ _angleZero_ _rcTextStyle_ _middleLeft_)
			(AddAtt "JUMP"			"Jump:"				"+28m"	"0,5.5"      _th180_ _angleZero_ _rcTextStyle_ _middleCenter_)
		)
		((= _ADM_ _NOBN_)
			(AddAtt "INN_KM"	"Innkommende km:"		"15"	"-1.0,-2.75" _th180_ _angleZero_ _rcTextStyle_ _middleRight_)
			(AddAtt "INN_M"		"Innkommende meter:"	"422"	"-1.0,-5.50" _th180_ _angleZero_ _rcTextStyle_ _middleRight_)
			(AddAtt "UT_KM"		"Utgående km:"			"15"	"1.0,-2.75"  _th180_ _angleZero_ _rcTextStyle_ _middleLeft_)
			(AddAtt "UT_M"		"Utgående m:"			"450"	"1.0,-5.50"  _th180_ _angleZero_ _rcTextStyle_ _middleLeft_)
			(AddAtt "SPRANG"	"Sprang:"				"+28m"	"0,5.5"      _th180_ _angleZero_ _rcTextStyle_ _middleCenter_)
		)
		((= _ADM_ _FRSR_)
			(AddAtt "KM_ENTREE"	(strcat "Km entr" _EACUTE_ "e:")	"15"	"-1.0,-2.75" _th180_ _angleZero_ _rcTextStyle_ _middleRight_)
			(AddAtt "M_ENTREE"	(strcat "m entr" _EACUTE_ "e:")		"422"	"-1.0,-5.50" _th180_ _angleZero_ _rcTextStyle_ _middleRight_)
			(AddAtt "KM_SORTIE"	"Km sortie:"						"15"	"1.0,-2.75"  _th180_ _angleZero_ _rcTextStyle_ _middleLeft_)
			(AddAtt "M_SORTIE"	"m sortie:"							"450"	"1.0,-5.50"  _th180_ _angleZero_ _rcTextStyle_ _middleLeft_)
			(AddAtt "SAUT"		"Saut:"								"+28m"	"0,5.5"      _th180_ _angleZero_ _rcTextStyle_ _middleCenter_)
		)
		((= _ADM_ _DEDB_)
			(AddAtt "EIN_KM"	"Eingangs-Km:"			"15"	"-1.0,-2.75" _th180_ _angleZero_ _rcTextStyle_ _middleRight_)
			(AddAtt "EIN_M"		"Eingangs-m:"			"422"	"-1.0,-5.50" _th180_ _angleZero_ _rcTextStyle_ _middleRight_)
			(AddAtt "AUS_KM"	"Ausgangs-Km:"			"15"	"1.0,-2.75"  _th180_ _angleZero_ _rcTextStyle_ _middleLeft_)
			(AddAtt "AUS_M"		"Ausgangs-m:"			"450"	"1.0,-5.50"  _th180_ _angleZero_ _rcTextStyle_ _middleLeft_)
			(AddAtt "SPRUNG"	"Sprung:"				"+28m"	"0,5.5"      _th180_ _angleZero_ _rcTextStyle_ _middleCenter_)
		)
		(AddTextAtPos layDef_Zero _th180_ "0,2" symbolText)
	)
	(MoveUp 20)
	(command _LINE_ _origin_ "0,20" _ENTER_)

	; DO NOT generate this one in multiple drawing scales,
	; instead, use RC-CopyAnnotationToDrawing command in RC - the <SymbolDefinition> DNA declaration 
	; may be found inside the StyleDefinitions XML file, annotating chainbreaks.
	(CreateSchematicBlockFromCurrentGraphics blockName)
)
