;=========================================================================================================================
;
; createStandardLayers.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved. 
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
(defun createStandardLayers ( / )
	;
	; Define layers, then create them
	;
	(setq 
		; Global definitions for all layers in use by RailCOMPLETE objects.
		;
		; These definitions are intended for toggling with RC-ShowLayer (see DNA code)
		;
		; 'Struct layDef' definition:
		;
		; item 0 = Layer's name (NB cannot contain nonstandard letters such as æøåÆØÅ, nor any \U+00D9 unicode definitions.
		; item 1 = Layer's color
		; item 2 = Layer's description
		; item 3 = Default color for objects intended for this layer (for use with 'addPlaced...()' routines
		;
		; Example: (setq myLayDef '("Layer name can contain spaces" "<CAD-system-color-for-the-layer-itself> "Descriptive text" "<CAD-system-color-for-objects-to-be-drawn-next>"))
		;
		
		; General / common layers
		;----------------------------------------------
		layer_Zero
		'(
			"0"
			"White"
			"RailCOMPLETE Felles - Default layer"
			"_ByBlock"
		)

		layer_Defpoints
		'(
			"Defpoints"
			"Yellow"
			"Defpoints - Objects on the Defpoints layer will not be plotted (._LAYER _Plot _No)"
			"_ByLayer"
		)

		layer_UnknownLayerNameRequested
		'(
			"___DEFAULT_RAILCOMPLETE_LAYER___"
			"Magenta" 
			"RailCOMPLETE Felles - Layer created at 2D library generation time due to a faulty LISP program - Please notify support(at)railcomplete(dot)no"
			"_ByLayer"
		)

		layer_InsertionPoint
		'(
			"JBTFE__FE_DIV_INNSETTINGSPUNKT"
			62
			"RailCOMPLETE Felles - Innsettingspunkt-markering"
			"_ByLayer"
		)
	
		layer_Description
		'(
			"JBTFE__FE_DIV_BESKRIVELSE"
			62
			"RailCOMPLETE Felles - Detaljert beskrivelse av objektvariant"
			"_ByLayer"
		)

		layer_Cabinet_ReservedSpaceForDoors
		'(
			"JBTFE__FE_DIV_RESERVERT_FOR_SKAPDOERER"
			62
			(strcat "RailCOMPLETE Felles - Reservert omr" _aa_ "de for skapd" _oe_ "rbevegelse")
			"_ByLayer"
		)


		layer_Cabinet_Wipeout
		'(
			"JBTFE__FE_DIV_SKAP_WIPEOUT"
			62
			"RailCOMPLETE Felles - Wipeout for skap og kabelbokser"
			"_ByLayer"
		)

		; Board or pole layers
		;----------------------------------------------
		layer_BoardOrPole_Wipeout
		'(
			"JBTSI__SKILT_WIPEOUT"
			62
			"RailCOMPLETE Skilt - Wipeout for skilt og stolper"
			"_ByLayer"
		)

		; Substructure layers (foundations, tunnels, roads etc)
		;----------------------------------------------
		layer_FoundationLocator
		'(
			"JBTUB__FE_FUN_LOKALISERING"
			62
			"RailCOMPLETE Underbygning - Fundamentlokalisering - en sirkel med fagtilh\U+00F8righet KL(KL), S(Signal), T(Tele) eller F(skiltfeste)"
			"_ByLayer"
		)
		

		; Superstructure (track and embankment) layers
		;----------------------------------------------
		layer_Turnout_Wipeout
		'(
			"JBTOB__KO_SPV_SPORVEKSEL_WIPEOUT"
			62
			"RailCOMPLETE Overbygning - Wipeout for sporveksler og sporkryss"
			"_ByLayer"
		)

		layer_Turnout_ForbiddenAreaForAxleCounterSensor
		'(
			"JBTOB__KO_SPV_FORBUDT_OMRAADE_FOR_AKSELTELLERE"
			62
			(strcat "RailCOMPLETE Overbygning - Forbudt omr" _aa_ "de for akseltellere (mindre enn 60cm til n" _ae_ "rmeste skinne")
			"_ByLayer"
		)

		layer_Turnout_LongSleepers
		'(
			"JBTOB__KO_SPV_LANGSVILLEPARTI"
			62
			"RailCOMPLETE Overbygning - Langsvilleparti etter bakkant sporveksel"
			"_ByLayer"
		)
		
		layer_Turnout_ShortSleepers
		'(
			"JBTOB__KO_SPV_KORTSVILLEPARTI"
			62
			"RailCOMPLETE Overbygning - Kortsvilleparti etter bakkant sporveksel"
			"_ByLayer"
		)

		layer_Turnout_TrackCenterLines
		'(
			"JBTOB__KO_SPV_SENTERLINJER"
			62
			"RailCOMPLETE Overbygning - Senterlinje for avvikesspor i sporveksel"
			"_ByLayer"
		)
		
		; Overhead Catenary System (OCS) layers
		;----------------------------------------------
		layer_CatenaryIsolator_Wipeout
		'(
			"JBTKL__EH_ISO_ISOLATOR_WIPEOUT"
			62
			"RailCOMPLETE KL - Wipeout for KL-isolatorer"
			"_ByLayer"
		)

		layer_TensioningDeviceType
		'(
			"JBTKL__EH_AEH_TYPENAVN"
			62
			"RailCOMPLETE KL - Avspenningstype"
			"_ByLayer"
		)
	
		layer_AutoTransformerTerminals
		'(
			"JBTKL__EH_AUT_POLER_FOR_AUTOTRANSFORMATOR"
			62
			"RailCOMPLETE KL - Poler for autotransformator"
			"_ByBlock"
		)
	
		layer_PushPullDirection
		'(
			"JBTKL__EH_UTL_KRAFTRETNING"
			62
			"RailCOMPLETE KL - Utligger kraftretning"
			"_ByBlock"
		)
	
		layer_CantileverType
		'(
			"JBTKL__EH_UTL_TYPENAVN"
			62
			"RailCOMPLETE KL - Utliggertype"
			"_ByLayer"
		)
	
		; Signaling
		;----------------------------------------------
		layer_Derailer_Wipeout
		'(
			"JBTSI__SA_SSP_WIPEOUT"
			62
			"RailCOMPLETE Signal - Wipeout for sporsperrer"
			"_ByLayer"
		)

		layer_AxleCounter_SnaplineForPositioning
		'(
			"JBTSI__SA_TEL_SNAPLINJE"
			62
			(strcat "RailCOMPLETE Signal - Snaplinje for tellepunkt (en linje fra akseltellersymbolet vinkelrett inn p" _aa_ " sporaksen)")
			"_ByLayer"
		)

		layer_AxleCounter_ReservedAreaForTuningUnit
		'(
			"JBTSI__SA_TEL_RESERVERT_OMRAADE_SOPP"
			62
			(strcat "RailCOMPLETE Signal - Reservert omr" _aa_ "de for plassering av akselteller tuning-enhet (aka 'sopp')")
			"_ByLayer"
		)

		layer_Balise_MetalFreeArea
		'(
			"JBTSI__SA_ATB_METALLFRITT_PROFIL"
			62
			(strcat "RailCOMPLETE Signal - Metallfritt profil omkring store baliser (sm" _aa_ " baliser har et noe mindre krav til metallfritt omr" _aa_ "de)")
			"_ByLayer"
		)

		layer_Balise_3m_Separation
		'(
			"JBTSI__SA_ATB_SNAPLINJER_3m"
			62
			"RailCOMPLETE Signal - Snaplinjer for 3m avstand mellom baliser"
			"_ByLayer"
		)

		layer_Balise_8m_Separation
		'(
			"JBTSI__SA_ATB_SNAPLINJER_8m"
			62
			(strcat "RailCOMPLETE Signal - Snaplinjer for 8m avstand mellom baliser (minstekravet er 7.5m fra siste styrte balise til isolert skj" _oe_ "t eller akselteller)")
			"_ByLayer"
		)

		layer_Balise_12m_Separation
		'(
			"JBTSI__SA_ATB_SNAPLINJER_12m"
			62
			(strcat "RailCOMPLETE Signal - Snaplinjer for 12m avstand mellom baliser (minstekravet er 10.5m mellom p" _aa_ "f" _oe_ "lgende balisegrupper)")
			"_ByLayer"
		)

		; The three next belong to JBTOB_KO because isolation joints are made by track people... But the layers are mainly used by signaling:
		layer_View_SchematicPlan
		'(
			"JBTOB__KO_SKJ_SKJEMATISK_PLAN"
			62
			(strcat "RailCOMPLETE Signal - Grafikk for skinneskj" _oe_ "t i 1-streng tegning 'Skjematisk Plan'")
			"_ByLayer"
		)

		layer_View_CablePlan
		'(
			"JBTOB__KO_SKJ_KABELPLAN"
			62
			(strcat "RailCOMPLETE Signal - Grafikk for skinneskj" _oe_ "t i 1-streng 'Kabelplan' og i geografiske tegninger")
			"_ByLayer"
		)

		layer_View_TrackIsolationPlan ; Is also relevant for OCS return current drawings
		'(
			"JBTOB__KO_SKJ_SPORISOLERING_OG_RETURKRETS"
			62
			(strcat "RailCOMPLETE Signal / Returkrets - Grafikk for skinneskj" _oe_ "t i 2-streng Signal skjematisk sporisolering eller KL returkretsskjema")
			"_ByLayer"
		)
	)

	; Create layers
	;--------------
	
	; General / common layers
	;(createLayer layer_Zero) - Exists already - and does not terminate the -LAYER _D dialog correctly, due to existing / nonexisting description - CLFEY didn't figure this out :-(
	; (command "._LAYER" "_C" (nth 1 layer_Zero) "") ; Change color for layer zero (can't be the current layer when this is done)
	; (command "._LAYER" "_D" (nth 2 layer_Zero) "") ; Change description for layer zero - no extra ENTER since description was void to start with - FAILS IF NON-VOID
	
	;(createLayer layer_Defpoints) ; This one is also problematiqu (fails) when debugging and the lauyer already exists :-(
	
	(createLayer layer_UnknownLayerNameRequested)
	(createLayer layer_InsertionPoint)
	(createLayer layer_Description)
	(createLayer layer_Cabinet_ReservedSpaceForDoors)
	(createLayer layer_Cabinet_Wipeout)
	
	; Board or pole layers
	(createLayer layer_BoardOrPole_Wipeout)

	; Substructure layers (foundations, tunnels, roads etc)
	(createLayer layer_FoundationLocator)
	
	; Superstructure (track and embankment) layers
	(createLayer layer_Turnout_Wipeout)
	(createLayer layer_Turnout_ForbiddenAreaForAxleCounterSensor)
	(createLayer layer_Turnout_LongSleepers)
	(createLayer layer_Turnout_ShortSleepers)
	(createLayer layer_Turnout_TrackCenterLines)

	; Overhead Catenary System (OCS) layers
	(createLayer layer_CatenaryIsolator_Wipeout)
	(createLayer layer_TensioningDeviceType)
	(createLayer layer_AutoTransformerTerminals)
	(createLayer layer_PushPullDirection)
	(createLayer layer_CantileverType)

	; Signaling
	(createLayer layer_Derailer_Wipeout)
	(createLayer layer_AxleCounter_SnaplineForPositioning)
	(createLayer layer_AxleCounter_ReservedAreaForTuningUnit)
	(createLayer layer_Balise_MetalFreeArea)
	(createLayer layer_Balise_3m_Separation)
	(createLayer layer_Balise_8m_Separation)
	(createLayer layer_Balise_12m_Separation)
	(createLayer layer_View_SchematicPlan)
	(createLayer layer_View_CablePlan)
	(createLayer layer_View_TrackIsolationPlan)
	
	(setLayer layer_Zero)
)





