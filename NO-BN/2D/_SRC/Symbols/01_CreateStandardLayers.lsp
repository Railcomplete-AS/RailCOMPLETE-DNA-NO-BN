;=========================================================================================================================
;
; CreateStandardLayers.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved. 
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================
(defun CreateStandardLayers ( / )
	;
	; Define layers, then create them
	;
	(setq 
		; Global definitions for all layers in use by RailCOMPLETE objects.
		;
		; These definitions are intended for toggling with RC-ShowLayer (see DNA code)
		;
		
		; General / common layers
		;----------------------------------------------
		layDef_Zero
		(list
			"0"
			_colorWhite_
			"RailCOMPLETE Felles - Default layer"
			_ByBlock_
		)

		layDef_Defpoints
		(list
			"Defpoints"
			_colorYellow_
			"Defpoints - Objects on the Defpoints layer will not be plotted (._LAYER _Plot _No)"
			_ByLayer_
		)

		layDef_UnknownLayerNameRequested
		(list
			"___DEFAULT_RAILCOMPLETE_LAYER___"
			_colorMagenta_
			"RailCOMPLETE Felles - Layer created at 2D library generation time due to a faulty LISP program - Please notify us at support@railcomplete.com"
			_ByLayer_
		)

		layDef_InsertionPoint
		(list
			"JBTFE__INNSETTINGSPUNKT"
			_colorMetaDataLayer_
			"RailCOMPLETE Felles - Innsettingspunkt-markering"
			_ByLayer_
		)
	
		layDef_Description
		(list
			"JBTFE__BESKRIVELSE"
			_colorMetaDataLayer_
			"RailCOMPLETE Felles - Detaljert beskrivelse av objektvariant"
			_ByLayer_
		)

		layDef_Cabinet_ReservedSpaceForDoors
		(list
			"JBTFE__RESERVERT_FOR_SKAPDOERER"
			_colorMetaDataLayer_
			(strcat "RailCOMPLETE Felles - Reservert omr" _ARING_ "de for skapd" _OSLASH_ "rbevegelse")
			_ByLayer_
		)


		layDef_Cabinet_Wipeout
		(list
			"JBTFE__WIPEOUT_SKAP"
			_colorMetaDataLayer_
			"RailCOMPLETE Felles - Wipeout under skap og kabelbokser"
			_ByLayer_
		)

		; Board or pole layers
		;----------------------------------------------
		layDef_BoardOrPole_Wipeout
		(list
			"JBTSI__WIPEOUT_SKILT"
			_colorMetaDataLayer_
			"RailCOMPLETE Skilt - Wipeout under skilt og stolper"
			_ByLayer_
		)

		; Substructure layers (foundations, tunnels, roads etc)
		;----------------------------------------------
		layDef_FoundationLocator
		(list
			"JBTUB__LOKALISERING_AV_FUNDAMENTER_OG_SKILTFESTER"
			_colorMetaDataLayer_
			(strcat "RailCOMPLETE Underbygning - Fundamentlokalisering - en sirkel med fagtilh" _OSLASH_ "righet KL(KL), S(Signal), T(Tele) eller F(skiltfeste)")
			_ByLayer_
		)
		
		layDef_MetricDetails
		(list
			(strcat "JBTFE__FE_DIV_METRISKE_DETALJER")
			_colorMetaDataLayer_
			(strcat "RailCOMPLETE Felles - Objektet vist i faktisk st" _OSLASH_ "rrelse (ikke-annotativ grafikk)")
			_ByLayer_
		)
		

		; Superstructure (track and embankment) layers
		;----------------------------------------------
		layDef_Turnout_LongSleepers
		(list
			"JBTOB__KO_SPV_LANGSVILLEPARTI"
			_colorMetaDataLayer_
			"RailCOMPLETE Overbygning - Langsvilleparti etter bakkant sporveksel"
			_ByLayer_
		)
		
		layDef_Turnout_ShortSleepers
		(list
			"JBTOB__KO_SPV_KORTSVILLEPARTI"
			_colorMetaDataLayer_
			"RailCOMPLETE Overbygning - Kortsvilleparti etter bakkant sporveksel"
			_ByLayer_
		)

		layDef_Turnout_TrackCenterLines
		(list
			"JBTOB__KO_SPV_SENTERLINJER"
			_colorMetaDataLayer_
			"RailCOMPLETE Overbygning - Senterlinje for avvikesspor i sporveksel"
			_ByLayer_
		)
		
		; Overhead Catenary System (OCS) layers
		;----------------------------------------------
		layDef_CatenaryIsolator_Wipeout
		(list
			"JBTKL__EH_ISO_ISOLATOR_WIPEOUT"
			_colorMetaDataLayer_
			"RailCOMPLETE KL - Wipeout for KL-isolatorer"
			_ByLayer_
		)

		layDef_AutoTransformerTerminals
		(list
			"JBTKL__EH_AUT_POLER_FOR_AUTOTRANSFORMATOR"
			_colorMetaDataLayer_
			"RailCOMPLETE KL - Poler for autotransformator"
			_ByBlock_
		)
	
		layDef_PushPullDirection
		(list
			"JBTKL__EH_UTL_KRAFTRETNING"
			_colorMetaDataLayer_
			"RailCOMPLETE KL - Utligger kraftretning"
			_ByBlock_
		)
	
		layDef_CantileverType
		(list
			"JBTKL__EH_UTL_TYPENAVN"
			_colorMetaDataLayer_
			"RailCOMPLETE KL - Utliggertype"
			_ByLayer_
		)
	
		; Signaling
		;----------------------------------------------
		layDef_Turnout_Wipeout
		(list
			"JBTSI__KO_SPV_SPORVEKSEL_WIPEOUT" ; Signaling
			_colorMetaDataLayer_
			"RailCOMPLETE Signal - Wipeout for sporveksler og sporkryss"
			_ByLayer_
		)

		layDef_Derailer_Wipeout
		(list
			"JBTSI__SA_SSP_WIPEOUT"
			_colorMetaDataLayer_
			"RailCOMPLETE Signal - Wipeout for sporsperrer"
			_ByLayer_
		)

		layDef_AxleCounter_SnaplineForPositioning
		(list
			"JBTSI__SA_TEL_SNAPLINJE"
			_colorMetaDataLayer_
			(strcat "RailCOMPLETE Signal - Snaplinje for tellepunkt (en linje fra akseltellersymbolet vinkelrett inn p" _ARING_ " sporaksen)")
			_ByLayer_
		)

		layDef_AxleCounter_MinimumRailSeparationAtSpeedsBelowOrAt_120_kmh
		(list
			"JBTSI__SA_TEL_MINSTEAVSTAND_TIL_NESTE_SKINNE_VED_HASTIGHETER_MINDRE_ELLER_LIK_120_KMH"
			_colorMetaDataLayer_
			(strcat "RailCOMPLETE Signal - Minsteavstand fra akseltellers skinne til n" _AELIG_ "rmeste neste skinne ved hastigheter mindre eller lik 120 km/h")
			_ByLayer_
		)

		layDef_AxleCounter_MinimumRailSeparationAtSpeedsAbove_120_kmh
		(list
			"JBTSI__SA_TEL_MINSTEAVSTAND_TIL_NESTE_SKINNE_VED_HASTIGHETER_OVER_120_KMH"
			_colorMetaDataLayer_
			(strcat "RailCOMPLETE Signal - Minsteavstand fra akseltellers skinne til n" _AELIG_ "rmeste neste skinne ved hastigheter over 120 km/h")
			_ByLayer_
		)

		layDef_AxleCounter_ReservedAreaForTuningUnit
		(list
			"JBTSI__SA_TEL_RESERVERT_OMRAADE_SOPP"
			_colorMetaDataLayer_
			(strcat "RailCOMPLETE Signal - Reservert omr" _ARING_ "de for plassering av akselteller tuning-enhet (aka 'sopp')")
			_ByLayer_
		)

		layDef_Balise_MetalFreeArea
		(list
			"JBTSI__SA_ATB_METALLFRITT_PROFIL"
			_colorMetaDataLayer_
			(strcat "RailCOMPLETE Signal - Metallfritt profil omkring store baliser (sm" _ARING_ " baliser har et noe mindre krav til metallfritt omr" _ARING_ "de)")
			_ByLayer_
		)

		layDef_Balise_3m_Separation
		(list
			"JBTSI__SA_ATB_SNAPLINJER_3m"
			_colorMetaDataLayer_
			"RailCOMPLETE Signal - Snaplinjer for 3m avstand mellom baliser"
			_ByLayer_
		)

		layDef_Balise_8m_Separation
		(list
			"JBTSI__SA_ATB_SNAPLINJER_8m"
			_colorMetaDataLayer_
			(strcat "RailCOMPLETE Signal - Snaplinjer for 8m avstand mellom baliser (minstekravet er 7.5m fra siste styrte balise til isolert skj" _OSLASH_ "t eller akselteller)")
			_ByLayer_
		)

		layDef_Balise_12m_Separation
		(list
			"JBTSI__SA_ATB_SNAPLINJER_12m"
			_colorMetaDataLayer_
			(strcat "RailCOMPLETE Signal - Snaplinjer for 12m avstand mellom baliser (minstekravet er 10.5m mellom p" _ARING_ "f" _OSLASH_ "lgende balisegrupper)")
			_ByLayer_
		)

		; The three next belong to track & embankment because isolation joints are made by track people... But the layers are mainly used by signaling:
		layDef_View_SchematicPlan
		(list
			"JBTOB__KO_SKJ_SKJEMATISK_PLAN"
			_colorMetaDataLayer_
			(strcat "RailCOMPLETE Signal - Grafikk for skinneskj" _OSLASH_ "t i 1-streng tegning 'Skjematisk Plan'")
			_ByLayer_
		)

		layDef_View_CablePlan
		(list
			"JBTOB__KO_SKJ_KABELPLAN"
			_colorMetaDataLayer_
			(strcat "RailCOMPLETE Signal - Grafikk for skinneskj" _OSLASH_ "t i 1-streng 'Kabelplan' og i geografiske tegninger")
			_ByLayer_
		)

		layDef_View_TrackIsolationPlan ; Is also relevant for OCS return current drawings
		(list
			"JBTOB__KO_SKJ_SPORISOLERING_OG_RETURKRETS"
			_colorMetaDataLayer_
			(strcat "RailCOMPLETE Signal / Returkrets - Grafikk for skinneskj" _OSLASH_ "t i 2-streng Signal skjematisk sporisolering eller KL returkretsskjema")
			_ByLayer_
		)
	)

	; Create layers
	;--------------
	
	; General / common layers
	; (CreateLayer layDef_Zero) - Exists already - and does not terminate the -LAYER _D dialog correctly, due to existing / nonexisting description - CLFEY didn't figure this out :-(
	; (command _LAYER_ _colorizeLayer_ (nth 1 layDef_Zero) _ENTER_) ; Change color for layer zero (can't be the current layer when this is done)
	; (command _LAYER_ _describeLayer_ (nth 2 layDef_Zero) _ENTER_) ; Change description for layer zero - no extra ENTER since description was void to start with - FAILS IF NON-VOID
	
	;(CreateLayer layDef_Defpoints) ; This one is also problematic (it fails) when debugging and the layer already exists :-(
	
	(CreateLayer layDef_UnknownLayerNameRequested)
	(CreateLayer layDef_InsertionPoint)
	(CreateLayer layDef_Description)
	(CreateLayer layDef_Cabinet_ReservedSpaceForDoors)
	(CreateLayer layDef_Cabinet_Wipeout)
	(CreateLayer layDef_MetricDetails)
	
	; Board or pole layers
	(CreateLayer layDef_BoardOrPole_Wipeout)

	; Substructure layers (foundations, tunnels, roads etc)
	(CreateLayer layDef_FoundationLocator)
	
	; Superstructure (track and embankment) layers
	(CreateLayer layDef_Turnout_Wipeout)
	(CreateLayer layDef_Turnout_LongSleepers)
	(CreateLayer layDef_Turnout_ShortSleepers)
	(CreateLayer layDef_Turnout_TrackCenterLines)

	; Overhead Catenary System (OCS) layers
	(CreateLayer layDef_CatenaryIsolator_Wipeout)
	(CreateLayer layDef_AutoTransformerTerminals)
	(CreateLayer layDef_PushPullDirection)
	(CreateLayer layDef_CantileverType)

	; Signaling
	(CreateLayer layDef_Derailer_Wipeout)
	(CreateLayer layDef_AxleCounter_SnaplineForPositioning)
	(CreateLayer layDef_AxleCounter_MinimumRailSeparationAtSpeedsBelowOrAt_120_kmh)
	(CreateLayer layDef_AxleCounter_MinimumRailSeparationAtSpeedsAbove_120_kmh)
	(CreateLayer layDef_AxleCounter_ReservedAreaForTuningUnit)
	(CreateLayer layDef_Balise_MetalFreeArea)
	(CreateLayer layDef_Balise_3m_Separation)
	(CreateLayer layDef_Balise_8m_Separation)
	(CreateLayer layDef_Balise_12m_Separation)
	(CreateLayer layDef_View_SchematicPlan)
	(CreateLayer layDef_View_CablePlan)
	(CreateLayer layDef_View_TrackIsolationPlan)
	
	(SetLayer layDef_Zero)
)
