;=========================================================================================================================
;
; CreateStandardLayers.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503 
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
(defun ModifyAdmString ( admString / start len )
	; METHOD:
	;  12345678901234
	; "FR-SR-2D-TFCO_" becomes "TFCO"
	; "DE-DB-2D-BTAG_" becomes "BTAG"
	; "NO-BN-2D-JBTFE_" becomes "JBTFE"
	; etc
	(setq 
		start	10 
		len		(- (strlen admString) start)
	)
	(substr admString start len) ; Remove the initial string.
)



(defun CreateStandardLayers ( / )
	;
	; Define layers, then create them
	;
	; Global definitions for all layers in use by RailCOMPLETE objects.
	; These definitions are intended for toggling with RC-ShowLayer (see DNA code)
	;
	; Note: The order of appearance must be the same for all layDef. See documentation for the SetLayer function.
	;
	(setq 
		; General / common layers
		;----------------------------------------------
		layDef_Zero
			(list 
				"0"
				_colorWhite_
				"RailCOMPLETE Felles - Default-lag"
				_ByBlock_
			)

		layDef_Defpoints
			(list 
				"Defpoints"
				_colorYellow_
				"Defpoints - Objekter p" _ARING_ " Defpoints-laget vil ikke bli plottet (._LAYER _Plot _No)"
				_ByLayer_
			)

		layDef_UnknownLayerNameRequested
			(list 
				"___UKJENT_LAGNAVN___"
				_colorMagenta_
				(strcat _PREFIX_ " " _RC_NAME_ " - Lagnavn som er blitt opprettet ved en Lisp programvarefeil da 2D biblioteket ble generert"
					" - Vennligst informer oss p" _ARING_ " support@railcomplete.com")
				_ByLayer_
			)

		layDef_InsertionPoint
			(list 
				(strcat (ModifyAdmString _RC_) "$$INNSETTINGSPUNKT")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _RC_NAME_ " - Innsettingspunkt for punktobjekter")
				_ByLayer_
			)
	
		layDef_Description
			(list 
				(strcat (ModifyAdmString _RC_) "$$SYMBOLBESKRIVELSE")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _RC_NAME_ " - Symbolbeskrivelse")
				_ByLayer_
			)

		layDef_MetricDetails
			(list 
				(strcat (ModifyAdmString _COM_) "$$METRISKE_DETALJER")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _COM_NAME_ " - Detaljer i et objekt vist i sin faktiske st" _OSLASH_ "rrelse (ikke-annotativ grafikk)")
				_ByLayer_
			)
		
		layDef_AnnotationTextLocator
			(list 
				(strcat (ModifyAdmString _RC_) "$$ANNOTERINGSTEKST_SYMBOL")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _RC_NAME_ " - Annoteringstekst-symbol for " _ARING_ " lettere gjenfinne tekstobjekter")
				_ByBlock_
			)

		layDef_Cabinet_ReservedSpaceForDoors
			(list 
				(strcat (ModifyAdmString _COM_) "$$RESERVERT_FOR_SKAPDOERER")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _COM_NAME_ " - Reservert omr" _ARING_ "de for skapd" _OSLASH_ "rbevegelse")
				_ByLayer_
			)

		layDef_Cabinet_Wipeout
			(list 
				(strcat (ModifyAdmString _COM_) "$$WIPEOUT_SKAP")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _COM_NAME_ " - Wipeout under skap og kabelbokser")
				_ByLayer_
			)

		; Board or pole layers
		;----------------------------------------------
		layDef_BoardOrPole_Wipeout
			(list 
				(strcat (ModifyAdmString _BNP_) "$$WIPEOUT_SKILT")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _BNP_NAME_ " - Wipeout under skilt og stolper")
				_ByLayer_
			)

		; Substructure layers (foundations, tunnels, roads etc)
		;----------------------------------------------
		layDef_FoundationLocator
			(list 
				(strcat (ModifyAdmString _SUB_) "$$LOKALISERING_AV_FUNDAMENTER_OG_SKILTFESTER")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SUB_NAME_ " - Fundamentlokalisering - KL(KL), S(Signal), T(Tele) eller F(skiltfeste)")
				_ByLayer_
			)

		; Superstructure (track and embankment) layers
		;----------------------------------------------
		layDef_Switch_LongSleepers
			(list 
				(strcat (ModifyAdmString _TRK_) "$$SPORVEKSEL_LANGSVILLEPARTI")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _TRK_NAME_ " - Langsvilleparti etter bakkant sporveksel (del av sporvekselsymbolet)")
				_ByLayer_
			)
		
		layDef_Switch_ShortSleepers
			(list 
				(strcat (ModifyAdmString _TRK_) "$$SPORVEKSEL_KORTSVILLEPARTI")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _TRK_NAME_ " - Kortsvilleparti etter bakkant sporveksel (del av sporvekselsymbolet)")
				_ByLayer_
			)

		layDef_Switch_Geometry
			(list 
				(strcat (ModifyAdmString _TRK_) "$$SPORVEKSEL_GEOMETRI")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _TRK_NAME_ " - Geometri for venstre og h" _OSLASH_ "yre ben i en sporveksel")
				_ByLayer_
			)
		
		; Overhead Catenary System (OCS) layers
		;----------------------------------------------
		layDef_CatenaryInsulator_Wipeout
			(list 
				(strcat (ModifyAdmString _OCS_) "$$WIPEOUT_ISOLATOR")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _OCS_NAME_ " - Wipeout under KL-isolatorer")
				_ByLayer_
			)

		layDef_AutoTransformerTerminals
			(list 
				(strcat (ModifyAdmString _OCS_) "$$AUTOTRANSFORMATOR_POLER")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _OCS_NAME_ " - Poler for autotransformator")
				_ByBlock_
			)

		layDef_PushPullDirection
			(list 
				(strcat (ModifyAdmString _OCS_) "$$UTLIGGER_KRAFTRETNING")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _OCS_NAME_ " - Utligger kraftretning")
				_ByBlock_
			)
	
		layDef_CantileverType
			(list 
				(strcat (ModifyAdmString _OCS_) "$$UTLIGGER_TYPE")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _OCS_NAME_ " - Utliggertype")
				_ByBlock_
			)

		layDef_HighVoltageSwitchActuator
			(list 
				(strcat (ModifyAdmString _OCS_) "$$MAN" "O" "VERMASKIN")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _OCS_NAME_ " - KL-bryter man"  "o" "vermaskin grafikk")
				_ByBlock_
			)



		; Signalling
		;----------------------------------------------
		layDef_Switch_SigCircle_Wipeout
			(list 
				(strcat (ModifyAdmString _SIG_) "$$WIPEOUT_SPORVEKSEL")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Wipeout under den lille sirkelen som markerer TK (teoretisk kryss) i sporvekselsymbolets signalvisning")
				_ByBlock_
			)
	
		layDef_Derailer_Wipeout
			(list 
				(strcat (ModifyAdmString _SIG_) "$$WIPEOUT_SPORSPERRE")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Wipeout under sporsperre")
				_ByBlock_
			)

		layDef_TvpSection_Wipeout
			(list 
				(strcat (ModifyAdmString _SIG_) "$$WIPEOUT_TODETEKSJONSAVSNITT")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Wipeout under togdeteksjonsavsnitt")
				_ByBlock_
			)

		layDef_AxleCounter_Wipeout
			(list 
				(strcat (ModifyAdmString _SIG_) "$$WIPEOUT_AKSELTELLER")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Wipeout under akselteller")
				_ByBlock_
			)

		layDef_AxleCounter_SnaplineForPositioning
			(list 
				(strcat (ModifyAdmString _SIG_) "$$AKSELTELLER_SNAPPELINJE")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Snappelinje for akselteller, markerer hvor i sporet akseltelleren er plassert")
				_ByBlock_
			)

		layDef_AxleCounter_MinimumRailSeparationAtSpeedsBelowOrAt_120_kmh
			(list 
				(strcat (ModifyAdmString _SIG_) "$$AKSELTELLER_SIDEVEIS_MINSTEAVSTAND_INNTIL_120_KMH")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Sideveis minsteavstand fra akseltellers skinne til n" _AELIG_ "rmeste naboskinne, ved hastighet under 120 km/h")
				_ByBlock_
			)

		layDef_AxleCounter_MinimumRailSeparationAtSpeedsAboveOrEqualTo_120_kmh
			(list 
				(strcat (ModifyAdmString _SIG_) "$$AKSELTELLER_SIDEVEIS_MINSTEAVSTAND_OVER_ELLER_VED_120_KMH")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Sideveis minsteavstand fra akseltellers skinne til n" _AELIG_ "rmeste naboskinne, ved hastighet over eller ved 120 km/h")
				_ByBlock_
			)

		layDef_AxleCounter_ReservedAreaForTuningUnit
			(list 
				(strcat (ModifyAdmString _SIG_) "$$AKSELTELLER_RESERVERT_OMRAADE_FOR_TUNINGENHET")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Reservert omr" _ARING_ "de for akseltellerens tuning-enhet, som har en ideell standardisert kabel-lengde")
				_ByBlock_
			)

		layDef_BaliseGroup_Wipeout
			(list 
				(strcat (ModifyAdmString _SIG_) "$$WIPEOUT_BALISEGRUPPE")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Wipeout under et balisegruppesymbol")
				_ByBlock_
			)
	
		layDef_Balise_MetalFreeArea
			(list 
				(strcat (ModifyAdmString _SIG_) "$$BALISE_METALLFRITT_PROFIL")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Metallfritt omr" _ARING_ "de omkring en balise, for " _ARING_ " hindre forstyrrelser i magnetisk balise - tog kommunikasjon")
				_ByBlock_
			)

		layDef_Balise_BaliseSeparation
			(list 
				(strcat (ModifyAdmString _SIG_) "$$BALISE_SNAPPELINJER_3m")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Snappelinjer i 3 meters avstand fra balisesenter, som markerer anbefalt avstand mellom nabo-baliser")
				_ByBlock_
			)

		layDef_Balise_ActuatorSeparation
			(list 
				(strcat (ModifyAdmString _SIG_) "$$BALISE_SNAPPELINJER_8m")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Snappelinjer i 8 meters avstand fra balisesenter, som markerer anbefalt minsteavstand fra en styrt balise til isolasjonen ved signal")
				_ByBlock_
			)

		layDef_Balise_GroupSeparation
			(list 
				(strcat (ModifyAdmString _SIG_) "$$BALISE_SNAPPELINJER_12m")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Snappelinjer i 12 meters avstand fra balisesenter, som markerer anbefalt minsteavstand mellom balisegrupper")
				_ByBlock_
			)

		; The three next belong to track & embankment because isolation joints are made by track people... But the layers are mainly used by signaling:
		layDef_View_SchematicPlan
			(list 
				(strcat (ModifyAdmString _SIG_) "$$VISNING_SKJEMATISK_PLAN")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Grafikk for skinneskj" _OSLASH_ "t i 1-streng tegning 'Skjematisk Plan'")
				_ByBlock_
			)

		layDef_View_CablePlan
			(list 
				(strcat (ModifyAdmString _SIG_) "$$VISNING_KABELPLAN")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Grafikk for skinneskj" _OSLASH_ "t i 1-streng tegning 'Kabelplan' og i geografiske tegninger")
				_ByBlock_
			)

		layDef_View_TrackIsolationPlan ; Is also relevant for OCS return current drawings
			(list 
				(strcat (ModifyAdmString _SIG_) "$$VISNING_SPORISOLERING_OG_RETURKRETS")
				_colorMetaDataLayer_
				(strcat _PREFIX_ " " _SIG_NAME_ " - Grafikk for skinneskj" _OSLASH_ "t i 2-streng tegning 'Skjematisk sporisolering' eller 'KL returkretsskjema'")
				_ByBlock_
			)
	);setq


	; Create the layers
	;------------------
	
	; General / common layers
	; (CreateLayer layDef_Zero) - Exists already - and does not terminate the -LAYER _D dialog correctly, due to existing / nonexisting description - CLFEY didn't figure this out :-(
	; (command _LAYER_ _colorizeLayer_ (nth 1 layDef_Zero) _ENTER_) ; Change color for layer zero (can't be the current layer when this is done)
	; (command _LAYER_ _describeLayer_ (nth 2 layDef_Zero) _ENTER_) ; Change description for layer zero - no extra ENTER since description was void to start with - FAILS IF NON-VOID
	
	;(CreateLayer layDef_Defpoints) ; This one is also problematic (it fails) when debugging and the layer already exists :-(
	
	(CreateLayer layDef_UnknownLayerNameRequested)
	(CreateLayer layDef_InsertionPoint)
	(CreateLayer layDef_Description)
	(CreateLayer layDef_MetricDetails)
	(CreateLayer layDef_AnnotationTextLocator)
	(CreateLayer layDef_Cabinet_ReservedSpaceForDoors)
	(CreateLayer layDef_Cabinet_Wipeout)
	
	; Board or pole layers
	(CreateLayer layDef_BoardOrPole_Wipeout)

	; Substructure layers (foundations, tunnels, roads etc)
	(CreateLayer layDef_FoundationLocator)
	
	; Superstructure (track and embankment) layers
	(CreateLayer layDef_Switch_SigCircle_Wipeout)
	(CreateLayer layDef_Switch_LongSleepers)
	(CreateLayer layDef_Switch_ShortSleepers)
	(CreateLayer layDef_Switch_Geometry)

	; Overhead Catenary System (OCS) layers
	(CreateLayer layDef_CatenaryInsulator_Wipeout)
	(CreateLayer layDef_AutoTransformerTerminals)
	(CreateLayer layDef_PushPullDirection)
	(CreateLayer layDef_CantileverType)
	(CreateLayer layDef_HighVoltageSwitchActuator)

	; Signalling
	(CreateLayer layDef_Derailer_Wipeout)
	(CreateLayer layDef_TvpSection_Wipeout)
	(CreateLayer layDef_AxleCounter_Wipeout)
	(CreateLayer layDef_AxleCounter_SnaplineForPositioning)
	(CreateLayer layDef_AxleCounter_MinimumRailSeparationAtSpeedsBelowOrAt_120_kmh)
	(CreateLayer layDef_AxleCounter_MinimumRailSeparationAtSpeedsAboveOrEqualTo_120_kmh)
	(CreateLayer layDef_AxleCounter_ReservedAreaForTuningUnit)
	(CreateLayer layDef_BaliseGroup_Wipeout)
	(CreateLayer layDef_Balise_MetalFreeArea)
	(CreateLayer layDef_Balise_BaliseSeparation)
	(CreateLayer layDef_Balise_ActuatorSeparation)
	(CreateLayer layDef_Balise_GroupSeparation)
	(CreateLayer layDef_View_SchematicPlan)
	(CreateLayer layDef_View_CablePlan)
	(CreateLayer layDef_View_TrackIsolationPlan)
	
	(SetLayer layDef_Zero)
)
