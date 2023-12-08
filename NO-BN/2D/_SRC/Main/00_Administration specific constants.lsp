;=======================================================
;
; Administration specific constants.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
;
(defun DefineAdministrationSpecificConstants ( / )

	; The various railway disciplines
	;----------------------------------------------------------
	; These are: (void) / com / sub / trk / cur / ocs / sig / pow / tel / bnp / otr
	; 
	; com		Common - general and railway-specific issues
	; bnp		Boards and ('n) poles
	; sub		Substructure (foundations, cable ducts, platforms, civil engineering: buildings, bridges, tunnels, roads)
	; trk		Track and embankment - ballast, sleepers, rails, switches, crossings, buffer stops and other track related objects
	; cur		Traction current system, transformers above catenary voltage levels, power distribution system other than OCS
	; ocs		Overhead catenary system
	; sig		Signalling
	; pow		Power supply, except traction current system (low voltage, up to ca 1000V to users, from 22kV public supplies or 15-25 kV railway HV)
	; tel		Telecommunications
	; otr		Other systems
	(setq
		; Se trv.banenor.no/wiki/Forside
		;
		; Norge - Bane NOR - 2D symboler - Jernbaneteknikk (JBT)
		_PREFIX_	"Jernbaneteknikk"
		_RC_		"NO-BN-2D-JBTRC_"	_RC_NAME_	"RailCOMPLETE"			; RailCOMPLETE internt
		_COM_		"NO-BN-2D-JBTFE_"	_COM_NAME_	"Felles"				; Felles
		_SUB_		"NO-BN-2D-JBTKU_"	_SUB_NAME_	"Underbygning"			; Konstruksjon underbygning
		_TRK_		"NO-BN-2D-JBTKO_"	_TRK_NAME_	"Overbygning"			; Konstruksjon overbygning
		_OCS_		"NO-BN-2D-JBTEH_"	_OCS_NAME_	"Kontaktledning"		; Elektro høyspent
		_SIG_		"NO-BN-2D-JBTSA_"	_SIG_NAME_	"Signalanlegg"			; Signalanlegg
		_TEL_		"NO-BN-2D-JBTTE_"	_TEL_NAME_	"Telekommunikasjon"		; Telekommunikasjon
		_POW_		"NO-BN-2D-JBTEL_"	_POW_NAME_	"Hjelpekraft"			; Elektro lavspent
		_BNP_		"NO-BN-2D-JBTSK_"	_BNP_NAME_	"Skilt"					; Skilt og stolper (bare om det ikke er inkludert i en annen disiplin)
		_OTH_		"NO-BN-2D-JBT?9_"	_OTH_NAME_	"Annet"					; Andre systemer
	)
	
	; Other administration specific constants
	(setq
		; Track gauges and normal sleeper spacing
		_normalGauge_						1.435	; Distance between the two inner rails for this administration's network.
		_railHeadDistance_					1.500	; Reference value when converting cant (superelevation) into rotations: atan(cant/referenceGauge).
		_sleeperSpacing_					0.600	; This administration's normal sleeper spacing.
		_schematicGauge_					9.000	; Spacing between rails in a schematic 2-line drawing (track circuit isolation, return current etc, showing both rails).
		_schematicTrackSpacing_				21.000	; Standard spacing between track centerlines in a schematic 1-line or 2-line drawing (signaling isolation, return current drawing - not track schematic).
		_geographicTrackSpacing_			4.700	; Standard spacing between track centerlines in real installations.
	
		; Proxy symbols (when no ordinary symbol has been defined)
		_proxySymbolRadius_					1.5
		_oneLetterProxySymbolTextHeight_	2.0
		_twoLetterProxySymbolTextHeight_	1.5
		_threeLetterProxySymbolTextHeight_	1.2
		
		; Symbol description - usually place text below symbols, in UPPERCASE
		_descriptionTextHeight_				_th020_
		_descriptionTextBoxWidth_			4.0
		
		; Hatch pattern densities when these are representing colors : These are administration-specific.
		;_blackHatch_					_solidHatch_
		;_redHatch_						_denseHatch_
		;_blueHatch_					_mediumHatch_
		;_yellowHatch_					_sparseHatch_
		;;_whiteHatch_					Do not hatch
	)

	; stuff, but railway specific:
	(setq
		_freeStanding_ 	"FREESTANDING"	; True: 	Free-standing board or signal (it has its one pole, being an object in its own right)
		_onSignalMast_	nil				; False: 	Not free-standing, mounted on a signal's mast and considered to be a part of the signal object)
		;
		_announcement_	"ANNOUNCEMENT"	; True: 	A warning about something ahead, speed warning etc
		_execution_		nil				; False:	Not announcing something, rather being the excution of something, target for braking curve etc.
		;
		_luminous_		"LUMINOUS"		; True:		A signal or board with an internal light source making it luminous
		_formSignal_	nil				; False:	Not luminious, just a metal plate generally being painted to show a symbol of some kind
		;
		_reflective_	"REFLECTIVE"	; True:		Applies to (non-luminous) form signals: The interesting board surface is reflective
		_passive_		nil				; False:	Not reflective, it will not shine in the dark as a reflective board would
		;
		_movable_		"MOVEABLE"		; True:		Moveable board or signal, i.e. can swing 0 or 90 degrees to be visible or not from the driver
		_fixed_			nil				; False:	Not moveable, the board or signal will always show its front to the driver
		;
		_mechanical_	"MECHANICAL"	; True:		Applies to moveable boards or signals, actuation without electricity (hand-thrown)
		_electrical_	nil				; False:	Not mechanical, the board or signal is moved using an electrically driven actuator
	)
)
