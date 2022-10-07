;=========================================================================================================================
; 
;
; DE-DB Signal-Kombinationen.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================



(defun DEDB-KLASSE-B-KOMPAKTSIGNAL ( / bt hp ra vr vo er zu ga gv ri gg mt sg  mountingMethod config thisBlockName thisDescription )
;
; Deutsche Bahn signal combos - only for signals of type "Kompaktsignal" (Ks).
; Ref 1: Richtlinien 301 – Signalbuch, V11, 2020-12-13
; Ref 2: 819.9002 Symbole für sicherungstechnische Pläne, V2.0, 2021-05-01
;
; (lots of other ad-hoc signals are modeled in other routines)
; 
;=====================================================================================
; TODO: Freestanding light signals and / or boards which shall be modeled elsewhere:
; ------------------------------------------------------------------------------------
; Ungültig				Ungültiges Signal (St Andrew cross) - board to be attached to main signal & distant signal
; Schutzhalt			Sh2=Schutzhalt.
; Abdrücksignal			Ra6=Halt / Ra7=Langsam abdrücken / Ra8=Mässig schnell abdrücken / Ra9=Zurückziehen
; Rangierhaltetafel		Ra10=Über die Tafel hinaus darf es nicht rangiert werden
; Wartezeichen			Ra11=Auftrag des Wärters zur Rangierfahrt abwarten NB! May occur with Formsignal Sh1 on same mast.
; Grenzzeichen			Ra12(???)/So12=Grenze, bis zu der bei zusammenlaufenden Gleisen das Gleis besetzt werden darf. 
; Isolierzeichen		Ra13=Kennzeichnung der Grenze der Gleisisolierung
; Weichenzignal			Wn1=Gerader Zweig / Wn2=Gebogener Zweig / Wn3=Gerade von links nach rechts /
;	---""---				Wn4=Gerade von rechts nach links / Wn5=Bogen von links nach links / Wn6=Bogen von rechts nach rechts
; Sperrsignal			(Freestanding shunting signal in main track) Sh0=Halt! Fahrverbot / Ra12=Rangierfahrt erlaubt.
; Bahnübergangstafel	Zs9=Nach dem zulässigen Vorbeifahren an dem Halt zeigenden oder gestörten
;	---""---				Lichthauptsignal Halt vor dem Bahnübergang! Weiterfahrt nach Sicherung.
; Gleissperrensignal	(Formsignal oder Lichtsignal) Sh0=Halt! Fahrverbot (Sperre aufgelegt) / Sh1=Fahrverbot aufgehoben ODER Wn7=Gleissperre abgelegt
; Stumpfgleissignal		(Formsignal oder Lichtsignal) Sh0=Halt! Fahrverbot
; Bremseprobesignal		Zp6=Bremse anlegen / Zp7=Bremse lösen / Zp8=Bremse in Ordnung
; Abfahrsignal			Zp9=Abfahren
; Türschließauftrag		Zp10=Türen schließen
; Fahrtanzeiger			(no number)=Orientierung, dass der Fahrdienstleiter der Abfahrt des Zuges zustimmt
; Block-Kennzeichen		To be modeled as a board (separate object) that may be attached to a Ks-Signal (not part of the Ks-symbol).
; Rautentafel			Zs103=Das Halt zeigende Hauptsignal gilt nicht für Rangierabteilungen: To be modeled as a separate board attached to a Ks-Signal.
; Schiebelokomotiv-Halt	Ts2=Halt für zurückkehrende Schiebelokomotiven und Sperrfahrten (free-standing board)
; Schiebelokomotiv-Wtf.	Ts3=Weiterfahrt für zurückkehrende Schiebelokomotiven und Sperrfahrten
; Zuordnungstafel		So20=Das durch die Zuordnungstafel gekennzeichnete Signal gilt für das Gleis, auf das die Spitze des Dreiecks weist (ARROW right/left, both)
; Trapeztafel			Ne1=Kennzeichnung der Stelle, wo bestimmte Züge vor einer Betriebsstelle zu halten haben.
; Vorsignaltafel		Ne2=Kennzeichnung des Standorts eines Vorsignals. Several variants (board, with light etc)
; Vorsignalbaken		Ne3=Ein Vorsignal ist zu erwarten. 3/2/1 "/" slanted lines = 250m/175m/100m before a main signal with ATP "release" (distant signal) function
; Schachbretttafel		Ne4=Das Hauptsignal steht – abweichend von der Regel – an einem anderen Standort
; Haltetafel			Ne5=Kennzeichnung des Halteplatzes der Zugspitze bei planmäßig haltenden Zügen ("Toglengdeskilt" ca)
; Haltepunkttafel		Ne6=Ein Haltepunkt ist zu erwarten ("Bremsestolpe")
; Schneepflugtafel		Ne7a=Pflugschar heben / Ne7b=Pflugschar senken
; Ankündigungsbake		Ne12=Überwachungssignal einer Rückfallweiche beachten
; Rückfallweiche		Ne13a=Die Rückfallweiche ist gegen die Spitze befahrbar / Ne13b=Die Rückfallweiche ist gegen die Spitze nicht befahrbar, vor der Weiche halten
; ETCS-Halt-Tafel		Ne14=Halt für Züge in ETCS-Betriebsart SR (leftsided/rightsided/above) + ID-boards
; ID-Schilder für Ks	Signal identification plates, all sorts and sizes
; Endtafel				So1=Fahren auf Sicht beenden
; Hauptsignalbaken		So19=Ein Hauptsignal ist zu erwarten. 3/2/1 Punkte = 250m/175m/100m before a main signal with ATP "stop" function
; Kreuztafel			So106=Bei fehlendem Vorsignal wird angezeigt, dass ein Hauptsignal zu erwarten ist
; 
;
;
; LANGSAMFAHRSCHEIBEN		Temporary speed restrictions - Optical or board, many aspects: Lf1-2-3-4
; GESCHWINDIGKEITSTAFELN	Permanent speed restrictions, many boards. Arrows.
;
;
;
;=====================================================================================
; TODO:  Several signals to be modeled as boards (possibly board with light) + arrows
; FAHRLEITUNGSSIGNALE	Overhead Catenary System
; ERTMS boards (other than Marker Boards)
; Typhoon (horn) boards
; Level crossing signals and boards towards trains / towards car traffic (with triangle board for shortened braking distance), Rautentafeln +++
; Telephone boards
; ATP (PZB) boards
; Maintenance personnel
; Third person protection and information boards
;=====================================================================================
;
;
;
; Signal combination principles: 
; - The loop covers only combinations with HP, VR or VP+VR. The "freestanding others" have their own loops / calls.
; - Signals with either HP or VR may NOT be mounted in "Low" position.
; - TODO: Which signals, where HP=VR=0, may be mounted low / yoke / wall / roof ?
; - Ks-Signal with shunting (Ra12) is only possible when a main signal (Hp) is present.
;
;
;
;    +------------------------------- Bt 0=Zugbedient, 1=Zug- oder Stellwerksbedient, 2=Stellwerksbedient
;    |  +----------------------------- Hp 0=Kein, 1=Ks-Hauptsignal (Hp0=Halt/Ks1=Fahrt)
;    |  |  +--------------------------- Ra 0=Kein, 1=Ks-Rangiersignal am Mast (Sh1=Fahrverbot aufgehebt + Ra12=Abdrücken)
;    |  |  |  +------------------------- Vr 0=Kein, 1=Ks-Vorsignal (Ks1=Fahrt/Ks2=Halt erwarten)
;    |  |  |  |  +----------------------- Vo 0=Kein, 1=Vorsichtsignal (Zs7=Am Signal Hp 0 oder am gestörten Lichthauptsignal ohne schriftlichen
;												Befehl vorbeifahren! Weiterfahrt auf Sicht)
;    |  |  |  |  |  +--------------------- Er 0=Kein, 1=Ersatzsignal (Zs1=Am Signal Hp0 oder am gestörten Lichthaupts. ohne schr. Befehl vorbeifahren)
;    |  |  |  |  |  |  +------------------- Zu 0=Kein, 1=Zusatzlicht (mehr als 5% verkürzten Abstand des Bremswegs)
;    |  |  |  |  |  |  |  +----------------- Ga 0=Kein, 1=Geschwindichkeitsanzeiger (Zs3=Die durch die Kennziffer angezeigte Geschwindigkeit
;    |                                                 darf vom Signal ab im anschließenden Weichenbereich nicht überschritten werden)
;    |  |  |  |  |  |  |  |  +--------------- Gv 0=Kein, 1=Geschwindichkeitsvoranzeiger (Zsv3=Geschwindigkeitsanzeiger (Zs 3) erwarten)
;    |  |  |  |  |  |  |  |  |  +------------- Ri 0=Kein, 1=Richtungsvoranzeiger (Zs2v) = Richtungsanzeiger (Zs2) (they are drawn the same way)
;    |  |  |  |  |  |  |  |  |  |  +----------- Gg 0=Kein, 1=Gegengleisanzeiger (Zs6=Der Fahrweg führt in das Streckengleis entgegen
;    |                                                    der gewöhnlichen Fahrtrichtung / Zs8 mit Ersatzsignal)
;    |  |  |  |  |  |  |  |  |  |  |  +--------- Mt 0=Kein, 1=MTafel (Zs12=Am Halt zeigenden oder gestörten Hauptsignal auf mündlichen
;    |                                                      oder fernmündlichen Auftrag vorbeifahren)
;    |  |  |  |  |  |  |  |  |  |  |  |  +------- Sg 0=Kein, 1=Stumpfgleis- und Frühhaltanzeiger (Zs13=Fahrt in ein Stumpfgleis oder in
;    |                                                       Gleis mit verkürztem Einfahrweg)
;    |  |  |  |  |  |  |  |  |  |  |  |  | 
;--- BT-HP-RA-VR-VO-ER-ZU-GA-GV-RI-GG-MT-SG

	(SetCadSystemDefaults)
	
	;(foreach mountingMethod '("MAST" "BRUECKE" "WAND" "DACH") ; TODO: Implement all mounting methods
	(foreach mountingMethod '("MAST" "BRUECKE")
	;(foreach mountingMethod '("MAST")
		
		; The 'Unknown combination' signal symbol is this one (first to be made in the loop):
		; -------------------BT-HP-RA-VR-VO-ER-ZU-GA-GV-RI-GG-MT-SG 
		;(DEDB-KOMPAKTSIGNAL 0  0  0  0  0  0  0  0  0  0  0  0  0  mountingMethod)

		; The following 'setq' is only needed if one or more of the "foreach" loops have been commented out, 
		; to avoid 'nil' values for commented-out arguments:
		(setq bt 0 hp 0  ra 0  vr 0  vo 0  er 0  zu 0  ga 0  gv 0  ri 0  gg 0  mt 0  sg 0) 

		;(foreach bt '(0 1 2)	; Bedientyp (3 verschiedene)
		(foreach bt '(  1  )	; Bedientyp (only '1=Zug- oder Stellwerksbedient')
			;(foreach vo '(0 1)		; Vorsichtsignal
			(foreach vo '(0  )		; Vorsichtsignal
				(foreach er '(0 1);		; Ersatzsignal
				;(foreach er '(0  );		; Ersatzsignal
					;(foreach zu '(0 1)		; Zusatzsignal
					(foreach zu '(0  )		; Zusatzsignal
						;(foreach ga '(0 1 2)	; Geschwindichkeitsanzeiger Formsignal/Lichtsignal
						(foreach ga '(0   2)	; Geschwindichkeitsanzeiger Formsignal/Lichtsignal
							;(foreach gv '(0 1 2)	; Geschwindichkeitsvoranzeiger Formsignal/Lichtsignal
							(foreach gv '(0   2)	; Geschwindichkeitsvoranzeiger Formsignal/Lichtsignal
								;(foreach ri '(0 1 2)	; Richtungsanzeiger/Richtungsvoranzeiger
								(foreach ri '(0   2)	; Richtungsanzeiger/Richtungsvoranzeiger
									;(foreach gg '(0 1 2)	; Gegengleisanzeiger Formsignal/Lichtsignal
									(foreach gg '(0   2)	; Gegengleisanzeiger Formsignal/Lichtsignal
										;(foreach mt '(0 1)		; Müntlichkeitstafel
										(foreach mt '(0  )		; Müntlichkeitstafel
											;(foreach sg '(0 1 2)	; Stumpfgleis- und Frühhaltanzeiger Formsignal/Lichtsignal
											(foreach sg '(0   2)	; Stumpfgleis- und Frühhaltanzeiger Formsignal/Lichtsignal
												(progn
													(DEDB-PrintSignalInfo)
													;-------------------BT-HP-RA-VR-VO-ER-ZU-GA-GV-RI-GG-MT-SG mountingMethod)
													(DEDB-KOMPAKTSIGNAL bt 1  0  0  vo er zu ga gv ri gg mt sg mountingMethod)
													(DEDB-KOMPAKTSIGNAL bt 1  1  0  vo er zu ga gv ri gg mt sg mountingMethod)
													(DEDB-KOMPAKTSIGNAL bt 1  0  1  vo er zu ga gv ri gg mt sg mountingMethod)
													(DEDB-KOMPAKTSIGNAL bt 1  1  1  vo er zu ga gv ri gg mt sg mountingMethod)
													(DEDB-KOMPAKTSIGNAL bt 0  0  1  vo er zu ga gv ri gg mt sg mountingMethod)
												)
											);sg
										);mt
									);gg
								);ri
							);gv
						);ga
					);zu
				);er
			);vo
		);bt
	);mountingMethod
)


(defun DEDB-PrintSignalInfo ( / )
	(TraceLevel3
		(strcat "KOMPAKTSIGNAL" 
			" BT:" (itoa bt)
			" HP:" (itoa hp)
			" RA:" (itoa ra)
			" VR:" (itoa vr)
			" VO:" (itoa vo) 
			" ER:" (itoa er)
			" ZU:" (itoa zu)
			" GA:" (itoa ga) 
			" GV:" (itoa gv) 
			" RI:" (itoa ri) 
			" GG:" (itoa gg) 
			" MT:" (itoa mt) 
			" SG:" (itoa sg) 
			" - " mountingMethod
		)
	)
)
(if _DEBUG_ (defun psi (/) (DEDB-PrintSignalInfo))) ; Shortcut




(defun DEDB-KOMPAKTSIGNAL ( bt hp ra vr vo er zu ga gv ri gg mt sg  mountingMethod / blockName description )
	;---------------------------------------------------------------------------------
	; DEBUG: Uncomment one of the lines below to set arguments:
	; mountingMethod is one of: "MAST" "BRUECKE" "WAND" "DACH"
	;
	; (setq bt 1 hp 1  ra 1  vr 1  vo 1 er 0  zu 1  ga 0  gv 0  ri 0  gg 0  mt 0  sg 0  mountingMethod "MAST" )  
	;
	; (setq bt 1 hp 1  ra 1  vr 1  vo 1 er 1  zu 1  ga 1  gv 1  ri 0  gg 0  mt 0  sg 0  mountingMethod "MAST" )  
	;
	;---------------------------------------------------------------------------------

	(setq 
		blockName		(strcat _SIG_ "SIG-" "KS-SIGNAL"	)
		description		(strcat "KS-SIGNAL"					)
	)

	; Set other locals
	(setq 
		config _emptyString_
	)
	
	; Set globals
	(InitalizeSignalSymbol)

	; The 'Unknown combination' signal symbol
	
	(if (= 0 (+ bt hp ra vr vo er zu ga gv ri gg mt sg))
		(progn
			(setq blockName (strcat blockName "")) ; No aspect names are added => then RC will choose the one with just the base blockname.
			(AddMissingSymbol)
		)
	)

	; Vorsichtsignal
	(if (= vo 1)(DEDB_AddVo))

	; Ersatzsignal
	(if (= er 1)(DEDB_AddEr))

	; Geschwindichkeitsanzeiger
	(if (= ga 1)(DEDB_AddGa _onSignalMast_ _formSignal_))
	(if (= ga 2)(DEDB_AddGa _onSignalMast_ _luminous_))
	
	; Hp/Ra/Vr combinations 5 possibilities - The freestanding Ra is modeled elsewhere as "Sperrsignal" with another symbol type.
	(cond 
		((and (= hp 0) (= ra 0) (= vr 0))	(alert "*** DE-DB Signal combinations: HP=RA=VR=0 should not occur here!"))
		((and (= hp 1) (= ra 0) (= vr 0))	(DEDB_AddHp bt zu)		) ; Hauptsignal can have Zusatzlicht
		((and (= hp 1) (= ra 1) (= vr 0))	(DEDB_AddHpRa bt zu)	) ; Hauptsignal can have Zusatzlicht
		((and (= hp 1) (= ra 0) (= vr 1))	(DEDB_AddHpVr bt zu)	) ; Hauptsignal can have Zusatzlicht
		((and (= hp 1) (= ra 1) (= vr 1))	(DEDB_AddHpRaVr bt zu)	) ; Hauptsignal can have Zusatzlicht
		((and (= hp 0) (= ra 0) (= vr 1))	(DEDB_AddVr bt zu)		) ; Vorsignal can have Zusatzlicht
	)

	; Geschwindichkeitvorsanzeiger
	(if (= gv 1)(DEDB_AddGv _onSignalMast_ _formSignal_))
	(if (= gv 2)(DEDB_AddGv _onSignalMast_ _luminous_))
	
	; Richtungsanzeiger/Richtungsvoranzeiger
	(if (= ri 1)(DEDB_AddRi _onSignalMast_ _announcement_))
	(if (= ri 2)(DEDB_AddRi _onSignalMast_ _execution_))
	
	; Gegengleisanzeiger
	(if (= gg 1)(DEDB_AddGg _onSignalMast_ _formSignal_))
	(if (= gg 2)(DEDB_AddGg _onSignalMast_ _luminous_))
	
	; Müntlichkeitstafel
	(if (= mt 1)(DEDB_AddMt))
	
	; Stumpfgleis
	(if (= sg 1)(DEDB_AddSg _formSignal_))
	(if (= sg 2)(DEDB_AddSg _luminous_))
	
	; Let config hold a string which encodes the signal combination:
	; bt: 0=Zugbedient, 1=Zug- oder Stellwerksbedient, 2=Stellwerksbedient
	(if (= bt 0) (setq config (strcat config "-BT0")))
	(if (= bt 1) (setq config (strcat config "-BT1")))
	(if (= bt 2) (setq config (strcat config "-BT2")))
	(if (= hp 1) (setq config (strcat config "-HP")))
	(if (= ra 1) (setq config (strcat config "-RA")))
	(if (= vr 1) (setq config (strcat config "-VR")))
	(if (= vo 1) (setq config (strcat config "-VO")))
	(if (= er 1) (setq config (strcat config "-ER")))
	(if (= zu 1) (setq config (strcat config "-ZU")))
	(if (= ga 1) (setq config (strcat config "-GAF")))	; Form signal
	(if (= ga 2) (setq config (strcat config "-GAL")))	; Luminous
	(if (= gv 1) (setq config (strcat config "-GVF")))	; Form signal
	(if (= gv 2) (setq config (strcat config "-GVL")))	; Luminous
	(if (= ri 1) (setq config (strcat config "-RIV")))	; Announcement (Vor-)
	(if (= ri 2) (setq config (strcat config "-RIA")))	; Execution (Anzeige)
	(if (= gg 1) (setq config (strcat config "-GGF")))	; Form signal
	(if (= gg 2) (setq config (strcat config "-GGL")))	; luminous
	(if (= mt 1) (setq config (strcat config "-MT")))	; always form signal
	(if (= sg 1) (setq config (strcat config "-SGF")))	; Form signal
	(if (= sg 2) (setq config (strcat config "-SGL")))	; Luminous
	(setq config (strcat config "-" mountingMethod))

	; Add yoke pole / add rest of main pole with Hp base:
	(cond
		((= mountingMethod "MAST")
			; A horizontal line as "base", insertion point is at bottom-of-pole.
			(if (= (+ gv ri gg mt sg) 0)
				; There is no auxiliary signal below the main/distant signal - use normal mast:
				(progn
					(ShiftSignalItemsUp (DEDB_GetKsNormalSupportHeight))
					(DEDB_DrawNormalSupportForKs)
				)
			; else
				; There is something below the main/distant signal - use a shortened mast:
				(progn
					(ShiftSignalItemsUp (DEDB_GetKslLowSupportHeight))
					(DEDB_DrawLowSupportForKs)	; Low signals are present, use shoter mast
				)
			)
		)
		((= mountingMethod "BRUECKE")
			; Hanging from above - short pole above topmost item, insertion point is at top-of-pole.
			(ShiftSignalItemsUp (- (+ (DEDB_GetKsYokeSuspensionMastHeight) totalHeight))) ; shift *down*
			(DEDB_DrawKsYokeSuspensionMast)
		)
		((= mountingMethod "WAND")
			; TODO
			; A vertical "base rectangle" on left or on right side, depending on side-of-track.
			; Attached sideways to a wall - short 90-degrees pole "knee" left or right of signal, insertion point is at center-of-base-rectangle.
		)
		((= mountingMethod "DACH")
			; TODO
			; Hanging from above as in a yoke, but from a horizontal "base rectangle", insertion point is at center-of-base-rectangle
		)
		(T (alert "*** Bad mounting method"))
	)
	
	; Use config to set this signal combination's description and block name:
	(setq thisBlockName (strcat blockName config))
	(setq thisDescription (strcat description " " (substr config 2 ))); Skip first '-' character in 'config'

	; Create blocks
	(AddDescriptionBelowOrigin thisDescription _descriptionTextHeight_) 
	(CreateSchematicBlockFromCurrentGraphics thisBlockName)
	(CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
)



;====================================================================================
; Add signal items
;====================================================================================
(defun DEDB_AddVo ( / )
	; DEDB: Vorsichtsignal
	(ShiftSignalItemsUp (DEDB_GetKsAuxiliaryItemHeight))
	(DEDB_DrawVo)
	(setq topOfMast (- topOfMast (DEDB_GetKsAuxiliaryItemHeight))) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
)



(defun DEDB_AddEr ( / )
	; DEDB: Ersatzsignal
	(ShiftSignalItemsUp (DEDB_GetKsAuxiliaryItemHeight))
	(DEDB_DrawEr)
	(setq topOfMast (- topOfMast (DEDB_GetKsAuxiliaryItemHeight))) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
)



(defun DEDB_AddGa ( freeStanding luminous / )
	; DEDB: Geschwindichkeitsanzeiger
	(ShiftSignalItemsUp (DEDB_GetKsAuxiliaryItemHeight))
	(DEDB_DrawGa freeStanding luminous)
	(setq topOfMast (- topOfMast (DEDB_GetKsAuxiliaryItemHeight))) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
)



(defun DEDB_AddHp ( bedienTyp zu / )
	; DEDB: Ks Hauptsignal
	(ShiftSignalItemsUp (DEDB_GetKsMainSignalHeight))
	(DEDB_DrawHp bedienTyp zu)
	(setq topOfMast (- topOfMast (DEDB_GetKsMainSignalHeight))) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
)



(defun DEDB_AddHpVr ( bedienTyp zu / )
	; DEDB: Ks Mehrabschnittsignal (Haupt- und Vorsignal)
	(ShiftSignalItemsUp (DEDB_GetKsCombinedSignalHeight))
	(DEDB_DrawHpVr bedienTyp zu)
	(setq topOfMast (- topOfMast (DEDB_GetKsCombinedSignalHeight))) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
)



(defun DEDB_AddHpRa ( bedienTyp zu / )
	; DEDB: Ks Hauptsignal mit Rangiersignal
	(ShiftSignalItemsUp (DEDB_GetKsMainSignalHeight))
	(DEDB_DrawHpRa bedienTyp zu)
	(setq topOfMast (- topOfMast (DEDB_GetKsMainSignalHeight))) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
)



(defun DEDB_AddHpRaVr ( bedienTyp zu / )
	; DEDB: Ks Mehrabschnittsignal mit Rangiersignal
	(ShiftSignalItemsUp (DEDB_GetKsCombinedSignalHeight))
	(DEDB_DrawHpRaVr bedienTyp zu)
	(setq topOfMast (- topOfMast (DEDB_GetKsCombinedSignalHeight))) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
)



(defun DEDB_AddVr ( bedienTyp zu / )
	; DEDB: Ks Vorsignal
	(ShiftSignalItemsUp (DEDB_GetKsDistantSignalHeight))
	(DEDB_DrawVr bedienTyp zu)
	(setq topOfMast (- topOfMast (DEDB_GetKsDistantSignalHeight))) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
)



(defun DEDB_AddGv ( freeStanding luminous / )
	; DEDB: Geschwindichkeitvorsanzeiger
	(ShiftSignalItemsUp (DEDB_GetKsAuxiliaryItemHeight))
	(DEDB_DrawGv freeStanding luminous)
	(setq topOfMast (- topOfMast (DEDB_GetKsAuxiliaryItemHeight))) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
)



(defun DEDB_AddRi ( freeStanding announcement / )
	; DEDB: Richtungsanzeiger
	(ShiftSignalItemsUp (DEDB_GetKsAuxiliaryItemHeight))
	(DEDB_DrawRi freeStanding announcement)
	(setq topOfMast (- topOfMast (DEDB_GetKsAuxiliaryItemHeight))) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
)



(defun DEDB_AddGg ( freeStanding luminous / )
	; DEDB: Gegengleisanzeiger
	(ShiftSignalItemsUp (DEDB_GetKsAuxiliaryItemHeight))
	(DEDB_DrawGg freeStanding luminous)
	(setq topOfMast (- topOfMast (DEDB_GetKsAuxiliaryItemHeight))) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
)



(defun DEDB_AddMt ( / )
	; DEDB: Müntlichkeitstafel
	(ShiftSignalItemsUp (DEDB_GetKsAuxiliaryItemHeight))
	(DEDB_DrawMt)
	(setq topOfMast (- topOfMast (DEDB_GetKsAuxiliaryItemHeight))) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
)



(defun DEDB_AddSg ( luminous / )
	; DEDB: Stumpfgleis
	(ShiftSignalItemsUp (DEDB_GetKsAuxiliaryItemHeight))
	(DEDB_DrawSg luminous)
	(setq topOfMast (- topOfMast (DEDB_GetKsAuxiliaryItemHeight))) ; roll back - topOfMast remains unchanged because this is now the top-of-mast item
	(AddAnchor)
)
