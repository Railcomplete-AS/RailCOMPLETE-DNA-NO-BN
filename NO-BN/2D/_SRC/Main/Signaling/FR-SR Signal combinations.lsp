;=========================================================================================================================
;
; FR-SR Signal classe Ncombinations.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================



(defun FRSR-SIGNAL-CLASSIQUE-CLASSE-B ( / )
	;=========================================================================================================================
	;
	; Réf. OP 00580, page 25 on signaling symbols.
	; Réf. IN 2488 (2002) on luminous signals, also their interrelation on page 58. DEPRECATED DOCUMENT
	; Réf. IN 1492 (2005, valable dès 2012) on luminous signals (replaces IN 2488).
	;
	; SNCF Réseau represents light signals as an upright rectangle, size 5 wide and 2.5 + n*4 + 2.5 high.
	; Inside the rectangle, there are a total of 14 different signal aspect names. "(X)" means an "X" inside a Ø3.80 circle.
	; Light signals are either mast mounted (mât), galley mounted (potence) or yoke mounted (portique).
	;
	; The symbol is "standing up" in all cases.
	; Aspects which have been deprecated shall be shown with a StAndrew cross over the letter(s).
	; Signals in free air usually have a sunblock screen, "parasol". Also called "Chassis-écran", but we take this to mean the mechanic frame with lanterns with or without the sunscreen.
	; Signals may be mounted "low", where tracks are too close for a signal mast mounting and no yoke is wanted.
	; Low signals are in use for shunting signals, "Carre violet".
	;

	
	; Relevant nomenclature and terminology:
	
	;
	;							IN2488
	;							Page
	;	Châssis-écrans			61		A mechanical frame with lanterns			With or without suncscreen
	;	Panneau					?		Synonym for châssis-écran (frame)			Means "the ensemble of indications that may be shown at the signal"
	;	Cible					?		The mechanical form of the sunscreen		One of A3=3 upright lanterns, C5=5 upright, F7 (6+1 in F-shape), H9 (6+3 H-shape), R6 (2+2+2 round),
	;																				ID2/ID3=3 lying white (or 2+cache) (is there ID2/ID3/ID4 and ID3/ID4/ID5 ?)
	;																				Alternative frame for C signals: rectangular lying with 2 red, or lying rounded 2 red. And 2-light violet for shunting, upright or low lying.
	;	Cache							A blinded spot (no lantern) in a frame		Enables the use of one panneau for many types of indications
	;	Signal							Anything that the driver must observe.
	;	Signal lumineux					Luminous signal. May be a form signal.		Or fixed / temporary speed signals modifying the line speed.
	;	Pancarte						
	;	Plaque							A (reflectorised) metal plate with letters	Used for identification of function and / or location on light signals
	;	Cocarde							Losange-shaped board, colored / letters.	The board can swing into driver's view = FERMÉ (closed signal), or swing out of view
	;																				OUVERT (open signal). Speed signals (tableaux indicateur de vitesse) may be made as cocardes or as luminous signals.
	;	Tableau							A signal that conveys a speed or other		Example: Speed signal. Raise pantograph.
	;									information through shape/color/text,
	;									but not as just lanterns.
	;	XXX ?????						????										We need a word for a signal post that controls stop/proceed indications or warnings about these.
	;
	;
	;							IN2488
	;							Page
	; LMTr						43		Livret de la marche des trains				Pour l'usage des mécaniciens							Description of the line. May contain permanent speed limits only in writing.
	; LTV						52		Limitations temporaires de vitesse			Temporary speed limitations								Due to sudden incidents, planned work etc.
	; Block section lengths		13		BAL => 500m..2800m							BAPR => min. 6000m										BM => from a station to the next station
	
	; SIGNAL SCREENS (chassis-écrans)	Colors from top:
	; ===============================	---------------
	; A							61		G/W-R/x-Y/V/R								Standing row of lanterns								3 lights, average spacing
	; C							61		V/R-W/xx-G-R-Y								Standing row of lanterns								5 lights, dense spacing
	; F							61		(Y+Y)-V/R/x-W/x-G-R-Y						Standing row of lanterns left, single right				6 dense left + 1 same size at op right
	; H							61		Y-(Y/x+Y/x)-(V/R+Y)-W/x-G-R-Y				Two standing rows of lanterns, high to the right		6 dense left + 3 wide spaced right
	; ID2-ID3 (ID4,ID5)			61		(W+W+W/x+W/x+W/x)							Lying track indicator									3..5 lights, wide spacing
	; R							61		(Y/x + Y/x)- G/x -(Y/x + R/x)- Y/x			Disc													6 lights, scattered 2-1-2-1 from top to bottom
	
	
	; BOARDS ON SIGNALS
	; =================
	;
	; IDENTIFICATION BOARD		IN2488		Appearance									Meaning													Usage / Mounting
	; Plaque d'identification	Page						
	; ------------------------	----	-----------------------						---------------------------								---------
	; Signal non franchissable	16,31	White "Nf" on square black board			Need order to proceed. "Nf" is canceled if Oe is active	Topmost on mast. Signals can change role by night (Oe => is a block signal).
	; Signal franchissable		16,31	White "F" on square black board				Towards block of type "BAL" (default type).				Topmost on mast. Line block signal behaviour.
	; Signal de BAPR (*)		16,31	White "PR" on square black board			Towards block with restrictions.						Topmost on mast. Line block signal behaviour.
	; Signal de BM (**)			16,31	White "BM" on square black board			Towards manual block (legacy system?).					Topmost on mast. Line block signal behaviour.
	; Disque					16,31	Black "D" on square white board				Single Red + "D" => bulb error							Topmost of Plaque d'identification / Plaque de repérage / Plaque de cantonnement
	; Avertissement				16,31	Black "A" on square white board				Single Red + "A" => bulb error							Topmost of Plaque d'identification / Plaque de repérage / Plaque de cantonnement
	;							
	; (*)	BAPR = Block Automatique à permissivité restreinte / BAPR VB = BAPR de voie banalisée							
	; (**)	BM = Block manuel							
	;							                                                                                                           
	; LOCATION BOARD			IN2488		Appearance									Meaning													Mounting
	; Plaque de repérage 		Page					                                                                                   
	; -------------------		----	-----------------------						---------------------------								---------
	; Signal number, signal		17		"c.901", "Cv.137", "GA.5" 					Identify the signal (type)								Middle of Plaque d'identification / Plaque de repérage / Plaque de cantonnement. 
	; group number or pos (PK)			(c=carré, Cv=C.violet, GA=guidon d'arrêt)															Note: The digits and letters may be spread over several lines.
	; Possibly also track number		"v.1", "v.2" etc (topmost line)							
	;							
	; BLOCK TYPE BOARD			IN2488		Appearance									Meaning													Mounting
	; Plaque de cantonnement 	Page	(Only relevant to "Carré" signals)							                                        
	; ----------------------	----	--------------------------------------		---------------------------								---------
	; BAPR						18		Black "PR" or "PR vers .." on white square	Type 'BAPR' block autom. permissivité restreinte ahead	Lowest of Plaque d'identification / Plaque de repérage / Plaque de cantonnement
	; BM						18		Black "BM" or "BM vers .." on white square	Type 'BM' blocck manuel ahead							Lowest of Plaque d'identification / Plaque de repérage / Plaque de cantonnement
	; (none)					18		No Plaque de cantonnement					Type 'BAL' block autom. lumineux ahead					(no board)
	;
	; OTHER BOARDS ON SIGNAL MAST
	; ---------------------------
	;							IN2488
	;							Page
	; BJ						37 	Bande lumineuse jaune horizontale			Cautious driving (to platform, short(ened) track)			Lying yellow luminous band on top of signal mast, above frame
	; Annulation				20	St Andrew cross over signal's frame			Not operational
	; VAT + digit				21	[Number] phone [white bluish light]			Voyant d'appel téléphonique: 
	;	 																		- Call dispatcher [after Number
	; 																			  minutes] if signal shows Stop.
	; 																			- Call now if blinking light.
	; VAT + red slash "/"			Barred-out handle [white bluish light]		- Do not call on Stop signal.
	;								Call now if blinking light.
	
	
	; MAIN SIGNALS, DISTANT SIGNALS AND SHUNTING SIGNALS
	; ==================================================
	; 
	; Abr Aspect / aspect name		p.	Luminous physical appearance				Non-luminous appearance			Meaning 							Usage / mounting
	; --- -----------------------	--	-----------------------------------------	-------------------------		------------------					---------------------------
	; vli VL	Voie libre			40	Green steady in vertical(?) frame			(none)							Proceed								At signal in main track, or in shunting track leading to main track.
	; vlc (VL)	Vert clignotant		36	Blinking green in vertical(?) frame			(none)							Expect 160 km/h at A|(A)|R|(R)		Only lines with V > 160 km/h, reduce speed to manageable levels.
	; ra6 (R)	Ralentissement 60	46	2 x Yellow blinking horizontal				(none)							Expect 60 at switch (group) ahead	Free-standing or with other signals, at >= braking dist from first group
	; avc (A)	Jaune clignotant	35	Blinking yellow low in vert. frame or disc	(none)							Expect deferred stop (A follows)	'A' is not installed at braking distance (line speed is too high).
	; ra3 R		Ralentissement 30	45	2 x Yellow steady upper horizontal			Upwards yellow triangle /|\		Expect 30 at switch (group) ahead	Free-standing or with other signals, at >= braking dist from first group
	; ave A		Avertissement		34	Yellow steady low in vert. frame or disc	Yellow losange					Expect stop							Placed at braking distance or more, max 3000 m from Stop signal.
	; rr6 (RR)	Rappel ralent. 60	46	2 x Yellow blinking horizontal				(none)							Proceed at 60 at following switch	Mounted on the closest carré preceding the switch ("Lås 6A")
	; rr3 RR	Rappel ralent. 30	45	2 x Yellow steady upper vertical			Downwards yellow triangle \-/	Proceed at 30 at following switch	Mounted on the closest carré preceding the switch ("Lås 3A")
	; man M		Blanc				41	White steady in vert. or hor. frame			(none)							Proceed with shunting (non-Carré)	Signal "C" (stop for train) can not be given at the same signal
	; mac (M)	Blanc clignotant	41	Blinking white in vert. or hor. frame		(none)							Proceed with shunting (at Carré)	Signal "C" (stop for train) can be given at the same signal.
	; 	   																																			Note: (M) does not allow the vehicle to start or pass as a train.
	; sec (S)	Rouge clignotant	29	Blinking red in vertical(?) frame			(has none)						15 km/h to end of block section		To avoid using semaphore where trains should not stop (platforms etc).
	; sem S		Sémaphore			28	Red steady in vertical(?) frame				Lying red bar.					Stop								Block section ('canton') ahead is occupied.
	; cre C		Carré ('square')	24	Two steady Red vertically or horizontally	2x2 red/white checkerboard		Stop								Stop before opposing or flank movement, parked train etc.
	; cvi Cv	Carré violet		25	Violet steady (vert. or hor. frame)			Violet square board.			Stop (shunting)						Stop shunting movement (generally low mounted). Also used in train tracks.
	;                                                                                                          	
	;     OTHER STOP SIGNAL ASPECTS	
	;     -------------------------
	;     GA	Guidon d'arrêt		26	Red horizontal bar							Lying red bar					Stop								Provides extra stop information in front of LX+.
	;     D		Disque				27	Yellow + Red steady horizontally			Large red disc					Delayed stop						Proceed at sight, stop in front of target.
	;     SAM	Sig. d'arrêt à main	30	Red steady lantern by night					R. flag | 2x2 R+Wh checkerboard	Stop								Work zones, trouble. Mount left, or right for right-hand tracks,
	;																																				or in the track.
	;
	; AUXILIARY LIGHT ON SIGNAL
	; -------------------------
	; Oeilleton ("eyecap")		31	Small white-bluish light at bottom left		-								Nf+Oe=Sém.BAL, Nf+Oe+pdcPR=Sém.BAPR, Nf+Oe+pdcBM=Sém.BM			"Station main signal towards block".
	;								Dark										No oeilleton					Nf = Nf+pdcPR = Nf+pdcBM = Carré	Main signal in a station area.
	; Note: The Oeilleton is there to convert a signal marked as "Nf" (cannot be passed without explicit order) into a block signal (proceed on green without explicit order from dispatcher), canceling the Nf.
	; In essence, the presence of "Nf" and the absence of the "Oe" marks the starting point for a station train route or for a movement towards the next station via a block section.
	;
	;
	;
	; There is also a "low type" 
	; "Type bas"
	; +---+
	; | M |
	; +---+
	; |Cv |
	; +---+
	
	; FREE-STANDING LUMINOUS SIGNALS
	; ==============================
	;                           Réf.IN-2488
	; Aspect / Aspect name				p.	Luminous physical appearance				Non-luminous appearance			Meaning 							Usage
	; ---------------------------		--	-----------------------------------------	-------------------------		------------------					---------------------------
	; SLM	Signal lum. de manoeuvre	75	Wh+Wh alternating vertically				(none)							"Tirez" (come closer)				(High) shunting signal
	;										Wh+Wh alternating horizontally				(none) 							"Refoulez" (go backwards)
	;										Dark										(none)							No shunting permitted
	;										All three white steady for 15 sec			(none)							End of shunting session				
	;                                   
	; LM	Limite de manoeuvre			75	(none)										White "LM" on black square		End of shunting
	;																					with white lining
	;                                   
	; LGR	Limite pour garage 			75	(none)										White "LGR" on black square		End of parking when reversing		In shunting / parking tracks.
	; 		par refoulement																with white lining
	;                                   
	;
	; TLC	Tabl.lum.de corr.v. conv.	67	'T' blanc sur noir (voie 'tiroir').			(none)							Tableau lumineux de correspondance	Vers voie 'tiroir' (impasse).
	;		(s'associe à un Sign.de groupe)	fix=carré suivant ouvert (éteint)											pour voies convergentes
	;		(Bane NOR "Togsporsignal")		cli=carré suivant fermé (rouge)												
	;
	; TIP	Tableau lumineux			68	Luminous digits								(none)							Track number for which the main		Supplement to ~Bane NOR "TOGSPORSIGNAL", for use at the
	; 		indicateur de provenance																					signal on same mast applies			exit from a set of shunting tracks (show who is allowed to leave).
	;                                   
	; ID	Indicateur de direction		64	Horiz. frame with 2..5 white lights			(none)							n Wh: go to track n from the left	Assist the driver in orienting himself properly.
	;                                   
	; TIDD	Tableau indicateur de		64	White band up + inclined left				(none)							Go left | go right at upcoming		Mounted on distant signal before
	;		Direction à Distance			or up + inclined right, on black											switch. Dark if next is at Stop		the Carré preceding the switch
	;                                   
	; TLD	Tableau lumineux de dir.	65	One or more lines of lum. text.				(none)							Identifies the next station or line	Mounted with the the Train Length marker for long trains.
	;                                   
	; SLD	Signal lumineux de départ	66	Half-white, half-green blinking plate		(none)
	;                                   
	; DD	Plaque Demande de départ	66	(none)										Blk "D"+"D" above each other	Ask for permission to depart 1 mn	On signal mast for carré
	; 																					plus phone icon on white sq.	before scheduled departure, or when
	;																													signal opens
	
	;
	;
	; FREE-STANDING small SIGNALS and boards
	; ============================================
	; Aspect, aspect name			page	Luminous 				Non-luminous appearance								Meaning 							Usage
	; --------------------------	----	--------------------	-------------------------							------------------					---------------------------
	; Chevron point en bas			67		(none)					Blk "V" on unframed white square					Braking curve target				Location towards which a braking curve towards switch was given.
	; Chevron point en haut			67		(none)					Blk "^" on unframed white square					Do not pass beyond marker			Location that shall not be passed (dead-end track etc) (fouling point).
	; Repérage heurtoir				32		Red or Violet steady	Red in main track, violet in shunting track			Stop before buffer stop				On or above buffer stop, centered in track
	; Annonce heurtoir				32		As non-l.				Wh 1="Heurtoir" 2="100m" on white framed blk sq.	Buffer stop at nnn meters			In front of buffer stop (40 to 100m typically)
	; Annonce impasse				32		(none)					Wh 1="Imp" on white framed blk square				Dead-end track ahead				Warn driver about dead-end track
	;
	; Mirliton 3 / 2 / 1			14,15	(none)					Upright Wh rectangle, Blk ///, // or /, high or low	90m..110m to signal					Use all three Mirlitons (placed at n*100 +/- 10m) if signa
	;																													190m..210m to signal				sighting distance is not sufficient, meaning that:
	; 																													290m..310m to signal			  	0 < V <=  60 km/h	==>  require 100m sighting
	;							               																							 					60 < V <= 120 km/h	==>  require 200m sighting
	; 																																						120 < V				==>  require 300m sighting
	;
	; LGV Repère F					88		(none)					Y. triangle on Bl.sq., pointing to track, pdi=F		Block signal						For line speeds above 220 km/h
	; LGV Repère Nf	[+ Oe]			88		(none)					Y. triangle on Bl.sq., pointing to track, pdi=Nf	Block signal or Carré				For line speeds above 220 km/h
	; LGV Repère Manoeuvre			89		(none)					Wh chevron on Blk losange pointing to track			Start for shunting or main route	Marks start point for shunting, or train after done shunting
	;
	; CAB at 3 / 2 / 1				89		(none)					Blk "CAB" on Wh sq. with Blk ///, // or /			Expect CAB signaling at n*100m		Announces transition to CABsignaling (at n x 100m ?)
	; CAB begin						89		(none)					Wh "CAB" on Blk sq. with Wh lining  				Transition to CAB signaling			Place on top of the last Carré
	; CAB end						89		(none)					Wh "CAB" on Blk sq. with Wh lining, "/" red bar		CAB signaling ends, lum. starts		Place suitably early before first Nf Carré signal
	;
	;=========================================================================================================================
	;
	; Each symbol is depicted as a rectangular standing frame containing a letter or letter combination on each line.
	; 
	; The following function would print such a symbol's contents:
	;
	;	(defun FRSR-PRINT-SIGNAL-INFO ( / )
	;		(TraceLevel3
	;			(strcat "SIGNAL" 
	;				(if (= vli 0) ".... " "VL.. ")
	;				(if (= vlc 0) ".... " "(VL) ")
	;				(if (= ra6 0) ".... " "(R). ")        
	;				(if (= avc 0) ".... " "(A). ")        
	;				(if (= ra3 0) ".... " "R... ")   
	;				(if (= ave 0) ".... " "A... ")   
	;				(if (= rr6 0) ".... " "(RR) ")         
	;				(if (= rr3 0) ".... " "RR.. ")       
	;				(if (= man 0) ".... " "M... ")   
	;				(if (= mac 0) ".... " "(M). ")        
	;				(if (= smc 0) ".... " "(S). ")        
	;				(if (= sem 0) ".... " "S... ")   
	;				(if (= cvi 0) ".... " "Cv.. ")       
	;				(if (= cre 0) ".... " "C... ")
	;				" - " mountingMethod
	;			)
	;		)
	;	)
	;
	; Instead of generating the whole set of 2^14 = 16,000 symbols, we will just generate one partial line for each aspect ('indication'), plus one 'top' and one 'bottom' element.
	; There is also one symbol for a low shunting signal, a smaller standing rectangle with a horizontal division by the two indications Cv | M.
	; Each middle section aspect comes in several variations. 'XXX' is to be substituted by the relevant 3-letter abbreviation for the individual signal aspects.
	;
	; In the RC application's signal object declaration in DNA, there shall be a custom attibute for each possible aspect, acting as an on/off switch.
	; The symbol declaration shall contain DotLiquid code that puts together the partial elements of the signal's frame.
	; This means that the "VL" aspect may end up in level 1 to 14, but "(S)" can only occur at levels 1,2,3 or 4 etc. "Cv" is always level 1.
	; Several combinations may never occur (such as a frame with just 1 aspect), but we generate them all anyway.

	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-DESSUS"		)		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-DESSUS 				15		) ; Placed at level 0..14 depending on the number of active indications. 
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-DESSOUS"		)		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-DESSOUS						)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION VL  ")		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION "VL"		14		) ; In practice, a signal cannot have that many indications
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION (VL)")		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION "(VL)"	13		) ; because no frame (cible) can accomodate all situations.
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION (R) ")		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION "(R)"	12		)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION (A) ")		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION "(A)"	11		)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION R   ")		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION "R"		10		)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION A   ")		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION "A"		9		)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION (RR)")		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION "(RR)"	8		)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION RR  ")		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION "RR"		7		)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION M   ")		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION "M"		6		)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION (M) ")		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION "(M)"	5		)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION (S) ")		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION "(S)"	4		)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION S   ")		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION "S"		3		)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION Cv  ")		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION "Cv"		2		)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION C   ")		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION "C"		1		)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-HORS-SERVICE"			)		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-HORS-SERVICE				14		) ; St Andrew's cross placed for all possible #indications
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-SUPPRIME"				)		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-SUPPRIME					14		)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-NON-SPECIFIE"			)		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-NON-SPECIFIE						)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-DISQUE"				)		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-DISQUE						14		) ; In practice, a Disque cannot have that many indications
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-GUIDON-ARRET"			)		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-GUIDON-ARRET						)
	(TraceLevel3 "FRSR-SIGNAL-CLASSIQUE-CLASSE-B-CARRE-VIOLET-BAS"		)		(FRSR-SIGNAL-CLASSIQUE-CLASSE-B-CARRE-VIOLET-BAS					) ; Accomodates two- or three indications
)



(defun FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-DESSUS ( levels / blockName description p1 p2 p3 p4 aspectHeight level topBlockName )
	; FR-SR Signal symbol top
	; Depending on the number of aspects to be shown in this frame, the top will be pushed up by 4 x level.
	;
	;     3-----4
	;     1     2
	;     (0.5 + n*4 to leave space for the bottom frame and the aspects below)
	;        .		origin = insertion point in overall symbol
	;
	(setq 
		blockName	(strcat _SIG_ "SIG-" "SIGNAL-CLASSIQUE-PANNEAU-DESSUS"	)
		description (strcat "SIGNAL CLASSIQUE"									) ; + level number, 1 or 2 digits
		p1	'(-2.5 0.0)
		p2	'( 2.5 0.0)
		p3	'(-2.5 0.5)
		p4	'( 2.5 0.5)
		aspectHeight	4.0
	)
	(setq level 0) ; Assume at least one indication present
	(repeat levels
		(DrawLine layDef_Zero p1 p3)
		(DrawLine layDef_Zero p3 p4)
		(DrawLine layDef_Zero p4 p2)
		(AddDescriptionBelowOrigin description -1.5)	; above top
		(MoveUp (+ _half_ (* aspectHeight level)))
		(setq topBlockName (strcat blockName "-" (itoa level)))
		(CreateSchematicBlockFromCurrentGraphics topBlockName)
		(AddGraphicsFromScaledSchematicBlock topBlockName _one_)
		(CreateAnnotativeBlockFromCurrentGraphics topBlockName)
		(setq level (1+ level))
	)
)



(defun FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-DESSOUS ( / blockName p1 p2 p3 p4 )
	; FR-SR Signal symbol bottom
	;
	;     3     4
	;     1--.--2
	;
	(setq 
		blockName	(strcat _SIG_ "SIG-" "SIGNAL-CLASSIQUE-PANNEAU" "-DESSOUS"		)
		p1	'(-2.5 0.0)
		p2	'( 2.5 0.0)
		p3	'(-2.5 0.5)
		p4	'( 2.5 0.5)
	)
	(DrawLine layDef_Zero p3 p1)
	(DrawLine layDef_Zero p1 p2)
	(DrawLine layDef_Zero p2 p4)
	(CreateSchematicBlockFromCurrentGraphics blockName)
	(AddGraphicsFromScaledSchematicBlock blockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics blockName)
)



(defun FRSR-SIGNAL-CLASSIQUE-CLASSE-B-PANNEAU-INDICATION ( ind levels / 
		blockName p1 p2 p3 p4 p5 p6 p7 p8 p9 p5b p6b p7b p8b t1 r 
		aspectHeight level 
		standardBlockName addedBlockName notInServiceBlockName recoveredBlockName
	)
	; Existant (coloré en noir) / à poser (coloré en rouge) / ancienne configuration (coloré en jaune)
	;
	; PRESENT (XXX-PRESENT)
	;     3     4
	;     |     |
	;     |  t1 |		t1 = text-element
	;     |     |
	;     1     2
	;     (0.5 + n*4 to leave space for the bottom frame and the aspects below)
	;        .		origin = insertion point in overall symbol
	;
	; NOT IN SERVICE (XXX-HORS-SERVICE)
	;     3-----4
	;     | \ / |
	;     |  t1 |		t1 = text-element (crossed out by the diagonal lines)
	;     | / \ |
	;     1-----2
	;     (0.5 + n*4 to leave space for the bottom frame and the aspects below)
	;        .		origin = insertion point in overall symbol
	;     
	; ADDED (XXX-AJOUTE)
	;   7-3-----4-8
	;   |*|     |*|
	;   |x|  t1 |y|		t1 = text-element, hatched area to its left and to its right outside the frame
	;   |*|     |*|
	;   5-1-----2-6
	;     (0.5 + n*4 to leave space for the bottom frame and the aspects below)
	;        .		origin = insertion point in overall symbol
	;     
	; RECOVERED (XXX-SUPPRIME)
	;     37---84		7b, 8b
	;     |\\ //|
	;     |  t1 |		t1 = text-element (crossed out by the doubled diagonal lines)
	;     |// \\|
	;     15---62		5b, 6b
	;     (0.5 + n*4 to leave space for the bottom frame and the aspects below)
	;        .		origin = insertion point in overall symbol
	;
	(setq 
		blockName	(strcat _SIG_ "SIG-" "SIGNAL-CLASSIQUE-PANNEAU-INDICATION-" ind	)
		; No description - the letter(s) are already a description in itself
		p1	'(-2.5 0.0)	; ALL
		p2	'( 2.5 0.0)
		p3	'(-2.5 4.0)
		p4	'( 2.5 4.0)

		p5	'(-3.5 0.0)	; ADDED
		p6	'( 3.5 0.0)
		p7	'(-3.5 4.0)
		p8	'( 3.5 4.0)
		px	'(-3.4 2.0)	; inside area to be hatched
		py	'( 3.4 2.0)	; inside area to be hatched

		p5b	'(-2.0 0.0)	; RECOVERED
		p6b	'( 2.0 0.0)
		p7b	'(-2.0 4.0)
		p8b	'( 2.0 4.0)

		t1	'( 0.0 2.0)
		r				1.9
		aspectHeight	4.0
	)

	(defun LocalGraphics (/)
		(DrawLine layDef_Zero p1 p3)
		(DrawLine layDef_Zero p2 p4)
		(if (= (substr ind 1 1) "(")
			(progn 
				; 'Indication' starts with an opening parenthesis '(' ==> draw a circle around the letter:
				(DrawCircleAtPos layDef_Zero t1 r _noWipeout_)
				(if (= (substr ind 3 1) ")")
					(AddTextAtPos layDef_Zero _th250_ t1 (substr ind 2 1)) ; Test is true: just one letter in cercle '(R)'
					(AddTextAtPos layDef_Zero _th250_ t1 (substr ind 2 2)) ; Test is false: assume two letters in cercle '(RR)'
				)
			)
		;else
			(AddTextAtPos layDef_Zero _th250_ t1 ind)
		)
	)
	
	(setq level 0)
	(repeat levels
		(setq
			standardBlockName		(strcat blockName "-PRESENT" 		"-" (itoa level))
			addedBlockName			(strcat blockName "-AJOUTE"			"-" (itoa level))
			notInServiceBlockName	(strcat blockName "-HORS-SERVICE"	"-" (itoa level))
			recoveredBlockName		(strcat blockName "-SUPPRIME"		"-" (itoa level))
		)

		; STANDARD
		(LocalGraphics)
		(MoveUp (+ _half_ (* aspectHeight level)))
		(CreateSchematicBlockFromCurrentGraphics standardBlockName)
		(AddGraphicsFromScaledSchematicBlock standardBlockName _one_)
		(CreateAnnotativeBlockFromCurrentGraphics standardBlockName)
		
;;;		; ADDED
;;;		(LocalGraphics)
;;;		(DrawLine layDef_Zero p1 p5)
;;;		(DrawLine layDef_Zero p5 p7)
;;;		(DrawLine layDef_Zero p7 p3)
;;;		(DrawHatchAtPoint _solidHatch_ px _angleZero_ _offsetZero_)
;;;		(DrawLine layDef_Zero p2 p6)
;;;		(DrawLine layDef_Zero p6 p8)
;;;		(DrawLine layDef_Zero p8 p4)
;;;		(DrawHatchAtPoint _solidHatch_ py _angleZero_ _offsetZero_)
;;;		(DrawLine layDef_Zero p5 p6)
;;;		(DrawLine layDef_Zero p7 p8)
;;;		(MoveUp (+ _half_ (* aspectHeight level)))
;;;		(CreateSchematicBlockFromCurrentGraphics addedBlockName)
;;;		(AddGraphicsFromScaledSchematicBlock addedBlockName _one_)
;;;		(CreateAnnotativeBlockFromCurrentGraphics addedBlockName)
;;;		
;;;		; NOT IN SERVICE
;;;		(LocalGraphics)
;;;		(DrawLine layDef_Zero p1 p2)
;;;		(DrawLine layDef_Zero p3 p4)
;;;		(DrawLine layDef_Zero p1 p4)
;;;		(DrawLine layDef_Zero p2 p3)
;;;		(MoveUp (+ _half_ (* aspectHeight level)))
;;;		(CreateSchematicBlockFromCurrentGraphics notInServiceBlockName)
;;;		(AddGraphicsFromScaledSchematicBlock notInServiceBlockName _one_)
;;;		(CreateAnnotativeBlockFromCurrentGraphics notInServiceBlockName)
;;;		
;;;		; RECOVERED
;;;		(LocalGraphics)
;;;		(DrawLine layDef_Zero p1 p2)
;;;		(DrawLine layDef_Zero p3 p4)
;;;		(DrawLine layDef_Zero p1 p8b)
;;;		(DrawLine layDef_Zero p5b p4)
;;;		(DrawLine layDef_Zero p2 p7b)
;;;		(DrawLine layDef_Zero p6b p3)
;;;		(MoveUp (+ _half_ (* aspectHeight level)))
;;;		(CreateSchematicBlockFromCurrentGraphics recoveredBlockName)
;;;		(AddGraphicsFromScaledSchematicBlock recoveredBlockName _one_)
;;;		(CreateAnnotativeBlockFromCurrentGraphics recoveredBlockName)

		(setq level (1+ level))
	)
)



(defun FRSR-SIGNAL-CLASSIQUE-CLASSE-B-HORS-SERVICE ( levels / blockName p1 p2 p3 p4 aspectHeight level notInServiceBlockName )
	; NOT IN OPERATION - HORS-SERVICE
	; The whole signal is out of service (the St Andrew's cross is to be superimposed on the rectangle showing the selected indications):
	;
	;      + . . +
	;      .     .
	;      .     .
	;    3 .     . 4
	;     \.     ./
	;      \     /
	;      .\   /.
	;      . \ / .
	;      .  /  .		Just the two diagonals, centered for all possible rectangle sizes
	;      . / \ .
	;      ./   \.
	;      /     \
	;     /.     .\
	;    1 .     . 2
	;      .     .
	;      .     .
	;      +  .  +
	;
	;
	(setq 
		blockName	(strcat _SIG_ "SIG-"	"SIGNAL-CLASSIQUE-HORS-SERVICE"	)
		; No description for this one, supposed to be superimposed on other symbol
		p1	'(-3.5 -4.5)
		p2	'( 3.5 -4.5)
		p3	'(-3.5  4.5)
		p4	'( 3.5  4.5)
		aspectHeight	4.0
	)
	(setq level 0)
	(repeat levels
		(setq
			notInServiceBlockName	(strcat blockName "-" (itoa level))
		)
		(DrawLine layDef_Zero p1 p4)
		(DrawLine layDef_Zero p2 p3)
		(MoveUp (HalfOf (+ _half_ (* aspectHeight level))))
		(CreateSchematicBlockFromCurrentGraphics notInServiceBlockName)
		(AddGraphicsFromScaledSchematicBlock notInServiceBlockName _one_)
		(CreateAnnotativeBlockFromCurrentGraphics notInServiceBlockName)

		(setq level (1+ level))
	)
)



(defun FRSR-SIGNAL-CLASSIQUE-CLASSE-B-SUPPRIME ( levels / blockName x y p1 p2 p3 p4 p5 p6 p7 p8 aspectHeight level recoveredBlockName )
	; RECOVERED - SUPPRIMÉ
	; The whole signal is removed (the double St Andrew's cross is to be superimposed on the rectangle showing the selected indications):
	; The whole signal is out of service 
	;
	;      + . . +
	;      .     .
	;   7  .     . 8
	;   3\ .     ./4
	;    \\.     //
	;     \\    //
	;      \\  //.
	;      .\\// .
	;      . //  .		Just the two double diagonals, centered for all possible rectangle sizes
	;      .//\\ .
	;      //  \\.
	;     //    \\
	;    //.     \\
	;   1/ .     .\2
	;   5  .     . 6
	;      .     .
	;      +  .  +
	;
	(setq 
		blockName	(strcat _SIG_ "SIG-"	"SIGNAL-CLASSIQUE-SUPPRIME"	)
		; No description for this one, supposed to be superimposed on other symbol
		p1	'(-3.5 -3.5)
		p2	'( 3.5 -3.5)
		p3	'(-3.5  3.5)
		p4	'( 3.5  3.5)
		p5	'(-3.5 -4.5)
		p6	'( 3.5 -4.5)
		p7	'(-3.5  4.5)
		p8	'( 3.5  4.5)
		aspectHeight	4.0
	)
	(setq level 0)
	(repeat levels
		(setq
			recoveredBlockName	(strcat blockName "-" (itoa level))
		)
		(DrawLine layDef_Zero p1 p8)
		(DrawLine layDef_Zero p3 p6)
		(DrawLine layDef_Zero p5 p4)
		(DrawLine layDef_Zero p7 p2)
		(MoveUp (HalfOf (+ _half_ (* aspectHeight level))))
		(CreateSchematicBlockFromCurrentGraphics recoveredBlockName)
		(AddGraphicsFromScaledSchematicBlock recoveredBlockName _one_)
		(CreateAnnotativeBlockFromCurrentGraphics recoveredBlockName)

		(setq level (1+ level))
	)
)



(defun FRSR-SIGNAL-CLASSIQUE-CLASSE-B-NON-SPECIFIE ( / blockName description x y )
	; NOT SPECIFIED - for use in the symbol insertion jig procedure.
	; This can also be shown whenever all individual aspects have been deselected.
	;
	;      +-----+
	;      |     |
	;      |     |
	;      |     |  
	;      |     | 
	;      |     |
	;      |     |
	;      |     |
	;      |     |
	;      |     |
	;      |     |
	;      |     |
	;      |     |
	;      |     |
	;      |     |
	;      |     |
	;      +--.--+
	;
	;
	(setq 
		blockName	(strcat _SIG_ "SIG-"	"SIGNAL-CLASSIQUE-NON-SPECIFIE"	)
		description	(strcat 				"SIGNAL CLASSIQUE NON-SPECIFIE"	)
		x	5
		y	13
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(MoveUp (HalfOf y))
	(AddDescriptionBelowOrigin description (- (+ 1.5 y )))
	(CreateSchematicBlockFromCurrentGraphics BlockName)
	(AddGraphicsFromScaledSchematicBlock BlockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics BlockName)
)



; TODO
(defun FRSR-SIGNAL-CLASSIQUE-CLASSE-B-DISQUE ( levels / blockName description r p1 p2 p3 )
	; DISQUE (can show 'go slow immediately' or AVERTISSEMENT = cannot show 'go slow immediately'
	;    _____
	;   /     \
	;  |   i1  |	One indication at the centre
	;  |       |
	;   \__.__/
	;
	;    _____
	;   /     \
	;  |   i2  |	Two symmetrical indications
	;  |   i1  |
	;   \__.__/
	;
	;    _____
	;   /  i3 \
	;  |   i2  |	Three symmetrical indications, none of them enclosed in a "blinking" circle
	;  |   i1  |
	;   \__.__/
	;
	;     4----5	(variable height points)
	;     | i'n'	The n-th indication, n between 3 and 14
	;     | :
	;     | :
	;     | i3
	;     | i2
	;     | i1
	;    _|___
	;   / |   \
	;  | 2|3   |	Three or more indications, one or more are enclosed in a "blinking" circle.
	;  |  1    |	The annotation symbol ends in a down-arrow shape.
	;   \__.__/
	;
	(setq 
		blockName	(strcat _SIG_ "SIG-"	"SIGNAL-CLASSIQUE-DISQUE"	)
		description	(strcat 				"SIGNAL CLASSIQUE DISQUE"	)
		r	4.0
		p1	'(-3.5  3.0)
		p2	'( 3.5  3.0)
		p3	'(-3.5 10.0)
		p4	'( 3.5 10.0)
		p5	'(-3.5  2.0)
	)
	(DrawBox layDef_Zero x y _noWipeout_)
	(AddDescriptionBelowOrigin description 0)
	(CreateSchematicBlockFromCurrentGraphics BlockName)
	(AddGraphicsFromScaledSchematicBlock BlockName _one_)
	(CreateAnnotativeBlockFromCurrentGraphics BlockName)
)



; TODO
(defun FRSR-SIGNAL-CLASSIQUE-CLASSE-B-GUIDON-ARRET ( / blockName description x y )
	; GUIDON D'ARRÊT
	; Red 'stop' bar, for use in front of level crossings ++.
	(progn )
)



; TODO
(defun FRSR-SIGNAL-CLASSIQUE-CLASSE-B-CARRE-VIOLET-BAS ( / blockName description r p1 p2 p3 )
	; CARRÉ VIOLET BAS - Low shunting signal
	; Intended solely for two or three indications (low signals can't have many lanterns, they use the 'A' frame which has three lamps max.
	; To be superimposed onto the "normal" indications. Since the normal indications have been placed at (0.5 + 4*level) above the insertion
	; point of the symbol's block, we need to "cheat" and add a short tail of 0.5.
	;
	; 
	; 5-----6		Add the top line to the symbol at 8.5 (two indications) above the insertion point.
	;   ...
	; 3-----4		Division line between the two items
	;   ...
	; 1--9--2		Bottom line at 0.5
	;    .
	;
	;
	; 7-----8		Add the top line to the symbol at 12.5 (three indications) above the insertion point.
	;   ...
	; 				No division line between middle and topmost item
	;   ...
	; 3-----4		Division line between bottom and middle item
	;   ...
	; 1--9--2		Bottom line at 0.5
	;    .
	;
	(progn )
)
