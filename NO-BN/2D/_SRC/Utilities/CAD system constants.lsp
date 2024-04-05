;=======================================================
;
; CAD system constants.lsp
;
; Copyright (c) 2015-2024 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================
;
; CAD system selection
;----------------------------------------------------------
(setq
	; CAD system selector '_CAD_'.
	;================================================================================================
	_ACAD_		"_acad_"	; Autodesk / AutoCAD and Autolisp compatible products from Autodesk
	_BCAD_		"_bcad_"	; Hexagon / Bricsys / BricsCAD. Not implemented as of 2021-04.
	;================================================================================================
	_CAD_		_ACAD_		; CAD SYSTEM SELECTION HERE! YOU MAY NOW TEST ON '(if (= _CAD_ _ACAD_) ...' etc
	;================================================================================================
)

(defun DefineCadSystemConstants ( / )
  (cond 
	((= _CAD_ _ACAD_)
	; IMPORTANT NOTES on AutoLISP intricacies:
	; 
	; 1)
	; Integer division such as (/ 3 2) returns 1, whereas (/ 3 2.0) and (/ 3.0 2) and (/ 3.0 2.0) return 1.5
	;
	; 2)
	; Note: Unicode hex 'nnnn' is '\U+nnnn' in AutoCAD text - but in later versions (2019) of AutoCAD,
	; non-English characters don't work at all. Use globals defined in the global-constant file, with
	; (eval (strcat "Skj" _OSLASH_ "testykke")) where you need to use the combined string.
	;
	; 3)
	; From AutoCAD 2022 and later, the LISPSYS system variable controls the interpreter / debugger, this
	; also affects how characters are treated. The LISPSYS variable should be provided as an argument when calling function 
	; MAIN(_) in the 99_Main.lsp file. This should conveniently be set in your personal bootstrap loader .lsp file.
	;
	; LISPSYS=0
	; Visual LISP IDE (VL IDE) is set as the default editor, however AutoLISP functions don't fully support Unicode characters.
	; AutoLISP source (LSP) files when saved and compiled use the ASCII (MBCS) character set.
	; Note: This setting results in the behavior of AutoCAD 2020 and earlier releases, and is supported on Windows only.
	;
	; LISPSYS=1
	; Visual Studio (VS) Code is set as the default editor and AutoLISP functions fully support Unicode characters.
	; AutoLISP source (LSP) files, when saved, use the encoding set in VS Code, and when compiled, they use the Unicode character set.
	;
	; LISPSYS=2
	; Visual Studio (VS) Code is set as the default editor and AutoLISP functions fully support Unicode characters.
	; AutoLISP source (LSP) files, when saved, use the encoding set in VS Code, and when compiled they use the ASCII (MBCS) character set.
	;
	; We cannot use special national characters in folder names in a 'findfile' Visual LISP command.
	; It also appears that Windows substitutes 'oe' for Scandinavian 'ø' in folder names without telling - so
	; avoid 'oe' as well (in folder names). (Discovered 2020-04-06 by CLFEY on Norconsult PC.)
	;
	; 3)
	; AutoLISP ignores overloaded commands when a dot "." precedes the command name, e.g. .MOVE
	;
	; 4)
	; AutoLISP interprets the command and its parameters according to the US-EN locale when an underscore '_'
	; precedes the command name or attribute name.
	;
	; 5) 
	; Subcommand names such as "Justify" in the ATTDEF command will lead to an error, write only "J" (or better,
	; "_J") and you're fine when writing LISP. This is NOT very consequently programmed in AutoLISP, since for
	; instance '(command "._MOVE" "_ALl" "" "0,0" "0,1")' (move everything right by 1 unit) accepts both the unique
	; abbreviation "AL" ("_AL") and the full text "ALL" (_selectAll_). But (command _ATTDEF_ _ENTER_ "tagname"
	; "prompt" "default_text" "Justify" _middleCenter_ "0,0" 3.5 0) (middle center adjustment at (0,0) with 3.5 high
	; letters, 0 deg rotation) does NOT work. The ATTDEF command requires you to use ONLY the letters that are
	; highlighted when using the command-line version command, i.e. "J" ("_J") instead of "Justify" ("_Justify").
	; The same goes for '"ARC" "CE"...' which works, but '"ARC" "CEnter"...' does NOT work.
	;
	; 6)
	; LISP function names must contain at least one letter (only digits does not work, of course).
	;
	(setq
		; Symbol modes (RailCOMPLETE needs this to select the type of annotative / not annotative symbol graphics)
		; Use these as suffix to the basic symbol names (blockName).
		; A corresponding declaration will be made in DNA to pick the appropriate symbol to show in CAD modelspace.
		_schematic_						"-Schematic"	; Non-annotative symbol size according to the administration's symbol catalogue for schematic drawings
		_scalable_						"-Geographic"	; Annotative symbol size according to the administration's symbol catalogue for use in geo drawings
		_metric_						"-Metric"		; Non-annotative real-size items, for instance metal-free area, bolt groups etc, for non-annotative use in geo drawings
	
		
		
		; Character sets, font sizes, text attributes and justification
		;============================================================================

		; Strings
		_emptyString_					""			; Same as _ENTER_ but used in a string context.

		; See https://en.wikipedia.org/wiki/List_of_Unicode_characters

		; AutoLISP 	Unicode	  Glyph Dec		HTML		Description and Unicode number
		; =============================================================================
		_NBSP_		"\U+00A0" ;	 	0160	&nbsp;		Non-breaking space	0096
		_IEXCL_		"\U+00A1" ;	¡	0161	&iexcl;		Inverted Exclamation Mark	0097
		_CENT_		"\U+00A2" ;	¢	0162	&cent;		Cent sign	0098
		_POUND_		"\U+00A3" ;	£	0163	&pound;		Pound sign	0099
		_CURREN_	"\U+00A4" ;	¤	0164	&curren;	Currency sign	0100
		_YEN_		"\U+00A5" ;	¥	0165	&yen;		Yen sign	0101
		_BRVBAR_	"\U+00A6" ;	¦	0166	&brvbar;	Broken bar	0102
		_SECT_		"\U+00A7" ;	§	0167	&sect;		Section sign	0103
		_UML_		"\U+00A8" ;	¨	0168	&uml;		Diaeresis (Umlaut)	0104
		_COPY_		"\U+00A9" ;	©	0169	&copy;		Copyright sign	0105
		_ORDF_		"\U+00AA" ;	ª	0170	&ordf;		Feminine Ordinal Indicator	0106
		_LAQUO_		"\U+00AB" ;	«	0171	&laquo;		Left-pointing double angle quotation mark	0107
		_NOT_		"\U+00AC" ;	¬	0172	&not;		Not sign	0108
		_SHY_		"\U+00AD" ;		0173	&shy;		Soft hyphen	0109
		_REG_		"\U+00AE" ;	®	0174	&reg;		Registered sign	0110
		_MACR_		"\U+00AF" ;	¯	0175	&macr;		macron	0111
		_DEG_		"\U+00B0" ;	°	0176	&deg;		Degree sign	0112
		_PLUSMN_	"\U+00B1" ;	±	0177	&plusmn;	Plus-minus sign	0113
		_SUP2_		"\U+00B2" ;	²	0178	&sup2;		Superscript two	0114
		_SUP3_		"\U+00B3" ;	³	0179	&sup3;		Superscript three	0115
		_ACUTE_		"\U+00B4" ;	´	0180	&acute;		Acute accent	0116
		_MICRO_		"\U+00B5" ;	µ	0181	&micro;		Micro sign	0117
		_PARA_		"\U+00B6" ;	¶	0182	&para;		Pilcrow sign	0118
		_MIDDOT_	"\U+00B7" ;	·	0183	&middot;	Middle dot	0119
		_CEDIL_		"\U+00B8" ;	¸	0184	&cedil;		Cedilla	0120
		_SUP1_		"\U+00B9" ;	¹	0185	&sup1;		Superscript one	0121
		_ORDM_		"\U+00BA" ;	º	0186	&ordm;		Masculine ordinal indicator	0122
		_RAQUO_		"\U+00BB" ;	»	0187	&raquo;		Right-pointing double angle quotation mark	0123
		_FRAC14_	"\U+00BC" ;	¼	0188	&frac14;	Vulgar fraction one quarter	0124
		_FRAC12_	"\U+00BD" ;	½	0189	&frac12;	Vulgar fraction one half	0125
		_FRAC34_	"\U+00BE" ;	¾	0190	&frac34;	Vulgar fraction three quarters	0126
		_IQUEST_	"\U+00BF" ;	¿	0191	&iquest;	Inverted Question Mark	0127
		_uAGRAVE_	"\U+00C0" ;	À	0192	&Agrave;	Latin Capital Letter A with grave	0128
		_uAACUTE_	"\U+00C1" ;	Á	0193	&Aacute;	Latin Capital letter A with acute	0129
		_uACIRC_	"\U+00C2" ;	Â	0194	&Acirc;		Latin Capital letter A with circumflex	0130
		_uATILDE_	"\U+00C3" ;	Ã	0195	&Atilde;	Latin Capital letter A with tilde	0131
		_uAUML_		"\U+00C4" ;	Ä	0196	&Auml;		Latin Capital letter A with diaeresis	0132
		_uARING_	"\U+00C5" ;	Å	0197	&Aring;		Latin Capital letter A with ring above	0133
		_uAELIG_	"\U+00C6" ;	Æ	0198	&AElig;		Latin Capital letter Æ	0134
		_uCCEDIL_	"\U+00C7" ;	Ç	0199	&Ccedil;	Latin Capital letter C with cedilla	0135
		_uEGRAVE_	"\U+00C8" ;	È	0200	&Egrave;	Latin Capital letter E with grave	0136
		_uEACUTE_	"\U+00C9" ;	É	0201	&Eacute;	Latin Capital letter E with acute	0137
		_uECIRC_	"\U+00CA" ;	Ê	0202	&Ecirc;		Latin Capital letter E with circumflex	0138
		_uEUML_		"\U+00CB" ;	Ë	0203	&Euml;		Latin Capital letter E with diaeresis	0139
		_uIGRAVE_	"\U+00CC" ;	Ì	0204	&Igrave;	Latin Capital letter I with grave	0140
		_uIACUTE_	"\U+00CD" ;	Í	0205	&Iacute;	Latin Capital letter I with acute	0141
		_uICIRC_	"\U+00CE" ;	Î	0206	&Icirc;		Latin Capital letter I with circumflex	0142
		_uIUML_		"\U+00CF" ;	Ï	0207	&Iuml;		Latin Capital letter I with diaeresis	0143
		_ETH_		"\U+00D0" ;	Ð	0208	&ETH;		Latin Capital letter Eth	0144
		_uNTILDE_	"\U+00D1" ;	Ñ	0209	&Ntilde;	Latin Capital letter N with tilde	0145
		_uOGRAVE_	"\U+00D2" ;	Ò	0210	&Ograve;	Latin Capital letter O with grave	0146
		_uOACUTE_	"\U+00D3" ;	Ó	0211	&Oacute;	Latin Capital letter O with acute	0147
		_uOCIRC_	"\U+00D4" ;	Ô	0212	&Ocirc;		Latin Capital letter O with circumflex	0148
		_uOTILDE_	"\U+00D5" ;	Õ	0213	&Otilde;	Latin Capital letter O with tilde	0149
		_uOUML_		"\U+00D6" ;	Ö	0214	&Ouml;		Latin Capital letter O with diaeresis	0150
		_TIMES_		"\U+00D7" ;	×	0215	&times;		Multiplication sign	0151
		_uOSLASH_	"\U+00D8" ;	Ø	0216	&Oslash;	Latin Capital letter O with stroke	0152
		_uUGRAVE_	"\U+00D9" ;	Ù	0217	&Ugrave;	Latin Capital letter U with grave	0153
		_uUACUTE_	"\U+00DA" ;	Ú	0218	&Uacute;	Latin Capital letter U with acute	0154
		_uUCIRC_	"\U+00DB" ;	Û	0219	&Ucirc;		Latin Capital Letter U with circumflex	0155
		_uUUML_		"\U+00DC" ;	Ü	0220	&Uuml;		Latin Capital Letter U with diaeresis	0156
		_uYACUTE_	"\U+00DD" ;	Ý	0221	&Yacute;	Latin Capital Letter Y with acute	0157
		_uTHORN_	"\U+00DE" ;	Þ	0222	&THORN;		Latin Capital Letter Thorn	0158
		_SZLIG_		"\U+00DF" ;	ß	0223	&szlig;		Latin Small Letter sharp S	0159
		_AGRAVE_	"\U+00E0" ;	à	0224	&agrave;	Latin Small Letter A with grave	0160
		_AACUTE_	"\U+00E1" ;	á	0225	&aacute;	Latin Small Letter A with acute	0161
		_ACIRC_		"\U+00E2" ;	â	0226	&acirc;		Latin Small Letter A with circumflex	0162
		_ATILDE_	"\U+00E3" ;	ã	0227	&atilde;	Latin Small Letter A with tilde	0163
		_AUML_		"\U+00E4" ;	ä	0228	&auml;		Latin Small Letter A with diaeresis	0164
		_ARING_		"\U+00E5" ;	å	0229	&aring;		Latin Small Letter A with ring above	0165
		_AELIG_		"\U+00E6" ;	æ	0230	&aelig;		Latin Small Letter Æ	0166
		_CCEDIL_	"\U+00E7" ;	ç	0231	&ccedil;	Latin Small Letter C with cedilla	0167
		_EGRAVE_	"\U+00E8" ;	è	0232	&egrave;	Latin Small Letter E with grave	0168
		_EACUTE_	"\U+00E9" ;	é	0233	&eacute;	Latin Small Letter E with acute	0169
		_ECIRC_		"\U+00EA" ;	ê	0234	&ecirc;		Latin Small Letter E with circumflex	0170
		_EUML_		"\U+00EB" ;	ë	0235	&euml;		Latin Small Letter E with diaeresis	0171
		_IGRAVE_	"\U+00EC" ;	ì	0236	&igrave;	Latin Small Letter I with grave	0172
		_IACUTE_	"\U+00ED" ;	í	0237	&iacute;	Latin Small Letter I with acute	0173
		_ICIRC_		"\U+00EE" ;	î	0238	&icirc;		Latin Small Letter I with circumflex	0174
		_IUML_		"\U+00EF" ;	ï	0239	&iuml;		Latin Small Letter I with diaeresis	0175
		_ETH_		"\U+00F0" ;	ð	0240	&eth;		Latin Small Letter Eth	0176
		_NTILDE_	"\U+00F1" ;	ñ	0241	&ntilde;	Latin Small Letter N with tilde	0177
		_OGRAVE_	"\U+00F2" ;	ò	0242	&ograve;	Latin Small Letter O with grave	0178
		_OACUTE_	"\U+00F3" ;	ó	0243	&oacute;	Latin Small Letter O with acute	0179
		_OCIRC_		"\U+00F4" ;	ô	0244	&ocirc;		Latin Small Letter O with circumflex	0180
		_OTILDE_	"\U+00F5" ;	õ	0245	&otilde;	Latin Small Letter O with tilde	0181
		_OUML_		"\U+00F6" ;	ö	0246	&ouml;		Latin Small Letter O with diaeresis	0182
		_DIVIDE_	"\U+00F7" ;	÷	0247	&divide;	Division sign	0183
		_OSLASH_	"\U+00F8" ;	ø	0248	&oslash;	Latin Small Letter O with stroke	0184
		_UGRAVE_	"\U+00F9" ;	ù	0249	&ugrave;	Latin Small Letter U with grave	0185
		_UACUTE_	"\U+00FA" ;	ú	0250	&uacute;	Latin Small Letter U with acute	0186
		_UCIRC_		"\U+00FB" ;	û	0251	&ucirc;		Latin Small Letter U with circumflex	0187
		_UUML_		"\U+00FC" ;	ü	0252	&uuml;		Latin Small Letter U with diaeresis	0188
		_YACUTE_	"\U+00FD" ;	ý	0253	&yacute;	Latin Small Letter Y with acute	0189
		_THORN_		"\U+00FE" ;	þ	0254	&thorn;		Latin Small Letter Thorn	0190
		_YUML_		"\U+00FF" ;	ÿ	0255	&yuml;		Latin Small Letter Y with diaeresis	0191


		; Text heights
		; Text height is 10x linewidth. Font name is based on the linewidth, i.e. '180' is 1.80 m text height (suitable for 1:1000 scale drawings) (0.18 mm linewidth)
		_th020_							0.2 
		_th050_							0.5
		_th070_							0.7
		_th0736_						0.736
		_th100_							1.0
		_th125_							1.25
		_th150_							1.5
		_th180_							1.8
		_th200_							2.0
		_th250_							2.5
		_th300_							3.0
		_th350_							3.5
		_th500_							5.0
		_th700_							7.0



		; Commands and arguments
		;============================================================================

		; Arguments used in several commands
		_ENTER_							""	 		; All commands: An empty string plays the role of an ENTER keystroke, to accept a proposed value inside a command dialogue
		_byBlock_						"_ByBlock"
		_byLayer_						"_ByLayer"

		
		; SELECT command, and all other commands using the SELECT dialogue
		_SELECT_						"._SELECT"
		_selectAll_						"_AL"		; SELECT command: Select all items ("ALl" since "Add" is also a menu choice)
		_lastSelection_					"_L"		; SELECT command: Select last item


		; LAYER command
		; Manipulate CAD layers
		; AuoCAD LAYER 'New' option:
		;     Creates layers. You can create two or more layers by entering names separated by commas.
		; AuoCAD LAYER 'Make' option:
		;     Creates a layer and makes it current. New objects are drawn on the current layer. If no layer exists for 
		;     the name you enter, a new layer with that name is created. The new layer is on and assumes the following 
		;     properties by default: color number 7, the CONTINUOUS linetype, and a lineweight of DEFAULT. If the layer
		;     exists but is turned off, it is turned on.
		_LAYER_							"._LAYER"
		_anyLayerName_					"*"
		_SetLayer_						"_Set"
		_unlockLayer_					"_Unlock"
		_lockLayer_						"_Lock"
		_createNewLayer_				"_New"
		_makeNewLayer_					"_Make"			
		_colorizeLayer_					"_Color"
		_describeLayer_					"_Description"
		_freezeLayer_					"_Freeze"
		_thawLayer_						"_Thaw"
		_plottability_					"_Plot"
		_isNotPlottable_				"_No"
		_turnOnLayer_					"_ON"
		_turnOffLayer_					"_OFf"


		; LAYERP command
		; Restore previous layer state
		_LAYERP_						"._LAYERP"


		; CLAYER
		; Sets the current layer, which is the default drawing layer (for any type of entity, except for TEXT / MTEXT if TEXTLAYER is set to another layer)
		_CLAYER_						"._CLAYER"


		; COLOR command
		_COLOR_							"._COLOR"
		; Define individual drawing colors (specified differently for varying language packs) => See Administration constants.
		_colorWhite_					7; "Blanc" ;	EN:White	FR:Blanc	DE:Weiss
		_colorYellow_					2; "Jaune"	;	EN:Yellow	FR:Jaune	DE:Gelb
		_colorMagenta_					6; "Magenta" ;	EN:Magenta 	FR:Magenta	DE:Magenta
		_colorMetaDataLayer_			62 ; a dark yellowish color

		; LINETYPE command
		; Set line type (ByBlock, ByLayer, continuous, dashed etc)
		_LINETYPE_						"._LINETYPE"
		_setLinetype_					"_S"
	
	
		; LWEIGHT command
		; Set line weight (thickness)
		_LWEIGHT_						"._LWEIGHT"

		
		; CETRANSPARENCY command (system variable)
		; Current entity transparanecy
		; -1 (ByLayer) Transparency value determined by layer (default setting)
		; -2 (ByBlock) Transparency value determined by block
		; 0 Fully opaque (not transparent)
		; 1-90 Transparency value defined as a percentage
		; To change the transparency of existing objects, use the Properties palette or the Layer Properties Manager.
		; Note: Transparency is limited to 90 percent to avoid confusion with layers that are turned off or frozen.
		; The transparency level for new hatch objects is controlled by the HPTRANSPARENCY system variable.
		_CETRANSPARENCY_				"._CETRANSPARENCY"


		; POINT command
		; The application using the symbol librart should use PTYPE command to define the actual point type graphics (regenerates to set size on screen when zooming).
		_POINT_							"._POINT"
		
		
		; LINE command
		_LINE_							"._LINE"

		
		; POLYLINE command (2D version)
		; Usage: 
		;	_POLYLINE_ p1 p2 p3 ... pn _openPolyline_
		;	_POLYLINE_ p1 p2 p3 ... pn _closedPolyline_
		;	_POLYLINE_ p1 _setPolylineArcMode_ _setPolylineArcCenter_ p2 _setPolylineArcAngle_ <number> _ENTER_
		_POLYLINE_						"._PLINE"
		_openPolyline_					""			
		_closedPolyline_				"_C"
		_setPolylineWidth_				"_W"
		_setPolylineLineMode_			"_L"
		_setPolylineArcMode_			"_A"
		_setPolylineArcCenter_			"_CE"
		_setPolylineArcRadius_			"_R"
		_setPolylineArcAngle_			"_A"
		_setPolylineArcDirection_		"_D"
		
		; Linewidths
		_lwZero_						0.0
		_lw050_							0.050


		; RECTANGLE command
		_RECTANGLE_						"._RECTANGLE"

		
		; CIRCLE command
		_CIRCLE_						"._CIRCLE"

		
		; ELLIPSE command
		; Usage:
		;	_ELLIPSE_ _setEllipseCenter_ p1 p2 <number>	
		;	(Center at p1, passes through p2, first axis from p1 to p2, second axis from p2 to point perpendicular to first axis and length <number>)
		_ELLIPSE_						"._ELLIPSE"
		_setEllipseCenter_				"_C"

		
		; ARC command
		_ARC_							"._ARC"
		_setArcCenter_					"_C"
		_setArcAngle_					"_A"

		
		; STYLE command
		; Define text style (font, size etc), such as "ISO"
		; Text style names, font (shape, SHX) files used by STYLE command and all textual commands
		_STYLE_										"._STYLE"
		_rcTextStyle_								"ISO"
		_defaultFontShx_							"iso3098.shx"	; NB! See loading command in MAIN.
		_textStyleAnnotativity_						"_A"
		_textStyleIsAnnotative_	 					"_Y"
		_textStyleIsNonAnnotative_					"_Y"
		_textStyleOrientationIsNotMatchedToLayout_	"_N"
		_textStyleHeightZeroMeansScalable_			"0.0"
		_textStyleWidthFactorOne_					"1.0"
		_textStyleNoObliquing_						"0.0"
		_textStyleNotBackwards_						"_N"
		_textStyleNotUpsideDown_					"_N"
		
		
		; Text justification, used by commands TEXT, MTEXT, ATTDEF
		_topLeft_						"_TL"
		_topCenter_						"_TC"
		_topRight_						"_TR"
		_middleLeft_					"_ML"
		_middleCenter_					"_MC"
		_middleRight_					"_MR"
		_bottomLeft_					"_BL"
		_bottomCenter_					"_BC"
		_bottomRight_					"_BR"
		_fit_							"_F"
		

		; TEXT command
		_TEXT_							"._TEXT"
		_justifyText_					"_J"
		_SetTextStyle_					"_S"

		; MTEXT command
		_MTEXT_							"._MTEXT"
		_setMtextStyle_					"_S"
		_setMtextHeight_				"_H"
		_setMtextLineSpacing_			"_L"
		_mTextLineSpaceinIsAtLeast_		"_A"
		_setMtextJustifcation_			"_J"
		_setMtextRotation_				"_R"
		_setMtextWidth_					"_W"

		
		; ATTDEF command
		; Create an attribute definition entity.
		_ATTDEF_						"._ATTDEF"
		_attDefIsAnnotative_			"_A"
		_setAttdefTextStyle_			"_S"
		_setAttdefJustifcation_			"_J"	; Must be "_J" (not "_justification", doesn't work!)
		_setAttdefRotation_				"_R"
		_setAttdefTextHeight_			"_H"
		_setAttdefTextWidth_			"_W"	; Text box width - only when  _multipleLines_ bit is set,
												; instead of giving a point for the other corner of the 'box'
		
		
		; ATTDEF flags
		_invisible_ 					1
		_constant_ 						2
		_verify_ 						4
		_preset_ 						8
		_lockPosition_ 					16
		_multipleLines_ 				32
		

		; RTOS - Real-to-String command
		; (rtos number [mode [precision]])
		; number: A number.
		; mode: An integer specifying the linear units mode. The mode corresponds to the values 
		; allowed for the LUNITS AutoCAD system variable. The mode can be one of the following numbers:
		;     1 Scientific
		;     2 Decimal
		;     3 Engineering (feet and decimal inches)
		;     4 Architectural (feet and fractional inches)
		;     5 Fractional
		; precision: An integer specifying the precision.
		; The mode and precision arguments correspond to the system variables LUNITS and LUPREC. If you omit 
		; the arguments, rtos uses the current settings of LUNITS and LUPREC.
		; Return Values: A string. The UNITMODE system variable affects the returned string when engineering,
		; architectural, or fractional units are selected (mode values 3, 4, or 5).
		_scientific_					1
		_decimal_						2
		_engineering_					3			; (feet and decimal inches)
		_architectural_					4			; (feet and fractional inches)
		_fractional_					5

	
		; COPY command
		_COPY_							"._COPY"

		
		; MOVE command
		_MOVE_							"._MOVE"
		_setMoveDisplacement_			"_D"
	
	
		; ROTATE command (2D)
		_ROTATE_						"._ROTATE"
	
	
		; MIRROR command
		_MIRROR_						"._MIRROR"
		_eraseMirrorSource_				"_Y"		; Confirm when asked "Erase source objects?"
		_keepMirrorSource_				"_N"		; Decline when asked "Erase source objects?", i.e. keep the source objects as well as the mirror-image

		; Mirror axes
		_xAxis_							"1,0"
		_yAxis_							"0,1"
		_diagonalAxis_					"1,1"
		_reverseDiagonalAxis_			"-1,1"


		; SCALE command
		_SCALE_							"._SCALE"
	
	
		; JOIN command
		_JOIN_							"._-JOIN"


		; HATCH command
		_HATCH_							"._-HATCH"	; NB! The dash '-' brings up to commandline version
		_selectHatchObjects_			"_S"
		_setHatchProperties_			"_P"
		_selectHatchOrigin_				"_O"
		_setNewHatchOrigin_ 			"_S" 
		_doNotStoreHatchOriginAsDefault_ "_N"
		; Hatch patterns
		_hatchPatternSlantedLines_		"ANSI31"
		_hatchPatternLosanges_			"ANSI37"
		; Hatch pattern densities
		_noHatch_						"_nohatch_"
		_solidHatch_					0.03
		_denseHatch_					0.06
		_mediumHatch_					0.12
		_lightHatch_					0.18
		_sparseHatch_					0.36
	

		; EXPLODE command
		_EXPLODE_						"._EXPLODE"

		
		; POLYGON command
		_POLYGON_						"._POLYGON"
		_inscribedPolygon_				"_I"
		_specifyEdgeOfPolygon_			"_E"
		
		; ARRAY command
		_ARRAY_							"._ARRAY"
		_rectangularArray_				"_R"
		_polarArray_					"_PO"
		_pathArray_						"_PA"
		_rotateObjects_					"_Y"		; When doing polar arrays, confirm that objects shall be rotated as they are placed around a rotational center
		_fullCircle_					360			; Decimal Degrees to fill a full circle with the specified number of items (the preceding number)

		
		; DRAWORDER command
		_DRAWORDER_						"._DRAWORDER"
		_aboveObjects_					"_A"		; Above, relative to other objects
		_underObjects_					"_U"		; Under, relative to other objects
		_aboveAllObjects_				"_F"		; Front, on top of everything else
		_underAllObjects_				"_B"		; Back, below everything else

		
		; WIPEOUT command
		; The AutoLISP system variable WIPEOUTFRAME has three settings:
		; WIPEOUTFRAME = 0		Frames are not displayed or plotted. Frames are temporarily displayed for object selection and selection preview.
		; WIPEOUTFRAME = 1		Frames are displayed and plotted
		; WIPEOUTFRAME = 2		(default) Frames are displayed, but not plotted
		_WIPEOUT_						"._WIPEOUT"
		_createWipeoutFromPolyline_		"_P"
		_keepWipeoutSource_ 			"_N" 		; Keep original item after mirroring
		_eraseWipeoutSource_			"_Y"		; Erase original item after mirroring
		_noWipeout_						nil			; Providing 'nil' instead of a layer definition to one of the DrawCircle / DrawBox functions suppresses adding of wipeout.


		; ERASE command
		_ERASE_							"._ERASE"
		_eraseAll_						"_A"
	
	
		; PURGE command
		_PURGE_							"._PURGE"
		; Primary PURGE arguments (What to purge):
		_purgeAll_						"_A"		; Purge everything that is currently purgable
		_purgeBlocks_					"_B"		; Purge specific blocks (a list of block names follows)
		_purgeDimStyles_				"_D"
		_purgeDetailViewStyles_			"_DE"
		_purgeMultiLeaderStyles_		"_MU"
		_purgeSectionViewStyles_		"_SE"
		_purgeTextStyles_				"_ST"
		_purgeTableStyles_				"_T"
		
		; Other primary PURGE arguments (things that might be purged - NB! Might need extra arguments - CLFEY did not check...):
		_purgeGroups_					"_G"
		_purgeLayers_					"_LA"
		_purgeLineTypes_				"_LT"
		_purgeOrhpanedData_				"_O"
		_purgeMaterials_				"_MA"
		_purgePlotStyles_				"_P"
		_purgeShapes_					"_SH"
		_purgeZeroLengthGeometry_		"_Z"
		
		; Secondary PURGE arguments:
		_purgeAnything_					"*"			; Anything from CAD tool's list
		_purgeWithoutVerification_		"_N"		; Decline the question "Verify each name to be purged?"

		
		; BLOCK command
		_BLOCK_							"._BLOCK"
		_redefineBlock_					"_Y"		; Confirm overwrite of existing block
		_blockAnnotativity_				"_A"		; Initiate the 'annotative?' dialogue
		_blockIsAnnotative_				"_Y"		; Confirm that block shall be created as annotative
		_blockIsNotAnnotative_			"_N"		; Decline that block shall not created as non-annotative
		_keepOrientation_				"_N"		; Negative answer to the question "Orient relative to sheet in paper space viewports?"

		
		; BCLOSE command
		; Close block editor (if open in debugger when script is started, else ignore)
		_BCLOSE_						"._BCLOSE"


		; INSERT command
		_INSERT_						"._INSERT"
		_setInsertionScale_ 			"_S"
		_setInsertionRotation_			"_R"

		
		; SAVEAS command
		_SAVEAS_						"._SAVEAS"
		_overwriteFile_					"_Y"


		; CLOSE command
		; Close CAD file (but cannot close the one that the ongoing Lisp program is running under)
		_CLOSE_							"._CLOSE"
		
		
		
		; Constants for graphics creation (independent of the underlying CAD system)
		;============================================================================

		; Single numeric values
		_zero_							0	; No decimals, this one shall be used bothas double and as integer
		_tenth_							0.10
		_fifth_							0.20
		_quarter_						0.25
		_third_							(/ 1.0 3)
		_half_							0.5
		_twoThirds_						(/ 2.0 3)
		_threeQuarters_					0.75
		_fourFifths_					0.80
		_one_							1.0
		_two_							2.0
		_three_							3.0
		_four_							4.0
		_five_							5.0
		_ten_							10.0
		
		; Offset values
		_offsetZero_					0

		; Decimal Degree angles		
		_angleZero_						0
		_angle15_						15
		_angle27_						27
		_angle29_						29
		_angle30_						30
		_angle45_						45
		_angle90_						90
		_angle180_						180
		_angleMinus45_					315
		_angleMinus90_					270
		
		; 2D points		
		_origin_						'( 0.00  0.00)
		_slightlyBelow_					'( 0.00 -0.05)
		_slightlyAbove_					'( 0.00  0.05)
		_slightlyLeft_					'(-0.05  0.00)
		_slightlyRight_					'( 0.05  0.00)
		
		

		; Various enum lists
		; Active/incative
		_active_						"_active_"
		_inactive_						"_inactive_"
		
		; Single/double
		_single_						"_single_"
		_double_						"_double_"
		
		; Present/Absent
		_present_						"_present_"
		_absent_						"_absent_"
		
		; Manual / Machine
		_manual_						"_manual_"
		_machine_						"_machine_"

		; RailCOMPLETE directions left/right/up/down
		_left_							"_left_"
		_right_							"_right_"
		_up_							"_up_"
		_down_							"_down_"
		
		; Compass directions East(North/West/South
		; Aliases for angles 0-90-180-270 in Decimal Degrees, 360 decimal degrees "mathematical" system, counterclockwise rotation
		; The CAD editor must be set up to work correspondingly.
		_east_							0
		_north_							90
		_west_							180
		_south_							270
	)
   );(ACAD)
  );cond
)
