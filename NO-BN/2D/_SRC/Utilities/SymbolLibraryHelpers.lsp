;=======================================================
;
; SymbolLibraryHelpers.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================
;
; Supporting LISP routines, needed for generation of 2D symbol libraries.
;
; Purpose: Provide a low-level CAD system LISP interface, i.e. an Application Programming Interface (API).
; Usage: Build a library of graphics primitive routines on top of this API, applicable to any LISP dialect. Then build concrete 
; railway object symbols from CAD-system independent primitives.
;


; CAD system global constants
;----------------------------------------------------------

(defun defineGlobalCadSystemConstants ( / )
	(princ) ; At least one (dummy) command is needed to make VLIDE print the function's name to the console at load time
	(setq

		; Constants specific to this DNA's railway administration
		;============================================================================

		; Track gauges and normal sleeper spacing
		_normalGauge_					1.435	; Distance between the two inner rails for this administration's network.
		_cantReferenceGauge_			1.500	; Reference value when converting cant (superelevation) into rotations: atan(cant/referenceGauge).
		_sleeperSpacing_				0.600	; This administrations normal sleeper spacing.
		_schematicGauge_				9.000	; Spacing between rails in a schematic 2-line drawing (insulation, return current etc, showing both rails).
		_schematicTrackSpacing_			21.000	; Standard spacing between track centerlines in a schematic 1-line or 2-line drawing (signaling isolation, return current drawing - not track schematic).
		_geographicTrackSpacing_		 4.700	; Standard spacing between track centerlines in real installations.



		; Character sets, font sizes, text attributes and justification
		;============================================================================

		; Strings
		_emptyString_					""
		
		; The 'Scandinavian extras'
		_uAE_							"\U+00C6"	; Æ = Unicode decimal 198 - hex 00C6 - AutoLISP \U+00C6
		_uOE_							"\U+00D8"	; Ø = Unicode decimal 216 - hex 00D8 - AutoLISP \U+00D8
		_uAA_							"\U+00C5"	; Å = Unicode decimal 197 - hex 00C5 - AutoLISP \U+00C5
		_ae_							"\U+00E6"	; æ = Unicode decimal 230 - hex 00E6 - AutoLISP \U+00E6
		_oe_							"\U+00F8"	; ø = Unicode decimal 248 - hex 00F8 - AutoLISP \U+00F8
		_aa_							"\U+00E5"	; å = Unicode decimal 229 - hex 00E5 - AutoLISP \U+00E5
		
		; Text heights
		; Text height is 10x linewidth. Font name is based on the linewidth, i.e. '180' is 1.80 m text height (suitable for 1:1000 scale drawings) (0.18 mm linewidth)
		_th020_							0.2 
		_th050_							0.5
		_th070_							0.7
		_th100_							1.0
		_th125_							1.25
		_th150_							1.5
		_th180_							1.8
		_th250_							2.5
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
		_setLayer_						"_Set"
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
		; Set default drawing color
		_COLOR_							"._COLOR"
		_colorWhite_					"White"
		_colorYellow_					"Yellow"
		_colorMagenta_					"Magenta"
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


		; RECTANGLE command
		_RECTANGLE_						"._RECTANGLE"

		
		; CIRCLE command
		_CIRCLE_						"._CIRCLE"

		
		; ARC command
		_ARC_							"._ARC"
		_setArcCenter_					"_C"
		_setArcAngle_					"_A"

		
		; STYLE command
		; Define text style (font, size etc), such as "ISO"
		; Text style names, font (shape, SHX) files used by STYLE command and all textual commands
		_STYLE_										"._STYLE"
		_rcTextStyle_								"ISO"
		_defaultFontShx_							"iso3098.shx"
		_textStyleAnnotativity_						"_A"
		_textStyleIsAnnotative_						"_Y"
		_textStyleIsNonAnnotative_						"_Y"
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
		

		; TEXT command
		_TEXT_							"._TEXT"
		_justifyText_					"_J"
		_setTextStyle_					"_S"

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
												; instead of giving a pos for the other corner of the 'box'
		
		
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
		_solidHatch_					0.02
		_denseHatch_					0.04
		_mediumHatch_					0.08
		_sparseHatch_					0.16
		; Hatch pattern densities when these are representing colors
		_blackHatch_					_solidHatch_
		_redHatch_						_denseHatch_
		_blueHatch_						_mediumHatch_
		_yellowHatch_					_sparseHatch_
		; _whiteHatch_					Do not hatch
	

		; EXPLODE command
		_EXPLODE_						"._EXPLODE"

		
		; POLYGON command
		_POLYGON_						"._POLYGON"
		_inscribed_						"_I"
		
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
		_noWipeout_						nil			; Providing 'nil' instead of a layer definition to one of the drawCircle / drawBox functions suppresses adding of wipeout.


		; ERASE command
		_ERASE_							"._ERASE"
		_eraseAll_						"_A"
	
	
		; PURGE command
		_PURGE_							"._PURGE"
		_purgeAll_						"_A"		; Purge everything that is currently purgable
		_purgeBlocks_					"_B"		; Purge specific blocks (a list of block names follows)
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
		_angle27_						27
		_angle29_						29
		_angle30_						30
		_angle45_						45
		_angle90_						90
		_angle180_						180
		_angleMinus45_					315
		_angleMinus90_					270
		
		; 2D points		
		_origo_							'( 0.00  0.00)
		_slightlyBelow_					'( 0.00 -0.01)
		_slightlyAbove_					'( 0.00  0.01)
		_slightlyLeft_					'(-0.01  0.00)
		_slightlyRight_					'( 0.01  0.00)
		
		

		; Constants specific to RailCOMPLETE
		;============================================================================

		; Proxy symbols (when no ordinary symbol has been defined)
		_proxySymbolRadius_					1.5
		_oneLetterProxySymbolTextHeight_	2.5
		_twoLetterProxySymbolTextHeight_	1.8
		_threeLetterProxySymbolTextHeight_	1.2
		
		; Symbol modes
		; Use these as suffix to the basic symbol names (blockName).
		; A corresponding declaration will be made in DNA to pick the appropriate symbol to show in CAD modelspace.
		_schematic_						"-Schematic"	; Non-annotative symbol size according to the administration's symbol catalogue for schematic drawings
		_scalable_						"-Geographic"	; Annotative symbol size according to the administration's symbol catalogue for use in geo drawings
		_metric_						"-Metric"		; Non-annotative real-size items, for instance metal-free area, bolt groups etc, for non-annotative use in geo drawings
	
		; Symbol description - usually place text below symbols, in UPPERCASE
		_descriptionTextHeight_			_th020_
		_descriptionTextBoxWidth_		3.0
		
		; 'Enum lists'
		_active_						"_active_"
		_inactive_						"_inactive_"
		;				
		_single_						"_single_"
		_double_						"_double_"
						
		_left_							"_left_"
		_right_							"_right_"
		_up_							"_up_"
		_down_							"_down_"
		
		; Compass directions (alias for angles 0..360 in Decimal Degrees, CW rotation)
		_east_							0
		_north_							90
		_west_							180
		_south_							270
	)
)



; File access from within the CAD system
;----------------------------------------------------------

(defun addSupportPath ( dir / tmp Cpath )
	(vl-load-com)
	(setq
		Cpath (getenv "ACAD") tmp 
		(strcat ";" dir ";")
	)
	(if (not (vl-string-search dir cpath)) (setenv "ACAD" (strcat Cpath ";" dir)))
	dir
)



(defun loadFolder ( folderName / )
	(if (findfile folderName)
		(mapcar 
			'(lambda (x)
				(if (wcmatch (strcase x) "*.LSP")
					(load (strcat (findfile folderName) "\\" x))
				)
			)
			(vl-directory-files (findfile folderName))
		)
		(print "NO .LSP FILES LOADED")
	)
	(princ (strcat _ENTER_ folderName " loaded...\n")) (prin1)
)




; Define global constants before further loading of LISP files
;--------------------------------------------------------------
(defineGlobalCadSystemConstants)



; CAD system default values
;----------------------------------------------------------

(defun purgeAll ( / )
	(command 
		_LAYER_ _unlockLayer_ _anyLayerName_ _ENTER_
		_ERASE_ _selectAll_ _ENTER_
	)
	(command _LAYER_ _setLayer_ "0" _ENTER_)
	(command 
		_PURGE_ _purgeAll_ _anyLayerName_ _purgeWithoutVerification_
		_PURGE_ _purgeAll_ _anyLayerName_ _purgeWithoutVerification_
		_PURGE_ _purgeAll_ _anyLayerName_ _purgeWithoutVerification_
	)
	'purgeAll
)



(defun setCadSystemDefaults ( / )
	(command
		_ERASE_ _selectAll_ _ENTER_ _ENTER_
		"._SNAP" 1.0
		"._OSNAP" "OFF"
		"._OSNAPCOORD" 1
		"._SNAPMODE" 0
		"._GRID" "ON"
		"._GRID" 0.1

		"._UNITS" 2 3 1 3 0 "_NO" ; Numbers=15.500, Angles=<45.000, 0 degrees=East, positive rotation=CCW
		"._INSUNITS" 6 ; Insertion unit - 6=meters
		"._LIGHTINGUNITS" 2 ; 2=International

		"._DIMZIN" 8 ; DIMZIN: Integer Saved in: Drawing Initial value: 0 (imperial) or 8 (metric) 
		; Values 0-3 affect feet-and-inch dimensions only: Value Description 0 
		; Suppresses zero feet and precisely zero inches 1 Includes zero feet and precisely zero inches 2 Includes zero feet and suppresses zero inches 3 Includes 
		; zero inches and suppresses zero feet 4 Suppresses leading zeros in decimal dimensions (for example, 0.5000 becomes .5000) 8 Suppresses trailing zeros in 
		; decimal dimensions (for example, 12.5000 becomes 12.5) 12 Suppresses both leading and trailing zeros (for example, 0.5000 becomes .5) DIMZIN also affects 
		; real-to-string conversions performed by the AutoLISP rtos and angtos functions.

		"._OSMODE" 0
		"._COORDS" 1
		"._PICKBOX" 5
		"._DYNPICOORDS" 1
		"._DYNPIFORMAT" 1
		"._ORTHOMODE" 0
		"._PICKFIRST" 1
		"._PICKADD" 0
		"._ATTREQ" 0
		"._ATTDIA" 0
		"._FILEDIA" 1
		"._ZOOM" "_WINDOW" "-11,-5" "11,5"
	)
	(command
		_POLYLINE_ _origo_ _setPolylineWidth_ _zero_ _zero_ _ENTER_ ; Default to thin polylines (from start to end)
	)
	'setCadSystemDefaults
)


(defun setDefaultObjectPropertiesToByBlock ( / )
	(setLayer layDef_Zero)
	(command
		_COLOR_ _byBlock_
		_LINETYPE_ _setLinetype_ _byBlock_ _ENTER_
		_LWEIGHT_ _byBlock_
		_CETRANSPARENCY_ _byBlock_
	)
	'setDefaultObjectPropertiesToByBlock
)
  
  
  
(defun setCurrentDefaultPropertiesToByLayer ( / )
	(command
		_COLOR_ _byLayer_
		_LINETYPE_ _setLinetype_ _byLayer_ _ENTER_
		_LWEIGHT_ _byLayer_
		"._CETRANSPARENCY" _byLayer_
	)
	'setCurrentDefaultPropertiesToByLayer
)
  
  
  
(defun setPropertiesToByBlockForAllBlocks ( / doc )
	(vlax-for block 
		(vla-get-blocks 
			(setq doc (vla-get-activedocument (vlax-get-acad-object)))
		)
		(vlax-for x block
			(progn
				(if (= (vla-get-layer x) "0")
					(vla-put-color x 0)
				)
				(vla-put-linetype x "_ByBlock")
				(vla-put-lineweight x -2)
				(vla-put-entitytransparency x "_ByBlock")
			)
		)
	)
	(vla-regen doc acActiveViewport)
	'setPropertiesToByBlockForAllBlocks
)


; CAD system style settings
;----------------------------------------------------------

(defun createIsoTextStyle ( / )
	;
	; 'Iso3098' will result in an error if not present in the appropriate AutoCAD folder.
	; Place the preferred font file in the GitHub RailCOMPLETE folder "...\Customization\NO-BN\2D\_SRC\Fonts".
	; When debugging on your computer, make sure this font also resides in "C:\Program Files\Autodesk\AutoCAD 2020\Fonts".
	; Change the folder name according to your AutoCAD application (2018, 2019, 2020...)
	;
	; NB! Inlcude this line: <<<_textStyleAnnotativity_ _textStyleIsAnnotative_ _textStyleOrientationIsNotMatchedToLayout_>>> after
	; the "_defaultFontShx_" line if the tect style should be annotative.
	; Note that a non-annotative text style used in a non-annotative TEXT / MTEXT / ATTDEF inside an annotative block, will behave
	; as an annotative entity. Also note that if the text style is annotative, then a non-annotative TEXT / MTEXT / ATTDEF inside a 
	; non-annotative block definition will behave annotatively, probably contraty to what you would need.
	; ==> I.e. only non-anno text style will be needed in your DNA's symbol library.
	;
	; "TextHeight = 0.0" means that the text can vary in size (after creation). 'Width' is the width ratio, 0.8 makes narrow letters etc.
	;
	(command _STYLE_ 
		_rcTextStyle_
		_defaultFontShx_ 
;		_textStyleAnnotativity_ _textStyleIsAnnotative_ _textStyleOrientationIsNotMatchedToLayout_
		_textStyleHeightZeroMeansScalable_
		_textStyleWidthFactorOne_
		_textStyleNoObliquing_
		_textStyleNotBackwards_
		_textStyleNotUpsideDown_ 
	)
	'createIsoTextStyle
)



(defun removeUnwantedStyles ( / )
	; Remove non-standard dimension styles
	(setDimStyle "STANDARD")
	(command "._PURGE" "_D" "*" "_N") ; Purge all dimension styles (except "standard" which can't be purged), no verify

	; Remove non-standard multileader styles
	(setMultileaderStyle "STANDARD")
	(command "._PURGE" "_MU" "*" "_N") ; Purge all multileader styles (except "standard" which can't be purged), no verify

	; Remove non-standard section view styles
	(setViewSectionStyle "Metric50")
	(command "._PURGE" "_SE" "*" "_N") ; Purge all section view styles (except "Metric50" which can't be purged), no verify

	; Remove non-standard deatil view styles
	(setDetailViewStyle "Metric50")
	(command "._PURGE" "_DE" "*" "_N") ; Purge all detail view styles (except "Metric50" which can't be purged), no verify

	(setTableStyle "STANDARD")
	(command "._PURGE" "_T" "*" "_N") ; Purge all table styles (except "standard" which can't be purged), no verify

	; Remove non-standrard text styles
	(setTextStyle "STANDARD")
	(command "._PURGE" "_ST" "*" "_N") ; Purge all text styles (except "standard" which can't be purged), no verify

	; Purge blocks which might have been in use by the now removed styles (all blocks, if there would be any more)
	(command "._PURGE" "_B" "*" "_N") ; Purge all section view styles (except "Metric50" which can't be purged), no verify
	
	'removeUnwantedStyles
)

(defun setDimStyle ( dimStyle / acdoc )
	(vl-load-com)
	(setq acdoc (vla-get-ActiveDocument (vlax-get-acad-object)))
	(if (tblsearch "DIMSTYLE" dimStyle)
		(vla-put-activeDimstyle 
			acdoc 
			(vla-item (vla-get-Dimstyles acdoc) dimStyle)
		)
	)
)

(defun setMultileaderStyle ( multileaderStyle / )
	(command "._CMLEADERSTYLE" multileaderStyle)
)

(defun setViewSectionStyle ( viewSectionStyle / )
	(command "._CVIEWSECTIONSTYLE" viewSectionStyle)
)

(defun setDetailViewStyle ( viewDetailStyle / )
	(command "._CVIEWDETAILSTYLE" viewDetailStyle)
)

(defun setTableStyle ( tableStyle / )
	(command "._CTABLESTYLE" tableStyle)
)

(defun setTextStyle ( textStyle / )
	(command "._STYLE" textStyle)
)



; CAD system LAYER settings
;----------------------------------------------------------

(defun findPrefixedLayers ( layprefix / laydata laylist prefixlen layname )
	(setq laydata (tblnext "LAYER" T) ; Rewind and get first entry in acad LAYER list
		  laylist (list)
		  prefixlen (strlen layprefix)
		  layprefix (strcase layprefix)
	)
	(while laydata
		(setq layname (strcase (cdr (assoc 2 laydata)))) ; pick next layer name from list
		(if (= layprefix (substr layname 1 prefixlen))
			(setq laylist (append laylist (list layname)))
		)
		(setq laydata (tblnext "LAYER" nil)) ; get next item from acad LAYER list
	)
	laylist
)



(defun isExistingLayer ( layerName / laydata layname lay )
	(setq laydata (tblnext "LAYER" T) ; Rewind and get first entry in acad LAYER list
		  lay nil
	)
	(while laydata
		(setq layname (strcase (cdr (assoc 2 laydata)))) ; pick next layer name from list
		(if (= layname (strcase layerName))
			(setq lay layname) ; got it!
		)
		(setq laydata (tblnext "LAYER" nil)) ; get next item from acad LAYER list
	)
	lay ; Return layer name (interpreted as T (true) if the layer name was found, or nil (false) if layer was not found
)



(defun createLayer ( layDef / currentLayer )
	; Create new layer, or modify if layer already exists.
	;
	; Option "New" (createNewLayer) instead of "Make" (makeNewLayer) will create layer if it does not exist, but it has some flaws: 'New' fails for some reason.
	; Option 'Make' instead of 'New' would just ignore & inform but not fail if layer already exists. ACAD internal state variable CLAYER (current layer) would be set to the specified layer.
	; Note: If you try use "Make" to an existing layer, then the option "Description" asks for accept to change existing description, if any. This makes the "Make" command difficult to use.
	;
	; Typical use: (createLayer layDef_View_SchematicPlan) etc
	;
	(setq
		layerName			(eval (nth 0 layDef))
		layerColor			(eval (nth 1 layDef))
		layerDescription	(eval (nth 2 layDef))
	)
	(if (isExistingLayer layerName)
		(progn
			; Change
			(setq currentLayer (getvar 'CLAYER)) ; stash current layer in local variable
			(command
				_LAYER_
					_setLayer_      layerName
					_colorizeLayer_ layerColor layerName
					_describeLayer_ layerDescription layerName _ENTER_ ; Description + extra ENTER to accept overwrite of existing description
			)
			(setvar 'CLAYER currentLayer) ; retrieve stashed value
		)
	;else
		; Create
		(command
			_LAYER_
				_makeNewLayer_  layerName
				_colorizeLayer_ layerColor layerName
				_describeLayer_ layerDescription layerName
				_ENTER_
		)
	)
	; Always reset - otherwise (createLayer ...) fails at least in the VLIDE debugger.
	; I never found out why and exactly where, but the ._LAYER "Set" ... command did not terminate properly, so the next command would fail.
	(setLayer layDef_Zero)
	'createLayer
)



(defun setLayer ( layDef / layerName objectColor )
	(if (= layDef nil)
	 	(alert "setLayer(): layDef argument cannot be nil.")
	 )
	(setq 
		layerName 	(nth 0 layDef)
		objectColor	(nth 3 layDef)
	)
	(cond
		((isExistingLayer layerName)
				(command _LAYER_ _thawLayer_ layerName _ENTER_)
				(command _LAYER_ _setLayer_ layerName _unlockLayer_ layerName _turnOnLayer_ layerName  _ENTER_)
				(command _COLOR_ objectColor) ; Default color for next graphics entities to be drawn
		)
		(T
			(createLayer layDef_UnknownLayerNameRequested) ; Create layer if nonexisting. Switch to this layer.
			(setLayer layDef_UnknownLayerNameRequested) ; recursive, fails if no definition exists for that layer in file createStandardLayers.lsp.
		)
	)
	'setLayer ; For some reason, this "kills" the hanging operation inside the -LAYER command, which otherwise will mess up during debugging
)



(defun setDefaultOjectColor ( color / )
	(command _COLOR_ color)
)



(defun setLayerAndObjectColor ( layDef objectColor / )
	(setLayer layDef)
	(command _COLOR_ objectColor) ; override layer's usual object color, which was first set with setLayer().
)



(defun freezeAllLayersExceptCurrentLayer ( / )
	(command _LAYER_ _freezeLayer_ _anyLayerName_ _ENTER_)	; Freeze all layers except current layer
)



(defun thawAllLayers ( / )
	(command _LAYER_ _thawLayer_ _anyLayerName_ _ENTER_)	; Thaw all layers except current layer
)



; CAD system TEXT entities
;----------------------------------------------------------

(defun addTextAtPosWithJustification ( layDef textHeight pos text justification / )
	(setLayer layDef)
	(command 
		_TEXT_
			_setTextStyle_ _rcTextStyle_
			_justifyText_ justification
			pos 
			textHeight 
			_angleZero_
			text
	)
	'addTextAtPosWithJustification
)



(defun addTextAtPos ( layDef textHeight pos text /  )
	; Single text, no line breaking
	; layDef textHeight pos text
	; Note: TEXT and MTEXT will be annotative entities if they reside inside an annotative block (RC object anonymous block etc)
	;
	; Note: ; For some reason, texts on layer 0 get "ByLayer" instead of "ByBlock" :-( but not when running MAIN() as a batch job.
	;
	(addTextAtPosWithJustification layDef textHeight pos text _middleCenter_)
	'addTextAtPos
)



(defun addMText ( layDef textHeight textBoxWidth pos text / )
	; Multiline text
	; Note: TEXT and MTEXT will be annotative entities if they reside inside an annotative block (RC object anonymous block etc)
	(setLayer layDef)
	(command 
		_MTEXT_
			pos 
			_setMtextStyle_ 		_rcTextStyle_
			_setMtextHeight_		textHeight
			_setMtextJustifcation_	_middleCenter_
			_setMtextRotation_		_angleZero_
			
			; Multiline box width (instead of giving next corner as a position):
			_setMtextWidth_			textBoxWidth

			text		; The text which will break inside the given text box width.
						; ...if Width is non-zero then text will break at spaces when needed.
						; ...if Width is zero then text will remain one one single line.
			_ENTER_		; No further text lines
	)
   'addMText
)



; CAD system text ATTRIBUTE entities
;----------------------------------------------------------
(defun addAtt ( attTag attPrompt attDefaultValue pos textHeight rotation textStyle justification / tmp )
	; Low-level access to CAD system Text Attribute entity creation
	; Add text attribute (a named storage for text in a symbol, which can be read and written to later).
	; Features the TAG, PROMPT amd VALUE field, a POSITION and lots of attribute flags (see global contants elsewhere).
	; (Note...: ATTDEF is a 'drive me crazy' command - the inputs vary wildly with AFLAGS settings.)
	;
	; See definition of symbolic constants _xxxx_ elsewhere.
	; 0		No attribute mode selected
	; 1		Invisible
	; 2		Constant
	; 4		Verify (ask twice about value)
	; 8 	Preset
	; 16	Lock position in block
	; 32	Multiple lines allowed
	;
	; Strangely, the 'Annotative' flag does not belong to the same byte as the six flags listed above.
	;
	; An annotative block can only contain non-annotative stuff. This 'stuff' will scale annotatively along with the rest of the block.
	; (and beware - if we *did* include an annotative TEXT, MTEXT or ATTDEF inside an annotative block, then AutoCAD would make themn into non-annotative
	; entities, to stay consistent.)
	;
	; A non-annotative block may contain both annotative and non-annotative TEXT, MTEXT and ATTDEF entities.
	;
;	(if (= (nth 0 pos) nil) 
;		(princ (strcat "\n==>addAtt: attTag=" attTag " attPrompt=" attPrompt " attDefaultValue=" attDefaultValue " pos=" pos " textHeight=" (rtos textHeight) " rotation=" (rtos rotation) " textStyle=" textStyle " justification=" justification))
;		(princ (strcat "\n==>addAtt: attTag=" attTag " attPrompt=" attPrompt " attDefaultValue=" attDefaultValue " pos=(" (rtos (nth 0 pos)) "," (rtos (nth 1 pos)) ") textHeight=" (rtos textHeight) " rotation=" (rtos rotation) " textStyle=" textStyle " justification=" justification))
;	)

	(setq tmp (getvar 'AFLAGS))
	(setvar 'AFLAGS _lockPosition_) ; We do not need multiple line attributes with our DNA's.
	(if (= attPrompt nil) 
		; Add 'empty prompt' since 'attPrompt' argument is nil
		(setq attPrompt "(no prompt)")	
	)
;	(if (= (logand aflags _multipleLines_) 0) ; Use LISP bitwise AND (NB - only nil is false. all numerical values are 'T' (true))
;		; Single line attribute:
;		(if (= annotativity _attDefIsAnnotative_)
;			; Annotative (for use in geo symbol)
;			(command
;				_ATTDEF_		
;					_attDefIsAnnotative_					; Toggle 'annotative' flag ON. All the other parameters were set when writing to 'AFLAGS.
;					_ENTER_ 								; The 'Done' selection: accept current AFLAGS settings (including the multiline-bit in AFLAGS)
;					attTag									; Name of attribute tag - which must correspond to RailCOMPLETE DNA syntax
;					attPrompt								; Prompt
;					attDefaultValue							; Default single-line text.
;					_setAttdefTextStyle_ textStyle			; 
;					_setAttdefJustifcation_ justification	; Must be "_J" (not "_Justify")
;					pos 									; First corner of multiline 'box'
;					textHeight								; No qualifier first since AutoCAD demands text height here.
;					rotation								; No qualifier first since AutoCAD demands text rotation here.
;					_ENTER_
;			)
;		; else: Non-annotative (for use in schematic symbol)
			(command
				_ATTDEF_		
					_ENTER_ 								; Accept current AFLAGS settings (including the multiline-bit in AFLAGS). We didn't set 'annotative'.
					attTag									; Name of attribute tag - which must correspond to RailCOMPLETE DNA syntax
					attPrompt								; Prompt
					attDefaultValue							; Default single-line text.
					_setAttdefTextStyle_ textStyle			; 
					_setAttdefJustifcation_ justification	; Must be "_J" (not "_Justify")
					pos 									; First corner of multiline 'box'
					textHeight								; No qualifier first since AutoCAD demands text height here.
					rotation								; No qualifier first since AutoCAD demands text rotation here.
					_ENTER_
			)
;		)
;	;else
;		; Multiple line attribute:
;		(if (= annotativity _attDefIsAnnotative_)
;			; Annotative (for use in geo symbol)
;			(command 
;				_ATTDEF_ 
;					_attDefIsAnnotative_					; Toggle 'annotative' flag ON. All the other parameters were set when writing to 'AFLAGS.
;					_ENTER_ 								; Accept current AFLAGS settings (including the multiline-bit in AFLAGS)
;					attTag									; Name of attribute tag - which must correspond to RailCOMPLETE DNA syntax
;					attPrompt								; Prompt
;					attDefaultValue							; Default multiline text. Use '\P' inside text string as the 'newline' character.
;					_ENTER_									; Stop asking for multiple lines (AutoCAD asks: 'Next line or done')
;					pos 									; First corner of multiline 'box'
;					_setAttdefTextHeight_ textHeight		; Text (letter) height. Ignore the 'line spacing' setting, i.e. keep default.
;					_setAttdefRotation_ rotation			; 
;					_setAttdefTextStyle_ textStyle
;					_setAttdefJustifcation_ justification	; Must be "_J" (not "_Justify")
;					_setAttdefTextWidth_ 					; Set text box width - only when  _multipleLines_ bit is set,
;					_zero_									; A width of 0 (zero) sets it to auto-width, instead of giving a pos 
;															; for the other corner of the 'box' and thereby setting a 'hard' text width.
;			)
;		; else: Non-annotative (for use in schematic symbol)
;			(command 
;				_ATTDEF_ 
;					_ENTER_ 								; Accept current AFLAGS settings (including the multiline-bit in AFLAGS). We didn't set 'annotative'.
;					attTag									; Name of attribute tag - which must correspond to RailCOMPLETE DNA syntax
;					attPrompt								; Prompt
;					attDefaultValue							; Default multiline text. Use '\P' inside text string as the 'newline' character.
;					_ENTER_									; Stop asking for multiple lines (AutoCAD asks: 'Next line or done')
;					pos 									; First corner of multiline 'box'
;					_setAttdefTextHeight_ textHeight		; Text (letter) height. Ignore the 'line spacing' setting, i.e. keep default.
;					_setAttdefRotation_ rotation			; 
;					_setAttdefTextStyle_ textStyle
;					_setAttdefJustifcation_ justification	; Must be "_J" (not "_Justify")
;					_setAttdefTextWidth_ 					; Set text box width - only when  _multipleLines_ bit is set,
;					_zero_									; A width of 0 (zero) sets it to auto-width, instead of giving a pos 
;															; for the other corner of the 'box' and thereby setting a 'hard' text width.
;			)
;		)
;    )
	(setvar "AFLAGS" tmp)
	'addAtt
)



(defun addTextAttributeAtPos ( layDef textHeight pos attDef / attTag attPrompt attDefaultValue )
	;
	; API access to CAD system Text Attribute entity creation
	; Single line text attribute, no line breaking
	;
	; 'Struct attDef' definition:
	;
	; item 0 = Attribute's tag name (NB! Cannot contain spaces)
	; item 1 = Attribute's prompt text
	; item 2 = Attribute's default value
	;
	; Example: (setq myAttDef '("Tag-name-without-spaces" "Prompt text can contain spaces" "Default value can contain spaces"))
	;
	(setq
		attTag			(eval (nth 0 attDef))
		attPrompt		(eval (nth 1 attDef))
		attDefaultValue	(eval (nth 2 attDef))
	)
	(setLayer layDef)
	(addAtt attTag attPrompt attDefaultValue pos textHeight _angleZero_ _rcTextStyle_ _middleCenter_)
	'addTextAttributeAtPos
)



; Graphics scaling
;----------------------------------------------------------
(defun scaleAll ( factor / )
	(command _SCALE_ _selectAll_ _ENTER_ _origo_ factor)
)



; Graphics rotation
;----------------------------------------------------------
(defun rotateLeft ( angle / )
	; Rotate CCW with angle [Decimal Degrees]
	(command _ROTATE_ _selectAll_ _ENTER_ _origo_ angle)
)



(defun rotateRight ( angle / )
	; Rotate CCW with angle [Decimal Degrees]
	(command _ROTATE_ _selectAll_ _ENTER_ _origo_ angle)
)



; Graphics mirroring
;----------------------------------------------------------

(defun mirrorAboutXaxis ( variation / )
	; Mirror about horizontal axis through origo
	; variation should be one of _keepMirrorSource_ or _eraseMirrorSource_ (see definition of global CAD constants)
	(cond 
		((= variation _keepMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origo_ _xAxis_ _keepMirrorSource_))
		((= variation _eraseMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origo_ _xAxis_ _eraseMirrorSource_))
		(T (alert "*** _keepMirrorSource_ or _eraseMirrorSource_ expected as argument to function mirrorAboutXaxis"))
    )
	'mirrorAboutXaxis
)



(defun mirrorAboutYaxis ( variation / )
	; Mirror about vertical axis through origo
	; variation should be one of _keepMirrorSource_ or _eraseMirrorSource_ (see definition of global CAD constants)
	(cond
		((= variation _keepMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _keepMirrorSource_))
		((= variation _eraseMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origo_ _yAxis_ _eraseMirrorSource_))
		(T (alert "*** _keepMirrorSource_ or _eraseMirrorSource_ expected as argument to function mirrorAboutYaxis"))
    )
	'mirrorAboutYaxis
)



(defun mirrorAboutDiagonal ( variation / )
	; Mirror about 45 degrees diagonal through origo
	; variation should be one of _keepMirrorSource_ or _eraseMirrorSource_ (see definition of global CAD constants)
	(cond
		((= variation _keepMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origo_ _diagonalAxis_ _keepMirrorSource_))
		((= variation _eraseMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origo_ _diagonalAxis_ _eraseMirrorSource_))
		(T (alert "*** _keepMirrorSource_ or _eraseMirrorSource_ expected as argument to function mirrorAboutDiagonal"))
    )
	'mirrorAboutDiagonal
)



(defun mirrorAboutReverseDiagonal ( variation / )
	; Mirror about 135 degrees diagonal through origo
	; variation should be one of _keepMirrorSource_ or _eraseMirrorSource_ (see definition of global CAD constants)
	(cond
		((= variation _keepMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origo_ _reverseDiagonalAxis_ _keepMirrorSource_))
		((= variation _eraseMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origo_ _reverseDiagonalAxis_ _eraseMirrorSource_))
		(T (alert "*** _keepMirrorSource_ or _eraseMirrorSource_ expected as argument to function mirrorAboutReverseDiagonal"))
    )
	'mirrorAboutReverseDiagonal
)



(defun moveToQuadrant ( quadrant selection / ) 
	; Mirror an object residing in the first quadrant (from 0 to 90 decimal degrees, 0 DD = East and 90 DD = North).
	; quadrant = number 1..4, results in mirroring operations resulting from mirroring about X-axis (0 DD) and / or Y-axis (90 DD).
	; 'selection' is typically "A" or (_selectAll_)
	; 1 = no action
	; 2 = mirror about the Y-axis
	; 3 = mirror about the Y- then the X-axis (or X-axis then Y-axis)
	; 4 = mirror about the X-axis
	;
	(cond
		((= quadrant 1)
			; No action
		)
		((= quadrant 2)
			(command _MIRROR_ selection _ENTER_ _origo_ _yAxis_ _eraseMirrorSource_)
		)
		((= quadrant 3)
			(command _MIRROR_ selection _ENTER_ _origo_ _yAxis_ _eraseMirrorSource_)
			(command _MIRROR_ selection _ENTER_ _origo_ _xAxis_ _eraseMirrorSource_)
		)
		((= quadrant 4)
			(command _MIRROR_ selection _ENTER_ _origo_ _xAxis_ _eraseMirrorSource_)
		)
	)
	'moveToQuadrant
)



; Draw hatch patterns
;--------------------
;
; See also definition of global constants - standard hatch densities
;
(defun drawHatch ( hatchDensity / )
	(command 
		_HATCH_ 
			_selectHatchObjects_ _lastSelection_ _ENTER_ 
			_setHatchProperties_ _hatchPatternSlantedLines_ hatchDensity _angleZero_ 
			_selectHatchOrigin_ _setNewHatchOrigin_ _slightlyAbove_ _doNotStoreHatchOriginAsDefault_
			_ENTER_
	)
	(command _DRAWORDER_ _lastSelection_ _ENTER_ _aboveAllObjects_)
	(command _EXPLODE_ _lastSelection_ _ENTER_)
	'drawHatch
)



(defun drawHatchFromPoint ( hatchDensity pt ang offset / )
	(command 
		_HATCH_ 
			pt
			_setHatchProperties_ _hatchPatternSlantedLines_ hatchDensity ang 
			_selectHatchOrigin_ _setNewHatchOrigin_ (strcat "0.0," (rtos offset)) _doNotStoreHatchOriginAsDefault_
			_ENTER_
	   _DRAWORDER_ _lastSelection_ _ENTER_ _aboveAllObjects_
	   _EXPLODE_ _lastSelection_ _ENTER_
	)
	'drawHatchFromPoint
)



(defun drawHatchFromSelectionUsingStyle ( hatchDensity selectionSet ang offset style / )
	(command 
		_HATCH_ 
			_selectHatchObjects_ selectionSet _ENTER_ 
			_setHatchProperties_ style hatchDensity ang 
			_selectHatchOrigin_ _setNewHatchOrigin_ (strcat "0.0," (rtos offset)) _doNotStoreHatchOriginAsDefault_
			_ENTER_
	   _EXPLODE_ _lastSelection_ _ENTER_
	)
	'drawHatchFromSelectionUsingStyle
)



(defun addWipeoutToLastClosedPolyline ( wipeoutLayDef keepOrErase / currentLayer )
	; Use predefined values for keepOrErase, see above at _WIPEOUT_ definitions.
	(if (not wipeoutLayDef) (alert (strcat "*** ERROR: addWipeoutToLastClosedPolyline( ) called with bad or nil wipeoutLayDef")))
	(setq currentLayer (getvar 'CLAYER)) ; stash current layer in local variable
	(setLayer wipeoutLayDef)
	(command _WIPEOUT_ _createWipeoutFromPolyline_ _lastSelection_ _ENTER_ keepOrErase)
	(command _DRAWORDER_ _lastSelection_ _ENTER_ _underAllObjects_)
	(setvar 'CLAYER currentLayer) ; retrieve stashed value
)



; CAD system Block manipulation (block table entries)
;----------------------------------------------------------
(defun createSchematicBlockFromCurrentGraphics ( blockName /  ) 
	; Create a non-annotative block from the present graphics in model space.
	; Assume that all AttDefs have been declared as non-annotative already.
	; See also AnnotativeBlock version.
	(setq blockName (strcat blockName _schematic_))
	(if (tblsearch "BLOCK" blockName) 
	; if existing block: 
		(command _BLOCK_ blockName _redefineBlock_ _blockAnnotativity_ _blockIsNotAnnotative_ _keepOrientation_ _origo_ _selectAll_ _ENTER_)
	; else just create first-time block:
		(command _BLOCK_ blockName                 _blockAnnotativity_ _blockIsNotAnnotative_ _keepOrientation_ _origo_ _selectAll_ _ENTER_)
	)
	(setq nSchematicBlocks (+ 1 nSchematicBlocks)) ; Global counter, increment.
	(setLayer layDef_Zero)
	(setDefaultObjectPropertiesToByBlock)
)



(defun addGraphicsFromScaledSchematicBlock ( blockName scale / )
	; Insert, scale and explode an existing block.
	; NB: No checking - the block must exist in the block table.
	(command 
		_INSERT_ (strcat blockName _schematic_) _setInsertionScale_ scale _setInsertionRotation_ _angleZero_ _origo_	; Retrieve schematic symbol - Set overall scale, rotation and position
		_EXPLODE_ _selectAll_ _ENTER_													; Convert inserted block to modelspace graphics entities
	)
	(setLayer layDef_Zero)
)



(defun createAnnotativeBlockFromCurrentGraphics ( blockName /  ) 
	; Create an annotative block from the present graphics in model space.
	; Typical use: The balise triangular symbol in an annotative symbol meant for geographic mode drawings
	; Assume that all AttDefs have been declared as annotative already.
	(setq blockName (strcat blockName _scalable_))
	(if (tblsearch "BLOCK" blockName) 
	; if existing block:
		(command _BLOCK_ blockName _redefineBlock_ _blockAnnotativity_ _blockIsAnnotative_ _keepOrientation_ _origo_ _selectAll_ _ENTER_)
	; else just create first-time block:
		(command _BLOCK_ blockName                 _blockAnnotativity_ _blockIsAnnotative_ _keepOrientation_ _origo_ _selectAll_ _ENTER_)
	)
	(setq nAnnotativeBlocks (+ 1 nAnnotativeBlocks)) ; Global counter, increment.
	(setLayer layDef_Zero)
	(setDefaultObjectPropertiesToByBlock)
)



(defun createMetricBlockFromCurrentGraphics ( blockName /  ) 
	; Create a non-annotative but real-size block from the present graphics in model space.
	; Typical use: Yokes / cantilevers / switches
	; Assume that all AttDefs have been declared as non-annotative already.
	(setq blockName (strcat blockName _metric_))
	(if (tblsearch "BLOCK" blockName) 
	; if existing block:
		(command _BLOCK_ blockName _redefineBlock_ _blockAnnotativity_ _blockIsNotAnnotative_ _keepOrientation_ _origo_ _selectAll_ _ENTER_)
	; else just create first-time block:
		(command _BLOCK_ blockName                 _blockAnnotativity_ _blockIsNotAnnotative_ _keepOrientation_ _origo_ _selectAll_ _ENTER_)
	)
	(setq nMetricBlocks (+ 1 nMetricBlocks)) ; Global counter, increment.
	(setLayer layDef_Zero)
	(setDefaultObjectPropertiesToByBlock)
)



(defun createAnnotativeBlockFromScaledSchematicBlock ( blockName scale / )
	;
	; Create an annotative block based on a scaled version of a schematic symbol retrieved from the BLOCK table as an INSERT.
	;
	; Signaling symbols are first programmed in LISP to their 'schematic plan' scale. However, some schematic symbols are quite big (the 
	; 'S-lås' symbol for instance) and may be scaled down before use in a geo drawing (annotative symbol). with , some must are 1:1 (yokes,
	; switches). Each scalable block is first stored as a schematic version, then re-inserted and possibly scaled, before it is stored as
	; an annotative symbol for use in geo drawings. 
	;
	; Note that a much-used paper scale will be the 1:250 drawing scale, because most producers will draw 350 meter of model on a landscape 
	; A1 sheet, and then reduce size to 50% (A1 -> A3) when printing on paper - this is a suitable size for seeing most details.
	; 
	; A1 sheets measure 594 x 841 mm. A suitable half-page lying viewport, allowing for paper margins, will be 800 mm wide (and ca 200 mm high).
	; If we deliver paper or PDF drawings in 1:1000 scale on A1 format, then we will orient and scale the contents of the viewport to cover
	; 800 meters x 200 m (horizontally x vertically). I.e., insert a rectangle measuring 800x200 meters in modelspace, and zoom-to-window in
	; the corresponding paperspace viewport.
	;
	; Since A1 is not a common printer paper size, it is customary to reduce print size to A3 (420x297) at print time.
	; That is, symbols intended for 1:1000 scale A1 will appear half in width and height. To compensate, you should set the annotative scale
	; in the CAD system to 2:1 to maintain symbol sizes on A3 paper sheets.
	;
	; Because the density of placed objects will be to crowded in an 1:1000 drawing, many suppliers tend to establish A1 size 1:250 scale
	; drawings. Then the paper size will be 420x297 mm. A suitable paperspace half-paper viewport will measure 800x200 in millimeters, and
	; will represent a 200 x 50 square meter area. The scehamtic-sized symbols will be far too big now, nbut a 4:1 reduction will usually be
	; fine. So set the CAD system annotative drawing scale to 4:1 before printing from paperspace.
	;
	(addGraphicsFromScaledSchematicBlock blockName scale)

	(setq blockName (strcat blockName _scalable_))
	(if (tblsearch "BLOCK" blockName)
		 ;If block exists already (such as 'NO-BN-2D-JBTSI-FILLED-nn' for switches / signaling symbols) which is generated for several switch types)...
		; or using VLIDE 'manually' several times...
		; ...then answer the additional question 'Redefine it?' which needs answer "_YES".
		; Then "_Annotative" triggers question "Create annotative block?" which needs answers "Yes". Then set insertion point and select all graphics (_selectAll_ + _ENTER_ (Enter)).
		;Redefine block definition:
		(command _BLOCK_ blockName _redefineBlock_ _blockAnnotativity_ _blockIsAnnotative_ _keepOrientation_ _origo_ _selectAll_ _ENTER_)
	; else just create first-time block:
		(command _BLOCK_ blockName                 _blockAnnotativity_ _blockIsAnnotative_ _keepOrientation_ _origo_ _selectAll_ _ENTER_)
	)
	(setq nAnnotativeBlocks (+ nAnnotativeBlocks 1))
	(setLayer layDef_Zero)
	(setDefaultObjectPropertiesToByBlock)
)


(defun eraseSchematicBlock ( blockNames / selectionSet )
	; Removes all inserts and definition of block(s)
	; usage (deleteBlock "blkname1,blkname2,blkname3,and_so_on")
	(if (setq selectionSet (ssget "x" (list (cons 0 "INSERT") (cons 2 blockNames)))) ; if any INSERTs found, erase them first
		(progn
			(command _LAYER_ _unlockLayer_ _anyLayerName_ _ENTER_)
			(command _ERASE_ selectionSet _ENTER_)
		)
	)
	(command _LAYERP_) ; restore previous layer state
	(command _PURGE_ _purgeBlocks_ blockNames _purgeWithoutVerification_) ; Erase specified block(s) from block table
	(setLayer layDef_Zero)
	(setq nSchematicBlocks (- nSchematicBlocks 1)) ; One block removed...
	(setDefaultObjectPropertiesToByBlock)
)
