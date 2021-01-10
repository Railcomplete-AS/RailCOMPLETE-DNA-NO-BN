;=======================================================
;
; SymbolLibraryHelpers.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2020. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-01-17 CLFEY Release 2021.a
;
;=========================================================================================================================
;
; Supporting LISP routines, needed for generation of 2D symbol libraries.
;
; Purpose: Provide a low-level CAD system LISP interface, i.e. an Application Programming Interface (API).
; Usage: Build a library of graphics primitive routines on top of this API, applicable to any LISP dialect. Then build concrete 
; railway object symbols from CAD-system independent primitives.
;
; Note on WIPEOUTs: The AutoLISP system variable WIPEOUTFRAME has three settings:
;
; WIPEOUTFRAME = 0		Frames are not displayed or plotted. Frames are temporarily displayed for object selection and selection preview.
; WIPEOUTFRAME = 1		Frames are displayed and plotted
; WIPEOUTFRAME = 2		(default) Frames are displayed, but not plotted
;


; CAD system global constants
;----------------------------------------------------------

(defun defineGlobalCadSystemConstants ( / )
	(setq
		; Flags for the AutoLISP command ATTRIBUTE:
		_invisible_ 		1
		_constant_ 			2
		_verify_ 			4
		_preset_ 			8
		_lockPosition_ 		16
		_multipleLines_ 	32
		
		; rtos - Real-to-String
		_scientific_		1
		_decimal_			2
		_engineering_		3	; (feet and decimal inches)
		_architectural_		4	; (feet and fractional inches)
		_fractional_		5
	
		; Mirror:
		_keep_ 				"_No" 	; keep original item after mirroring
		_erase_ 			"_Yes"	; erase original item after mirroring

		; The 'Scandinavian extras':
		_uAE_	"\U+00C6"	; Æ = Unicode decimal 198 - hex 00C6 - AutoLISP \U+00C6
		_uOE_	"\U+00D8"	; Ø = Unicode decimal 216 - hex 00D8 - AutoLISP \U+00D8
		_uAA_	"\U+00C5"	; Å = Unicode decimal 197 - hex 00C5 - AutoLISP \U+00C5
		_ae_	"\U+00E6"	; æ = Unicode decimal 230 - hex 00E6 - AutoLISP \U+00E6
		_oe_	"\U+00F8"	; ø = Unicode decimal 248 - hex 00F8 - AutoLISP \U+00F8
		_aa_	"\U+00E5"	; å = Unicode decimal 229 - hex 00E5 - AutoLISP \U+00E5
		
		; Text heights:
		; Text height is 10x linewidth. Font name is based on the linewidth, i.e. '180' is 1.80 m text height (suitable for 1:1000 scale drawings) (0.18 mm linewidth)
		_th020_	0.2 
		_th050_	0.5
		_th070_	0.7
		_th100_	1.0
		_th125_	1.25
		_th150_	1.5
		_th180_	1.8
		_th250_	2.5
		_th350_	3.5
		_th500_	5.0
		_th700_	7.0
		
		; Wipeouts
		_noWipeout_	nil	; Providing 'nil' instead of a layer definition to one of the drawCircle / drawBox functions suppresses adding of wipeout.
	)

	(setq
		; Constants for graphics creation (independent of the underlying CAD system)

		; Single values
		_quarter_		0.25
		_third_			(/ 1.0 3)
		_half_			0.5
		_twoThirds_		(/ 2.0 3)
		_threeQuarters_	0.75
		_one_			1.0
		_two_			2.0
		_three_			3.0
		_four_			4.0
		_five_			5.0
		
		; 2D points
		_origo_			'( 0.00  0.00)
		_slightlyBelow_	'( 0.00 -0.01)
		_slightlyAbove_	'( 0.00  0.01)
		_slightlyLeft_	'(-0.01  0.00)
		_slightlyRight_	'( 0.01  0.00)
		
		; Polylines open/closed
		_open_			""
		_closed_		"_CLOSE"
		
		; Hatch pattern densities
		_noHatch_		"no-hatch"
		_filledHatch_	0.02
		_denseHatch_	0.04
		_mediumHatch_	0.08
		_sparseHatch_	0.16
		
		; Hatch patterns representing colors
		_blackHatch_	_filledHatch_
		_redHatch_		_denseHatch_
		_blueHatch_		_mediumHatch_
		_yellowHatch_	_sparseHatch_
		; _whiteHatch_	Do not hatch
	
		; Proxy symbols (when no ordinary symbol has been defined)
		_proxySymbolRadius_					1.5
		_oneLetterProxySymbolTextHeight_	2.5
		_twoLetterProxySymbolTextHeight_	1.8
		_threeLetterProxySymbolTextHeight_	1.2
	
		; Symbol description - usually place text below symbols, in UPPERCASE
		_descriptionTextHeight_		_th020_
		_descriptionTextBoxWidth_	3.0
		
		; Attribute definitions
		_attTextHeight_				_th125_
		
		; 'Enum lists' - #TODO# replace by GUIDs
		_active_	"_active_"
		_inactive_	"_inactive_"
		;
		_single_	"_single_"
		_double_	"_double_"
		
		_left_		"_left_"
		_right_		"_right_"
		_up_		"_up_"
		_down_		"_down_"
		
		; Directions (alias for angles 0..360 in Decimal Degrees)
		_east_		0
		_north_		90
		_west_		180
		_south_		270
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
	(princ (strcat "" folderName " loaded...\n")) (prin1)
)



; CAD system default values
;----------------------------------------------------------

(defun purgeAll ( / )
	(command 
		"._LAYER" "Unlock" "*" ""
		"._ERASE" "_ALL" ""
	)
	(command "._LAYER" "_Set" "0" "")
	(command 
		"._PURGE" "_ALL" "*" "_NO"
		"._PURGE" "_ALL" "*" "_NO"
		"._PURGE" "_ALL" "*" "_NO"
	)
	'purgeAll
)



(defun setCadSystemDefaults ( / )
	(command
		"._ERASE" "_ALL" "" ""
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
		"._PLINE" "0,0" "_WIDTH" 0 0 "" ; Default to thin polylines
	)
	'setCadSystemDefaults
)


(defun setDefaultObjectPropertiesToByBlock ( / )
	(setLayer layer_Zero)
	(command
		"._COLOR" "_ByBlock"
		"._LINETYPE" "_SET" "_ByBlock" ""
		"._LWEIGHT" "_ByBlock"
		"._CETRANSPARENCY" "_ByBlock"
	)
	'setDefaultObjectPropertiesToByBlock
)
  
  
  
(defun setCurrentDefaultPropertiesToByLayer ( / )
	(command
		"._COLOR" "_ByLayer"
		"._LINETYPE" "_SET" "_ByLayer" ""
		"._LWEIGHT" "_ByLayer"
		"._CETRANSPARENCY" "_ByLayer"
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
	;   'Iso3098'will result in an error if not present in the appropriate AutoCAD folder.
	;	Place the preferred font file in the GitHub RailCOMPLETE folder "...\Customization\NO-BN\2D\_SRC\Fonts".
	;	When debugging on your computer, make sure this font also resides in "C:\Program Files\Autodesk\AutoCAD 2020\Fonts".
	;	Change the folder name according to your AutoCAD application (2018, 2019, 2020...)
	;
	; "TextHeight = 0.0" means that the text van vary in size (after creation)
	;------------------   StyleName FontFile Annotative "Annotative" "_NO" TextHeight Width Obliquing Backwards UpsideDowm )
	(command "._STYLE"   "ISO"      "iso3098.shx"        "A"         "_NO" "0.0"      "1.0" "0.0"     "_NO"     "_NO" )
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
	; Option "New" instead of "Make" will create layer if it does not exist, but it has some flaws: 'New' fails for some reason.
	; Option 'Make' instead of 'New' would just ignore & inform but not fail if layer already exists. ACAD internal state variable CLAYER (current layer) would be set to the specified layer.
	; Note: If you try use "Make" to an existing layer, then the option "Description" asks for accept to change existing description, if any. This makes the "Make" command difficult to use.
	;
	; Typical use: (createLayer layer_View_SchematicPlan) etc
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
				"._LAYER"
					"_Set" layerName
					"_Color" layerColor layerName ; Color
					"_Description" layerDescription layerName "" ; Description + extra ENTER to accept overwrite of existing description
			)
			(setvar 'CLAYER currentLayer) ; retrieve stashed value
		)
	;else
		; Create
		(command
			"._LAYER"
				"_Make" layerName ; New
				"_Color" layerColor layerName ; Color
				"_Description" layerDescription layerName ; Description
				""
		)
	)
	; Always reset - otherwise (createLayer ...) fails at least in the VLIDE debugger.
	; I never found out why and exactly where, but the ._LAYER "Set" ... command did not terminate properly, so the next command would fail.
	(setLayer layer_Zero)
	'createLayer
)



(defun setLayer ( layDef / layerName )
	(if (= layDef nil)
	 	(alert "setLayer(): layDef argument cannot be nil.")
	 )
	(setq 
		layerName 	(nth 0 layDef)
		objectColor	(nth 3 layDef)
	)
	(cond
		((isExistingLayer layerName)
				(command "._LAYER" "_Thaw" layerName "") ; Thaw it, change to it, unlock and turn on
				(command "._LAYER" "_Set" layerName "_Unlock" layerName "_ON" layerName  "")
				(command "._COLOR" objectColor) ; Default color for next graphics entities to be drawn
		)
		(T
			(createLayer layer_UnknownLayerNameRequested) ; Create layer if nonexisting. Switch to this layer.
			(setLayer layer_UnknownLayerNameRequested) ; recursive, fails if no definition exists for that layer in file createStandardLayers.lsp.
		)
	)
	'setLayer ; For some reason, this "kills" the hanging operation inside the -LAYER command, which otherwise will mess up during debugging
)



(defun setLayerAndObjectColor ( layDef objectColor / )
	(setLayer layDef)
	(command "._COLOR" objectColor) ; override layer's usual object color, which was first set with setLayer().
)



(defun freezeAllLayersExceptCurrentLayer ( / )
	(command "._LAYER" "_F" "*" "")	; Freeze all layers except current layer
)



(defun thawAllLayers ( / )
	(command "._LAYER" "_T" "*" "")	; Thaw all layers except current layer
)



(defun setDefaultOjectColor ( color / )
	(command "._COLOR" color)
)



; CAD system TEXT entities
;----------------------------------------------------------

(defun addText ( text pos textHeight rotation textStyle justify / )
	(command 
		"._TEXT"
			"_STYLE" textStyle 
			"_JUSTIFY" justify 
			pos 
			textHeight 
			rotation 
			text
	)
	'addText
)



(defun addTextAtPos ( layDef textHeight pos text / rotation textStyle justify layerName )
	; Single text, no line breaking
	; layDef textHeight pos text
	;
	; Note: ; For some reason, texts on layer 0 get "_ByLayer" insted of "_ByBlock" :-( but not when running MAIN() as a batch job.
	;
	(setq
		rotation	0 ; degrees
		textStyle	"ISO"
		justify		"_MC" ; middle centered
		layerName	(nth 0 layDef)
	)
	(setLayer layDef) ; Sets layer and drawing color
	(command "._TEXTLAYER" layerName) ; otherwise it goes to the default text layer which was previously set with TEXTLAYER
	(addText text pos textHeight rotation textStyle justify)
	'addTextAtPos
)



(defun addMText ( text pos textHeight width rotation textStyle justify / )
	; Multiline text
	(command 
		"._MTEXT"
			pos 
			"_S" textStyle	; Style
			"_H" textHeight	; Text height
			"_J" justify	; Justification
			"_R" rotation	; Rotation
			"_W" width		; Multiline box width (instead of giving next corner as a position)
			text			; The text which will break inside the given text box width.
							; ...if Width is non-zero then text will break at spaces when needed.
							; ...if Width is zero then text will remain one one single line.
			""				; No further text lines
	)
   'addMText
)



(defun addMTextAtPos ( layDef textHeight textBoxWidth pos text / rotation textStyle justify layerName )
	; Multiline text at given pos and text height / box width (or zero)
	; layDef textHeight textBoxWidth pos text
	(setq
		rotation	0	; degrees
		textStyle	"ISO"
		justify		"_MC" ; middle centered
		layerName	(nth 0 layDef)
	)
	(setLayer layDef) ; Set layer and drawing color
	(command "._TEXTLAYER" layerName) ; otherwise it goes to the previously set value for TEXTLAYER
	(addMText text pos textHeight textBoxWidth rotation textStyle justify)
	'addMTextAtPos
)



; CAD system text ATTRIBUTE entities
;----------------------------------------------------------

(defun addAtt ( attTag attPrompt attDefaultValue pos textHeight rotation textStyle justify aflags / tmp )
	; Low-level access to CAD system Text Attribute entity creation
	; Add text attribute (a named storage for text in a symbol, which can be read and written to later).
	; Features the TAG, PROMPT amd VALUE field, a POSITION and lots of attribute flags (see global contants elsewhere).
	; ATTDEF is a 'drive me crazy' command - the inputs vary wildly AFLAGS settings.
	; 0		No attribute mode selected
	; 1		Invisible
	; 2		Constant
	; 4		Verify (ask twice about value)
	; 8 	Preset
	; 16	Lock position in block
	; 32	Multiple lines allowed
	;
	; Test cases when debugging LISP code:
	;
	; (defun x0() (setq tag "TEKSTOVER" prom "Tekst over" val "" pos (list 0 (* 1.333 side)) height 1.0 rot 0 textstyle "iso" justification "_MC" aflags _lockPosition_))
	; (defun x1() (setq tag "TAG1" prom "PROMPT1" val "TEXT1" pos "1.1,1.1" height 1.1 rot 11.11 textStyle "iso" justify "_MC" aflags 48)) ; LockPosition + MultipleLines
	; (defun x2() (setq tag "TAG2" prom nil       val "TEXT2" pos "2.2,2.2" height 2.2 rot 22.22 textStyle "iso" justify "_MC" aflags 16)) ; LockPosition, prom = nil
	; (defun x3() (setq tag "TAG3" prom "PROMPT3" val "TEXT3" pos "3.3,3.3" height 3.3 rot 33.33 textStyle "iso" justify "_MC" aflags 16)) ; LockPosition
	; (defun vals() (foreach x '(tag prom val pos height rot textStyle justify aflags) (print (eval x))))
	; (defun test1() (progn (x1) (addAtt tag prom val pos height rot textStyle justify aflags)))

	(setq tmp (getvar 'AFLAGS))
	(setvar 'AFLAGS aflags)
	(if (= (logand aflags _multipleLines_) 0) ; (NB - only nil is false. all numerical values are 'T' (true))
		; Single line attribute:
		(if (= attPrompt nil)
			(command
				"._ATTDEF"
				""					; Accept current AFLAGS settings
				attTag				; Name of attribute tag
				""					; Add empty prompt when 'prom' is nil
				attDefaultValue		; ...doesn't ask for more lines...
				"_S" textStyle		; Text adjustment
				"_J" justify		; Text adjustment
				pos					; Text position (list x y)
				textHeight 			; Text height
				rotation 			; Text rotation (the whole text, not obliquing)
			)
			(command 
				"._ATTDEF"
				""					; Accept current AFLAGS settings
				AttTag				; Name of attribute tag
				attPrompt			; ==> Add empty prompt since 'prom' is nil
				attDefaultValue		; ...doesn't ask for more lines...
				"_S" textStyle		; Text adjustment
				"_J" justify		; Text adjustment
				pos					; Text position (list x y)
				textHeight 			; Text height
				rotation 			; Text rotation (the whole text, not obliquing)
			)
		)
	;else
		; Multiple line attribute:
		(command 
			"._ATTDEF" 
				"" ; accept current AFLAGS settings
				attTag				; name of attribute tag
				attPrompt			; prompt
				attDefaultValue ""	; Default text value + plus stop asking for multiple lines
				pos 				; First corner of multiline 'box'
				"_S" textStyle
				"_J" justify
				"_H" textHeight		; Text adjustment - must be preceded by "_H" 
				"_R" rotation		; Text adjustment - must be preceded by "_R 
				"_W" 0				; Text box width - only when  _multipleLines_ bit is set, instead of giving a pos for the other corner of the 'box'
		)
    )
	(setvar "AFLAGS" tmp)
	'addAtt
)



(defun addTextAttributeAtPos ( layDef textHeight pos attDef / attTag attPrompt attDefaultValue rotation textStyle justify )
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
		rotation		0 ; degrees
		textStyle		"ISO"
		justify			"_MC" ; middle centered
	)
	(setLayer layDef) ; Set layer and drawing color
	(addAtt attTag attPrompt attDefaultValue pos textHeight rotation textStyle justify _lockPosition_)
	'addTextAttributeAtPos
)



(defun addMTextAttributeAtPos ( layDef textHeight pos attDef / attTag attPrompt attDefaultValue rotation textStyle justify )
	; API access to CAD system Text Attribute entity creation
	; Multiple line attribute text, automatic line breaking
	(setq
		attTag				(eval (nth 0 attDef))
		attPrompt			(eval (nth 1 attDef))
		attDefaultValue		(eval (nth 2 attDef))
		rotation			0 				; degrees
		textStyle			"ISO"
		justify				"_MC" 			; middle centered
	)
	(setLayer layDef) ; Set layer and drawing color
	(addAtt attTag attPrompt attDefaultValue pos textHeight rotation textStyle justify (+ _lockPosition_ _multipleLines_))
	'addMTextAttributeAtPos
)



; Graphics scaling
;----------------------------------------------------------
(defun scaleAll ( factor / )
	(command "._SCALE" "_ALL" "" _origo_ factor)
)



; Graphics rotation
;----------------------------------------------------------
(defun rotateLeft ( angle / )
	; Rotate CCW with angle [Decimal Degrees]
	(command "._ROTATE" "_ALL" "" "0,0" angle)
)



(defun rotateRight ( angle / )
	; Rotate CCW with angle [Decimal Degrees]
	(command "._ROTATE" "_ALL" "" "0,0" angle)
)



; Graphics mirroring
;----------------------------------------------------------

(defun mirrorAboutXaxis ( variation / )
	; Mirror about horizontal axis through origo
	; variation should be one of _keep_ or _erase_ (see definition of global CAD constants)
	(cond 
		((= variation _keep_) (command "._MIRROR" "_ALL" "" "0,0" "1,0" _keep_))
		((= variation _erase_) (command "._MIRROR" "_ALL" "" "0,0" "1,0" _erase_))
		(T (alert "*** _keep_ or _erase_ expected as argument to function mirrorAboutXaxis"))
    )
	'mirrorAboutXaxis
)



(defun mirrorAboutYaxis ( variation / )
	; Mirror about vertical axis through origo
	; variation should be one of _keep_ or _erase_ (see definition of global CAD constants)
	(cond
		((= variation _keep_) (command "._MIRROR" "_ALL" "" "0,0" "0,1" _keep_))
		((= variation _erase_) (command "._MIRROR" "_ALL" "" "0,0" "0,1" _erase_))
		(T (alert "*** _keep_ or _erase_ expected as argument to function mirrorAboutYaxis"))
    )
	'mirrorAboutYaxis
)



(defun mirrorAboutDiagonal ( variation / )
	; Mirror about 45 degrees diagonal through origo
	; variation should be one of _keep_ or _erase_ (see definition of global CAD constants)
	(cond
		((= variation _keep_) (command "._MIRROR" "_ALL" "" "0,0" "1,1" _keep_))
		((= variation _erase_) (command "._MIRROR" "_ALL" "" "0,0" "1,1" _erase_))
		(T (alert "*** _keep_ or _erase_ expected as argument to function mirrorAboutDiagonal"))
    )
	'mirrorAboutDiagonal
)



(defun mirrorAboutReverseDiagonal ( variation / )
	; Mirror about 135 degrees diagonal through origo
	; variation should be one of _keep_ or _erase_ (see definition of global CAD constants)
	(cond
		((= variation _keep_) (command "._MIRROR" "_ALL" "" "0,0" "-1,1" _keep_))
		((= variation _erase_) (command "._MIRROR" "_ALL" "" "0,0" "-1,1" _erase_))
		(T (alert "*** _keep_ or _erase_ expected as argument to function mirrorAboutReverseDiagonal"))
    )
	'mirrorAboutReverseDiagonal
)



(defun moveToQuadrant ( quadrant selection / ) 
	; Mirror an object residing in the first quadrant (from 0 to 90 decimal degrees, 0 DD = East and 90 DD = North).
	; quadrant = number 1..4, results in mirroring operations resulting from mirroring about X-axis (0 DD) and / or Y-axis (90 DD).
	; 'selection' is typically "A" or ("_ALL")
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
			(command "._MIRROR" selection "" "0,0" "0,1" "_YES")
		)
		((= quadrant 3)
			(command "._MIRROR" selection "" "0,0" "0,1" "_YES")
			(command "._MIRROR" selection "" "0,0" "1,0" "_YES")
		)
		((= quadrant 4)
			(command "._MIRROR" selection "" "0,0" "1,0" "_YES")
		)
	)
	'moveToQuadrant
)



; Draw hatch patterns
;--------------------
;
; See also definition of global constants - standard hatch densities
;
(defun drawHatch ( scale )
	(command 
		"._-HATCH" "S" "_LAST" "" "Properties" "ANSI31" scale "0" "ORIGIN" "_SET" "0.0,0.01" "_NO" ""
	   "._DRAWORDER" "_LAST" "" "_Front"
	   "._EXPLODE" "_LAST" ""
	)
	'drawHatch
)



(defun drawHatchFromPoint ( scale pt ang offset )
	(command 
		"._-HATCH" pt "Properties" "ANSI31" scale ang "ORIGIN" "_SET" (strcat "0.0," (rtos offset)) "_NO" ""
	   "._DRAWORDER" "_LAST" "" "_Front"
	   "._EXPLODE" "_LAST" ""
	)
	'drawHatchFromPoint
)



(defun drawHatchOptions ( scale ang offset style selection )
	(command 
		"._-HATCH" "S" selection "" "Properties" style scale ang "ORIGIN" "_SET" (strcat "0.0," (rtos offset)) "_NO" ""
	   "._EXPLODE" "_LAST" ""
	)
	'drawHatchOptions
)


(defun drawHatchOptionsSelectPoint ( scale pt ang offset style )
	(command
		"._-HATCH" pt "Properties" style scale ang "ORIGIN" "_SET" (strcat "0.0," (rtos offset)) "_NO" ""
	   "._EXPLODE" "_LAST" ""
	)
	'drawHatchOptionsSelectPoint
)



(defun addWipeoutToLastClosedPolyline ( wipeoutLayDef keepOrErase / currentLayer )
	(if (not wipeoutLayDef) (alert (strcat "*** ERROR: addWipeoutToLastClosedPolyline( ) called with bad or nil wipeoutLayDef")))
	(setq currentLayer (getvar 'CLAYER)) ; stash current layer in local variable
	(setLayer wipeoutLayDef)
	(cond 
		((= keepOrErase _keep_) 
			(command "._WIPEOUT" "_POLYLINE" "_LAST" "" _keep_)
			(command "._DRAWORDER" "_LAST" "" "_Back")
		)
		((= keepOrErase _erase_)
			(command "._WIPEOUT" "_POLYLINE" "_LAST" "" _erase_)
			(command "._DRAWORDER" "_LAST" "" "_Back")
		)
		(T (alert (strcat "*** ERROR: addWipeoutToLastClosedPolyline( ) called with bad keepOrErase argument [" keepOrErase "].")))
	)
	(setvar 'CLAYER currentLayer) ; retrieve stashed value
)



; CAD system Block manipulation (block table entries)
;----------------------------------------------------------

(defun createSchematicBlockFromCurrentGraphics ( blockName / ) 
	; Create one block from present graphics in modelspace
	; In the case when the blocks are nested:
	(setq blockName (strcat blockName "-S"))
	(if (tblsearch "BLOCK" blockName)
		 ;If block exists already (such as 'NO-BN-2D-JBTSI-FILLED-nn' for switches / signaling symbols) which is generated for several switch types)
			;Redefine block definition:
			(command "._BLOCK" blockName "_YES" "0,0" "_ALL" "") ; Additional question 'Redefine it?' needs answer "_YES"
		; else just create first-time block:
			(command "._BLOCK" blockName "0,0" "_ALL" "") 
	)
	(setq nSchematicBlocks (+ nSchematicBlocks 1)) ; Global: Increment for each block created with routine createSchematicBlockFromCurrentGraphics.
	(setLayer layer_Zero)
	(setDefaultObjectPropertiesToByBlock)
)



(defun createGeoBlockInCurrentPaperScaleFromCurrentGraphics ( paperScale blockName /  ) 
	; Create one block from the present graphics in model space, already scaled and ready to be stored as a scaled block!
	; See more comments in the iteraion-variant of this routine, below.
	(setq blockName (strcat blockName "-1_" paperScale))
	(if (tblsearch "BLOCK" blockName) ; (See annotation further up)
		(command "._BLOCK" blockName "_YES" "0,0" "_ALL" "")
		(command "._BLOCK" blockName "0,0" "_ALL" "") 
	)
	(setq nScaledBlocks (+ 1 nScaledBlocks)) ; Global: Increment for each block created with routine (createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName).
	(setLayer layer_Zero)
	(setDefaultObjectPropertiesToByBlock)
)



(defun createGeoBlockInAllPaperScalesFromBlock ( inputBlockName conversionScale outputBlockName / paperScale dwgScale scaledBlockName )
	;
	; Create several scaled blocks based on the corresponding schematic symbol retrieved from the BLOCK table as an INSERT.
	; The scales are retrieved from the global list 'paperScaleList', a list of text strings "250" for 1:250 drawings, "500" for 1:500 
	; drawings etc defined in MAIN().
	;
	; Note: We assume that everything in the schematic block can be scaled uniformly (no real-metric scale things there).
	;
	; 'conversionScale' denotes the scaling factor to apply to a schematic plan's symbol in order to produce a suitable 1:500 scale symbol from
	; schematic graphics.
	;
	; The signal symbols are generally programmed in LISP to their 'schematic plan' scales. Some symbols are quite big (the 'S-lås' symbol 
	; for instance), some are 1:1 (yokes, switches). Each scalable block is first stored as a schematic version, without any scaling (dwgScale = 1.0).
	; A call to (createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName)(conversionScale blockName) pre-scales graphics with factor 
	; 'conversionScale' to convert into a suitable default 1:1000 scale size.
	; Note that the ordinary drawing scale will be 1:250 drawing scale, because most producers will draw 350 meter of model on an lying A1 sheet, 
	; and then reduce size to 50% (A1 -> A3) when printing on paper - this is a suitable size for seeing most details.
	; 
	; "1:500" is the default size for Bane NOR overhead catenary system (OCS) symbols. Signal symbols vary in size.

	(foreach paperScale paperScaleList
		; Non-schematic symbols (switches etc) should provide '1.0' as conversionScale whenever the symbol is already drawn to a suitable geo scale by its LISP routine.
		; All geographical scale drawing symbols receive a "-1_100", "-1_250" etc scale suffix.
		; Most scalable symbols are created to 1:500 scale, the conversionScale parameter should then be '1.0'.
		; But many signaling symbols are created to schematic scale and therefore require a dedicated conversion into the 1:500 scale.
		(setq dwgScale (* conversionScale (/ (atof paperScale) 1000.0)))  ; <===== This sets what is the reference for drawing scale "_one_".

		;(setq inputBlockName (substr st 1 (- (strlen st) 2))) ; Remove '-S' at end of schematic symbols's block name
		;(setq inputBlockName (strcat inputBlockName "-S")) ; Add '-S'
		(addScaledGraphicsFromBlock inputBlockName dwgScale) ; Retrieve, explode and scale the corresponding schematic symbol's block
		
		; Name for the scaled block:
		(setq scaledBlockName (strcat outputBlockName "-1_" paperScale))
		
		(if (tblsearch "BLOCK" scaledBlockName)		; Store block (See annotation further up)
			(command "._BLOCK" scaledBlockName "_YES" "0,0" "_ALL" "")
			(command "._BLOCK" scaledBlockName "0,0" "_ALL" "") 
		)
		(setq nScaledBlocks (+ nScaledBlocks 1)) ; Global: Increment for each block created with routine (createGeoBlockInAllPaperScalesFromBlock blockName _one_ blockName).
		(setDefaultObjectPropertiesToByBlock)
	)
)



(defun createGeoBlockInAllPaperScalesFromCurrentGraphics ( conversionScale outputBlockName / paperScale dwgScale scaledBlockName )
	;
	; Create several scaled blocks based on the corresponding graphics in modelspace.
	; The scales are retrieved from the global list 'paperScaleList', a list of text strings "250" for 1:250 drawings, "500" for 1:500 
	; drawings etc defined in MAIN().
	;
	; Note: We assume that everything in the current graphics can be scaled uniformly (no real-metric scale things there).
	;
	; "1:500" is the default size for Bane NOR overhead catenary system (OCS) symbols. Signal symbols vary in size.

	(createSchematicBlockFromCurrentGraphics "TMP") ; (Note: a suffix "-S" denoting 'schematic symbol' will be added to "TMP")

	(foreach paperScale paperScaleList
		(setq dwgScale (* conversionScale (/ (atof paperScale) 1000.0)))  ; <===== This sets what is the reference for drawing scale "_one_".
		(setq scaledBlockName (strcat outputBlockName "-1_" paperScale))

		(addScaledGraphicsFromBlock "TMP" dwgScale) ; Retrieve, explode and scale (Note: '-S' will be added inside function call)
		(if (tblsearch "BLOCK" scaledBlockName)		; Store block (See annotation further up)
			(command "._BLOCK" scaledBlockName "_YES" "0,0" "_ALL" "")
			(command "._BLOCK" scaledBlockName "0,0" "_ALL" "") 
		)
		(setq nScaledBlocks (+ nScaledBlocks 1))
		(setDefaultObjectPropertiesToByBlock)
	)

	; Cleanup
	(eraseBlock "TMP-S") ; also sets layer 0 and decrements the schematic block counter
)



(defun addScaledGraphicsFromBlock ( blockName scale / )
	; Retrieve, explode and scale an existing symbol
	(command 
		"._INSERT" (strcat blockName "-S") "_S" scale "_R" 0.0 "0,0"	; Retrieve schematic symbol - Set overall scale, rotation 0.0, pos. (0,0).
		"._EXPLODE" "_ALL" ""										; Convert inserted block to modelspace graphics entities
	)
	(setLayer layer_Zero)
)



(defun copyAndScaleBlock ( fromBlockName toBlockName scale / )
	(command 
		"._INSERT" fromblockName "_S" scale "_R" 0.0 "0,0"
		"._EXPLODE" "_ALL" ""
	)
	(if (tblsearch "BLOCK" toBlockName) ; (See annotation further up)
		(command "._BLOCK" toBlockName "_YES" "0,0" "_ALL" "")
		(command "._BLOCK" toBlockName "0,0" "_ALL" "")
	)
	(setLayer layer_Zero)
	(setDefaultObjectPropertiesToByBlock)
)



(defun eraseBlock ( blockNames / ss )
	; Removes all inserts and definition of block(s)
	; usage (deleteBlock "blkname1,blkname2,blkname3,and_so_on")
	(if (setq ss (ssget "x" (list (cons 0 "INSERT") (cons 2 blockNames)))) ; if any INSERTs found, erase them first
		(progn
			(command "-layer" "_U" "*" "")	; unlock all layers
			(command "erase" ss "")			; erase 
		)
	)
	(command "._LAYERP") ; restore previous layer state
	(command "._PURGE" "_B" blockNames "_N") ; Erase specified block(s) from block table
	(setLayer layer_Zero)
	(setq nSchematicBlocks (- nSchematicBlocks 1)) ; One block removed...
	(setDefaultObjectPropertiesToByBlock)
)
