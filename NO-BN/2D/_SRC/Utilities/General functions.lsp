;=======================================================
;
; General functions.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

; Print a trace while running under a debugger
;----------------------------------------------------------
(defun ResetTraceLevels ( / )
	(setq nTotalBlocks 0)
	(setq majorStep 0)
	(setq minorStep 0)
	(setq microStep 0)
)

(defun TraceLevel1 ( msg / )
	(if (= majorStep nil) (setq majorStep 0))
	(if (= nTotalBlocks nil) (setq nTotalBlocks 0))
	(setq majorStep (1+ majorStep))
	(setq minorStep 0)
	(setq microStep 0)
	(princ (strcat (itoa nTotalBlocks) " --- Step " (itoa majorStep) " - " msg "\n")) (princ)
)

(defun TraceLevel2 ( msg / )
	(if (= minorStep nil) (setq minorStep 0)) ; should not happen, level 1 should be called first
	(setq minorStep (1+ minorStep))
	(setq microStep 0)
	(princ (strcat (itoa nTotalBlocks) " --- Step " (itoa majorStep) "." (itoa minorStep) " - " msg "\n")) (princ)
)

(defun TraceLevel3 ( msg / )
	(if (= microStep nil) (setq microStep 0)) ; should not happen, level 2 should be called first
	(setq microStep (1+ microStep))
	(princ (strcat (itoa nTotalBlocks) " --- Step " (itoa majorStep) "." (itoa minorStep) "." (itoa microStep) " - " msg "\n")) (princ)
)



; File access from within the CAD system
; TODO 2021-04-04: Adapt any CAD system according to current _CAD_ value. At present, only ACAD will work properly.
;----------------------------------------------------------

(defun AddSupportPath ( dir / tmp Cpath )
	(vl-load-com)
	(setq
		Cpath (getenv "ACAD") tmp 
		(strcat ";" dir ";")
	)
	(if (not (vl-string-search dir cpath)) (setenv "ACAD" (strcat Cpath ";" dir)))
	dir
)



(defun LoadFolder ( folderName / )
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




; Define CAD system constants (again) before further loading of LISP files
;--------------------------------------------------------------
(DefineCadSystemConstants)



; CAD system default values
;----------------------------------------------------------

(defun PurgeAll ( / )
	(command 
		_LAYER_ _unlockLayer_ _anyLayerName_ _ENTER_
		_ERASE_ _selectAll_ _ENTER_
	)
	(command _LAYER_ _SetLayer_ "0" _ENTER_)
	(command 
		_PURGE_ _purgeAll_ _anyLayerName_ _purgeWithoutVerification_
		_PURGE_ _purgeAll_ _anyLayerName_ _purgeWithoutVerification_
		_PURGE_ _purgeAll_ _anyLayerName_ _purgeWithoutVerification_
	)
	'PurgeAll
)



(defun SetCadSystemDefaults ( / )
	; TODO 2021-04-04 CLFEY Adapt to several CAD systems
	(command
		_ERASE_ _selectAll_ _ENTER_ _ENTER_
		"._SNAP" 1.0
		"._OSNAP" "_OFF"
		"._OSNAPCOORD" 1
		"._SNAPMODE" 0
		"._GRID" "_ON"
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
		_POLYLINE_ _origin_ _setPolylineWidth_ _zero_ _zero_ _ENTER_ ; Default to thin polylines (from start to end)
	)
	'SetCadSystemDefaults
)


(defun SetDefaultObjectPropertiesToByBlock ( / )
	(SetLayer layDef_Zero)
	(command
		_COLOR_ _byBlock_
		_LINETYPE_ _setLinetype_ _byBlock_ _ENTER_
		_LWEIGHT_ _byBlock_
		_CETRANSPARENCY_ _byBlock_
	)
	'SetDefaultObjectPropertiesToByBlock
)
  
  
  
;Not in use;
;(defun SetCurrentDefaultPropertiesToByLayer ( / )
;	(command
;		_COLOR_ _byLayer_
;		_LINETYPE_ _setLinetype_ _byLayer_ _ENTER_
;		_LWEIGHT_ _byLayer_
;		"._CETRANSPARENCY" _byLayer_
;	)
;	'SetCurrentDefaultPropertiesToByLayer
;)
  
  
  
;Not in use:
;(defun SetPropertiesToByBlockForAllBlocks ( / block x doc )
;	(vlax-for block 
;		(vla-get-blocks 
;			(setq doc (vla-get-activedocument (vlax-get-acad-object)))
;		)
;		(vlax-for x block
;			(progn
;				(if (= (vla-get-layer x) "0")
;					(vla-put-color x 0)
;				)
;				(vla-put-linetype x "_ByBlock")
;				(vla-put-lineweight x -2)
;				(vla-put-entitytransparency x "_ByBlock")
;			)
;		)
;	)
;	(vla-regen doc acActiveViewport)
;	'SetPropertiesToByBlockForAllBlocks
;)


; CAD system style settings
;----------------------------------------------------------

(defun CreateIsoTextStyle ( / )
	;
	; >>>>>>When debugging on your computer, make sure the default ISO font also resides in "\Program Files\Autodesk\AutoCAD 2020\Fonts".<<<<<<
	; (i.e. to the font folder for the acad version you are using).
	;
	; The preferred font will result in an error if not present in the appropriate AutoCAD folder.
	; Place your preferred font file in the GitHub RailCOMPLETE folder "...\Customization\XX-YY\2D\_SRC\Fonts".
	; Change the folder name according to your AutoCAD application (2018, 2019, 2020...)
	; Set _defaultFontShx_ to the filename (with extension) for the font file, e.g. "es_icocp.shx" etc.
	;
	; NB! Include this line: <<<_textStyleAnnotativity_ _textStyleIsAnnotative_ _textStyleOrientationIsNotMatchedToLayout_>>> after
	; the "_defaultFontShx_" line if the text style should be annotative.
	; Note that a non-annotative text style used in a non-annotative TEXT / MTEXT / ATTDEF inside an annotative block, will behave
	; as an annotative entity. Also note that if the text style is annotative, then a non-annotative TEXT / MTEXT / ATTDEF inside a 
	; non-annotative block definition will behave annotatively, probably contrary to what you would need.
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
	'CreateIsoTextStyle
)



(defun RemoveUnwantedStyles ( / )
	; Remove non-standard dimension styles
	; These styles may have appeared in your allegedlt empty drawing because of your own company's CAD automation templates.
	(SetDimStyle "STANDARD")
	(command _PURGE_ _purgeDimStyles_ _purgeAnything_ _purgeWithoutVerification_) ; Purge all dimension styles (except "standard" which can't be purged), no verify

	; Remove non-standard multileader styles
	(SetMultileaderStyle "STANDARD")
	(command _PURGE_ _purgeMultiLeaderStyles_ _purgeAnything_ _purgeWithoutVerification_) ; Purge all multileader styles (except "standard" which can't be purged), no verify

	; Remove non-standard section view styles
	(SetViewSectionStyle "Metric50")
	(command _PURGE_ _purgeSectionViewStyles_ _purgeAnything_ _purgeWithoutVerification_) ; Purge all section view styles (except "Metric50" which can't be purged), no verify

	; Remove non-standard deatil view styles
	(SetDetailViewStyle "Metric50")
	(command _PURGE_ _purgeDetailViewStyles_ _purgeAnything_ _purgeWithoutVerification_) ; Purge all detail view styles (except "Metric50" which can't be purged), no verify

	(SetTableStyle "STANDARD")
	(command _PURGE__purgeTableStyles_ _purgeAnything_ _purgeWithoutVerification_) ; Purge all table styles (except "standard" which can't be purged), no verify

	; Remove non-standrard text styles
	(SetTextStyle "STANDARD")
	(command _PURGE_ _purgeTextStyles_ _purgeAnything_ _purgeWithoutVerification_) ; Purge all text styles (except "standard" which can't be purged), no verify

	; Purge blocks which might have been in use by the now removed styles (all blocks, if there would be any more)
	(command _PURGE_ _purgeBlocks_ _purgeAnything_ _purgeWithoutVerification_) ; Purge all section view styles (except "Metric50" which can't be purged), no verify
	
	'RemoveUnwantedStyles
)

;Not in use:
;(defun SetDimStyle ( dimStyle / acdoc )
;	(vl-load-com)
;	(setq acdoc (vla-get-ActiveDocument (vlax-get-acad-object)))
;	(if (tblsearch "DIMSTYLE" dimStyle)
;		(vla-put-activeDimstyle 
;			acdoc 
;			(vla-item (vla-get-Dimstyles acdoc) dimStyle)
;		)
;	)
;)

;Not in use:
;(defun SetMultileaderStyle ( multileaderStyle / )
;	(command "._CMLEADERSTYLE" multileaderStyle)
;)

;Not in use:
;(defun SetViewSectionStyle ( viewSectionStyle / )
;	(command "._CVIEWSECTIONSTYLE" viewSectionStyle)
;)

;Not in use:
;(defun SetDetailViewStyle ( viewDetailStyle / )
;	(command "._CVIEWDETAILSTYLE" viewDetailStyle)
;)

;Not in use:
;(defun SetTableStyle ( tableStyle / )
;	(command "._CTABLESTYLE" tableStyle)
;)

(defun SetTextStyle ( textStyle / )
	(command "._STYLE" textStyle)
)



; CAD system LAYER settings
;----------------------------------------------------------

;Not in use:
;(defun FindPrefixedLayers ( layprefix / laydata laylist prefixlen layname )
;	(setq laydata (tblnext "LAYER" T) ; Rewind and get first entry in acad LAYER list
;		  laylist (list)
;		  prefixlen (strlen layprefix)
;		  layprefix (strcase layprefix)
;	)
;	(while laydata
;		(setq layname (strcase (cdr (assoc 2 laydata)))) ; pick next layer name from list
;		(if (= layprefix (substr layname 1 prefixlen))
;			(setq laylist (append laylist (list layname)))
;		)
;		(setq laydata (tblnext "LAYER" nil)) ; get next item from acad LAYER list
;	)
;	laylist
;)



(defun IsExistingLayer ( layerName / laydata layname lay )
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



(defun CreateLayer ( layDef / adm currentLayer )
	; Create new layer, or modify if layer already exists.
	;
	; Option "New" (createNewLayer) instead of "Make" (makeNewLayer) will create layer if it does not exist, but it has some flaws: 'New' fails for some reason.
	; Option 'Make' instead of 'New' would just ignore & inform but not fail if layer already exists. ACAD internal state variable CLAYER (current layer) would be set to the specified layer.
	; Note: If you try use "Make" to an existing layer, then the option "Description" asks for accept to change existing description, if any. This makes the "Make" command difficult to use.
	;
	; Typical use: (CreateLayer layDef_View_SchematicPlan) etc
	;
	; 'Struct layDef' definition: See function SetLayer()
	; 'admNr' is a global identifier set with other administration-specific constants, see that Lisp file.
	;
	(setq
		layerName			(eval (nth 0 layDef))
		layerColor			(eval (nth 1 layDef))
		layerDescription	(eval (nth 2 layDef))
		; We do not use (nth 3 ...) here
	)
	(if (IsExistingLayer layerName)
		(progn
			; Change
			(setq currentLayer (getvar 'CLAYER)) ; stash current layer in local variable
			(command
				_LAYER_
					_SetLayer_      layerName
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
	; Always reset - otherwise (CreateLayer ...) fails at least in the VLIDE debugger.
	; I never found out why and exactly where, but the ._LAYER "Set" ... command did not terminate properly, so the next command would fail.
	(SetLayer layDef_Zero)
	'CreateLayer
)



(defun SetLayer ( layDef / layerName objectColor )
	; 'Struct layDef' definition - 2-dimensional table:
	; Outer level:
	; 	item 0 = admNr nr 0
	; 	item 1 = admNr nr 1
	;	etc
	; Inner level:
	; 	item 0 = Layer's name (NB Use specific constants for each CAD system when special characters are needed).
	; 	item 1 = Layer's color
	; 	item 2 = Layer's description
	; 	item 3 = Default color for objects intended for this layer (for use with 'AddPlaced...()' routines
	;
	; Usage:
	; 	(setq myLayerName (eval (nth 0 (nth admNr layDef))))
	; 	(setq myLayerColor (eval (nth 0 (nth admNr layDef))))
	;	etc
	;
	(if (= layDef nil)
	 	(alert "SetLayer(): layDef argument cannot be nil.")
	 )
	(setq 
		layerName	(eval (nth 0 layDef))
		objectColor	(eval (nth 3 layDef))
	)
	(cond
		((IsExistingLayer layerName)
				(command _LAYER_ _thawLayer_ layerName _ENTER_)
				(command _LAYER_ _SetLayer_ layerName _unlockLayer_ layerName _turnOnLayer_ layerName _ENTER_)
				(command _COLOR_ objectColor) ; Default color for next graphics entities to be drawn
		)
		(T
			(CreateLayer layDef_UnknownLayerNameRequested) ; Create layer if nonexisting. Switch to this layer.
			(SetLayer layDef_UnknownLayerNameRequested) ; recursive, fails if no definition exists for that layer in file CreateStandardLayers.lsp.
		)
	)
	'SetLayer ; For some reason, this "kills" the hanging operation inside the -LAYER command, which otherwise will mess up during debugging
)



;Not in use:
;(defun SetDefaultOjectColor ( color / )
;	(command _COLOR_ color)
;)



;Not in use:
;(defun SetLayerAndObjectColor ( layDef objectColor / )
;	(SetLayer layDef)
;	(command _COLOR_ objectColor) ; Override layer's usual object color, which was first set with SetLayer().
;)



(defun FreezeAllLayersExceptCurrentLayer ( / )
	(command _LAYER_ _freezeLayer_ _anyLayerName_ _ENTER_)	; Freeze all layers except current layer
)



(defun ThawAllLayers ( / )
	(command _LAYER_ _thawLayer_ _anyLayerName_ _ENTER_)	; Thaw all layers except current layer
)



; CAD system TEXT entities
;----------------------------------------------------------

(defun AddTextAtPosWithJustification ( layDef textHeight pos text justification / )
	(SetLayer layDef)
	(command 
		_TEXT_
			_SetTextStyle_ _rcTextStyle_
			_justifyText_ justification
			pos 
			textHeight 
			_angleZero_
			text
	)
	'AddTextAtPosWithJustification
)



(defun AddTextBetweenTwoBasePoints ( layDef textHeight basePointLeft basePointRight text / )
	(SetLayer layDef)
	(command 
		_TEXT_
			_SetTextStyle_ _rcTextStyle_
			_justifyText_ _fit_
			basePointLeft basePointRight
			textHeight 
			text
	)
	'AddTextBetweenTwoBasePoints
)



(defun AddTextAtPos ( layDef textHeight pos text /  )
	; Single text, no line breaking
	; layDef textHeight pos text
	; Note: TEXT and MTEXT will be annotative entities if they reside inside an annotative block (RC object anonymous block etc)
	;
	; Note: ; For some reason, texts on layer 0 get "ByLayer" instead of "ByBlock" :-( but not when running MAIN() as a batch job.
	;
	(AddTextAtPosWithJustification layDef textHeight pos text _middleCenter_)
	'AddTextAtPos
)



(defun AddMText ( layDef textHeight textBoxWidth pos text / )
	; Multiline text
	; Note: TEXT and MTEXT will be annotative entities if they reside inside an annotative block (RC object anonymous block etc)
	(SetLayer layDef)
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
   'AddMText
)



; CAD system text ATTRIBUTE entities
;----------------------------------------------------------
(defun AddAtt ( attTag attPrompt attDefaultValue pos textHeight rotation textStyle justification / tmp )
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
	; If you need to test - use Autolisp LOGAND etc to do bitwise AND operations.
	;
	(setq tmp (getvar 'AFLAGS))
	(setvar 'AFLAGS _lockPosition_)
	(if (= attPrompt nil) 
		; Add 'empty prompt' since 'attPrompt' argument is nil
		(setq attPrompt "(no prompt)")	
	)
	(command
		_ATTDEF_		
			_ENTER_ 								; Accept current AFLAGS settings (including the multiline-bit in AFLAGS). We didn't set 'annotative'.
			attTag									; Name of attribute tag - which must correspond to RailCOMPLETE DNA syntax
			attPrompt								; Prompt
			attDefaultValue							; Default single-line text.
			_setAttdefTextStyle_ textStyle			; 
			_setAttdefJustifcation_ justification	; 
			pos 									; Specify justification reference point
			textHeight								; No qualifier first since AutoCAD demands text height here.
			rotation								; No qualifier first since AutoCAD demands text rotation here.
	)
	(setvar 'AFLAGS tmp)
	'AddAtt
)



(defun AddMultilineAtt ( attTag attPrompt attDefaultValue pos textHeight rotation textStyle justification width / tmp )
	; NB! The command dialog is quite different when multiline entity is needed.
	(setq tmp (getvar 'AFLAGS))
	(setvar 'AFLAGS (+ _lockPosition_ _multipleLines_)) ; We need multiple line attributes with our DNA's - in the Watch object. See AutoCAD escaped characters (+\ etc).
	(if (= attPrompt nil) 
		; Add 'empty prompt' since 'attPrompt' argument is nil
		(setq attPrompt "(no prompt)")	
	)
	(command
		_ATTDEF_		
			_ENTER_ 								; Accept current AFLAGS settings (including the multiline-bit in AFLAGS). We didn't set 'annotative'.
			attTag									; Name of attribute tag - which must correspond to RailCOMPLETE DNA syntax
			attPrompt								; Prompt
			attDefaultValue							; Default single-line text.
			_ENTER_									; Don't ask for more lines now - assume the user uses acad escaped characters for newline (\P)
			pos 									; Specify justification reference point. Acad then asks for "Specify the other corner", but offers the down-arrow menu.
			_setAttdefTextStyle_ textStyle			; 
			_setAttdefJustifcation_ justification	; 
			_setAttdefTextHeight_ textHeight		; No qualifier first since AutoCAD demands text height here.
			_setAttdefRotation_ rotation			; No qualifier first since AutoCAD demands text rotation here.
			_setAttdefTextWidth_ width				; Since we specify the box width, the command stops ask for the lower right corner, and terminates.
	)
	(setvar 'AFLAGS tmp)
	'AddMultilineAtt
)



(defun AddTextAttributeAtPos ( layDef textHeight pos attDef / attTag attPrompt attDefaultValue )
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
	(SetLayer layDef)
	(AddAtt attTag attPrompt attDefaultValue pos textHeight _angleZero_ _rcTextStyle_ _middleCenter_)
	'AddTextAttributeAtPos
)



(defun GetAttributeTag			( attDef / ) (eval (nth 0 attDef)))
(defun GetAttributePrompt		( attDef / ) (eval (nth 1 attDef)))
(defun GetAttributeDefaultValue	( attDef / ) (eval (nth 2 attDef)))



(defun AddMultilineTextAttributeAtPos ( layDef textHeight pos attDef / attTag attPrompt attDefaultValue width )
	(setq
		attTag			(eval (nth 0 attDef))
		attPrompt		(eval (nth 1 attDef))
		attDefaultValue	(eval (nth 2 attDef))
		width 			0 		; Width = 0 induces autoscaled box width. Assume that the user adds newline characters as needed (escaped characters).
	)
	(SetLayer layDef)
	(AddMultilineAtt attTag attPrompt attDefaultValue pos textHeight _angleZero_ _rcTextStyle_ _middleCenter_ width)
	'AddMultilineTextAttributeAtPos
)



; Graphics scaling
;----------------------------------------------------------
(defun ScaleAll ( factor / )
	(command _SCALE_ _selectAll_ _ENTER_ _origin_ factor)
)



; Graphics rotation
;----------------------------------------------------------
(defun RotateLeft ( angle / )
	; Rotate CCW with angle [Decimal Degrees]
	(command _ROTATE_ _selectAll_ _ENTER_ _origin_ angle)
)



(defun RotateRight ( angle / )
	; Rotate CCW with angle [Decimal Degrees]
	(command _ROTATE_ _selectAll_ _ENTER_ _origin_ (- angle))
)



; Graphics mirroring
;----------------------------------------------------------

(defun MirrorAboutXaxis ( variation / )
	; Mirror about horizontal axis through origin
	; variation should be one of _keepMirrorSource_ or _eraseMirrorSource_ (see definition of global CAD constants)
	(cond 
		((= variation _keepMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origin_ _xAxis_ _keepMirrorSource_))
		((= variation _eraseMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origin_ _xAxis_ _eraseMirrorSource_))
		(T (alert "*** _keepMirrorSource_ or _eraseMirrorSource_ expected as argument to function MirrorAboutXaxis"))
    )
	'MirrorAboutXaxis
)



(defun MirrorAboutYaxis ( variation / )
	; Mirror about vertical axis through origin
	; variation should be one of _keepMirrorSource_ or _eraseMirrorSource_ (see definition of global CAD constants)
	(cond
		((= variation _keepMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _keepMirrorSource_))
		((= variation _eraseMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origin_ _yAxis_ _eraseMirrorSource_))
		(T (alert "*** _keepMirrorSource_ or _eraseMirrorSource_ expected as argument to function MirrorAboutYaxis"))
    )
	'MirrorAboutYaxis
)



(defun MirrorAboutDiagonal ( variation / )
	; Mirror about 45 degrees diagonal through origin
	; variation should be one of _keepMirrorSource_ or _eraseMirrorSource_ (see definition of global CAD constants)
	(cond
		((= variation _keepMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origin_ _diagonalAxis_ _keepMirrorSource_))
		((= variation _eraseMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origin_ _diagonalAxis_ _eraseMirrorSource_))
		(T (alert "*** _keepMirrorSource_ or _eraseMirrorSource_ expected as argument to function MirrorAboutDiagonal"))
    )
	'MirrorAboutDiagonal
)



(defun MirrorAboutReverseDiagonal ( variation / )
	; Mirror about 135 degrees diagonal through origin
	; variation should be one of _keepMirrorSource_ or _eraseMirrorSource_ (see definition of global CAD constants)
	(cond
		((= variation _keepMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origin_ _reverseDiagonalAxis_ _keepMirrorSource_))
		((= variation _eraseMirrorSource_) (command _MIRROR_ _selectAll_ _ENTER_ _origin_ _reverseDiagonalAxis_ _eraseMirrorSource_))
		(T (alert "*** _keepMirrorSource_ or _eraseMirrorSource_ expected as argument to function MirrorAboutReverseDiagonal"))
    )
	'MirrorAboutReverseDiagonal
)



(defun MoveToQuadrant ( quadrant selection / ) 
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
			(command _MIRROR_ selection _ENTER_ _origin_ _yAxis_ _eraseMirrorSource_)
		)
		((= quadrant 3)
			(command _MIRROR_ selection _ENTER_ _origin_ _yAxis_ _eraseMirrorSource_)
			(command _MIRROR_ selection _ENTER_ _origin_ _xAxis_ _eraseMirrorSource_)
		)
		((= quadrant 4)
			(command _MIRROR_ selection _ENTER_ _origin_ _xAxis_ _eraseMirrorSource_)
		)
	)
	'MoveToQuadrant
)



; Draw hatch patterns
;--------------------
;
; See also definition of global constants - standard hatch densities
;
(defun DrawHatch ( hatchDensity / )
	; Hatch the currently selected closed polyline
	(command 
		_HATCH_ 
			_selectHatchObjects_ _lastSelection_ _ENTER_ 
			_setHatchProperties_ _hatchPatternSlantedLines_ hatchDensity _angleZero_ 
			_selectHatchOrigin_ _setNewHatchOrigin_ _slightlyAbove_ _doNotStoreHatchOriginAsDefault_
			_ENTER_
	)
	(command _DRAWORDER_ _lastSelection_ _ENTER_ _aboveAllObjects_)
	(command _EXPLODE_ _lastSelection_)
	'DrawHatch
)



(defun DrawHatchAtPoint ( hatchDensity pt ang offset / )
	(command 
		_HATCH_ 
			pt
			_setHatchProperties_ _hatchPatternSlantedLines_ hatchDensity ang 
			_selectHatchOrigin_ _setNewHatchOrigin_ (strcat "0.0," (rtos offset)) _doNotStoreHatchOriginAsDefault_
			_ENTER_
	)
	(command _DRAWORDER_ _lastSelection_ _ENTER_ _aboveAllObjects_)
	(command _EXPLODE_ _lastSelection_)
	'DrawHatchAtPoint
)



(defun DrawHatchFromSelectionUsingStyle ( hatchDensity selectionSet style / )
; Note: '_origin_' does not work here as 'new origin', for some reason... - use "0,0" instead.
(command 
		_HATCH_ 
			_selectHatchObjects_ selectionSet _ENTER_ 
			_setHatchProperties_ style hatchDensity _angleZero_ 
			_selectHatchOrigin_ _setNewHatchOrigin_ "0,0" _doNotStoreHatchOriginAsDefault_ 
			_ENTER_
	)
	(command _DRAWORDER_ _lastSelection_ _ENTER_ _aboveAllObjects_)
	(command _EXPLODE_ _lastSelection_)
	'DrawHatchFromSelectionUsingStyle
)



(defun AddWipeoutToLastClosedPolyline ( wipeoutLayDef keepOrErase / currentLayer )
	; Use predefined values for keepOrErase, see above at _WIPEOUT_ definitions.
	(if (not wipeoutLayDef) (alert (strcat "*** ERROR: AddWipeoutToLastClosedPolyline( ) called with bad or nil wipeoutLayDef")))
	(setq currentLayer (getvar 'CLAYER)) ; stash current layer in local variable
	(SetLayer wipeoutLayDef)
	(command _WIPEOUT_ _createWipeoutFromPolyline_ _lastSelection_ keepOrErase)
	(command _DRAWORDER_ _lastSelection_ _ENTER_ _underAllObjects_)
	(setvar 'CLAYER currentLayer) ; retrieve stashed value
)



; CAD system Block manipulation (block table entries)
;----------------------------------------------------------
(defun CreateSchematicBlockFromCurrentGraphics ( blockName /  ) 
	; Create a non-annotative block from the present graphics in model space.
	; Assume that all AttDefs have been declared as non-annotative already.
	; See also AnnotativeBlock version.
	(setq blockName (strcat blockName _schematic_))
	(if (tblsearch "BLOCK" blockName) 
	; if existing block: 
		(command _BLOCK_ blockName _redefineBlock_ _blockAnnotativity_ _blockIsNotAnnotative_ _keepOrientation_ _origin_ _selectAll_ _ENTER_)
	; else just create first-time block:
		(command _BLOCK_ blockName                 _blockAnnotativity_ _blockIsNotAnnotative_ _keepOrientation_ _origin_ _selectAll_ _ENTER_)
	)
	(setq nSchematicBlocks (+ nSchematicBlocks 1))
	(setq nTotalBlocks (+ nTotalBlocks 1))
	(SetLayer layDef_Zero)
	(SetDefaultObjectPropertiesToByBlock)
)
	


(defun AddGraphicsFromScaledSchematicBlock ( blockName scale / )
	; Insert, scale and explode an existing block.
	; NB: No checking - the block must exist in the block table.
	(command ; Retrieve schematic symbol - Set overall scale, rotation and position:
		_INSERT_ (strcat blockName _schematic_) _setInsertionScale_ scale _setInsertionRotation_ _angleZero_ _origin_
	)
	(command ; Convert inserted block to modelspace graphics entities:
		_EXPLODE_ _lastSelection_
	)
	(SetLayer layDef_Zero)
)



(defun CreateAnnotativeBlockFromCurrentGraphics ( blockName /  ) 
	; Create an annotative block from the present graphics in model space.
	; Typical use: The balise triangular symbol in an annotative symbol meant for geographic mode drawings
	; Assume that all AttDefs have been declared as annotative already.
	(setq blockName (strcat blockName _scalable_))
	(if (tblsearch "BLOCK" blockName) 
	; if existing block:
		(command _BLOCK_ blockName _redefineBlock_ _blockAnnotativity_ _blockIsAnnotative_ _keepOrientation_ _origin_ _selectAll_ _ENTER_)
	; else just create first-time block:
		(command _BLOCK_ blockName                 _blockAnnotativity_ _blockIsAnnotative_ _keepOrientation_ _origin_ _selectAll_ _ENTER_)
	)
	(setq nAnnotativeBlocks (+ nAnnotativeBlocks 1))
	(setq nTotalBlocks (+ nTotalBlocks 1))
	(SetLayer layDef_Zero)
	(SetDefaultObjectPropertiesToByBlock)
)



(defun CreateMetricBlockFromCurrentGraphics ( blockName /  ) 
	; Create a non-annotative but real-size block from the present graphics in model space.
	; Typical use: Yokes / cantilevers / switches
	; Assume that all AttDefs have been declared as non-annotative already.
	(setq blockName (strcat blockName _metric_))
	(if (tblsearch "BLOCK" blockName) 
	; if existing block:
		(command _BLOCK_ blockName _redefineBlock_ _blockAnnotativity_ _blockIsNotAnnotative_ _keepOrientation_ _origin_ _selectAll_ _ENTER_)
	; else just create first-time block:
		(command _BLOCK_ blockName                 _blockAnnotativity_ _blockIsNotAnnotative_ _keepOrientation_ _origin_ _selectAll_ _ENTER_)
	)
	(setq nMetricBlocks (+ nMetricBlocks 1))
	(setq nTotalBlocks (+ nTotalBlocks 1))
	(SetLayer layDef_Zero)
	(SetDefaultObjectPropertiesToByBlock)
)



(defun CreateAnnotativeBlockFromScaledSchematicBlock ( blockName xscale / )
	;
	; Create an annotative block based on a scaled version of a schematic symbol retrieved from the BLOCK table as an INSERT.
	;
	; Signalling symbols are first programmed in LISP to their 'schematic plan' scale. However, some schematic symbols are quite big (the 
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
	(AddGraphicsFromScaledSchematicBlock blockName xscale)

	(setq blockName (strcat blockName _scalable_))
	(if (tblsearch "BLOCK" blockName)
		 ;If block exists already (such as 'FR-SR-2D-JBTSI-DENSE-nn' for switches / signaling symbols) which is generated for several switch types)...
		; or using VLIDE 'manually' several times...
		; ...then answer the additional question 'Redefine it?' which needs answer "_YES".
		; Then "_Annotative" triggers question "Create annotative block?" which needs answers "Yes". Then set insertion point and select all graphics (_selectAll_ + _ENTER_ (Enter)).
		;Redefine block definition:
		(command _BLOCK_ blockName _redefineBlock_ _blockAnnotativity_ _blockIsAnnotative_ _keepOrientation_ _origin_ _selectAll_ _ENTER_)
	; else just create first-time block:
		(command _BLOCK_ blockName                 _blockAnnotativity_ _blockIsAnnotative_ _keepOrientation_ _origin_ _selectAll_ _ENTER_)
	)
	(setq nAnnotativeBlocks (+ nAnnotativeBlocks 1))
	(setq nTotalBlocks (+ nTotalBlocks 1))
	(SetLayer layDef_Zero)
	(SetDefaultObjectPropertiesToByBlock)
)


(defun EraseSchematicBlock ( blockNames / selectionSet )
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
	(SetLayer layDef_Zero)
	(setq nSchematicBlocks (- nSchematicBlocks 1))
	(setq nTotalBlocks (- nTotalBlocks 1))
	(SetDefaultObjectPropertiesToByBlock)
)
