;=========================================================================================================================
;
; Signal combination subroutines.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

;====================================================================================
; Manage global variables, keep track of the growing signal as items are added
;====================================================================================
(defun InitalizeSignalSymbol ( / )
	(setq 
		isTopmostItem 		T	; If True ('T'): next free-standing auxiliary signal item will be centered on pole axis
		topOfMast			0.0 ; Global, keep track of where current bottom center of uppermost item is
		totalHeight			0.0 ; Global, keep track of current total symbol height
		freeSpaceLeftSide	0.0 ; Global, space for extra lanterns to the left of main pole
		freeSpaceRightSide	0.0 ; Global, space for extra lanterns to the right of main pole
		freeSpaceBias		3.0 ; Use such as: 'Give priority to RIGHT side if freeSpaceRightSide >= freeSpaceLeftSide + freeSpaceBias'
	)
	; DEBUG: Use VLIDE or other means to set (setq _DEBUG_ T) to include model space cleanup and printing of globals:
	(if _DEBUG_ (command _ERASE_ _selectAll_ _ENTER_)) ; For debugging - clean up modelspace (your computer screen)
	(if _DEBUG_ (PrintSignalCombinationGlobals))
)



(defun PrintSignalCombinationGlobals ( / ) 
	; Show global values for signal combination generation - useful when debugging
	(princ 
		(strcat 
			"\n\nGLOBALS:"
			"\n isTopmostItem      = " (if isTopmostItem "Yes" "No")
			"\n totalHeight        = " (rtos totalHeight 2 2)
			"\n topOfMast          = " (rtos topOfMast 2 2)
			"\n freeSpaceLeftSide  = " (rtos freeSpaceLeftSide 2 2)
			"\n freeSpaceRightSide = " (rtos freeSpaceRightSide 2 2)
			"\n freeSpaceRightSide = " (rtos freeSpaceRightSide 2 2) 
			"\n freeSpaceBias      = " (rtos freeSpaceBias 2 2) 
			"\n "
		)
	)
	(princ) ; Suppress LISP echo of last printed string
)
(if _DEBUG_ (defun psg (/) (PrintSignalCombinationGlobals))) ; Shortcut



;====================================================================================
; Add signal items
;====================================================================================
(defun AddAnchor ( / )
	; Force next items to start here, on the right side
	; no pushing up
	; Please comment out the next line - graphics only added in debug versions
	;(command _CIRCLE_ _origin_ 0.5) ; For debugging - add small 'anchor' circle
	(setq freeSpaceLeftSide 0.0)
	(setq freeSpaceRightSide 0.0)
	(setq isTopmostItem nil)	; Force subsequent auxiliary signals to be drawn off-center from pole axis
)



(defun ShiftSignalItemsUp ( offs / )
	(command _MOVE_ _selectAll_ _ENTER_ _origin_ (list 0 offs)) ; Shift everything up by 'd'. Negative 'd' shifts down.
	(setq topOfMast (+ topOfMast offs))
	(setq totalHeight (+ totalHeight offs))
)



(defun AddMissingSymbol ( / )
	(ShiftSignalItemsUp 3.5) ; Update globals - make space for two lines of 1.25 height text plus line spacing
	(AddMText layDef_Zero _th125_ (* 90 _th125_) _origin_ "MANGLER SYMBOL")
)
