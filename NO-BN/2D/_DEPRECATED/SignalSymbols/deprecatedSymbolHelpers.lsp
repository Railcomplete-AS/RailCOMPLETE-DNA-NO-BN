;
; Deprecated.lsp
;
; TODO 2019-05-08 CLFEY: Deprecated - can be removed?
;


 (defun blockDescription	(blockName description)
	(vl-load-com)
	(if (setq blk_ent (entlast))
		(progn
			(setq 
				blk_obj (vlax-ename->vla-object blk_ent)
				blk_def (vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) (vla-get-effectivename blk_obj))
			)
			(vl-catch-all-apply
				(function	
					(lambda	()
						(vla-put-comments
							blk_def
							description
						)
					)
				)
			)
		)
    )
	'blockDescription
)


(defun moveAttribute (displacement / atSet	actDoc atLst actSp curTxt aFlg laySt mDel bPt
		dPt *error*)
;;http://www.cadtutor.net/forum/showthread.php?21202-Align-and-move-block-attributes-with-and-to-a-line

  (vl-load-com)
 
  (defun *error*(msg)
    (if tSet
      (progn
   (setvar "CMDECHO" 0)
          (command "_.erase" tSet "")
          (setvar "CMDECHO" 1)
 ); end progn
      ); end if
    (if laySt
      (asmi-LayersStateRestore laySt)
      ); end if
    (if actDoc
      (vla-EndUndoMark actDoc)
      ); end if
    (princ)
    ); end of *error*

  
  (defun asmi-LayersUnlock(/ restLst)
  (setq restLst '())
  (vlax-for lay(vla-get-Layers
     (vla-get-ActiveDocument
       (vlax-get-acad-object)))
    (setq restLst
     (append restLst
       (list
         (list
         lay
         (vla-get-Lock lay)
         ); end list
         ); end list
       ); end append
    ); end setq
    (vla-put-Lock lay :vlax-false)
    ); end vlax-for
   restLst
  ); end of asmi-LayersUnlock

  
  (defun asmi-LayersStateRestore(StateList)
  (foreach lay StateList
    (vla-put-Lock(car lay)(cadr lay))
    ); end foreach
   (princ)
  ); end of asmi-LayersStateRestore


  (defun asmi-GetAttributes (Block / atArr caArr)
    (append
      (if
	(not
	  (vl-catch-all-error-p
	    (setq atArr	(vl-catch-all-apply
			  'vlax-safearray->list
			  (list
			    (vlax-variant-value
			      (vla-GetAttributes Block)
			    )
			  )
			)
	    )
	  )
	)
	 atArr
      )					; end if
      (if
	(not
	  (vl-catch-all-error-p
	    (setq caArr	(vl-catch-all-apply
			  'vlax-safearray->list
			  (list
			    (vlax-variant-value
			      (vla-GetConstantAttributes Block)
			    )
			  )
			)
	    )
	  )
	)
	 caArr
      )					; end if
    )					; end if
  )					; end asmi-GetAttributes


;TODO 2019-05-08 CLFEY: Deprecated - can be removed?
  (defun asmi-GetActiveSpace (/ actDoc)
    (setq actDoc
	   (vla-get-ActiveDocument
	     (vlax-get-acad-object)
	   )
    )
    (if	(= 1 (getvar "TILEMODE"))
      (vla-get-ModelSpace actDoc)
      (vla-get-PaperSpace actDoc)
    )					; end if
  )					; end of asmi-GetActiveSpace

  (if
    (setq atLst
	   (ssget "L" '((0 . "INSERT") (66 . 1)))
    )
     (progn
       (setq atLst
		    (apply 'append
			   (mapcar 'asmi-GetAttributes
				   (mapcar 'vlax-ename->vla-object
					   (vl-remove-if
					     'listp
					     (mapcar 'cadr (ssnamex atLst))
					   )
				   )
			   )
		    )			; end apply
	     tSet   (ssadd)
	     actSp  (asmi-GetActiveSpace)
	     laySt  (asmi-LayersUnlock)
	     actDoc (vla-get-ActiveDocument
		      (vlax-get-acad-object)
		    )
       )				; end setq
       (vla-StartUndoMark actDoc)
       (foreach	att atLst
	 (setq curTxt
		(vla-AddText
		  actSp
		  "Text"
		  (vlax-3D-point '(0.0 0.0 0.0))
		  1.0
		)
	 )
	 (ssadd (vlax-vla-object->ename curTxt) tSet)
	 (foreach pr '("TextString"	"StyleName"
		       "Height"		"ScaleFactor"
		       "Backward"	"ObliqueAngle"
		       "UpsideDown"	"Rotation"
		       "Color"		"Layer"
		       "Linetype"	"Lineweight"
		       "Alignment"
		      )
	   (vlax-put-Property
	     curTxt
	     pr
	     (vlax-get-Property att pr)
	   )
	 )				; end foreach
	 (cond
	   ((= 0 (vla-get-Alignment att))
	    (vla-put-InsertionPoint
	      curTxt
	      (vla-get-InsertionPoint att)
	    )
	    (setq aFlg "InsertionPoint")
	   )				; end condition #1
	   ((member (vla-get-Alignment att) '(3 5))
	    (vla-put-InsertionPoint
	      curTxt
	      (vla-get-InsertionPoint att)
	    )
	    (vla-put-TextAlignmentPoint
	      curTxt
	      (vla-get-TextAlignmentPoint att)
	    )
	    (vla-put-ScaleFactor
	      curTxt
	      (vla-get-ScaleFactor att)
	    )
	    (setq aFlg "InsertionPoint")
	   )				; end condition #2
	   ((not (member (vla-get-Alignment att) '(0 3 5)))
	    (vla-put-TextAlignmentPoint
	      curTxt
	      (vla-get-TextAlignmentPoint att)
	    )
	    (setq aFlg "TextAlignmentPoint")
	   )				; end condition #3
	 )				; end cond
       )				; end foreach
       (command "_.move" tSet "" "Displacement" displacement)
       (setq mDel
	      (mapcar '-
		      (vlax-get
			(vlax-ename->vla-object
			  (ssname tSet (1- (sslength tSet)))
			)
			aFlg
		      )
		      (vlax-get (last atLst) aFlg)
	      )				; end mapcar
       )				; end setq
       (foreach	att atLst
	 (setq bPt (vlax-get att aFlg)
	       dPt (mapcar '+ bPt mDel)
	 )				; end setq
	 (vla-Move att (vlax-3d-Point bPt) (vlax-3d-Point dPt))
       )				; end foreach
       (setvar "CMDECHO" 0)
       (command "_.erase" tSet "")
       (setvar "CMDECHO" 1)
       (asmi-LayersStateRestore laySt)
       (vla-EndUndoMark actDoc)
     )					; end progn
  )					; end if
  'moveAttribute
); end of c:amove


(defun explodeText (/ bz ss n lvs lss lvp lvl lul ent luu)
  ;;;NB: DO NOT USE
  ;;;Does not work in accoreconsole
  (setq ss (ssget "_L"))
  (setvar "cmdecho" 0) (command "undo" "be") (setvar "mirrtext" 1)
  (command "zoom" "e")
  (setq bz (getvar "OSMODE")) (setvar "OSMODE" 0)
  (setq lvs (getvar "viewsize") lss (getvar "screensize") lvp (getvar "viewctr")
      lvl (list (list (- (car lvp) (* 0.5 (* lvs (/ (car lss) (cadr lss))))) (- (cadr lvp) (* 0.5 lvs)))
                   (list (+ (car lvp) (* 0.5 (* lvs (/ (car lss) (cadr lss))))) (+ (cadr lvp) (* 0.5 lvs))))
          lul (list (caar lvl) (cadadr lvl)) n 0)
  (repeat (sslength ss)
    	(setq ent (ssname ss n))
    	(setq luu (strcat (getenv "Temp") "\\textb.wmf"))
    	(command "mirror" ent "" lvp "@0,1" "_YES"
	     "wmfout" luu ent "" "erase" ent ""
  	"wmfin" luu lul "2" "" ""
    	"mirror" (entlast) "" lvp "@0,1" "_YES"
  	"explode" (entlast) "erase" (ssget "p") "r" "w"
  	(polar (car lvl) (* 0.25 pi) (max (abs (/ lvs (cadr lss))) (abs (/ (* lvs (/ (car lss) (cadr lss))) (car lss)))))
  	(cadr lvl) "") ;;end_command
  	(setq n (+ n 1))
  	(command "zoom" "p")
  	(setvar "mirrtext" 0) (setvar "osmode" bz) (command "undo" "e")
  	(vl-file-delete luu)
  	(command "._CHANGE" "P" "" "Properties" "Color" "_ByLayer" "")
  	)
  (command "._PURGE" "Blocks" "WMF*" "_NO")
  'explodeText
)