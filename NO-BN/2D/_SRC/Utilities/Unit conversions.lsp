;=========================================================================================================================
;
; UnitConversions.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2022. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2022-10-07 CLFEY New distribution of LISP source to DNA repositories.
;
;=========================================================================================================================

(defun R->D (r) ; rads to degrees
	(* 180.0 (/ r pi))
)



(defun D->R (d) ;degrees to rads
	(* pi (/ d 180.0))
)
