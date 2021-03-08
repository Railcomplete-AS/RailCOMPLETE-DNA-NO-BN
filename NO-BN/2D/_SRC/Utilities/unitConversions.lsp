;=========================================================================================================================
;
; UnitConversions.lsp
;
; Copyright Railcomplete AS / NO916118503, 2015-2021. All rights reserved.
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; 2021-02-10 CLFEY Release 2021.a
;
;=========================================================================================================================

(defun R->D (r) ; rads to degrees
	(* 180.0 (/ r pi))
)



(defun D->R (d) ;degrees to rads
	(* pi (/ d 180.0))
)
