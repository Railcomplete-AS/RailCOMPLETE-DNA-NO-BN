;
; UnitConversions.lsp
;
(defun R->D (r) ; rads to degrees
	(* 180.0 (/ r pi))
)



(defun D->R (d) ;degrees to rads
	(* pi (/ d 180.0))
)


(defun C:I2M (/ m in string) ;inches to meters
	(setq in (GETDIST "\nValue in Inches: ")) ; A prompt appears (either as tooltip in modelspace, or in the console line).
	(setq m (* in 0.0254))
	(alert (strcat (rtos in 2 1) " inches is " (rtos m 2 3) " meters.")) ; A box pops up with an "OK" button. Control returns when the user clicks 'OK'.
)
