(defun R->D (r) ; rads to degrees
	(* 180.0(/ r pi))
)



(defun D->R (d) ;degrees to rads
	(* pi(/ d 180.0))
)



(defun ii (in) ;inches to meters
	(* in 25.4)
)



(defun C:I2M (/ mm in string) ;inches to meters
	(setq mm (GETDIST "\nValue in Inches: "))
	(setq in (/ mm 25.4))
	(setq string (rtos in 2 3))
	(alert (strcat "Value is " string " inches.")) ; prompts alert box
	(princ)
)