; How to use this script
; Set up a base drawing "base.dwg" with the layers you need, but keep the model space empty
; Place the base drawing in a folder where the drawings will be generated.
; Start Autocad with a new drawing
; Use the APPLOAD command to load this script
; Run the script using the now loaded RORPAKKE command.
; Remember to rename the files using norwegian characters.

(defun C:RORPAKKE ()
  ; Set a path to a folder
  (setq folderPath  "C:\\Users\\KnutHelland\\OneDrive - Railcomplete AS\\Documents\\ror\\")
  ; Set to Single Document Interface
  (setvar "SDI" 1)
  ; Turn off new lisps
  (setvar "LISPINIT" 0)


  (defun coordString (x y)
    (strcat (rtos x) "," (rtos y))
  )

  (setq maxN 7)

  (setq concreteLayer "3D_ConcreteCasing_XS_Concrete01")
  (setq plasticLayer "3D_PlasticTube_XS_Plastic01")

  (setq vertPadding 0.085)
  (setq horPadding 0.085)
  (setq pipeSize 0.14)
  (setq pipeholeR 0.055)
  (setq pipeR 0.05)

  (setq nx 1)
  (while (<= nx  maxN)
    (setq ny 1)
    (while (<= ny maxN)

      ; Open file
      (command "_.fileopen" (strcat folderPath "base.dwg"))

      (setvar "CLAYER" concreteLayer)
      (setq height (+ (* 2 vertPadding) (* ny pipeSize)))
      (setq halfWidth (/ (+ (* 2 horPadding) (* nx pipeSize)) 2))
      (COMMAND "PLINE" (coordString (- halfWidth) 0) (coordString halfWidth 0) (coordString halfWidth (- height)) (coordString (- halfWidth) (- height)) "C")
      
      (setq ix 1)
      (while (<= ix nx)
        (setq iy 1)
        (while (<= iy ny)
          (COMMAND "CIRCLE" 
            (coordString
              (+ (- halfWidth) horPadding (* (- ix 0.5) pipeSize)) 
              (- 0 vertPadding (* (- iy 0.5) pipeSize)) 
            )
            (rtos pipeholeR)
          )
          (setq iy (+ iy 1))
        )
        (setq ix (+ ix 1))
      )

      (setvar "CLAYER" plasticLayer)
      (setq ix 1)
      (while (<= ix nx)
        (setq iy 1)
        (while (<= iy ny)
          (COMMAND "CIRCLE" 
            (coordString
              (+ (- halfWidth) horPadding (* (- ix 0.5) pipeSize)) 
              (- 0 vertPadding (* (- iy 0.5) pipeSize)) 
            )
            (rtos pipeholeR)
          )
          (COMMAND "CIRCLE" 
            (coordString
              (+ (- halfWidth) horPadding (* (- ix 0.5) pipeSize)) 
              (- 0 vertPadding (* (- iy 0.5) pipeSize)) 
            )
            (rtos pipeR)
          )
          (setq iy (+ iy 1))    
        )
        
        (setq ix (+ ix 1))
      )
            
      ; Set current layer to 0
      (setvar "clayer" "0")
      
      ; Purge and audit 
      ;(command "-PU" "R" "*" "N" "-PU" "A" "*" "N" "AUDIT" "Y")
              
      ; Set up viewports to standard model setup
      (command "._VPORTS" "_Mode" "_Display" "_SIngle" "._NAVVCUBE" "_Off" "._NAVVCUBE" "_On" "._VPCONTROL" "_On")
      (command "._PERSPECTIVE" "0" "._VIEW" "_Orthographic" "_Top")

      ; Save and close
      (command "SAVEAS" "2018" (strcat folderPath "NO-BN-SWEEP-KU-KFO-OMSTOPT-RORPAKKE-(" (rtos height 2 3)  "x" (rtos (* 2 halfWidth) 2 3) ")-(" (rtos ny 2 0) "x" (rtos nx 2 0) "-110).dwg"))
      (command "CLOSE")
      
      (setq ny (+ ny 1))
    ); fory
    (setq nx (+ nx 1))
  ); forx

  ; Reset sysvars
  (setvar "SDI" 0)
  (setvar "LISPINIT" 1)
)
