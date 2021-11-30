;NSIS Modern User Interface
;Header Bitmap Example Script
;Written by Joost Verburg

;--------------------------------
; Last changed: 2021-05-14 THBEN/CLFEY
; Change: Rely on PRODUCT and BUNDLEDIR to be defined by calling YAML file
;--------------------------------
;
;Include Modern UI
!include "MUI2.nsh"

!define MUI_ICON "rc.ico"
!define MUI_UNICON "rcun.ico"
!define /date DATE "%Y%m%d"
!define PRODUCT "RailCOMPLETE-2019.0-3D Library" 
!define BUNDLEDIR "NO-BN"


;--------------------------------
;General

  ;Name and file
  Name "${PRODUCT}"
  OutFile "${PRODUCT}.install-${DATE}.exe"

  ;Default installation folder
  InstallDir "$APPDATA\Autodesk\ApplicationPlugins\RC.bundle\Adm\${BUNDLEDIR}\3D\STD"
  
  ;Get installation folder from registry if available
  ;InstallDirRegKey HKCU "Software\${PRODUCT}" ""

  ;Request application privileges for Windows Vista
  RequestExecutionLevel admin

;--------------------------------
;Interface Configuration

  !define MUI_HEADERIMAGE
  !define MUI_HEADERIMAGE_BITMAP "installerheader.bmp" ; optional
  !define MUI_ABORTWARNING

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_LICENSE "License.txt"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "${PRODUCT}" SecRC1 

  SetOutPath "$INSTDIR"
  
  ;Add files
  File /r "..\${BUNDLEDIR}\3D\STD\*"
  ;File "..\${BUNDLEDIR}\UserGuide\video\*.*"
  

SectionEnd

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_SecRC1 ${LANG_ENGLISH} "3D Library for 2019.o DNA"

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecRC1} $(DESC_SecRC1)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END
 
