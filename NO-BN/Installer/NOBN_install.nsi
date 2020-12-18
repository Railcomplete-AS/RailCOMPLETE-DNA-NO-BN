;NSIS Modern User Interface
;Header Bitmap Example Script
;Written by Joost Verburg

;--------------------------------
;Include Modern UI
!include "MUI2.nsh"

!define PRODUCT "BaneNOR"
!define BUNDLEDIR "NO-BN"
!define MUI_ICON "rc.ico"
!define MUI_UNICON "rcun.ico"
!define /date DATE "%Y%m%d"

;--------------------------------
;General

  ;Name and file
  Name "${PRODUCT}"
  OutFile "${PRODUCT}.install-${DATE}.exe"

  ;Default installation folder
  InstallDir "$APPDATA\Autodesk\ApplicationPlugins\RC.bundle\Adm\${BUNDLEDIR}"
  
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
  
  ;;Cleanup 
  ;Delete "$INSTDIR\Adm\NO-BN\DNA\StyleDefinitions\StyleDefinitions.xml"
  ;RMDir /r "$INSTDIR\Adm\NO-BN\DNA\StyleDefinitions"
  ;
  ;;LuaToolTips
  ;Delete "$INSTDIR\Adm\NO-BN\NO-BN\LUA\LuaTooltipPages\TooltipViews\*.*"
  ;RMDir /r "$INSTDIR\Adm\NO-BN\LUA\LuaTooltipPages\TooltipViews"
  ;Delete "$INSTDIR\Adm\NO-BN\NO-BN\LUA\LuaTooltipPages\*.*"
  ;RMDir /r "$INSTDIR\Adm\NO-BN\LUA\LuaTooltipPages"
  ;
  ;;Remove all old dll's, and help resources
  ;Delete "$INSTDIR\Contents\*.*"
  ;;Delete "$INSTDIR\UserGuide\video\*.*"
  ;Delete "$INSTDIR\UserGuide\img\*.*"
  ;Delete "$INSTDIR\UserGuide\*.*"
  ;Delete "$INSTDIR\Help\ReleaseNotes\*.txt"
  ;
  ;CreateDirectory "$DOCUMENTS\RailCOMPLETE\LuaScripts"
  ;CreateDirectory "$DOCUMENTS\RailCOMPLETE\TableDefinitions"
  ;CreateDirectory "$DOCUMENTS\RailCOMPLETE\DNA"
  ;CreateDirectory "$DOCUMENTS\RailCOMPLETE\StyleDefinitions"
  
  ;Add files
  File /r "..\..\TMP\${BUNDLEDIR}\*"
  ;File "..\${BUNDLEDIR}\UserGuide\video\*.*"
  
  ;Store installation folder
  WriteRegStr HKCU "Software\${PRODUCT}" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${Product}" "DisplayName" "${Product}"
  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${Product}" "UninstallString" "$INSTDIR\uninstall.exe"
  WriteRegDWORD HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Example2" "NoModify" 1
  WriteRegDWORD HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Example2" "NoRepair" 1


SectionEnd

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_SecRC1 ${LANG_ENGLISH} "Administration tools for ${PRODUCT}."

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecRC1} $(DESC_SecRC1)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section
Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...

  Delete "$INSTDIR\Uninstall.exe"

  RMDir /r "$INSTDIR"

  DeleteRegKey HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${Product}"
  DeleteRegKey /ifempty HKCU "Software\${PRODUCT}"

SectionEnd
