# RailCOMPLETE-DNA-NO-BN
RailCOMPLETE customization for Norway, Bane NOR

## INSTRUCTIONS FOR BUILDING AN 'XX-YY' ADMINISTRATION'S 2D SYMBOL LIBRARY DWG FILE

1. Let 'XX' be the relevant ISO 3166-2 two-letter country code. Let 'YY' be a two-letter abbreviation of the relevant railway administration in country XX.
2. Go to subfolder '...XX-YY\Adm\2D\\_SRC' to edit the 2D dwg-file's LISP source code.
3. Start AutoCAD, use command VLIDE (or read about using the Visual Studio debugger). Load your personal bootstrap file from the '...\2D\\_BOOTSTRAPS' folder.
4. If needed, adapt line number 27 in your personal bootstrap .LSP file, this will set the root folder for LISP on your computer.
5. Read the Help info from the (HLP) command provided by the bootstrap LISP code.
6. Run the (MkLib) command to build the 2D library file, named after date and time, such as in 'XX-YY-20221009_093223-2D'.
7. Rename the produced 2D library file into a name of the form 'XX-YY-2021.a-2D.dwg', compatible with the information contained in the batch file 'DefineDnaVersion.bat'.
8. Launch batch file `...\Github\RailCOMPLETE-XX-YY\MakeDnaAndTransferFilesToAppdata.bat` to build and transfer a bundled DNA to your local test installation folder, `%appdata%\Roaming\Autodesk\ApplicationPlugins\RC.bundle`.
9. Start AuoCAD with RC. Use command `_RC-AGENT-WRITESYMBOLTHUMBNAILSTOFILE` to create a combined 2D and 3D thumbnails '.rc' file.
10. Push the fresh DNA, now with 2D library and thumbnails file, to Github and pull into MAIN. This trigger the Azure build process, see Azure pipeline for `XX-YY`.
11. Download the published `XX-YY` DNA installation file from the Azure cloud build pipeline site and distribute to users.

**Note** Always remember to check consistency between 2D symbol names in the XML of the DNA and in the 2D DWG file. Also check consistency between all resource files and the corresponding XML element in the DNA source-XML (in `XX-YY Rootfile.xml`). The name of the 2D symbol file shall be stated as a resource in the corresponding DNA's XML root file.
