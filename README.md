# RailCOMPLETE-NO-BN
RailCOMPLETE customization for Norway, Bane NOR

## INSTRUCTIONS FOR BUILDING AN 'XX-YY' ADMINISTRATION'S 2D SYMBOL LIBRARY DWG FILE

1. Go to Github repo 'RailCOMPLETE-ALL-2D-LIBRARIES' to build 2D dwg-file library, followthe README instructions there.
2. Copy the fresh 2D-library dwg-file from step 1) to yourthis repository's folder `...\Github\RailCOMPLETE-XX-YY\XX-YY\2D`, where `XX-YY` is one of FR-SR, DE-DB etc.
3. Launch batch file `...\Github\RailCOMPLETE-XX-YY\MakeDnaAndTransferFilesToAppdata.bat` to build and transfer a bundled DNA to your local test installation folder, `%appdata%\Roaming\Autodesk\ApplicationPlugins\RC.bundle`.
4. Start AuoCAD with RC. Use command `_RC-AGENT-WRITESYMBOLTHUMBNAILSTOFILE` to create a combined 2D and 3D thumbnails '.rc' file.
5. Push the fresh DNA, now with 2D library and thumbnails file, to Github and pull into MAIN. This trigger the Azure build process, see Azure pipeline for `XX-YY`.
6. Download the published `XX-YY` DNA installation file from the Azure cloud build pipeline site and distribute to users.

**Note** Always remember to check consistency between 2D symbol names in the XML of the DNA and in the 2D DWG file. ALso check consistency between all resource files and the corrsponding XML element in the DNA source-XML (in `XX-YY Rootfile.xml`).
