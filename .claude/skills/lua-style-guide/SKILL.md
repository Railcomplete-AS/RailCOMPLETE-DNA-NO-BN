- [ ] # RailCOMPLETE Lua Coding and Commenting Style Guide

  > **Purpose:** This document defines the coding and commenting conventions for all Lua scripts and function libraries published by RailCOMPLETE AS (RCAS). It shall be used as a reference when writing new code or cleaning up existing code. The goal is that it should not be easy to tell who wrote a particular piece of code — all RCAS-published Lua files should look and feel consistent.
  >
  > **Audience:** Lua developers working on RailCOMPLETE scripts and libraries, and AI assistants (Claude) tasked with reviewing or refactoring such code.
  >
  > **Version:** 2026-04-09 v1.3

  ## 1. File Organization

  ### 1.1 Top-Level Sections in a Lua File

  Every Lua file shall be organized into clearly labeled sections, separated by **three blank lines** before each section. The section headers are written as block comments in uppercase, using the exact format shown below (three dashes, no space, UPPERCASED SECTION NAME, no space, three trailing dashes).

  For **script files** (files that are executed directly), the standard order is:

  ```
  --[[ file-level block comment ]]
  
  
  
  ---GLOBAL CONSTANTS---
  
  
  
  ---INCLUDES---
  
  
  
  ---LOCAL CONSTANTS---
  
  
  
  ---FUNCTIONS---
  
  
  
  ---SCRIPT---
  ```

  For **library files** (files loaded via `includeLuaFile()`), the standard order is:

  ```
  --[[ file-level block comment ]]
  
  
  
  ---INCLUDES---
  
  
  
  ---LOCAL CONSTANTS---
  
  
  
  ---FUNCTIONS---
  ```

  Library files use `---LOCAL CONSTANTS---` (not `---CONSTANTS---` or `---GLOBAL CONSTANTS---`) to make it explicit to the developer that libraries are not supposed to declare global constants. All constants in a library file shall be declared with the `local` keyword.

  ### 1.2 File-Level Block Comment

  Every Lua file shall begin with a block comment (`--[[ ... ]]`) containing:

  1. The short name of the file (matching the filename without extension).
  2. A row of equals signs (`=`) whose length matches the short name, for visual separation.
  3. A brief description of the file's purpose.
  4. Usage examples showing how to include and call the file's functions.
  5. A note on global constants that the file expects to be inherited from the calling sandbox (if applicable).
  6. A version history with entries in the format: `YYYY-MM-DD vX.Y AUTHOR Description.`

  Example for a library file:

  ```lua
  --[[
      lib2
      ====
      Generic Lua functions callable from scripts.
  
      The base folder for includeLuaFile() calls is: "...\RC.bundle\Adm\XX-YY", where 'XX-YY'
      is your railway administration's DNA abbreviation.
  
      Usage:
  
      -- Include library functions for use in a Lua script or in a Lua function - example:
      lib2 = includeLuaFile("Lua\\Functions\\lib2.lua")
      return lib2.writeln("Hello, world!", _ok) -- Print a message in the log window
  
      Note on global constants - these must be declared as a global constant before
      including the present Lua file:
  
      _HEADER_ holds the header text for popup windows. If nil, then 'Keyword' will
      be the popup window's header.
      _DEBUG_ leads, if true, to additional actions to help in debugging.
      _TRACE_ leads, if true, to more output to the log window.
  
      2026-01-30 v1.0 CLFEY Created.
      2026-02-19 v1.1 CLFEY Updated stop() function. Added language support.
      2026-04-08 v1.2 CLFEY Minor layout issues. Pass globals to child sandboxes.
  --]]
  ```

  Example for a script file:

  ```lua
  --[[
      Pas 1.a - Importer les voies depuis fichiers shapefile VOIE et REPERE_KILOMETRIQUE
      ==================================================================================
      FR-SR workflow script.
      Import tracks from shapefile using the DotSpatial.Data API.
      The script needs the REPERE_KILOMETRIQUE shapefile to orient the tracks correctly.
  
      2025-12-20 v1.0 CLFEY Created.
      2026-01-27 v1.1 CLFEY Added creation of Reference alignment from path.
      2026-04-08 v1.4 CLFEY Minor layout issues. Pass globals to child sandboxes.
  --]]
  ```

  ### 1.3 Trailing Blank Line

  Every Lua file shall end with exactly **one blank line** after the last line of code. This is required because the RailCOMPLETE embedded Avalon Lua editor does not handle the last line of a file well (long cursor and end-of-line behavior are unreliable).

  ## 2. Naming Conventions

  ### 2.1 File Naming and Placement

  1. **Library files** are named using the pattern `lib<N>.lua` for generic libraries (e.g., `lib1.lua`, `lib2.lua`) or `lib<N>_<CONTEXT>.lua` for administration-specific or domain-specific libraries (e.g., `lib2_VA.lua`).
  2. **Do not use hyphens in filenames** that will be used as Lua identifiers. For example, prefer `lib2_FRSR.lua` over `lib2_FR-SR.lua`, because the local variable holding the included library should mirror the filename (e.g., `local lib2_FRSR = includeLuaFile(...)`).
  3. **Script files** are named descriptively, often in the target audience's language. For French workflows, script names are in French (e.g., `Pas 1.a - Importer les voies depuis shapefile VOIE et REPERE_KILOMETRIQUE.lua`).

  **File placement** follows a strict rule based on the file's content:

  - **Function libraries** — Files that contain only function declarations (no directly executable statements) shall be placed in the `...\Adm\XX-YY\Lua\Functions` folder or in a subfolder thereof. This applies regardless of whether the functions are generic across multiple administrations or specific to the current one.
  - **Scripts** — Files that contain directly executable Lua statements (such as calling a function that performs an action) shall be placed in the `...\Adm\XX-YY\Lua\Scripts` folder or in a subfolder thereof.

  In these paths, `XX-YY` is the railway administration's DNA abbreviation (e.g., `FR-SR` for SNCF Réseau, `NO-BN` for Bane NOR).

  ### 2.2 Library Hierarchy and Scope

  Libraries follow a hierarchy of generality. The number in the library name (`lib1`, `lib2`) indicates which Lua contexts the library's functions can be called from:

  1. **`lib1.lua`** — Generic Lua utility functions, identical across many administrations, callable from **any Lua context**: from a Lua script, from another Lua function, or from a Lua expression in a property belonging to a RailCOMPLETE object contained in a DWG file endowed with a RailCOMPLETE DNA.[^1]
  2. **`lib1_<ADM>.lua` or `lib1_<DOMAIN>.lua`** — Administration-specific or domain-specific utility functions that are still callable from **any Lua context** (scripts, functions, and property expressions). These contain no scripting-only API calls. For example, a `lib1_FRSR.lua` might provide French-specific string formatting helpers that can be used in object property expressions.
  3. **`lib2.lua`** — Generic Lua utility functions, identical across many administrations, callable from **scripts only**. These rely on script-only APIs such as `write()`, `askForKeyword()`, `runCommand()`, etc., which are not available in the property-expression context.
  4. **`lib2_<ADM>.lua` or `lib2_<DOMAIN>.lua`** — Administration-specific or domain-specific functions that are still general-purpose within their scope, callable from **scripts only** (e.g., `lib2_VA.lua` for French track/superstructure "Voie et Abord" functions). If a Lua expression inside a RailCOMPLETE object property attempts to call a `lib2_...` function that invokes a scripting-only API, that call will fail.
  5. Domain-specific function files with descriptive names — Highly specialized functions for a particular workflow. These should be named in the target audience's language if appropriate.

  **Rule:** A library file at a given level must not contain text strings or logic specific to a narrower scope. For example, `lib1.lua` and `lib2.lua` must not contain administration-specific text. Administration-specific text belongs in `lib1_<ADM>.lua`, `lib2_<ADM>.lua`, or lower.

  [^1]: DNA: "Definition of Network Assets" — a set of object type declarations along with default values, relationships to other object types, sanity checks, etc.

  ### 2.3 Constants

  1. Constant identifiers are written in **UPPERCASE** with a **single underscore prefix**, a **single underscore suffix**, and **underscore infixes** between words:

     - Correct: `_STOP_KEYWORD_`, `_TRACE_`, `_VERSION_`
     - Wrong: `__stopKeyword__` (double underscores are conventionally reserved for the underlying Lua interpreter / C++ runtime).

  2. Lua does not have true constants. A "constant" is simply a variable that you do not intend to change. Treat identifiers following this naming convention as constants by policy.[^2]

  3. **Rationale for leading and trailing underscores:** The underscores serve as delimiters that make search-and-replace operations across multiple files safer. If you want to rename the constant `_OK_` to `_IS_OK_`, you search for `_OK_` and replace with `_IS_OK_`. Without the underscores, searching for a plain word like `OK` would inadvertently match occurrences inside strings, comments, and unrelated identifiers.

  4. Examples of well-formed constant names:

     ```lua
     _HEADER_ = "My Script"
     _VERSION_ = "2026-04-08 v1.4"
     _TRACE_ = false
     _DEBUG_ = false
     _YES_ = "Oui"
     _NO_ = "Non"
     _TERMINATE_ = "Terminer"
     _TERMINATED_ = "Terminé."
     _TRACK_VARIANT_ = "Traverses et rails - 3D simple"
     _USE_EXISTING_REF_ = "Choisir existant"
     _CREATE_NEW_REF_ = "Créer nouveau"
     _1_MM_ = 1e-3
     ```

  [^2]: Lua 5.4 introduced a `<const>` qualifier for local variables (e.g., `local x <const> = 42`). However, its use is discouraged in RailCOMPLETE Lua code because its behavior becomes confusing in the context of parent/child sandboxes created by `includeLuaFile()`. Stick to the naming convention (`_UPPER_CASE_`) as the indicator that an identifier is intended to be constant.

  ### 2.4 Functions

  1. Function names use **camelCase**: `getShapefilePaths()`, `importTracksFromShapefile()`.
  2. Functions intended to be private/internal to a file should be declared with `local function`.
  3. Functions intended to be exported from a library (callable as `lib2.functionName()`) are declared without `local`.

  ### 2.5 Variables

  1. Local variables use **camelCase**: `shapefileFolder`, `trackName`, `coordinatePoints`.
  2. Loop counters and throwaway variables may use short names: `i`, `j`, `k`, `x`, `v`, `_`.

  ## 3. Constants: Global vs. Local and Sandbox Inheritance

  ### 3.1 The `includeLuaFile()` Sandbox Model

  RailCOMPLETE's `includeLuaFile()` API creates a **child sandbox** for each included file. Global identifiers declared in the parent sandbox **before** the `includeLuaFile()` call are inherited by the child sandbox. Key rules:

  1. **Global constants in the parent are visible in the child.** If a script declares `_HEADER_ = "My Script"` as a global (no `local` keyword) before including a library, that library can read `_HEADER_`.
  2. **The child can override an inherited global** by assigning a new value to it. This override is local to the child sandbox and does not propagate back up to the parent.
  3. **If the child declares a `local` variable with the same name** as an inherited global, the local shadows the global within that child. Furthermore, if the child itself calls `includeLuaFile()` to create a grandchild sandbox, the grandchild will **not** inherit the child's local — it will either see the original global from the parent or nothing at all.
  4. **Order matters:** Global constants must be declared **before** the `includeLuaFile()` calls that need them.

  ### 3.2 Recommended Declaration Order in Script Files

  The following example shows the correct order for a script file. Constants that need to be inherited by included libraries are declared as **globals** (without `local`) in the `---GLOBAL CONSTANTS---` section. Constants used only within the script itself are declared as **locals** in the `---LOCAL CONSTANTS---` section:

  ```lua
  ---GLOBAL CONSTANTS---
  _HEADER_ = "Pas 1.a - Import tracks from VOIE and REPERE_KILOMETRIQUE shapefiles"
  _VERSION_ = "2026-04-08 v1.4"
  
  _DEBUG_ = false
  _TRACE_ = false
  _YES_ = "Oui"
  _NO_ = "Non"
  _OK_ = "OK"
  _HELP_ = "Aide"
  _TERMINATE_ = "Terminer"
  _TERMINATED_ = "Terminé"
  
  
  
  ---INCLUDES---
  local lib1 = includeLuaFile("Lua\\Functions\\lib1.lua")
  local lib2 = includeLuaFile("Lua\\Functions\\lib2.lua")
  local VA = includeLuaFile("Lua\\Functions\\lib2_VA.lua")
  
  
  
  ---LOCAL CONSTANTS---
  local _TRACKS_AND_KILOMETRATION_MARKERS_ = "Importer les VOIEs, puis utiliser les ..."
  local _TRACK_VARIANT_ = "Traverses et rails - 3D simple"
  
  local _USAGE_ = [[
  Pré-requis :
  - Données d'entrée exportées depuis Gaïa Data Étude...
  
  Version : ]].._VERSION_
  ```

  In this example, `_HEADER_`, `_VERSION_`, `_DEBUG_`, `_TRACE_`, `_YES_`, `_NO_`, `_OK_`, `_HELP_`, `_TERMINATE_`, and `_TERMINATED_` are all global. They are inherited by `lib1`, `lib2`, and `VA`. For instance, `lib2.show()` uses `_HEADER_` for its popup window header, and `lib2.trace()` checks `_TRACE_`. The included libraries may declare their own `local _VERSION_` without affecting the parent's `_VERSION_`.

  The `_USAGE_` constant and other script-specific constants are local because they are only used within the script itself.

  ### 3.3 Sandbox Inheritance: A Detailed Example

  ```lua
  -- File: Script1.lua (the outermost level)
  _HEADER_ = "Script1"              -- Global: inherited by all child sandboxes.
  _VERSION_ = "2026-04-08 v1.4"     -- Global: inherited by all child sandboxes.
  _TRACE_ = true                    -- Global: lib2.trace() will see this.
  local _USAGE_ = "Do this..."      -- Local: NOT inherited by any child sandbox.
  
  local lib1 = includeLuaFile("Lua\\Functions\\lib1.lua")
  -- Inside lib1.lua:
  --   _HEADER_ is visible and equals "Script1" (inherited).
  --   _VERSION_ is visible and equals "2026-04-08 v1.4" (inherited).
  --   _TRACE_ is visible and equals true (inherited).
  --   lib1.lua declares: local _VERSION_ = "2026-04-08 1.1"
  --     This shadows the inherited _VERSION_ inside lib1's sandbox.
  --     The parent's _VERSION_ is unaffected.
  --   _USAGE_ is NOT visible (it was declared local in the parent).
  ```

  ### 3.4 Constants in Library Files

  Library files shall declare all their own constants as `local`:

  ```lua
  ---LOCAL CONSTANTS---
  local _VERSION_ = "2026-04-08 1.2"
  local _1_MM_ = 1e-3
  ```

  This ensures that each library's internal metadata is self-contained and does not collide with the calling script's constants.

  ## 4. Comments

  ### 4.1 General Comment Formatting

  1. **Always exactly one space** after the double dash:

     - Correct: `-- This is a comment`
     - Wrong: `--This is a comment`
     - **Exception:** Section headers use three dashes with no spaces (see Section 1.1): `---GLOBAL CONSTANTS---`

  2. **Capitalize the first letter** of every comment:

     - Correct: `-- Force save and update data`
     - Wrong: `-- force save and update data`

  3. **No trailing period** unless the comment contains two or more sentences:

     - Single sentence: `-- Force save and update data`
     - Two sentences: `-- Force save and update data. This triggers the DNA formula recalculation.`

  4. **Inline comments** (to the right of a code line) are separated from the code by at least one space. If consecutive lines all have inline comments, align the comments using tab stops so they start at the same column position for readability:

     ```lua
     runCommand("_PICKADD 0 ")    -- Disables PICKADD
     runCommand("_FILEDIA 1 ")    -- Open files with a normal explorer window
     runCommand("_ORTHOMODE 0 ")  -- The cursor is not snapped to grid lines
     runCommand("_GRID _OFF ")    -- Do not display grid lines
     runCommand("_SNAP _OFF ")    -- Do not snap to grid
     ```

  ### 4.2 Preceding-Line Comments

  When a comment is placed on its own line immediately above the code it describes, it shall end with a punctuation mark signaling what follows:

  - End with a **colon** `:` when the following code **performs an action**:

    ```lua
    -- Force save and update data:
    track.name = track.name
    ```

    ```lua
    -- Enable all OSNAPs:
    runCommand("_-OSNAP _END,_MID,_CEN,_GCE,_NOD,_QUA,_INT,_EXT,_INS,_PER,_TAN,_NEA,_APP,_PAR ")
    ```

  - End with a **question mark** `?` when the following code **tests a condition**:

    ```lua
    -- Are there any existing reference alignments?
    refAlignments = DocumentData.ObjectCollection:filter(
        function (x) return x.RcType == rctype_ReferenceAlignment and x:isVisible() end)
    ```

    ```lua
    -- Is this a derailer alignment that should be skipped?
    if shape.NumParts == 1 and shape.NumPoints == 2
        and math.abs(geometriesTable[recordNumber].Length - 2.0) < 0.1 then
    ```

  ### 4.3 Translated Prompt Comments

  When a line of code contains a user-facing string in a foreign language (e.g., French) and you want to provide the English meaning as a comment, wrap the English translation in **double quotes** and end with a **colon**. Only add a translation comment when the foreign-language text would not be understood by an English-speaking developer; skip it when the text is self-evident or nearly identical to English:

  ```lua
  -- "Could not find path to ... Please select new own alignment for ...":
  write("Impossible de trouver un parcours vers "..rkObject.Var1..
      ". Veuillez sélectionner un nouvel axe propre pour "..rkObject.code.."\n", _warning)
  -- "Select the folder containing the shapefiles":
  shapefileFolder = askForFolderName("Sélectionnez le dossier contenant les fichiers shapefile")
  -- "Track created:":
  lib2.writeln("Création de voie : "..trackName)
  -- "Added marker for PK ... for reference alignment ...":
  lib2.writeln("Marqueur ajouté pour PK."..marker.Name..
      " pour l'axe de référence "..referenceAlignmentName, _ok)
  ```

  ### 4.4 Dead Code and Alternative Code

  1. **Remove dead code** whenever possible. Do not leave commented-out code in production files unless there is a clear reason to keep it (e.g., it may become relevant again soon).

  2. If dead code must be retained, use a **long comment** (`--[[ ... ]]`):

     - **Single line:** Place `--[[` and `--]]` on the same line, wrapping both the dead code and any inline comment it may have:

       ```lua
       --[[ local marker = createPointObject(possibleAlignment, "JBTFE_MRK Markør", "Knappenål", coordinate) --]]
       ```

       ```lua
       --[[ a = b -- Assign b to a --]]
       ```

     - **Multiple lines:** Place `--[[` and `--]]` on their own lines, flush with the **left margin** (column 1), immediately before and after the dead code block:

       ```lua
       
       ```

  --[[ local upObjectsAndPaths = table.select( getUpObjectsWithPaths(startAlignmentPos, rctype_Marker)) :where(function (x) return x.Object.id == rkObject.id end) local paths = upObjectsAndPaths:select(function (x) return x.Path end) --]] ```

  ## 5. Function Documentation and Tooltips

  ### 5.1 The RailCOMPLETE Tooltip System

  RailCOMPLETE's embedded Lua editor provides an **auto-complete / tooltip** feature. When the user types a function name, the editor offers to complete it and displays a tooltip. The tooltip is composed of two parts:

  1. The **single Lua comment line immediately preceding the `function` declaration** — this provides the description.
  2. The **line of code containing the `function` keyword** — this shows the function signature.

  Both lines are displayed together in the tooltip. Only the comment line directly above the declaration is picked up; if multiple comment lines precede the function, only the last one becomes the tooltip text.

  **To keep tooltips clean and readable**, the line containing the `function` keyword should contain only the function keyword (optionally preceded by `local`), the function name, the parenthesized argument list, and nothing else. The function body should begin on the next line:

  ```lua
  -- Writes a message to the log window and appends a newline. Call as lib2.writeln(msg, symbol = nil).
  function writeln(msg, symbol)
      if msg then
          write(msg.."\n", symbol or _noSymbol)
      else
          write("\n", symbol or _noSymbol)
      end
  end
  ```

  In rare cases, for very short functions, it is acceptable to write the entire function on one line. This can actually eliminate the need for a lengthy tooltip comment, since the user sees "the real thing" directly. Use this sparingly — many users would be confused by seeing a function's internals in the tooltip:

  ```lua
  -- Point-to-string conversion. Call as lib1.p2s(p) where p is a 3D point.
  function p2s(p) return string.format("(%.03f,  %.03f,  %.03f)", p.X, p.Y, p.Z) end
  ```

  ### 5.2 Tooltip Format for Generic Libraries (lib1, lib2)

  Generic library files (`lib1.lua`, `lib2.lua`) are maintained as identical code in a master (English) repository and then distributed to each target-language repository. In the **master repository**, the tooltip comment shall be written in **English only**:

  ```lua
  -- Returns a string containing the input number 'x' rounded to three decimal places. Call as lib1.round(math.pi) which returns '3.142'.
  function round(x)
      return string.format("%.03f", x)
  end
  -- Returns a table with partial strings extracted from the input string, split at the given split character. Call as lib1.splitString("The quick brown fox", " ") which returns {"The", "quick", "brown", "fox"}.
  function splitString(s, splitChar)
  ```

  The tooltip should include:

  1. A brief description of what the function does.
  2. How to call the function, including the library prefix and example arguments.
  3. An example return value if it aids understanding.

  ### 5.3 Language-Specific Repositories and Tooltip Translation

  Our GitHub repositories for Lua code are separated into **one repository per target language** (i.e., per target audience / railway administration). The generic library files (`lib1.lua`, `lib2.lua`) are meant to contain **identical Lua function declarations** across all language repositories. The only permitted difference between repositories is the tooltip comment line immediately preceding each function declaration.

  In each target-language repository, the general principle of **dual comment lines** applies: an English comment (for the developer) followed by a target-language comment (for the end user, which becomes the tooltip). The target-language line must be the one directly above the `function` declaration so that it is picked up by the Lua editor.

  **Example — English (master) repository (`lib1.lua`):**

  ```lua
  -- Returns a table with partial strings. Call as lib1.splitString("The quick brown fox", " ") which returns {"The", "quick", "brown", "fox"}.
  function splitString(s, splitChar)
  ```

  **Example — French repository (`lib1.lua`):**

  ```lua
  -- Returns a table with partial strings. Call as lib1.splitString("The quick brown fox", " ") which returns {"The", "quick", "brown", "fox"}.
  -- Renvoie un tableau de sous-chaînes extraites de la chaîne d'entrée, découpée au caractère de séparation donné. Appel : lib1.splitString("The quick brown fox", " ") qui renvoie {"The", "quick", "brown", "fox"}.
  function splitString(s, splitChar)
  ```

  **Example — German repository (`lib1.lua`):**

  ```lua
  -- Returns a table with partial strings. Call as lib1.splitString("The quick brown fox", " ") which returns {"The", "quick", "brown", "fox"}.
  -- Gibt eine Tabelle mit Teilzeichenfolgen zurück. Aufruf: lib1.splitString("The quick brown fox", " ") gibt {"The", "quick", "brown", "fox"} zurück.
  function splitString(s, splitChar)
  ```

  **Example — Norwegian repository (`lib1.lua`):**

  ```lua
  -- Returns a table with partial strings. Call as lib1.splitString("The quick brown fox", " ") which returns {"The", "quick", "brown", "fox"}.
  -- Returnerer en tabell med delstrenger. Kall som lib1.splitString("The quick brown fox", " ") som returnerer {"The", "quick", "brown", "fox"}.
  function splitString(s, splitChar)
  ```

  When verifying cross-repository consistency, confirm that all language-dependent Lua function repositories contain the **exact same Lua function declarations** (same function name, same arguments, same function body). Only the tooltip comment lines may differ.

  ### 5.4 Tooltip Format for Administration- and Domain-Specific Libraries

  Administration-specific and domain-specific libraries (e.g., `lib2_VA.lua` for FR-SR) exist in only **one** language repository and are not shared across administrations. For these files, the tooltip shall be written in **two lines**: first English (for the developer), then the target language (for the end user). The target-language line must be **directly above** the declaration so it is picked up as the tooltip:

  ```lua
  -- Import tracks from VOIE and REPERE_KILOMETRIQUE shapefiles. Call as: importTracksFromShapefile(shapefileTable = nil).
  -- Importer les voies depuis des shapefiles VOIE et REPERE_KILOMETRIQUE. Appel : importTracksFromShapefile(shapefileTable = nil).
  function importTracksFromShapefile(shapefileTable, arg1)
  -- Returns a table with paths to shapefiles in a selected folder. Call as: getShapefilePaths(shapefileFolder = nil).
  -- Renvoie un tableau contenant les chemins d'accès aux fichiers shapefile d'un dossier sélectionné. Appel : getShapefilePaths(shapefileFolder = nil).
  function getShapefilePaths(shapefileFolder)
  ```

  **Rationale:** Code developers may not be fluent in the target language. The English line ensures developers understand the function. The target-language line ensures end users see a tooltip in their own language.

  For a German administration, the pattern would be English followed by German:

  ```lua
  -- This function does X. Call as: doX(arg1, arg2).
  -- Diese Funktion macht X. Aufruf: doX(arg1, arg2).
  function doX(arg1, arg2)
  ```

  ### 5.5 Functions Not Intended for End Users

  Helper functions that are local to a file and not part of the public API do not require bilingual tooltips. A single English comment is sufficient:

  ```lua
  -- Check whether the given alignment info indicates the point lies on the alignment:
  local function isOnAlignment(alignmentInfo)
  -- Recursively find all .shp files in the given folder and its subfolders:
  local function getShapefilesInFolder(folder, shapefileTable)
  -- Collect reference alignment names from existing track objects' Var0 property:
  local function getReferenceAlignmentNamesFromExistingAlignments()
  ```

  ## 6. Language Constants and Multilingual Text

  ### 6.1 When to Use Constants for UI Strings

  User-facing strings (prompts, menu options, messages) that appear in code can be handled in two ways:

  1. **Inline strings** — The foreign-language string is written directly in the code, with an English translation comment above it.
  2. **Named constants** — The string is assigned to a constant with an English-language identifier.

  **Recommended approach:**

  - **Option lists and menu keywords** should use named constants. This makes the code readable for non-native developers and allows reuse. Constants that are shared across scripts and their included libraries should be declared as **globals** in the script's `---GLOBAL CONSTANTS---` section so they are inherited by child sandboxes:

    ```lua
    ---GLOBAL CONSTANTS---
    _YES_ = "Oui"
    _NO_ = "Non"
    _HELP_ = "Aide"
    _TERMINATE_ = "Terminer"
    _TERMINATED_ = "Terminé."
    
    -- Later, in code:
    -- "Select your action:":
    option = askForKeyword("Sélectionnez votre action :",
        {_INSERT_CONNECTION_OBJECTS_, _HELP_, _TERMINATE_}, _HEADER_)
    if option == _TERMINATE_ or option == nil then
    ```

  - **One-off prompt strings and short messages** may be written inline with an English comment:

    ```lua
    -- "Select the folder containing the shapefiles":
    shapefileFolder = askForFolderName("Sélectionnez le dossier contenant les fichiers shapefile")
    ```

  - **If a constant is not generic** (not reused across files), declare it close to where it is used, typically in the `---LOCAL CONSTANTS---` section or immediately before the relevant code block.

  ### 6.2 Shared Language Constants Across Scripts

  When multiple scripts for the same administration share the same set of language constants (e.g., `_YES_`, `_NO_`, `_HELP_`, `_TERMINATE_`), there is some unavoidable repetition because each script must declare them as globals in its own `---GLOBAL CONSTANTS---` section for sandbox inheritance to work.

  An alternative approach would be to collect shared constants in a dedicated file such as `lib0_FRSR.lua`, but this has a significant drawback: since `includeLuaFile()` creates a child sandbox, constants declared inside the included file (whether global or local) do not propagate back to the parent. You would therefore need to reference them with a prefix (e.g., `fr._YES_`), which makes the code less readable:

  ```lua
  -- Less readable — constants require a prefix:
  local fr = includeLuaFile("Lua\\Functions\\lib0_FRSR.lua")
  option = askForKeyword("Continue?", {fr._YES_, fr._NO_, fr._HELP_, fr._TERMINATE_})
  ```

  **Recommended approach:** Accept the repetition and declare shared language constants as globals directly in each script's `---GLOBAL CONSTANTS---` section. This keeps the code simple, readable, and compatible with sandbox inheritance:

  ```lua
  -- More readable — constants are used directly:
  _YES_ = "Oui"
  _NO_ = "Non"
  _HELP_ = "Aide"
  _TERMINATE_ = "Terminer"
  -- ...
  option = askForKeyword("Continue?", {_YES_, _NO_, _HELP_, _TERMINATE_})
  ```

  When asking Claude to create or review a new script for an administration, include the standard set of global language constants as part of the template.

  ### 6.3 Multilingual Strategy for Generic Libraries

  Rather than maintaining a single Lua file that handles multiple languages at runtime (e.g., using a `language()` helper function), the preferred approach is:

  1. Maintain a **master English-language version** of each generic library (`lib1.lua`, `lib2.lua`) in a shared repository.
  2. For each target-language repository, create a **translated copy** where only the tooltip comments and any user-facing strings are translated.
  3. Use Claude or another translation tool to produce and verify the translated copies, ensuring that all function declarations and function bodies remain identical across repositories.

  This approach avoids runtime language-switching complexity, keeps each file self-contained, and makes it straightforward to verify that the code logic is identical across all repositories.

  ## 7. Formatting Rules

  ### 7.1 Indentation

  Use **tabs** for indentation. Each nested block adds one tab level. The tab width setting in your text editor shall be **4 spaces**. This matches the setting in RailCOMPLETE's embedded Lua editor.

  ### 7.2 Spaces Before and After Commas

  In code contexts (not inside string literals), always place **no space before a comma** and **exactly one space after a comma**:

  ```lua
  local x, y, z = getCoordinates()
  table.insert(results, value)
  ```

  If the comma is the last character on a line, the newline replaces the trailing space. The chosen indentation for the first continuation line applies to all subsequent continuation lines:

  ```lua
  local result = createExternalLibraryObject(
      "RailCOMPLETE.RailMLModel.ReferenceAlignmentSegment",
      {},
      {AlignmentRef = ref, Pos = posStart})
  a = doX("The quick brown fox ",
      "jumped over ",
      "the lazy ",
      "dog")
  ```

  Note: Commas inside string literals are part of the string content and do not follow these spacing rules. To make the distinction visually clear in code examples, consider using wider spacing in formatted strings:

  ```lua
  return string.format("(%.03f,   %.03f,   %.03f)", p.X, p.Y, p.Z)
  ```

  ### 7.3 Spaces Around Operators

  Use spaces around binary operators (`=`, `==`, `~=`, `<`, `>`, `<=`, `>=`, `+`, `-`, `*`, `/`, `and`, `or`):

  ```lua
  local distance = math.sqrt((x2 - x1)^2 + (y2 - y1)^2)
  if option == _TERMINATE_ or option == nil then
  alignment.code = "="
  ```

  Exceptions:

  - The **exponentiation operator** `^` and **unary minus** may omit spaces when the expression is clearer that way: `(p1.X - p2.X)^2`
  - The **string concatenation operator** `..` — see Section 10.2 for the specific rules.

  Double spaces (or more) are not permitted in Lua expressions; only single spaces are used. Multiple spaces are permitted only inside string literals.

  ### 7.4 Comparison Order

  When testing an unknown value against a known value, place the identifier holding the **unknown value on the left** and the **known value on the right**:

  ```lua
  -- Correct:
  if option == _TERMINATE_ then
  if attributes.TYPE == "BIF" then
  if _DNA_COUNTRY_ == "NO" then
  
  -- Wrong:
  if _TERMINATE_ == option then
  if "BIF" == attributes.TYPE then
  ```

  ### 7.5 Line Length and Line Breaking

  There is no strict maximum line length. However, keep in mind that the RailCOMPLETE embedded Avalon Lua editor window is often fairly narrow (because the user also needs to see other things in the CAD system's modelspace), and shows less per line than a full-screen IDE such as VS Code or Notepad++.

  **Guideline:** If a line exceeds roughly 120 characters, consider breaking it for readability. When breaking a line, indent the continuation at least one tab stop beyond the starting line, unless both parts are segments of the same concatenated string:

  ```lua
  local applicableAlignments = table.select(allTracks,
      function (x) return {
          track = x,
          alignmentInfo = getAlignmentInfo(x.id, ndvCoordinatePoint)
      } end)
      :where(function(x) return isOnAlignment(x["alignmentInfo"]) end)
  -- String continuation — both lines are parts of the same string, so same indentation:
  lib2.show("L'importation de voies sera faite sans introduire les repères "..
  "kilométriques correspondants. Pour compléter le plan de tracé, "..
  "vous devez suivre les pas suivants :")
  ```

  ### 7.6 Blank Lines for Readability

  1. **No blank line** between code lines that belong closely together.
  2. **One blank line** as "air" for readability — for example, before each `elseif` when the branches contain roughly 10 or more lines of code. Think of this as a paragraph break within a single context.
  3. **Three blank lines** before each top-level section header within a file (`---GLOBAL CONSTANTS---`, `---INCLUDES---`, `---LOCAL CONSTANTS---`, `---FUNCTIONS---`, `---SCRIPT---`).
  4. **Three blank lines** before each top-level function definition, or before the comment block that introduces it. **Exception:** The first function immediately after a `---FUNCTIONS---` section header needs no additional blank lines beyond the three that precede the section header itself.
  5. **One blank line** as the very last line of the file (see Section 1.3).

  Example showing three blank lines between sections and before functions:

  ```lua
  ---LOCAL CONSTANTS---
  local _VERSION_ = "2026-04-08 1.1"
  local _DEBUG_ = false
  local _TRACE_ = false
  
  
  
  ---FUNCTIONS---
  -- Point-to-string conversion. Call as lib1.p2s(p) where p is a 3D point.
  function p2s(p)
      return string.format("(%.03f,   %.03f,   %.03f)", p.X, p.Y, p.Z)
  end
  
  
  
  -- Returns a string containing the input number 'x' rounded to three decimal places. Call as lib1.round(math.pi) which returns '3.142'.
  function round(x)
      return string.format("%.03f", x)
  end
  ```

  ## 8. Duplicate Code

  ### 8.1 No Duplicate Function Definitions

  The same function must not be defined in two different files with different implementations. If two libraries need similar functionality with slight differences, factor out the common parts into a shared function and specialize only where necessary.

  ### 8.2 Refactoring Duplicates

  When you encounter duplicate code during a review:

  1. Identify which version is more correct or more general.
  2. Keep that version in the appropriate library.
  3. Have the other file call the shared version, or remove the duplicate entirely.
  4. Verify that all callers still work after the change.

  ## 9. Error Handling

  ### 9.1 General Guidance

  Some functions are called for their return value, while others are called purely for their side effects (printing to the log window, modifying a DWG file, creating objects, etc.). There is no blanket requirement that every function must return a specific success/failure value.

  However, functions that **can fail** in expected ways (e.g., user cancellation, missing data, file not found) should communicate this to the caller. The recommended pattern is to **return `nil`** on failure so the caller can check:

  ```lua
  local paths = getShapefilePaths(folder)
  if not paths then return end
  ```

  ### 9.2 Error Dialogs

  - Use **`lib2.show(msg, _error)`** followed by `return` for recoverable errors where the user should be informed but the script can continue or exit gracefully:

    ```lua
    if not shapefileNdv then
        -- "Could not find NDV shapefile.":
        lib2.show("Impossible de trouver le shapefile 'NDV'.", _error)
        return
    end
    ```

  - Use **`lib2.stop(msg)`** for unrecoverable errors that should halt the script with a user-visible error popup. This is appropriate for programming errors or impossible states:

    ```lua
    lib2.stop("Bad arguments to importTracksFromShapefile(): ["
        ..tostring(arg1)..", "..tostring(arg2).."].")
    ```

  ### 9.3 Guard Clauses

  Every function that receives optional arguments or depends on external data should validate inputs early:

  ```lua
  function importTracksFromShapefile(shapefileTable, arg1)
      -- Get shapefiles if not passed:
      if not shapefileTable then
          local shapefileFolder = askForFolderName("Sélectionnez le dossier...")
          if not shapefileFolder then return end
          local shapefilePaths = getShapefilePaths(shapefileFolder)
          shapefileTable = getShapefileTable(shapefilePaths)
      end
      -- ...
  end
  ```

  ## 10. Miscellaneous Rules

  ### 10.1 Semicolons

  Do not use semicolons to terminate statements. Lua does not require them and they add visual noise.

  ### 10.2 String Concatenation

  It is recommended to have **no spaces** surrounding the `..` concatenation operator. This keeps concatenated text items visually coupled, making it easier to read the resulting string as a whole:

  ```lua
  local msg = "Track created: "..trackName
  lib2.writeln("Associe la voie "..track.code.." à l'axe de référence "..refAlignment.code)
  ```

  When a concatenated expression must be broken across multiple lines, break **after** the `..` operator. Continuation lines for string segments may be aligned at the same indentation:

  ```lua
  lib2.show(
      tostring(nSwitches).." aiguillages créés : "..(switchNames or "-").."\n\n"..
      tostring(nCrossings).." traversées obliques créées : "..(crossingNames or "-").."\n\n"..
      tostring(nContinuations).." raccordements d'axe créées : "..(continuationNames or "-"))
  ```

  ### 10.3 `goto` and Labels

  The `goto` statement with `::continue::` labels is acceptable for skipping to the next iteration of a loop when the alternative would be deeply nested `if` statements. Place the `::continue::` label at the end of the loop body, at the same indentation level as the loop contents:

  ```lua
  for _, shape in pairs(shapeVoieTable) do
      local recordNumber = shape.RecordNumber
      local alignmentName = shapefileVoie.Attributes.Table.Rows[recordNumber - 1].ItemArray[2]
  
      -- Is this a derailer alignment that should be skipped?
      if shape.NumParts == 1 and shape.NumPoints == 2
          and math.abs(geometriesTable[recordNumber].Length - 2.0) < 0.1 then
          lib2.show("Dérailleur ignoré : "..alignmentName, _warning)
          goto continue
      end
  
      -- ... process shape ...
  
      ::continue::
  end
  ```

  ### 10.4 Magic Numbers

  Avoid unexplained numeric literals in code. Use named constants or add an inline comment explaining the value. Named constants are preferred because they can be reused and make the code self-documenting:

  ```lua
  ---LOCAL CONSTANTS---
  local _1_MM_ = 1e-3
  -- Using a named constant for the tolerance:
  if RC__getDistance2D(pointA, pointB) < _1_MM_ then
  
  -- Acceptable alternative with an inline comment:
  if RC__getDistance2D(previousCoordinate, c) > 1e-6 then -- 1 µm tolerance
  ```

  ### 10.5 Boolean Flags: `_DEBUG_` and `_TRACE_`

  - `_DEBUG_` — When `true`, enables extra code that creates auxiliary objects or produces additional output useful during development. Debug code should be wrapped in `if _DEBUG_ then ... end`:

    ```lua
    if _DEBUG_ then
        -- Create an auxiliary line object to visualize the milepost markers:
        local referenceMileageHelpLine = createAlignmentObject(
            rctype_AuxiliaryLine, "Ligne auxiliaire",
            refAlignmentHorizontalGeometries[referenceAlignmentName])
        referenceMileageHelpLine.code = "="
        referenceMileageHelpLine.code =
            "Axe de référence intermédiaire pour orienter les voies individuelles"
    end
    ```

  - `_TRACE_` — When `true`, enables verbose logging via `lib2.trace()`. Trace output is written to the log window but does not create popups or auxiliary objects:

    ```lua
    lib2.trace(imu)
    ```

  Both flags default to `false` in production code.

  ### 10.6 The `beginUndoBufferItem()` / `endUndoBufferItem()` Pattern

  When a script or function creates or modifies multiple CAD objects that should be undoable as a single operation, wrap the relevant block in `beginUndoBufferItem()` and `endUndoBufferItem()`:

  ```lua
  beginUndoBufferItem()
  
  for _, track in pairs(tracks) do
      -- ... create or modify objects ...
  end
  
  endUndoBufferItem()
  ```

  Ensure that every `beginUndoBufferItem()` has a matching `endUndoBufferItem()`, even when the function returns early due to errors. Consider placing the `endUndoBufferItem()` call before any early `return` statements that follow the `beginUndoBufferItem()`.

  **Important:** Any call to the AutoCAD console using `runCommand()` will break the undo grouping established by the surrounding `beginUndoBufferItem()` / `endUndoBufferItem()` pair. Do not place `runCommand()` calls between these two statements. For example, the following would break undo grouping:

  ```lua
  -- WRONG — runCommand() breaks the undo grouping:
  beginUndoBufferItem()
  runCommand("_GRIPS 0 ")  -- This breaks the undo buffer!
  -- ... create objects ...
  endUndoBufferItem()
  ```

  ### 10.7 Formula Reset Pattern

  RailCOMPLETE objects may have properties governed by DNA formulas. To override a formula-driven property with a literal value, first assign `"="` to clear the formula, then assign the desired value:

  ```lua
  alignment.code = "="        -- Remove possible existing formula from DNA
  alignment.code = trackName   -- Set the literal value
  ```

  Similarly, to trigger a recalculation/save without changing the value:

  ```lua
  track.name = track.name -- Force save and update data
  ```

  ## 11. Summary Checklist

  Before submitting or publishing a Lua file, verify:

  - [ ] File begins with a block comment containing name, description, usage, and version history.
  - [ ] Sections are in the correct order and preceded by three blank lines.
  - [ ] Section header for constants in library files reads `---LOCAL CONSTANTS---`.
  - [ ] Global constants are declared before `includeLuaFile()` calls in script files.
  - [ ] All constants use `_UPPER_CASE_` naming with single underscores.
  - [ ] Every exported function has a tooltip comment on the line immediately above it.
  - [ ] Generic library tooltips are in English only (translated per target-language repository).
  - [ ] Administration- / domain-specific function tooltips have two lines: English then target language.
  - [ ] All comments (except section headers) have exactly one space after `--`, start with a capital letter, and follow the colon/period rules.
  - [ ] Preceding-line comments end with a colon (action follows) or a question mark (test follows).
  - [ ] English translation comments for foreign-language prompts are in double quotes and end with a colon.
  - [ ] Dead code is either removed or enclosed in `--[[ ... ]]` long comments.
  - [ ] No duplicate function definitions across files.
  - [ ] No space before, one space after every comma in code (does not apply to strings).
  - [ ] Unknown value's identifier on the left, known value on the right in comparisons.
  - [ ] No spaces around the `..` string concatenation operator.
  - [ ] Function library files reside in `Lua\Functions` (or a subfolder thereof).
  - [ ] Script files reside in `Lua\Scripts` (or a subfolder thereof).
  - [ ] File ends with exactly one blank line.
  - [ ] `_DEBUG_` and `_TRACE_` are set to `false` in production code.
  - [ ] Every `beginUndoBufferItem()` has a matching `endUndoBufferItem()`.
  - [ ] No `runCommand()` call between `beginUndoBufferItem()` and its matching `endUndoBufferItem()`.
  - [ ] Tab width is set to 4 spaces.
