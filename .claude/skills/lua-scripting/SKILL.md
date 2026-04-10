---
name: lua-scripting
description: Guide for writing standalone Lua scripts in DNA repositories -- coding style, commenting conventions, technical patterns, API usage, and common pitfalls
---

# RailCOMPLETE Lua Scripting Guide

> **Purpose:** This document defines the coding style, commenting conventions, and technical patterns for all Lua scripts and function libraries published by RailCOMPLETE AS (RCAS). It serves as the single reference when writing new code, reviewing existing code, or generating code with AI assistants.
>
> **Goal:** All RCAS-published Lua files should look and feel consistent — it should not be easy to tell who wrote a particular piece of code.

$ARGUMENTS

For the full API reference, see `.claude/references/080-luacommands.html`. For RC commands, see `.claude/references/050-commands.html`. For debugger usage, see `.claude/references/080-luadebugger.html`.

---

# PART A — CODING STYLE

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

Omit any section that has no content. For example, a script with no function definitions omits `---FUNCTIONS---`.

**Note:** Some existing files in the codebase use two-dash headers (`--CONSTANTS--`, `--FUNCTIONS--`). New and updated code shall use the three-dash format (`---CONSTANTS---`, `---FUNCTIONS---`) defined above.

### 1.2 File-Level Block Comment

Every Lua file shall begin with a block comment (`--[[ ... ]]`) containing:

1. The short name of the file (matching the filename without extension).
2. A row of equals signs (`=`) whose length matches the short name, for visual separation.
3. A brief description of the file's purpose.
4. Usage examples showing how to include and call the file's functions (for library files; script files may omit this or replace it with a workflow summary).
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

In these paths, `XX-YY` is the railway administration's DNA abbreviation (e.g., `FR-SR` for SNCF Reseau, `NO-BN` for Bane NOR).

### 2.2 Library Hierarchy and Scope

Libraries follow a hierarchy of generality. The number in the library name (`lib1`, `lib2`) indicates which Lua contexts the library's functions can be called from:

1. **`lib1.lua`** — Generic Lua utility functions, identical across many administrations, callable from **any Lua context**: from a Lua script, from another Lua function, or from a Lua expression in a property belonging to a RailCOMPLETE object contained in a DWG file endowed with a RailCOMPLETE DNA.[^1]
2. **`lib1_<ADM>.lua` or `lib1_<DOMAIN>.lua`** — Administration-specific or domain-specific utility functions that are still callable from **any Lua context** (scripts, functions, and property expressions). These contain no scripting-only API calls.
3. **`lib2.lua`** — Generic Lua utility functions, identical across many administrations, callable from **scripts only**. These rely on script-only APIs such as `write()`, `askForKeyword()`, `runCommand()`, etc., which are not available in the property-expression context.
4. **`lib2_<ADM>.lua` or `lib2_<DOMAIN>.lua`** — Administration-specific or domain-specific functions that are still general-purpose within their scope, callable from **scripts only** (e.g., `lib2_VA.lua` for French track/superstructure "Voie et Abord" functions).
5. Domain-specific function files with descriptive names — Highly specialized functions for a particular workflow.

**Rule:** A library file at a given level must not contain text strings or logic specific to a narrower scope. For example, `lib1.lua` and `lib2.lua` must not contain administration-specific text.

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
   _TERMINATED_ = "Termine."
   _TRACK_VARIANT_ = "Traverses et rails - 3D simple"
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

Constants that need to be inherited by included libraries are declared as **globals** (without `local`) in the `---GLOBAL CONSTANTS---` section. Constants used only within the script itself are declared as **locals** in the `---LOCAL CONSTANTS---` section:

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
_TERMINATED_ = "Termine"



---INCLUDES---
local lib1 = includeLuaFile("Lua\\Functions\\lib1.lua")
local lib2 = includeLuaFile("Lua\\Functions\\lib2.lua")
local VA = includeLuaFile("Lua\\Functions\\lib2_VA.lua")



---LOCAL CONSTANTS---
local _TRACKS_AND_KILOMETRATION_MARKERS_ = "Importer les VOIEs, puis utiliser les ..."
local _TRACK_VARIANT_ = "Traverses et rails - 3D simple"

local _USAGE_ = [[
Pre-requis :
- Donnees d'entree exportees depuis Gaia Data Etude...

Version : ]] .. _VERSION_
```

In this example, `_HEADER_`, `_VERSION_`, `_DEBUG_`, `_TRACE_`, `_YES_`, `_NO_`, `_OK_`, `_HELP_`, `_TERMINATE_`, and `_TERMINATED_` are all global. They are inherited by `lib1`, `lib2`, and `VA`. The included libraries may declare their own `local _VERSION_` without affecting the parent's `_VERSION_`.

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

When a line of code contains a user-facing string in a foreign language (e.g., French) and you want to provide the English meaning as a comment, wrap the English translation in **double quotes** and end with a **colon**. Only add a translation comment when the foreign-language text would not be understood by an English-speaking developer:

```lua
-- "Could not find path to ... Please select new own alignment for ...":
write("Impossible de trouver un parcours vers " .. rkObject.Var1 ..
    ". Veuillez selectionner un nouvel axe propre pour " .. rkObject.code .. "\n", _warning)
-- "Select the folder containing the shapefiles":
shapefileFolder = askForFolderName("Selectionnez le dossier contenant les fichiers shapefile")
-- "Track created:":
lib2.writeln("Creation de voie : " .. trackName)
```

### 4.4 Dead Code and Alternative Code

1. **Remove dead code** whenever possible. Do not leave commented-out code in production files unless there is a clear reason to keep it (e.g., it may become relevant again soon).

2. If dead code must be retained, use a **long comment** (`--[[ ... ]]`):

   - **Single line:** Place `--[[` and `--]]` on the same line, wrapping both the dead code and any inline comment it may have:

     ```lua
     --[[ local marker = createPointObject(possibleAlignment, "JBTFE_MRK Marker", "Knappenal", coordinate) --]]
     ```

   - **Multiple lines:** Place `--[[` and `--]]` on their own lines, flush with the **left margin** (column 1), immediately before and after the dead code block.

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
        write(msg .. "\n", symbol or _noSymbol)
    else
        write("\n", symbol or _noSymbol)
    end
end
```

In rare cases, for very short functions, it is acceptable to write the entire function on one line:

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
-- Renvoie un tableau de sous-chaines extraites de la chaine d'entree, decoupee au caractere de separation donne. Appel : lib1.splitString("The quick brown fox", " ") qui renvoie {"The", "quick", "brown", "fox"}.
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

Administration-specific and domain-specific libraries (e.g., `lib2_VA.lua` for FR-SR) exist in only **one** language repository and are not shared across administrations. For these files, the tooltip shall be written in **two lines**: first English (for the developer), then the target language (for the end user):

```lua
-- Import tracks from VOIE and REPERE_KILOMETRIQUE shapefiles. Call as: importTracksFromShapefile(shapefileTable = nil).
-- Importer les voies depuis des shapefiles VOIE et REPERE_KILOMETRIQUE. Appel : importTracksFromShapefile(shapefileTable = nil).
function importTracksFromShapefile(shapefileTable, arg1)
```

### 5.5 Functions Not Intended for End Users

Helper functions that are local to a file and not part of the public API do not require bilingual tooltips. A single English comment is sufficient:

```lua
-- Check whether the given alignment info indicates the point lies on the alignment:
local function isOnAlignment(alignmentInfo)
-- Recursively find all .shp files in the given folder and its subfolders:
local function getShapefilesInFolder(folder, shapefileTable)
```

## 6. Language Constants and Multilingual Text

### 6.1 When to Use Constants for UI Strings

User-facing strings (prompts, menu options, messages) that appear in code can be handled in two ways:

1. **Inline strings** — The foreign-language string is written directly in the code, with an English translation comment above it.
2. **Named constants** — The string is assigned to a constant with an English-language identifier.

**Recommended approach:**

- **Option lists and menu keywords** should use named constants. This makes the code readable for non-native developers and allows reuse:

  ```lua
  ---GLOBAL CONSTANTS---
  _YES_ = "Oui"
  _NO_ = "Non"
  _HELP_ = "Aide"
  _TERMINATE_ = "Terminer"

  -- Later, in code:
  -- "Select your action:":
  option = askForKeyword("Selectionnez votre action :",
      {_INSERT_CONNECTION_OBJECTS_, _HELP_, _TERMINATE_}, _HEADER_)
  if option == _TERMINATE_ or option == nil then
  ```

- **One-off prompt strings and short messages** may be written inline with an English comment:

  ```lua
  -- "Select the folder containing the shapefiles":
  shapefileFolder = askForFolderName("Selectionnez le dossier contenant les fichiers shapefile")
  ```

### 6.2 Shared Language Constants Across Scripts

When multiple scripts for the same administration share the same set of language constants (e.g., `_YES_`, `_NO_`, `_HELP_`, `_TERMINATE_`), there is some unavoidable repetition because each script must declare them as globals in its own `---GLOBAL CONSTANTS---` section for sandbox inheritance to work.

**Recommended approach:** Accept the repetition and declare shared language constants as globals directly in each script's `---GLOBAL CONSTANTS---` section. This keeps the code simple, readable, and compatible with sandbox inheritance.

### 6.3 Multilingual Strategy for Generic Libraries

Rather than maintaining a single Lua file that handles multiple languages at runtime, the preferred approach is:

1. Maintain a **master English-language version** of each generic library (`lib1.lua`, `lib2.lua`) in a shared repository.
2. For each target-language repository, create a **translated copy** where only the tooltip comments and any user-facing strings are translated.
3. Use Claude or another translation tool to produce and verify the translated copies, ensuring that all function declarations and function bodies remain identical across repositories.

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
```

### 7.3 Spaces Around Operators

Use spaces around binary operators (`=`, `==`, `~=`, `<`, `>`, `<=`, `>=`, `+`, `-`, `*`, `/`, `..`, `and`, `or`):

```lua
local distance = math.sqrt((x2 - x1)^2 + (y2 - y1)^2)
if option == _TERMINATE_ or option == nil then
alignment.code = "="
```

Exceptions:

- The **exponentiation operator** `^` and **unary minus** may omit spaces when the expression is clearer that way: `(p1.X - p2.X)^2`

Double spaces (or more) are not permitted in Lua expressions; only single spaces are used. Multiple spaces are permitted only inside string literals.

### 7.4 Comparison Order

When testing an unknown value against a known value, place the identifier holding the **unknown value on the left** and the **known value on the right**:

```lua
-- Correct:
if option == _TERMINATE_ then
if attributes.TYPE == "BIF" then

-- Wrong:
if _TERMINATE_ == option then
if "BIF" == attributes.TYPE then
```

### 7.5 Line Length and Line Breaking

There is no strict maximum line length. However, keep in mind that the RailCOMPLETE embedded Avalon Lua editor window is often fairly narrow.

**Guideline:** If a line exceeds roughly 120 characters, consider breaking it for readability. When breaking a line, indent the continuation at least one tab stop beyond the starting line, unless both parts are segments of the same concatenated string:

```lua
local applicableAlignments = table.select(allTracks,
    function (x) return {
        track = x,
        alignmentInfo = getAlignmentInfo(x.id, ndvCoordinatePoint)
    } end)
    :where(function(x) return isOnAlignment(x["alignmentInfo"]) end)
-- String continuation — both lines are parts of the same string, so same indentation:
lib2.show("L'importation de voies sera faite sans introduire les reperes " ..
"kilometriques correspondants. Pour completer le plan de trace, " ..
"vous devez suivre les pas suivants :")
```

### 7.6 Blank Lines for Readability

1. **No blank line** between code lines that belong closely together.
2. **One blank line** as "air" for readability — for example, before each `elseif` when the branches contain roughly 10 or more lines of code. Think of this as a paragraph break within a single context.
3. **Three blank lines** before each top-level section header within a file (`---GLOBAL CONSTANTS---`, `---INCLUDES---`, `---LOCAL CONSTANTS---`, `---FUNCTIONS---`, `---SCRIPT---`).
4. **Three blank lines** before each top-level function definition, or before the comment block that introduces it. **Exception:** The first function immediately after a `---FUNCTIONS---` section header needs no additional blank lines beyond the three that precede the section header itself.
5. **One blank line** as the very last line of the file (see Section 1.3).

## 8. Duplicate Code

### 8.1 No Duplicate Function Definitions

The same function must not be defined in two different files with different implementations. If two libraries need similar functionality with slight differences, factor out the common parts into a shared function and specialize only where necessary.

### 8.2 Refactoring Duplicates

When you encounter duplicate code during a review:

1. Identify which version is more correct or more general.
2. Keep that version in the appropriate library.
3. Have the other file call the shared version, or remove the duplicate entirely.
4. Verify that all callers still work after the change.

## 9. Miscellaneous Style Rules

### 9.1 Semicolons

Do not use semicolons to terminate statements. Lua does not require them and they add visual noise.

### 9.2 String Concatenation

Use **spaces** around the `..` concatenation operator, consistent with all other binary operators (see Section 7.3):

```lua
local msg = "Track created: " .. trackName
lib2.writeln("Associe la voie " .. track.code .. " a l'axe de reference " .. refAlignment.code)
```

When a concatenated expression must be broken across multiple lines, break **after** the `..` operator:

```lua
lib2.show(
    tostring(nSwitches) .. " aiguillages crees : " .. (switchNames or "-") .. "\n\n" ..
    tostring(nCrossings) .. " traversees obliques creees : " .. (crossingNames or "-"))
```

### 9.3 `goto` and Labels

The `goto` statement with `::continue::` labels is acceptable for skipping to the next iteration of a loop when the alternative would be deeply nested `if` statements. Place the `::continue::` label at the end of the loop body, at the same indentation level as the loop contents:

```lua
for _, shape in pairs(shapeVoieTable) do
    local recordNumber = shape.RecordNumber

    -- Is this a derailer alignment that should be skipped?
    if shape.NumParts == 1 and shape.NumPoints == 2
        and math.abs(geometriesTable[recordNumber].Length - 2.0) < 0.1 then
        lib2.show("Derailleur ignore : " .. alignmentName, nil, _warning)
        goto continue
    end

    -- ... process shape ...

    ::continue::
end
```

### 9.4 Magic Numbers

Avoid unexplained numeric literals in code. Use named constants or add an inline comment explaining the value:

```lua
---LOCAL CONSTANTS---
local _1_MM_ = 1e-3
-- Using a named constant for the tolerance:
if RC__getDistance2D(pointA, pointB) < _1_MM_ then

-- Acceptable alternative with an inline comment:
if RC__getDistance2D(previousCoordinate, c) > 1e-6 then -- 1 um tolerance
```

### 9.5 Boolean Flags: `_DEBUG_` and `_TRACE_`

- `_DEBUG_` — When `true`, enables extra code that creates auxiliary objects or produces additional output useful during development:

  ```lua
  if _DEBUG_ then
      -- Create an auxiliary line object to visualize the milepost markers:
      local referenceMileageHelpLine = createAlignmentObject(
          rctype_AuxiliaryLine, "Ligne auxiliaire",
          refAlignmentHorizontalGeometries[referenceAlignmentName])
  end
  ```

- `_TRACE_` — When `true`, enables verbose logging via `lib2.trace()`. Trace output is written to the log window but does not create popups or auxiliary objects.

Both flags default to `false` in production code.

---

# PART B — TECHNICAL PATTERNS AND API

## 10. Overview: Two Lua Contexts

Scripts differ from property formulas in several ways:

| Aspect | Property Formulas | Scripts |
|--------|-------------------|---------|
| Context | `LuaContext` (sandboxed, per-object) | `RunScriptLuaContext` (full drawing access) |
| Execution | Synchronous, triggered on property eval | Async, user-initiated |
| `this` | Current railway object | Not available (select objects explicitly) |
| Scope | Read-only computation | Create, modify, delete objects |
| API | ~88 object-level functions | All object-level + ~60 script-only functions |

## 11. User Interaction

### askForKeyword — Multiple Choice Dialog

```lua
local option = askForKeyword("Select action:", {"Option 1", "Option 2", "Abort"})

-- With header/title:
local option = askForKeyword("Choose format:", {"Excel", "CSV", "JSON"}, _HEADER_)
```

**Menu loop pattern:**
```lua
local option
repeat
    option = askForKeyword("Select action:", {"Insert", "Help", "Exit"}, _HEADER_)

    if option == "Help" then
        lib2.show("Help text here...")
    elseif option == "Insert" then
        -- Do work
    end
until option == "Exit" or option == nil
```

### askForObject / askForPointObject — Object Selection

```lua
local obj = askForObject("Select an object in the drawing")
local pointObj = askForPointObject("Select a point object")

-- Multiple objects (loop until user cancels):
local objects = {}
repeat
    local obj = askForPointObject("Select a point object (press Enter when done)")
    if obj then table.insert(objects, obj) end
until not obj
```

### askForAlignment — Alignment Selection

```lua
local alignment = askForAlignment("Select reference alignment: ")

-- Get the underlying CAD polyline:
local polyline = cadInterface.getCadEntityFromRcObject(alignment)
```

### askForPoint — Point Picking

**Important**: `askForPoint()` returns nil if the user presses Escape. Always check for nil:

```lua
local point = askForPoint("Click insertion point:")
if not point then return end
```

### askForDouble / askForInteger / askForString — Value Input

```lua
local radius = askForDouble("Enter search radius [m]:")
local count = askForInteger("Enter number of objects:")
local name = askForString("Enter a name:")

-- With default value:
local mileage = askForDouble("Enter start mileage [m]:", 0)
```

### askForFileName / askForFolderName — File/Folder Dialogs

```lua
local filename = askForFileName("Select Excel file with coordinates")
local folder = askForFolderName("Select output folder")

-- Optional file (Escape to skip):
local optionalFile = askForFileName("Select file (Escape to skip)")
if optionalFile then
    -- Process file
end
```

### showMessage — Display Message

```lua
showMessage("Operation completed successfully.")
```

## 12. Object Creation and Manipulation

### Alignment Creation

```lua
-- From geometry segments:
local segments = {}
table.insert(segments, createLineSegment(p1, p2))
table.insert(segments, createCurveSegment2(p2, directionDeg, p3))
local geometry = createHorizontalGeometry(segments)
local track = createAlignmentObject(rctype_Track, "Variant Name", geometry)

-- From coordinate points (implicit geometry):
local points = {getPoint3D(x1, y1, z1), getPoint3D(x2, y2, z2), getPoint3D(x3, y3, z3)}
local track = createAlignmentObject(rctype_Track, "Variant Name", points)

-- Set properties after creation (see formula reset pattern in Section 12):
track.code = "="
track.code = "V1"
track.name = track.name -- Force save and update data
```

### Geometry Segments

```lua
-- Straight line:
local line = createLineSegment(startPoint, endPoint)

-- Circular arc (from start point, direction, end point):
local arc = createCurveSegment2(startPoint, directionDegrees, endPoint)

-- Clothoid/spiral:
local clothoid = createClothoidSegment(startPoint, directionDegrees, length, startRadius, A, isPositive)

-- Combine into horizontal geometry:
local geometry = createHorizontalGeometry(segments)
```

### Point Object Creation

```lua
-- createPointObject(alignment, rctype, variant, position, distFromAlignment, leftSide):
local signal = createPointObject(
    track,
    rctype_Signal,
    "Signal classique, cible ACFH",
    pos,
    3.5,    -- Distance from alignment [m]
    true    -- Left side of track
)

-- insertPointObject (modern, simpler API):
local obj = insertPointObject(alignment, rctype, variant, position)
local obj = insertPointObject(alignment, rctype, position)  -- No variant
```

### Property Assignment

**Direct assignment:**
```lua
signal.name = "Signal A"
signal.DrawTail = true
signal.dir = "down"
object.VerticalOffset = 5.5
```

**Formula reset pattern** — RailCOMPLETE objects may have properties governed by DNA formulas. To override a formula-driven property with a literal value, first assign `"="` to clear the formula, then assign the desired value:
```lua
alignment.code = "="         -- Remove possible existing formula from DNA
alignment.code = trackName   -- Set the literal value

obj.VerticalOffset = "=" -- Remove elevation formula
obj.VerticalOffset = z   -- Assign explicit value
```

**Force save/update** — trigger a recalculation/save without changing the value:
```lua
alignment.name = alignment.name  -- Triggers save and recalculation of derived fields
```

### Relations

```lua
setRelation(sourceObj, targetObj, "RelationType")
```

### Profile Creation

```lua
-- Vertical profile (no alignment arg — returns a profile object):
local events = {}
table.insert(events, createVerticalEvent(pos1, elevation1))
table.insert(events, createVerticalEvent(pos2, elevation2))
createVerticalProfile(events)

-- Cant profile:
local cantEvents = {}
table.insert(cantEvents, createCantEvent(pos, cantValue))
createCantProfile(cantEvents)

-- Speed profile:
local speedEvents = {}
table.insert(speedEvents, createSpeedEvent(pos, speedKmh))
createSpeedProfile(speedEvents)

-- Mileage profile (eventType is required: "Start", "ChainBreak", or "Milepost"):
local mileageEvents = {}
table.insert(mileageEvents, createMileageEvent(pos, mileageValue, "Milepost"))
createMileageProfile(mileageEvents)

-- Update vertical data from point list:
updateAlignmentVerticalData(alignment, table.select(pointList, function(p)
    return getPoint3D(p.X, p.Y, p.Z)
end))
```

### Deletion

```lua
eraseObject(obj)
```

## 13. File I/O

### Excel Reading

```lua
-- 1. Open file:
local filename = askForFileName("Select Excel file")
local file = getContentsFromFile(FileType.Excel, "", filename)

-- 2. Get sheet names:
local sheets = getExpandoObjectPropertyNames(file)
local sheetName = sheets[0]  -- 0-indexed!

-- 3. Get rows:
local items = file[sheetName]
local nItems = getCollectionLength(items)

-- 4. Process rows:
for i = 0, nItems - 1 do
    local row = items[i]
    local x = row["X"]       -- Column access by header name
    local y = row["Y"]
    local z = row["Z"]       -- nil if column doesn't exist

    if type(x) == "number" and type(y) == "number" then
        -- Process valid row
    else
        lib2.writeln(string.format("Skipping row %d: invalid data", i + 1), _warning)
    end
end
```

### XML Reading

```lua
local xml = getContentsFromFile(FileType.Xml, "Select XML file", "*.xml")
local props = getExpandoObjectPropertyNames(xml)
local nProps = getCollectionLength(props)

for i = 0, nProps - 1 do
    local value = xml[props[i]]
    lib2.writeln(props[i] .. " = " .. tostring(value))
end
```

### Text File Reading

```lua
local file = getContentsFromFile(FileType.Text, "", filename)
local lines = {}
for s in file:gmatch("[^\r\n]+") do
    table.insert(lines, s)
end
```

### JSON

```lua
-- Read JSON from file (first arg is filename, second is target object):
local data = deserializeJson(filename, targetObject)

-- Export table to JSON string:
local jsonString = exportToJson(luaTable)

-- Write JSON string to file:
exportStringToFile(jsonString, "output.json")
```

### File Output

```lua
exportStringToFile(contentString, "output.txt")
```

### Directory Listing

```lua
local files = getFilesInFolder(folderPath)
local folders = getFoldersInFolder(folderPath)
```

## 14. Running Commands

### Pattern

Commands require a **trailing space** to execute:

```lua
runCommand("_COMMANDNAME args ")  -- Note trailing space
```

### Return Value

```lua
local result = runCommand("_RC-ShowVersion  ")
local logOutput = result.log       -- Command output text
local status = result.result       -- Execution status
```

**Important:** `runCommand()` breaks undo buffer grouping — see Undo Buffer Grouping in Section 14 for details.

### Common AutoCAD Commands

```lua
-- System variables:
runCommand("_PICKADD 0 ")
runCommand("_FILEDIA 1 ")
runCommand("_ORTHOMODE 0 ")
runCommand("_GRID _OFF ")
runCommand("_SNAP _OFF ")
runCommand("_NAVVCUBE _OFF ")
runCommand("_DYNMODE 3 ")
runCommand("_SELECTIONCYCLING 2 ")

-- Units setup:
runCommand('_-UNITS 2 3 1 3 0 _NO ')

-- Object snap modes:
runCommand("_-OSNAP _END,_MID,_CEN,_NOD,_INT,_PER,_TAN,_NEA ")

-- Color:
runCommand("_-COLOR _BYLAYER ")

-- Drawing:
runCommand("_CIRCLE " .. x .. "," .. y .. " " .. radius .. " ")

-- Zoom:
runCommand("_z _e ")       -- Zoom extents

-- Regen / Purge:
runCommand("_REGEN ")
runCommand("_-PURGE _ALL * _NO ")

-- Save:
runCommand("_QSAVE ")
```

### LISP Wrapping

For commands that need LISP syntax:

```lua
runCommand('(command "._ZOOM" "_EXTENTS") ')
```

### RC Commands

```lua
runCommand("RC-CommandName ")  -- See 050-commands.html for full list
```

### Version Checking

```lua
local tmp = runCommand("_RC-ShowVersion  ").log
local rcVersion = tmp:match("version (%d+%.%d+)%.%d+%.%d+")
```

### Undo Buffer Grouping

When a script or function creates or modifies multiple CAD objects that should be undoable as a single operation, wrap the relevant block in `beginUndoBufferItem()` and `endUndoBufferItem()`:

```lua
beginUndoBufferItem()

for _, track in pairs(tracks) do
    -- ... create or modify objects ...
end

endUndoBufferItem()
```

Ensure that every `beginUndoBufferItem()` has a matching `endUndoBufferItem()`, even when the function returns early due to errors.

**Important:** Any call to the AutoCAD console using `runCommand()` will break the undo grouping established by the surrounding `beginUndoBufferItem()` / `endUndoBufferItem()` pair. Do not place `runCommand()` calls between these two statements:

```lua
-- WRONG -- runCommand() breaks the undo grouping:
beginUndoBufferItem()
runCommand("_GRIPS 0 ")  -- This breaks the undo buffer!
-- ... create objects ...
endUndoBufferItem()
```

## 15. Advanced Patterns

### Selection Sets

After creating objects, make them the current selection:

```lua
local created = {}
for i = 0, nItems - 1 do
    local obj = insertPointObject(alignment, rctype, variant, positions[i])
    table.insert(created, obj)
end
setSelectionSet(created)
```

### External Libraries

Call C# methods from external DLLs:

```lua
-- Open a shapefile via DotSpatial:
local shapefile = runExternalLibraryFunction(
    "DotSpatial.Data.Shapefile",
    "OpenFile",
    {shapefilePath}
)

-- Create a .NET object:
local milepost = createExternalLibraryObject(
    "RailCOMPLETE.Model.GeometryModels.Milepost",
    {},  -- Constructor args
    {Name = "1.0KM", Pos = 1000, Mileage = 1000}  -- Property assignments
)
```

### Library Inclusion

```lua
local lib1 = includeLuaFile("Lua\\Functions\\lib1.lua")
local lib2 = includeLuaFile("Lua\\Functions\\lib2.lua")

-- Use library functions:
lib2.zoomExtents()
local id = RC__identify(obj)
```

See Section 3 for sandbox inheritance rules governing `includeLuaFile()`.

### CAD Entity Creation

Create raw AutoCAD entities:

```lua
-- Points and vectors:
local point = cadInterface.createCadEntity("Geometry.Point3d", {x, y, z})
local vector = cadInterface.createCadEntity("Geometry.Vector3d", {0, 0, 1})
local point2d = cadInterface.createCadEntity("Geometry.Point2d", {x, y})

-- Circle:
local circle = cadInterface.createCadEntity("DatabaseServices.Circle", {point, vector, radius})

-- Line:
local line = cadInterface.createCadEntity("DatabaseServices.Line", {
    getAcadPoint3D(p1), getAcadPoint3D(p2)
})

-- Polyline:
local polyline = cadInterface.createCadEntity("DatabaseServices.Polyline", {})
local index = 0
for _, v in pairs(points) do
    polyline:AddVertexAt(index, cadInterface.createCadEntity("Geometry.Point2d", {v.X, v.Y}), 0, 0, 0)
    index = index + 1
end
polyline.Closed = true

-- MText:
local text = cadInterface.createCadEntity("DatabaseServices.MText", {})
text.Height = 2.0
text.TextHeight = 1.25
text.TextStyleId = cadInterface.getTextStyleId("ISO")
text.Contents = "Line 1\\PLine 2"  -- \\P = newline
text.Attachment = cadInterface.createCadEntity("DatabaseServices.AttachmentPoint", {"TopRight"})
text.Location = getAcadPoint3D(textPosition)

-- Dimension:
local dim = cadInterface.createCadEntity("DatabaseServices.AlignedDimension", {})
dim.XLine1Point = getAcadPoint3D(p1)
dim.XLine2Point = getAcadPoint3D(p2)
dim.DimLinePoint = getAcadPoint3D(dimPoint)
dim.DimensionText = string.format("%.0f", distance)

-- Add entities to drawing:
cadInterface.addEntitiesToModelSpace({circle, line, polyline, text})
```

### Block Operations

```lua
-- Create a block:
local blockName = "myBlock_" .. generateGuid()
cadInterface.createBlock(blockName, entities)

-- Create block reference (instance):
local ref = cadInterface.createBlockReference(blockName, insertionPoint)
ref.ScaleFactors = cadInterface.createCadEntity("Geometry.Scale3d", {scaleFactor})

-- Check if block exists:
if not cadInterface.blockExist(blockName) then
    cadInterface.createBlock(blockName, entities)
end

-- Get CAD entity from RC object:
local cadEntity = cadInterface.getCadEntityFromRcObject(rcObject)
```

### Table Operations

These extend Lua's built-in table library:

```lua
-- Transform collection to Lua table:
local luaTable = table.select(rcCollection, function(x) return x end)

-- Filter:
local filtered = table.where(items, function(x) return x.RcType == rctype_Signal end)

-- Get first match:
local first = table.firstOrNil(items, function(x) return x.code == "V1" end)

-- Sort:
table.sort(items, function(a, b) return a.Mileage < b.Mileage end)
```

### Coordinate Conversions

```lua
-- Get alignment-relative coordinates:
local linearAddr = getLinearAddress(worldPosition, alignment)
-- linearAddr.DistanceAlong, .LateralOffset, .LongitudinalOffset, .VerticalOffset

-- Get alignment info at a point:
local ai = alignment:getAlignmentInfo(point)
if ai.NormalProjectionExists then
    local pos = ai.RelativePosition
    local mileage = ai.Mileage
end

-- WCS vector from ACS vector:
local wcsVector = getWcsVectorFromAcsVector(obj, lateralOffset, longitudinalOffset)
```

## 16. Error Handling

### General Guidance

Functions that **can fail** in expected ways (e.g., user cancellation, missing data, file not found) should communicate this to the caller. The recommended pattern is to **return `nil`** on failure so the caller can check:

```lua
local paths = getShapefilePaths(folder)
if not paths then return end
```

### Error Dialogs

- Use **`lib2.show(msg, nil, _error)`** followed by `return` for recoverable errors:

  ```lua
  if not shapefileNdv then
      -- "Could not find NDV shapefile.":
      lib2.show("Impossible de trouver le shapefile 'NDV'.", nil, _error)
      return
  end
  ```

- Use **`lib2.stop(msg)`** for unrecoverable errors that should halt the script:

  ```lua
  lib2.stop("Bad arguments to importTracksFromShapefile(): ["
      .. tostring(arg1) .. ", " .. tostring(arg2) .. "].")
  ```

### Guard Clauses

Every function that receives optional arguments or depends on external data should validate inputs early:

```lua
function importTracksFromShapefile(shapefileTable)
    -- Get shapefiles if not passed:
    if not shapefileTable then
        local shapefileFolder = askForFolderName("Selectionnez le dossier...")
        if not shapefileFolder then return end
        local shapefilePaths = getShapefilePaths(shapefileFolder)
        shapefileTable = getShapefileTable(shapefilePaths)
    end
    -- ...
end
```

## 17. Real Examples

### Import Objects from Excel

```lua
--[[
    Insert objects at XY coordinates from Excel
    ============================================
    2025-01-15 v1.0 AUTHOR Created.
--]]



---GLOBAL CONSTANTS---
_HEADER_ = "Insert objects at XY(Z) coordinates from Excel"
_VERSION_ = "2025-01-15 v1.0"
_DEBUG_ = false
_TRACE_ = false



---INCLUDES---
local lib2 = includeLuaFile("Lua\\Functions\\lib2.lua")



---LOCAL CONSTANTS---
local _USAGE_ = [[
    Insert objects at XY(Z) coordinates from Excel
    ===============================================
    Input: Excel file with columns X, Y (and optionally Z).
    Usage: Select file, select template object, objects are inserted.
]]



---SCRIPT---
lib2.show(_USAGE_)

-- Select file:
local filename = askForFileName("Select Excel file with X, Y columns")
local file = getContentsFromFile(FileType.Excel, "", filename)
local sheets = getExpandoObjectPropertyNames(file)
local sheetName = sheets[0]
local items = file[sheetName]
local nItems = getCollectionLength(items)

lib2.show(nItems .. " rows found in sheet '" .. sheetName .. "'")

-- Select template object:
lib2.show("Select an existing object as template. Its RcType and Variant will be used.")
local template = askForObject("Select template object")
if template.Alignment == nil then
    lib2.show("Aborting: template object must have an alignment.", nil, _error)
    return
end

-- Process rows:
local objTable = {}
local nCreated = 0

beginUndoBufferItem()

for i = 0, nItems - 1 do
    local row = items[i]
    local x = row["X"]
    local y = row["Y"]
    local z = row["Z"]

    -- Is this row valid?
    if type(x) ~= "number" or type(y) ~= "number" then
        lib2.writeln(string.format("Skipping row %d: X='%s' or Y='%s' is not a number.",
            i + 1, tostring(x), tostring(y)), _warning)
        goto continue
    end

    local p = getPoint3D(x, y)
    local obj = insertPointObject(
        template.Alignment, template.RcType, template.Variant, p
    )

    if obj then
        if z and type(z) == "number" then
            obj.VerticalOffset = "="
            obj.VerticalOffset = z
        end
        table.insert(objTable, obj)
        nCreated = nCreated + 1
        lib2.writeln(string.format("%d: Created at (%.3f, %.3f)", i + 1, x, y), _ok)
    else
        lib2.writeln(string.format("Row %d: Failed to create object at (%.3f, %.3f)", i + 1, x, y), _error)
    end

    ::continue::
end

endUndoBufferItem()

setSelectionSet(objTable)
lib2.show("\n" .. nCreated .. " objects created out of " .. nItems .. " rows.")
```

### Batch Create Tracks from Shapefile

```lua
--[[
    Import tracks from Shapefile
    ============================
    2025-02-01 v1.0 AUTHOR Created.
--]]



---GLOBAL CONSTANTS---
_HEADER_ = "Import Tracks"
_VERSION_ = "2025-02-01 v1.0"
_DEBUG_ = false
_TRACE_ = false



---INCLUDES---
local lib1 = includeLuaFile("Lua\\Functions\\lib1.lua")
local lib2 = includeLuaFile("Lua\\Functions\\lib2.lua")



---LOCAL CONSTANTS---
local _TRACK_VARIANT_ = "Traverses et rails - 3D simple"



---SCRIPT---
lib2.show("Import tracks from a Shapefile (.shp)")

local shapefilePath = askForFileName("Select the Shapefile (.shp)")
local shapefile = runExternalLibraryFunction(
    "DotSpatial.Data.Shapefile", "OpenFile", {shapefilePath}
)
local shapes = table.select(shapefile.ShapeIndices)
local geometries = table.select(shapefile.Features, function(x) return x.Geometry end)

lib2.writeln("Importing from: " .. shapefilePath)

beginUndoBufferItem()

for _, shape in pairs(shapes) do
    local recordNumber = shape.RecordNumber
    local trackName = tostring(shapefile.Attributes.Table.Rows[recordNumber - 1].ItemArray[2])

    -- Is this a very short segment (derailer) that should be skipped?
    if shape.NumParts == 1 and shape.NumPoints == 2 and
       math.abs(geometries[recordNumber].Length - 2.0) < 0.1 then
        lib2.writeln("Skipping derailer: " .. trackName, _warning)
        goto continue
    end

    local parts = table.select(shape.Parts)
    for _, part in pairs(parts) do
        local coords = table.select(part)
        local points = {}

        for idx, vertex in ipairs(coords) do
            local z = shapefile.Z[shape.StartIndex + idx - 1]
            table.insert(points, getPoint3D(vertex.X, vertex.Y, z))
        end

        local alignment = createAlignmentObject(rctype_Track, _TRACK_VARIANT_, points)
        if alignment then
            alignment.code = "="
            alignment.code = trackName
            -- Force save and update data:
            alignment.name = alignment.name
            lib2.writeln("Created track: " .. trackName, _ok)
        else
            lib2.writeln("Failed to create track: " .. trackName, _error)
        end
    end

    ::continue::
end

endUndoBufferItem()

-- Zoom to extents (note: must be outside undo buffer, see Section 14):
runCommand("_z _e ")
lib2.show("Import complete")
```

## 18. Common Pitfalls

### 0-Indexed RC Collections vs 1-Indexed Lua Tables

RailCOMPLETE collections (from `getRelatedObjects()`, `getContentsFromFile()`, `filter()`, `getExpandoObjectPropertyNames()`) are **0-indexed**. Lua tables created with `table.insert()` are **1-indexed**.

```lua
-- RC collection: 0-indexed
local items = file[sheetName]
for i = 0, getCollectionLength(items) - 1 do
    local row = items[i]
end

-- Lua table: 1-indexed
local luaTable = {}
table.insert(luaTable, "a")  -- luaTable[1] = "a"
```

### askForPoint() Cancellation

`askForPoint()` returns nil when the user presses Escape. Always check for nil:

```lua
local point = askForPoint("Click a point:")
if not point then return end
```

### Trailing Space in runCommand()

Commands won't execute without a trailing space:

```lua
runCommand("_CIRCLE 0,0 10 ")  -- Works (trailing space)
runCommand("_CIRCLE 0,0 10")   -- May hang waiting for input!
```

### string.format() Patterns

```lua
string.format("%.3f", value)     -- 3 decimal places: "1.234"
string.format("%04d", number)    -- 4-digit zero-padded: "0042"
string.format("%d: %s", i, msg)  -- Integer and string
```

## 19. Constants Reference

### FileType Constants

```lua
FileType.Excel    -- .xlsx files
FileType.Xml      -- .xml files
FileType.Text     -- .txt files
FileType.Csv      -- .csv files
FileType.Json     -- .json files
FileType.Lua      -- .lua files
```

### Output Symbols

```lua
_noSymbol    -- Standard output
_ok          -- Green checkmark
_warning     -- Yellow triangle
_error       -- Red X
```

## 20. Documentation Reference

- **Full API reference**: `.claude/references/080-luacommands.html` — all object-level + script-only functions
- **Lua language tutorial**: `.claude/references/070-lua.html` — syntax, types, control structures
- **RC commands**: `.claude/references/050-commands.html` — commands available via `runCommand()`
- **Debugger**: `.claude/references/080-luadebugger.html` — breakpoints, watches, stepping
