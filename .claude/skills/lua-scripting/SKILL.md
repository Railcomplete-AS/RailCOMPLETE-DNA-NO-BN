---
name: lua-scripting
description: Guide for writing standalone Lua scripts in DNA repositories -- coding style, commenting conventions, technical patterns, API usage, and common pitfalls
---

# RailCOMPLETE Lua Scripting Guide

> **Purpose:** Coding style, commenting conventions, and technical patterns for all Lua scripts and function libraries published by RailCOMPLETE AS (RCAS).
>
> **Goal:** All RCAS-published Lua files should look and feel consistent — it should not be easy to tell who wrote a particular piece of code.

$ARGUMENTS

For the full API reference, see `.claude/references/080-luacommands.html`. For RC commands, see `.claude/references/050-commands.html`. For debugger usage, see `.claude/references/080-luadebugger.html`.

---

# PART A — CODING STYLE

## File Organization

### Top-Level Sections in a Lua File

Every Lua file is organized into clearly labeled sections, separated by **three blank lines** before each section. Section headers use the exact format `---UPPERCASED SECTION NAME---` (three dashes, no spaces).

**Script files:** `---GLOBAL CONSTANTS---`, `---INCLUDES---`, `---LOCAL CONSTANTS---`, `---FUNCTIONS---`, `---SCRIPT---`

**Library files** (loaded via `includeLuaFile()`): `---INCLUDES---`, `---LOCAL CONSTANTS---`, `---FUNCTIONS---`

Library files use `---LOCAL CONSTANTS---` (not `---GLOBAL CONSTANTS---`) to make it explicit that libraries shall not declare global constants. Omit any section with no content.

**Note:** Some existing files use two-dash headers (`--CONSTANTS--`). New and updated code shall use the three-dash format.

### File-Level Block Comment

Every Lua file begins with `--[[ ... ]]` containing: (1) short name matching the filename, (2) a row of `=` signs matching the name length, (3) brief purpose description, (4) usage examples for libraries, (5) note on expected global constants if applicable, (6) version history as `YYYY-MM-DD vX.Y AUTHOR Description.`

```lua
--[[
    lib2
    ====
    Generic Lua functions callable from scripts.

    Usage:
    lib2 = includeLuaFile("Lua\\Functions\\lib2.lua")
    return lib2.writeln("Hello, world!", _ok)

    Note on global constants - these must be declared before including this file:
    _HEADER_, _DEBUG_, _TRACE_

    2026-01-30 v1.0 CLFEY Created.
    2026-04-08 v1.2 CLFEY Minor layout issues.
--]]
```

### Trailing Blank Line

Every file ends with exactly **one blank line** after the last line of code (the embedded Lua editor handles the last line poorly).

## Naming Conventions

### File Naming and Placement

- **Libraries:** `lib<N>.lua` for generic, `lib<N>_<CONTEXT>.lua` for domain-specific (e.g., `lib2_VA.lua`).
- **No hyphens** in filenames used as Lua identifiers — the local variable should mirror the filename.
- **Scripts:** Named descriptively, often in the target audience's language.
- **Libraries** go in `Lua\Functions\`, **scripts** go in `Lua\Scripts\`.

### Library Hierarchy and Scope

The number in the library name indicates callable contexts:

1. **`lib1.lua`** — Generic utilities, callable from **any Lua context** (scripts, functions, property expressions).
2. **`lib1_<ADM>.lua`** — Administration-specific, still callable from **any context**.
3. **`lib2.lua`** — Generic utilities, callable from **scripts only** (uses `write()`, `askForKeyword()`, `runCommand()`, etc.).
4. **`lib2_<ADM>.lua`** — Administration-specific, **scripts only**.
5. Domain-specific files with descriptive names — specialized workflow functions.

**Rule:** A library must not contain text or logic specific to a narrower scope than its level.

### Constants

Constant identifiers: **UPPERCASE** with single underscore prefix, suffix, and infixes: `_STOP_KEYWORD_`, `_TRACE_`, `_VERSION_`.

The underscores make search-and-replace safer across files. Lua has no true constants — treat these as immutable by convention. Avoid the `<const>` qualifier (confusing with `includeLuaFile()` sandboxes).

### Functions and Variables

- Functions: **camelCase** (`getShapefilePaths()`). Private functions use `local function`.
- Variables: **camelCase** (`shapefileFolder`). Loop counters may use `i`, `j`, `k`, `_`.

## Constants: Global vs. Local and Sandbox Inheritance

### The `includeLuaFile()` Sandbox Model

`includeLuaFile()` creates a **child sandbox**. Key rules:

1. **Globals declared before the call are inherited** by the child.
2. The child can override an inherited global (local to the child, doesn't propagate back).
3. A child's `local` variable shadows the global — and is **not** inherited by grandchild sandboxes.
4. **Order matters:** Declare globals **before** the `includeLuaFile()` calls that need them.

### Recommended Declaration Order in Script Files

```lua
---GLOBAL CONSTANTS---
_HEADER_ = "My Script"
_VERSION_ = "2026-04-08 v1.4"
_DEBUG_ = false
_TRACE_ = false
_YES_ = "Oui"
_NO_ = "Non"



---INCLUDES---
local lib1 = includeLuaFile("Lua\\Functions\\lib1.lua")
local lib2 = includeLuaFile("Lua\\Functions\\lib2.lua")



---LOCAL CONSTANTS---
local _TRACK_VARIANT_ = "Traverses et rails - 3D simple"
```

Globals (`_HEADER_`, `_VERSION_`, etc.) are inherited by included libraries. Libraries should declare their own constants as `local`.

## Comments

### General Formatting

1. **One space** after `--`: `-- This is a comment` (exception: section headers `---GLOBAL CONSTANTS---`).
2. **Capitalize** the first letter.
3. **No trailing period** unless two or more sentences.
4. **Inline comments** separated by at least one space from code; align consecutive inline comments at the same column:

   ```lua
   runCommand("_PICKADD 0 ")    -- Disables PICKADD
   runCommand("_FILEDIA 1 ")    -- Open files with a normal explorer window
   runCommand("_ORTHOMODE 0 ")  -- The cursor is not snapped to grid lines
   ```

### Preceding-Line Comments

End with punctuation signaling what follows:

- **Colon** when the next code **performs an action**: `-- Force save and update data:`
- **Question mark** when the next code **tests a condition**: `-- Is this a derailer that should be skipped?`

### Translated Prompt Comments

When code contains a foreign-language string, wrap the English translation in **double quotes** and end with a **colon**:

```lua
-- "Select the folder containing the shapefiles":
shapefileFolder = askForFolderName("Selectionnez le dossier contenant les fichiers shapefile")
```

### Dead Code

Remove dead code whenever possible. If retained, use long comments (`--[[ ... ]]`): single line wraps code inline; multi-line places `--[[` and `--]]` on their own lines at column 1.

## Function Documentation and Tooltips

### The Tooltip System

RailCOMPLETE's Lua editor tooltip uses **the single comment line immediately preceding the `function` declaration** plus the function signature line. Only the last comment line before the declaration is picked up.

Keep the `function` line clean — just the keyword, name, and arguments:

```lua
-- Writes a message to the log window and appends a newline. Call as lib2.writeln(msg, symbol = nil).
function writeln(msg, symbol)
    write(msg and (msg .. "\n") or "\n", symbol or _noSymbol)
end
```

Very short functions may be one line:

```lua
-- Point-to-string conversion. Call as lib1.p2s(p) where p is a 3D point.
function p2s(p) return string.format("(%.03f,  %.03f,  %.03f)", p.X, p.Y, p.Z) end
```

### Tooltip Format for Generic Libraries

In the **master (English) repository**, tooltip comments are English only. Include: (1) what the function does, (2) how to call it with the library prefix, (3) an example return value if helpful.

### Language-Specific Repositories and Tooltip Translation

Generic libraries (`lib1.lua`, `lib2.lua`) have **identical function declarations** across language repositories. Only tooltip comments differ. Target-language repositories use **dual comment lines** — English first (for the developer), then the target language (for the end user, which becomes the tooltip):

```lua
-- Returns a table with partial strings. Call as lib1.splitString("The quick brown fox", " ").
-- Renvoie un tableau de sous-chaines. Appel : lib1.splitString("The quick brown fox", " ").
function splitString(s, splitChar)
```

### Administration-Specific Libraries

These exist in only one repository. Tooltips use **two lines** — English then target language:

```lua
-- Import tracks from shapefiles. Call as: importTracksFromShapefile(shapefileTable = nil).
-- Importer les voies depuis des shapefiles. Appel : importTracksFromShapefile(shapefileTable = nil).
function importTracksFromShapefile(shapefileTable, arg1)
```

### Private Functions

Local helper functions need only a single English comment:

```lua
-- Check whether the alignment info indicates the point lies on the alignment:
local function isOnAlignment(alignmentInfo)
```

## Language Constants and Multilingual Text

**Option lists and menu keywords** should use named constants for readability and reuse:

```lua
_YES_ = "Oui"
_NO_ = "Non"
_TERMINATE_ = "Terminer"

option = askForKeyword("Selectionnez votre action :", {_INSERT_, _HELP_, _TERMINATE_}, _HEADER_)
```

**One-off prompt strings** may be inline with an English comment (see *Translated Prompt Comments*).

When multiple scripts share the same language constants, accept the repetition — each script declares them as globals in its own `---GLOBAL CONSTANTS---` section for sandbox inheritance.

## Formatting Rules

### Indentation

**Tabs**, one per nesting level. Tab width: **4 spaces** (matches the embedded Lua editor).

### Commas

No space before, one space after. If a comma is the last character on a line, the newline replaces the trailing space:

```lua
local result = createExternalLibraryObject(
    "RailCOMPLETE.RailMLModel.ReferenceAlignmentSegment",
    {},
    {AlignmentRef = ref, Pos = posStart})
```

### Operators

Spaces around binary operators (`=`, `==`, `~=`, `<`, `>`, `+`, `-`, `*`, `/`, `..`, `and`, `or`). Exceptions: `^` and unary minus may omit spaces. No double spaces in expressions (only inside string literals). When breaking a `..` concatenation across lines, break **after** the `..` operator.

### Comparison Order

Place the **unknown value on the left**, known on the right: `if option == _TERMINATE_ then` (not `if _TERMINATE_ == option then`).

### Line Length and Line Breaking

No strict max. Guideline: break around 120 characters. Indent continuations at least one tab beyond the starting line, except string continuations at the same level:

```lua
lib2.show("L'importation de voies sera faite sans introduire les reperes " ..
"kilometriques correspondants.")
```

### Blank Lines

1. **No blank line** between closely related code.
2. **One blank line** as readability "air" (e.g., before `elseif` in long branches).
3. **Three blank lines** before section headers and before each top-level function definition.
4. **One blank line** as the very last line of the file (see *Trailing Blank Line*).

## Miscellaneous Style Rules

### Semicolons

Do not use semicolons.

### `goto` and Labels

`goto` with `::continue::` labels is acceptable for loop skipping when the alternative would be deeply nested `if` blocks. Place `::continue::` at the end of the loop body, at loop-body indentation.

### Magic Numbers

Avoid unexplained numeric literals. Use named constants or inline comments:

```lua
local _1_MM_ = 1e-3
if RC__getDistance2D(pointA, pointB) < _1_MM_ then
```

### `_DEBUG_` and `_TRACE_`

- `_DEBUG_` — enables extra auxiliary objects/output during development.
- `_TRACE_` — enables verbose logging via `lib2.trace()`.

Both default to `false` in production code.

---

# PART B — TECHNICAL PATTERNS AND API

> For complete function signatures, see `.claude/references/080-luacommands.html`.

## Overview: Two Lua Contexts

| Aspect | Property Formulas | Scripts |
|--------|-------------------|---------|
| Context | `LuaContext` (sandboxed, per-object) | `RunScriptLuaContext` (full drawing access) |
| `this` | Current railway object | Not available |
| Scope | Read-only computation | Create, modify, delete objects |
| API | ~88 object-level functions | All object-level + ~60 script-only |

## User Interaction

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

**Object selection loop:**
```lua
local objects = {}
repeat
    local obj = askForPointObject("Select a point object (Enter when done)")
    if obj then table.insert(objects, obj) end
until not obj
```

**Point picking** — always check for nil (user may press Escape):
```lua
local point = askForPoint("Click insertion point:")
if not point then return end
```

**File/folder dialogs:**
```lua
local filename = askForFileName("Select Excel file")
local folder = askForFolderName("Select output folder")
```

**Value input with default:**
```lua
local mileage = askForDouble("Enter start mileage [m]:", 0)
```

## Object Creation and Manipulation

### Alignment Creation

```lua
-- From coordinate points:
local points = {getPoint3D(x1, y1, z1), getPoint3D(x2, y2, z2)}
local track = insertAlignment(rctype_Track, "Variant Name", points)

-- From geometry segments:
local segments = {}
table.insert(segments, createLineSegment(p1, p2))
table.insert(segments, createCurveSegment2(p2, directionDeg, p3))
local track = insertAlignment(rctype_Track, "Variant Name", createHorizontalGeometry(segments))
```

### Point Object Creation

```lua
local signal = insertPointObject(track, rctype_Signal, "Signal variant", pos, 3.5, true)
-- Args: alignment, rctype, variant, position, distFromAlignment, leftSide
```

### Property Assignment

**Formula reset pattern** — clear a DNA formula before assigning a literal:
```lua
alignment.code = "="         -- Remove formula
alignment.code = trackName   -- Set value
```

**Force save/update:**
```lua
alignment.name = alignment.name  -- Triggers recalculation
```

### Profiles

Vertical, cant, speed, and mileage profiles follow the same pattern:
```lua
local events = {}
table.insert(events, createVerticalEvent(pos, elevation))
createVerticalProfile(events)
-- Similarly: createCantEvent/Profile, createSpeedEvent/Profile
-- Mileage requires eventType: createMileageEvent(pos, value, "Milepost")
```

### Other Operations

```lua
setRelation(sourceObj, targetObj, "RelationType")
eraseObject(obj)
updateAlignmentVerticalData(alignment, pointList)
```

## File I/O

### Excel Reading

```lua
local file = getFileFromPath(FileType.Excel, filename)
local sheets = getExpandoObjectPropertyNames(file)
local items = file[sheets[0]]  -- 0-indexed!
local nItems = getCollectionLength(items)

for i = 0, nItems - 1 do
    local row = items[i]
    local x = row["X"]  -- Column access by header name
end
```

### Text and XML

```lua
-- Text:
local file = getFileFromPath(FileType.Text, filename)
for s in file:gmatch("[^\r\n]+") do ... end

-- XML:
local xml = getFileFromPrompt(FileType.Xml, "Select XML file")
local props = getExpandoObjectPropertyNames(xml)
```

### JSON

```lua
local data = deserializeJson(filename, targetObject)
local jsonString = exportToJson(luaTable)
exportStringToFile(jsonString, "output.json")
```

### File Output and Directories

```lua
exportStringToFile(contentString, "output.txt")
local files = getFilesInFolder(folderPath)
local folders = getFoldersInFolder(folderPath)
```

## Running Commands

Commands require a **trailing space** to execute:

```lua
runCommand("_COMMANDNAME args ")  -- Note trailing space!
```

### Return Value

```lua
local result = runCommand("_RC-ShowVersion  ")
local logOutput = result.log
```

### Common AutoCAD Commands

```lua
runCommand("_PICKADD 0 ")    -- Disables PICKADD
runCommand("_FILEDIA 1 ")    -- Normal file dialogs
runCommand("_-OSNAP _END,_MID,_CEN,_NOD,_INT,_PER,_TAN,_NEA ")
runCommand("_z _e ")          -- Zoom extents
runCommand("_QSAVE ")         -- Save
```

### LISP Wrapping and RC Commands

```lua
runCommand('(command "._ZOOM" "_EXTENTS") ')
runCommand("RC-CommandName ")  -- See 050-commands.html
```

### Version Checking

```lua
local tmp = runCommand("_RC-ShowVersion  ").log
local rcVersion = tmp:match("version (%d+%.%d+)%.%d+%.%d+")
```

### Undo Buffer Grouping

Wrap multi-object operations in `beginUndoBufferItem()` / `endUndoBufferItem()`:

```lua
beginUndoBufferItem()
for _, track in pairs(tracks) do
    -- ... create or modify objects ...
end
endUndoBufferItem()
```

**Important:** `runCommand()` breaks undo grouping — never place it between `beginUndoBufferItem()` and `endUndoBufferItem()`.

## Advanced Patterns

### Selection Sets

```lua
local created = {}
-- ... insert objects into created ...
setSelectionSet(created)
```

### External Libraries

```lua
local shapefile = runExternalLibraryFunction("DotSpatial.Data.Shapefile", "OpenFile", {path})
local obj = createExternalLibraryObject("Namespace.Type", {}, {Prop = value})
```

### CAD Entity Creation

```lua
local point = cadInterface.createCadEntity("Geometry.Point3d", {x, y, z})
local circle = cadInterface.createCadEntity("DatabaseServices.Circle", {point, vector, radius})
local line = cadInterface.createCadEntity("DatabaseServices.Line", {getAcadPoint3D(p1), getAcadPoint3D(p2)})

-- Polyline:
local polyline = cadInterface.createCadEntity("DatabaseServices.Polyline", {})
for i, v in ipairs(points) do
    polyline:AddVertexAt(i - 1, cadInterface.createCadEntity("Geometry.Point2d", {v.X, v.Y}), 0, 0, 0)
end

cadInterface.addEntitiesToModelSpace({circle, line, polyline})
```

### Block Operations

```lua
cadInterface.createBlock(blockName, entities)
local ref = cadInterface.createBlockReference(blockName, insertionPoint)
ref.ScaleFactors = cadInterface.createCadEntity("Geometry.Scale3d", {scaleFactor})
if not cadInterface.blockExist(blockName) then ... end
local cadEntity = cadInterface.getCadEntityFromRcObject(rcObject)
```

### Table Operations

```lua
local luaTable = table.select(rcCollection, function(x) return x end)
local filtered = table.where(items, function(x) return x.RcType == rctype_Signal end)
local first = table.firstOrNil(items, function(x) return x.code == "V1" end)
table.sort(items, function(a, b) return a.Mileage < b.Mileage end)
```

### Coordinate Conversions

```lua
local ai = getAlignmentInfo(alignment.id, point)
if ai.NormalProjectionExists then
    local pos = ai.RelativePosition
    local mileage = ai.Mileage
end
local wcsVector = getWcsVectorFromAcsVector(obj, lateralOffset, longitudinalOffset)
```

## Error Handling

Return `nil` on expected failures so the caller can check:

```lua
local paths = getShapefilePaths(folder)
if not paths then return end
```

- **`lib2.show(msg, nil, _error)`** + `return` for recoverable errors.
- **`lib2.stop(msg)`** for unrecoverable errors that halt the script.

## Real Examples

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



---SCRIPT---

-- Select file:
local filename = askForFileName("Select Excel file with X, Y columns")
local file = getFileFromPath(FileType.Excel, filename)
local sheets = getExpandoObjectPropertyNames(file)
local items = file[sheets[0]]
local nItems = getCollectionLength(items)

-- Select template object:
lib2.show("Select an existing object as template.")
local template = askForObject("Select template object")
if template.Alignment == nil then
    lib2.show("Aborting: template must have an alignment.", nil, _error)
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
        lib2.writeln(string.format("Skipping row %d: invalid data.", i + 1), _warning)
        goto continue
    end

    local obj = insertPointObject(
        template.Alignment, template.RcType, template.Variant, getPoint3D(x, y))
    if obj then
        if z and type(z) == "number" then
            obj.VerticalOffset = "="
            obj.VerticalOffset = z
        end
        table.insert(objTable, obj)
        nCreated = nCreated + 1
    end

    ::continue::
end

endUndoBufferItem()
setSelectionSet(objTable)
lib2.show(nCreated .. " objects created out of " .. nItems .. " rows.")
```

## Common Pitfalls

### 0-Indexed RC Collections vs 1-Indexed Lua Tables

RC collections (from `getRelatedObjects()`, `getFileFromPath()`, `filter()`, etc.) are **0-indexed**. Lua tables from `table.insert()` are **1-indexed**.

```lua
-- RC collection: for i = 0, getCollectionLength(items) - 1 do
-- Lua table: luaTable[1] is the first element
```

Also watch for: `askForPoint()` returning nil on Escape (see *User Interaction*), and `runCommand()` requiring a trailing space (see *Running Commands*).

### string.format() Patterns

```lua
string.format("%.3f", value)     -- "1.234"
string.format("%04d", number)    -- "0042"
string.format("%d: %s", i, msg)  -- Integer and string
```

## Constants Reference

```lua
-- FileType:
FileType.Excel, FileType.Xml, FileType.Text, FileType.Csv, FileType.Json, FileType.Lua

-- Output symbols:
_noSymbol, _ok, _warning, _error
```

## Documentation Reference

- **Full API reference**: `.claude/references/080-luacommands.html`
- **Lua tutorial**: `.claude/references/070-lua.html`
- **RC commands**: `.claude/references/050-commands.html`
- **Debugger**: `.claude/references/080-luadebugger.html`
