---
name: lua-scripting
description: Guide for writing standalone Lua scripts in DNA repositories -- user interaction, object creation, file I/O, AutoCAD commands, and CAD entity drawing
---

# Lua Scripts Guide

This guide covers writing standalone Lua scripts that execute actions in the drawing. Scripts run in the `RunScriptLuaContext` which provides all object-level API functions plus ~60 script-only functions for user interaction, object manipulation, file I/O, and CAD entity creation.

$ARGUMENTS

For the full API reference, see `.claude/documentation/080-luacommands.html`. For RC commands, see `.claude/documentation/050-commands.html`. For debugger usage, see `.claude/documentation/080-luadebugger.html`.

## Overview

Scripts differ from property formulas in several ways:

| Aspect | Property Formulas | Scripts |
|--------|-------------------|---------|
| Context | `LuaContext` (sandboxed, per-object) | `RunScriptLuaContext` (full drawing access) |
| Execution | Synchronous, triggered on property eval | Async, user-initiated |
| `this` | Current railway object | Not available (select objects explicitly) |
| Scope | Read-only computation | Create, modify, delete objects |
| API | ~88 object-level functions | All object-level + ~60 script-only functions |

## Script Structure

### Standard Template

```lua
--[[
    Script Title
    ============
    Brief description of what the script does.

    2025-01-15 v1.0 AUTHOR Created
    2025-03-20 v1.1 AUTHOR Added feature X
--]]

-- Helper functions
function writeln(t) write((t or "") .. "\n") end
function show(t) writeln(t) askForKeyword(t, {"OK"}) end

-- Constants
local _HEADER_ = "Script Title"

-- Display initial message
show([[
    Script Title
    ============
    Description of usage, inputs, outputs.

    - Step 1: Select a file
    - Step 2: Select a template object
    - Step 3: Objects are created
]])

-- Main logic
-- ...
```

### Helper Functions

**writeln** — output with newline:
```lua
function writeln(t) write((t or "") .. "\n") end
```

**writeln with status symbol**:
```lua
function writeln(t, symbol) write((t or "") .. "\n", symbol or _noSymbol) end
```

**show** — modal dialog:
```lua
function show(t) writeln(t) askForKeyword(t, {"OK"}) end
```

**show with abort option**:
```lua
function show(t)
    writeln(t)
    local r = askForKeyword(t, {"OK", "Abort"})
    if r == "Abort" then Halt() end
end
```

### Constants

```lua
local _HEADER_ = "Script Title"
local _VERSION_ = "1.0"
local _DEBUG_ = false
local _YES_ = "Yes"
local _NO_ = "No"
local _HELP_ = "Help"
local _TERMINATE_ = "Terminate"
```

## User Interaction

### askForKeyword — Multiple Choice Dialog

```lua
local option = askForKeyword("Select action:", {"Option 1", "Option 2", "Abort"})

-- With header/title
local option = askForKeyword("Choose format:", {"Excel", "CSV", "JSON"}, _HEADER_)
```

**Menu loop pattern:**
```lua
local option
repeat
    option = askForKeyword("Select action:", {"Insert", "Help", "Exit"}, _HEADER_)

    if option == "Help" then
        show("Help text here...")
    elseif option == "Insert" then
        -- do work
    end
until option == "Exit" or option == nil
```

### askForObject / askForPointObject — Object Selection

```lua
local obj = askForObject("Select an object in the drawing")
local pointObj = askForPointObject("Select a point object")

-- Multiple objects
local objects = askForPointObjects("Select point objects (press Enter when done)")
```

### askForAlignment — Alignment Selection

```lua
local alignment = askForAlignment("Select reference alignment: ")

-- Get the underlying CAD polyline
local polyline = cadInterface.getCadEntityFromRcObject(alignment)
```

### askForPoint — Point Picking

**Important**: `askForPoint()` throws an exception if the user presses Escape. Wrap in a nil check if cancellation should be graceful:

```lua
local point = askForPoint("Click insertion point:")
if not point then return end
```

### askForDouble / askForInteger / askForString — Value Input

```lua
local radius = askForDouble("Enter search radius [m]:")
local count = askForInteger("Enter number of objects:")
local name = askForString("Enter a name:")

-- With default value
local mileage = askForDouble("Enter start mileage [m]:", 0)
```

### askForFileName / askForFolderName — File/Folder Dialogs

```lua
local filename = askForFileName("Select Excel file with coordinates")
local folder = askForFolderName("Select output folder")

-- Optional file (Escape to skip)
local optionalFile = askForFileName("Select file (Escape to skip)")
if optionalFile then
    -- process file
end
```

### showMessage — Display Message

```lua
showMessage("Operation completed successfully.")
```

## Object Creation and Manipulation

### Alignment Creation

```lua
-- From geometry segments
local segments = {}
table.insert(segments, createLineSegment(p1, p2))
table.insert(segments, createCurveSegment2(p2, directionDeg, p3))
local geometry = createHorizontalGeometry(segments)
local track = createAlignmentObject(rctype_Track, "Variant Name", geometry)

-- From coordinate points (implicit geometry)
local points = {getPoint3D(x1, y1, z1), getPoint3D(x2, y2, z2), getPoint3D(x3, y3, z3)}
local track = createAlignmentObject(rctype_Track, "Variant Name", points)

-- Set properties after creation
track.code = "="        -- reset formula first
track.code = "V1"       -- then assign value
track.name = track.name -- force save and update derived fields
```

### Geometry Segments

```lua
-- Straight line
local line = createLineSegment(startPoint, endPoint)

-- Circular arc (from start point, direction, end point)
local arc = createCurveSegment2(startPoint, directionDegrees, endPoint)

-- Clothoid/spiral
local clothoid = createClothoidSegment(startPoint, directionDegrees, length, startRadius, A, isPositive)

-- Combine into horizontal geometry
local geometry = createHorizontalGeometry(segments)
```

### Point Object Creation

```lua
-- createPointObject(alignment, rctype, variant, position, distFromAlignment, leftSide)
local signal = createPointObject(
    track,
    rctype_Signal,
    "Signal classique, cible ACFH",
    pos,
    3.5,    -- distance from alignment [m]
    true    -- left side of track
)

-- insertPointObject (modern, simpler API)
local obj = insertPointObject(alignment, rctype, variant, position)
local obj = insertPointObject(alignment, rctype, position)  -- no variant
```

### Property Assignment

**Direct assignment:**
```lua
signal.name = "Signal A"
signal.DrawTail = true
signal.dir = "down"
object.VerticalOffset = 5.5
```

**Formula binding pattern** — reset formula, then assign:
```lua
obj.name = "="          -- remove any existing formula
obj.name = "New Name"   -- assign concrete value

obj.VerticalOffset = "=" -- remove elevation formula
obj.VerticalOffset = z   -- assign explicit value
```

**Force save/update:**
```lua
alignment.name = alignment.name  -- triggers save and recalculation of derived fields
```

### Relations

```lua
setRelation(sourceObj, targetObj, "RelationType")
```

### Profile Creation

```lua
-- Vertical profile
local events = {}
table.insert(events, createVerticalEvent(pos1, elevation1))
table.insert(events, createVerticalEvent(pos2, elevation2))
createVerticalProfile(alignment, events)

-- Cant profile
local cantEvents = {}
table.insert(cantEvents, createCantEvent(pos, cantValue))
createCantProfile(alignment, cantEvents)

-- Speed profile
local speedEvents = {}
table.insert(speedEvents, createSpeedEvent(pos, speedKmh))
createSpeedProfile(alignment, speedEvents)

-- Mileage profile
local mileageEvents = {}
table.insert(mileageEvents, createMileageEvent(pos, mileageValue))
createMileageProfile(alignment, mileageEvents)

-- Update vertical data from point list
updateAlignmentVerticalData(alignment, table.select(pointList, function(p)
    return getPoint3D(p.X, p.Y, p.Z)
end))
```

### Deletion

```lua
eraseObject(obj)
```

## File I/O

### Excel Reading

```lua
-- 1. Open file
local filename = askForFileName("Select Excel file")
local file = getContentsFromFile(FileType.Excel, "", filename)

-- 2. Get sheet names
local sheets = getExpandoObjectPropertyNames(file)
local sheetName = sheets[0]  -- 0-indexed!

-- 3. Get rows
local items = file[sheetName]
local nItems = getCollectionLength(items)

-- 4. Process rows
for i = 0, nItems - 1 do
    local row = items[i]
    local x = row["X"]       -- column access by header name
    local y = row["Y"]
    local z = row["Z"]       -- nil if column doesn't exist

    if type(x) == "number" and type(y) == "number" then
        -- process valid row
    else
        writeln(string.format("Skipping row %d: invalid data", i + 1))
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
    writeln(props[i] .. " = " .. tostring(value))
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
-- Read JSON
local data = deserializeJson(jsonString)

-- Export to JSON
exportToJson(luaTable, "output.json")
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

## Running Commands

### Pattern

Commands require a **trailing space** to execute:

```lua
runCommand("_COMMANDNAME args ")  -- note trailing space
```

### Return Value

```lua
local result = runCommand("_RC-ShowVersion  ")
local logOutput = result.log       -- command output text
local status = result.result       -- execution status
```

### Common AutoCAD Commands

```lua
-- System variables
runCommand("_PICKADD 0 ")
runCommand("_FILEDIA 1 ")
runCommand("_ORTHOMODE 0 ")
runCommand("_GRID _OFF ")
runCommand("_SNAP _OFF ")
runCommand("_NAVVCUBE _OFF ")
runCommand("_DYNMODE 3 ")
runCommand("_SELECTIONCYCLING 2 ")

-- Units setup
runCommand('_-UNITS 2 3 1 3 0 _NO ')

-- Object snap modes
runCommand("_-OSNAP _END,_MID,_CEN,_NOD,_INT,_PER,_TAN,_NEA ")

-- Color
runCommand("_-COLOR _BYLAYER ")

-- Drawing
runCommand("_CIRCLE " .. x .. "," .. y .. " " .. radius .. " ")

-- Zoom
runCommand("_z _e ")       -- zoom extents
runCommand("_zoom _e\n")   -- alternate form

-- Regen / Purge
runCommand("_REGEN ")
runCommand("_-PURGE _ALL * _NO ")

-- Save
runCommand("_QSAVE ")
```

### LISP Wrapping

For commands that need LISP syntax:

```lua
runCommand('(command "._ZOOM" "_EXTENTS") ')
```

### RC Commands

```lua
runCommand("RC-CommandName ")  -- see 050-commands.html for full list
```

### Version Checking

```lua
local tmp = runCommand("_RC-ShowVersion  ").log
local rcVersion = tmp:match("version (%d+%.%d+)%.%d+%.%d+")
```

## Advanced Patterns

### Undo Buffer

Wrap modifications in an undo group so they can be undone as one step:

```lua
beginUndoBufferItem()

-- create/modify objects here
local obj = insertPointObject(alignment, rctype, variant, position)
obj.name = "="
obj.name = "New Name"

endUndoBufferItem()
```

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
-- Open a shapefile via DotSpatial
local shapefile = runExternalLibraryFunction(
    "DotSpatial.Data.Shapefile",
    "OpenFile",
    {shapefilePath}
)

-- Create a .NET object
local milepost = createExternalLibraryObject(
    "RailCOMPLETE.Model.GeometryModels.Milepost",
    {},  -- constructor args
    {Name = "1.0KM", Pos = 1000, Mileage = 1000}  -- property assignments
)
```

### Library Inclusion

```lua
lib1 = includeLuaFile("Lua\\Functions\\lib1.lua")
lib2 = includeLuaFile("Lua\\Functions\\lib2.lua")

-- Use library functions
lib2.zoomExtents()
local id = RC__identify(obj)
```

### CAD Entity Creation

Create raw AutoCAD entities:

```lua
-- Points and vectors
local point = cadInterface.createCadEntity("Geometry.Point3d", {x, y, z})
local vector = cadInterface.createCadEntity("Geometry.Vector3d", {0, 0, 1})
local point2d = cadInterface.createCadEntity("Geometry.Point2d", {x, y})

-- Circle
local circle = cadInterface.createCadEntity("DatabaseServices.Circle", {point, vector, radius})

-- Line
local line = cadInterface.createCadEntity("DatabaseServices.Line", {
    getAcadPoint3D(p1), getAcadPoint3D(p2)
})

-- Polyline
local polyline = cadInterface.createCadEntity("DatabaseServices.Polyline", {})
local index = 0
for _, v in pairs(points) do
    polyline:AddVertexAt(index, cadInterface.createCadEntity("Geometry.Point2d", {v.X, v.Y}), 0, 0, 0)
    index = index + 1
end
polyline.Closed = true

-- MText
local text = cadInterface.createCadEntity("DatabaseServices.MText", {})
text.Height = 2.0
text.TextHeight = 1.25
text.TextStyleId = cadInterface.getTextStyleId("ISO")
text.Contents = "Line 1\\PLine 2"  -- \\P = newline
text.Attachment = cadInterface.createCadEntity("DatabaseServices.AttachmentPoint", {"TopRight"})
text.Location = getAcadPoint3D(textPosition)

-- Dimension
local dim = cadInterface.createCadEntity("DatabaseServices.AlignedDimension", {})
dim.XLine1Point = getAcadPoint3D(p1)
dim.XLine2Point = getAcadPoint3D(p2)
dim.DimLinePoint = getAcadPoint3D(dimPoint)
dim.DimensionText = string.format("%.0f", distance)

-- Add entities to drawing
cadInterface.addEntitiesToModelSpace({circle, line, polyline, text})
```

### Block Operations

```lua
-- Create a block
local blockName = "myBlock_" .. generateGuid()
cadInterface.createBlock(blockName, entities)

-- Create block reference (instance)
local ref = cadInterface.createBlockReference(blockName, insertionPoint)
ref.ScaleFactors = cadInterface.createCadEntity("Geometry.Scale3d", {scaleFactor})

-- Check if block exists
if not cadInterface.blockExist(blockName) then
    cadInterface.createBlock(blockName, entities)
end

-- Get CAD entity from RC object
local cadEntity = cadInterface.getCadEntityFromRcObject(rcObject)
```

### Table Operations

These extend Lua's built-in table library:

```lua
-- Transform collection to Lua table
local luaTable = table.select(rcCollection, function(x) return x end)

-- Filter
local filtered = table.where(items, function(x) return x.RcType == rctype_Signal end)

-- Get first match
local first = table.firstOrNil(items, function(x) return x.code == "V1" end)

-- Sort
table.sort(items, function(a, b) return a.Mileage < b.Mileage end)
```

### Coordinate Conversions

```lua
-- Get alignment-relative coordinates
local linearAddr = getLinearAddress(worldPosition, alignment)
-- linearAddr.DistanceAlong, .LateralOffset, .LongitudinalOffset, .VerticalOffset

-- Get alignment info at a point
local ai = alignment:getAlignmentInfo(point)
if ai.NormalProjectionExists then
    local pos = ai.RelativePosition
    local mileage = ai.Mileage
end

-- WCS vector from ACS vector
local wcsVector = getWcsVectorFromAcsVector(obj, lateralOffset, longitudinalOffset)
```

## Real Examples

### Import Objects from Excel

```lua
--[[
    Insert objects at XY coordinates from Excel
    ============================================
    2025-01-15 v1.0 AUTHOR Created
--]]

function writeln(t) write((t or "") .. "\n") end
function show(t) writeln(t) askForKeyword(t, {"OK"}) end

show([[
    Insert objects at XY(Z) coordinates from Excel
    ===============================================
    Input: Excel file with columns X, Y (and optionally Z).
    Usage: Select file, select template object, objects are inserted.
]])

-- Select file
local filename = askForFileName("Select Excel file with X, Y columns")
local file = getContentsFromFile(FileType.Excel, "", filename)
local sheets = getExpandoObjectPropertyNames(file)
local sheetName = sheets[0]
local items = file[sheetName]
local nItems = getCollectionLength(items)

show(nItems .. " rows found in sheet '" .. sheetName .. "'")

-- Select template object
show("Select an existing object as template. Its RcType and Variant will be used.")
local template = askForObject("Select template object")
if template.Alignment == nil then
    show("Aborting: template object must have an alignment.")
    return
end

-- Process rows
local objTable = {}
local nCreated = 0

beginUndoBufferItem()

for i = 0, nItems - 1 do
    local row = items[i]
    local x = row["X"]
    local y = row["Y"]
    local z = row["Z"]

    if type(x) ~= "number" or type(y) ~= "number" then
        writeln(string.format("Skipping row %d: X='%s' or Y='%s' is not a number.",
            i + 1, tostring(x), tostring(y)))
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
        writeln(string.format("%d: Created at (%.3f, %.3f)", i + 1, x, y))
    else
        writeln(string.format("Row %d: Failed to create object at (%.3f, %.3f)", i + 1, x, y))
    end

    ::continue::
end

endUndoBufferItem()

setSelectionSet(objTable)
show("\n" .. nCreated .. " objects created out of " .. nItems .. " rows.")
```

### Batch Create Tracks from Shapefile

```lua
--[[
    Import tracks from Shapefile
    ============================
    2025-02-01 v1.0 AUTHOR Created
--]]

lib1 = includeLuaFile("Lua\\Functions\\lib1.lua")
lib2 = includeLuaFile("Lua\\Functions\\lib2.lua")

local _HEADER_ = "Import Tracks"
local _TRACK_VARIANT_ = "Traverses et rails - 3D simple"

show("Import tracks from a Shapefile (.shp)")

local shapefilePath = askForFileName("Select the Shapefile (.shp)")
local shapefile = runExternalLibraryFunction(
    "DotSpatial.Data.Shapefile", "OpenFile", {shapefilePath}
)
local shapes = table.select(shapefile.ShapeIndices)
local geometries = table.select(shapefile.Features, function(x) return x.Geometry end)

writeln("Importing from: " .. shapefilePath)

beginUndoBufferItem()

for _, shape in pairs(shapes) do
    local recordNumber = shape.RecordNumber
    local trackName = tostring(shapefile.Attributes.Table.Rows[recordNumber - 1].ItemArray[2])

    -- Skip very short segments (derailers)
    if shape.NumParts == 1 and shape.NumPoints == 2 and
       math.abs(geometries[recordNumber].Length - 2.0) < 0.1 then
        writeln("Skipping derailer: " .. trackName, _warning)
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
            alignment.name = alignment.name  -- force save
            writeln("Created track: " .. trackName)
        else
            writeln("Failed to create track: " .. trackName, _error)
        end
    end

    ::continue::
end

endUndoBufferItem()
runCommand("_z _e ")
show("Import complete")
```

### Draw AutoCAD Circles from Coordinates

```lua
--[[
    Insert circles at XY coordinates
    =================================
    2025-01-10 v1.0 AUTHOR Created
--]]

function writeln(t) write((t or "") .. "\n") end
function show(t) writeln(t) askForKeyword(t, {"OK"}) end

local filename = askForFileName("Select Excel file with X, Y columns")
local file = getContentsFromFile(FileType.Excel, "", filename)
local sheets = getExpandoObjectPropertyNames(file)
local items = file[sheets[0]]
local nItems = getCollectionLength(items)
local radius = 0.5

-- Check RC version for undo buffer support
local tmp = runCommand("_RC-ShowVersion  ").log
local rcVersion = tmp:match("version (%d+%.%d+)%.%d+%.%d+")

for i = 0, nItems - 1 do
    local x = items[i]["X"]
    local y = items[i]["Y"]

    if type(x) == "number" and type(y) == "number" then
        if rcVersion > "2024.2" then
            beginUndoBufferItem()
            local insertionPoint = cadInterface.createCadEntity("Geometry.Point3d", {x, y, 0})
            local normalVector = cadInterface.createCadEntity("Geometry.Vector3d", {0, 0, 1})
            local circle = cadInterface.createCadEntity("DatabaseServices.Circle",
                {insertionPoint, normalVector, radius})
            cadInterface.addEntitiesToModelSpace({circle})
            endUndoBufferItem()
        else
            runCommand("_CIRCLE " .. x .. "," .. y .. " " .. radius .. " ")
        end
        writeln(string.format("%04d Inserted circle at (%.3f, %.3f)", i + 1, x, y))
    end
end

show("Done. " .. nItems .. " rows processed.")
```

## Common Pitfalls

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

### askForPoint() Cancel Exception

`askForPoint()` throws an exception when the user presses Escape. Always check for nil:

```lua
local point = askForPoint("Click a point:")
if not point then return end
```

### Trailing Space in runCommand()

Commands won't execute without a trailing space:

```lua
runCommand("_CIRCLE 0,0 10 ")  -- works (trailing space)
runCommand("_CIRCLE 0,0 10")   -- may hang waiting for input!
```

### Formula Reset Before Assignment

To replace a formula-bound property with a concrete value, first reset it with `"="`:

```lua
obj.VerticalOffset = "="    -- remove formula
obj.VerticalOffset = 5.0    -- assign value
```

### goto continue / ::continue:: Pattern

Lua has no `continue` keyword. Use `goto` with a label at the end of the loop body:

```lua
for i = 0, n - 1 do
    if shouldSkip then
        goto continue
    end

    -- process item

    ::continue::
end
```

### string.format() Patterns

```lua
string.format("%.3f", value)     -- 3 decimal places: "1.234"
string.format("%04d", number)    -- 4-digit zero-padded: "0042"
string.format("%d: %s", i, msg)  -- integer and string
```

### Force Save After Property Changes

After modifying alignment properties, force a save by re-assigning name:

```lua
alignment.code = "="
alignment.code = "V1"
alignment.name = alignment.name  -- triggers save and derived field update
```

## FileType Constants

```lua
FileType.Excel    -- .xlsx files
FileType.Xml      -- .xml files
FileType.Text     -- .txt files
FileType.Csv      -- .csv files
FileType.Json     -- .json files
FileType.Lua      -- .lua files
```

## Output Symbols

```lua
_noSymbol    -- standard output
_ok          -- green checkmark
_warning     -- yellow triangle
_error       -- red X
```

## Documentation Reference

- **Full API reference**: `.claude/documentation/080-luacommands.html` — all object-level + script-only functions
- **Lua language tutorial**: `.claude/documentation/070-lua.html` — syntax, types, control structures
- **RC commands**: `.claude/documentation/050-commands.html` — commands available via `runCommand()`
- **Debugger**: `.claude/documentation/080-luadebugger.html` — breakpoints, watches, stepping
