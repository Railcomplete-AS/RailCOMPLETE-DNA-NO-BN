---
name: lua-properties
description: Guide for writing Lua-controlled property formulas in DNA XML files -- LuaFunction declarations, LuaExpression bindings, TextPosition/TextRotation formulas, and model checks
---

# Lua Controlled Properties Guide

This guide covers writing Lua code in the DNA context: reusable `<LuaFunction>` declarations, inline `<LuaExpression>` property formulas, and TextPosition/TextRotation formulas. All code runs in a sandboxed per-object context where `this` properties are available as globals.

$ARGUMENTS

For the full API reference, see `.claude/documentation/080-luacommands.html`. For Lua language basics, see `.claude/documentation/070-lua.html`.

## Overview

Lua-controlled properties let DNA authors compute object property values dynamically. The Lua code runs inside a sandbox with ~88 API functions available. Each evaluation has access to the current railway object (`this`) whose properties are exposed as global variables (e.g., `RcType`, `Variant`, `Alignment`, `Mileage`, `dir`, `geoCoord`).

There are three XML elements that embed Lua in DNA files:

| Element | Purpose |
|---------|---------|
| `<LuaFunction>` | Declare a reusable named function |
| `<LuaExpression>` | Bind a Lua formula to a specific property |
| TextPosition/TextRotation | Special formulas for text attribute placement |

## DNA XML Structure

### LuaFunction — Reusable Function Declarations

```xml
<LuaFunction Name="PREFIX_category_functionName()"
             ReturnType="String"
             HideFromUser="false"
             Description="Human-readable description of what this function computes.">
    <Signature>String PREFIX_category_functionName()</Signature>
    <Signature>String PREFIX_category_functionName(IBaseObject obj)</Signature>
    <Formula>
        function PREFIX_category_functionName(obj)
            obj = obj or this
            -- implementation
            return value
        end
    </Formula>
</LuaFunction>
```

**Attributes:**

| Attribute | Required | Description |
|-----------|----------|-------------|
| `Name` | Yes | Function name with `()` suffix |
| `ReturnType` | Yes | `String`, `Double`, `Boolean`, `Int`, `Tuple`, `Collection`, `ObjectRef` |
| `Description` | Yes | Tooltip shown to users |
| `HideFromUser` | No | `"true"` hides from intellisense (for internal helpers) |

**Child elements:**

| Element | Purpose |
|---------|---------|
| `<Signature>` | One or more method signatures for tooltips |
| `<Formula>` | The Lua function body (no CDATA wrapper needed) |

### LuaExpression — Property Formula Binding

Binds a Lua expression to a property by name:

```xml
<!-- Simple property formula -->
<LuaExpression Name="PropertyName">
    <Formula>PREFIX_category_functionName()</Formula>
</LuaExpression>

<!-- 3D geometry binding -->
<LuaExpression Name="Geometry3D_0.Name">
    <Formula>_myObject_Geometry3DName()</Formula>
</LuaExpression>
<LuaExpression Name="Geometry3D_0.Offset.Z">
    <Formula>_myObject_OffsetZ()</Formula>
</LuaExpression>

<!-- Model check formula -->
<LuaExpression Name="mc_SomeCheck" IsModelCheck="true">
    <Formula>PREFIX_category_chkSomething()</Formula>
</LuaExpression>
```

### TextPosition and TextRotation Formulas

These control the placement of text attributes in the drawing:

```xml
<!-- Position returns (acsX, acsY) tuple -->
<LuaExpression Name="TextAttribute_LABEL.Position">
    <Formula>_myObject_LabelTextPosition(20, 0)</Formula>
</LuaExpression>

<!-- Rotation returns angle in decimal degrees -->
<LuaExpression Name="TextAttribute_LABEL.Rotation">
    <Formula>_myObject_LabelTextRotation()</Formula>
</LuaExpression>

<!-- Simple position using built-in flip helper -->
<LuaExpression Name="TextAttribute_NAME.Position">
    <Formula>RC__flipAcsTuple(20, 0)</Formula>
</LuaExpression>
```

## Naming Conventions

### Function Name Prefixes

Use a discipline-based prefix matching the DNA administration:

```
NOBN_sig_    Signalling (NO-BN)
NOBN_ocs_    Overhead Catenary System (NO-BN)
NOBN_trk_    Track (NO-BN)
NOBN_com_    Common/shared (NO-BN)
FRSR_sig_    Signalling (FR-SR)
RC__         RailCOMPLETE general library functions
```

### Function Name Patterns

```
PREFIX_category_getSomething()     -- Getter: returns a value
PREFIX_category_chkSomething()     -- Model check: returns (message, objects, status)
_ObjectName_OffsetZ()              -- Private helper (leading underscore)
RC__toInt()                        -- Utility function
```

### Template Pattern

Use this template from `_NO-BN-TemplateFunctionDeclaration.xml` as a starting point:

```xml
<LuaFunction Name="PREFIX_category_myFunction()"
             ReturnType="String"
             HideFromUser="false"
             Description="Describe what the function computes.">
    <Signature>String PREFIX_category_myFunction()</Signature>
    <Signature>String PREFIX_category_myFunction(IBaseObject obj)</Signature>
    <Formula>
        function PREFIX_category_myFunction(obj)
            obj = obj or this
            -- implementation here
            return result
        end
    </Formula>
</LuaFunction>
```

## The `this` Object

In property formula context, the current railway object is `this`. Its properties are available as global variables:

### Identity and Type
```lua
RcType          -- e.g., "JBTSA_SIG Signal" (object type string)
Variant         -- e.g., "3-lys hovedsignal" (variant/subtype)
name            -- Display name
code            -- Identifier code
id              -- Unique GUID string
Discipline      -- Discipline category
```

### Position and Alignment
```lua
Alignment       -- Parent alignment object reference (nil if unplaced)
Mileage         -- Distance along alignment [m]
ReferenceMileage -- Mileage on reference alignment
DistanceToAlignment -- Lateral distance [m]
LateralOffset   -- Perpendicular offset
VerticalOffset  -- Vertical elevation relative to alignment
geoCoord        -- World coordinates: geoCoord.X, geoCoord.Y, geoCoord.Z
SymbolOffset    -- Symbol display offset: SymbolOffset.X, SymbolOffset.Y
AlignmentTangent -- Alignment tangent angle at object position [radians]
```

### Direction and Side
```lua
dir             -- "up", "down", "both", "none", or "unknown"
RightSided      -- true if on right side of alignment
LeftSided       -- true if on left side
MileageIncreasesTowardsLeft -- true if km increases leftward in UCS
```

### Drawing Properties
```lua
Layer           -- Layer name (e.g., "@@" prefix means historic)
Color           -- Color value
Derogation      -- Derogation state: "REQUESTED", "GRANTED", etc.
```

### Formula Access
```lua
LuaExpressions  -- Collection of all formula results on this object
                -- Each has .IsModelCheck, .Symbol, .Value properties
```

## Key API Patterns

### Object Discovery

```lua
-- Related objects (returns collection + count, 0-INDEXED)
local r, n = getRelatedObjects("RelationType")
local r, n = getRelatedObjects("RelationType", sourceObj)
if n > 0 then
    local first = r[0]  -- 0-indexed!
end

-- Topology: next/previous along alignment
local nextSignal = getUpObject(rctype_Signal)      -- next in "up" direction
local prevSignal = getDownObject(rctype_Signal)     -- next in "down" direction

-- Nearby objects (2D distance search)
local poles, nPoles = getNearbyPointObjects2D(50, rctype_OcsPole)
local tracks = getNearbyAlignments(rctype_Track)
```

### Alignment Information

```lua
local ai = getAlignmentInfo()               -- at this object's position
local ai = getAlignmentInfo(alignmentId)    -- on a specific alignment

-- Available fields:
ai.Mileage                    -- Distance along alignment [m]
ai.Elevation                  -- Vertical elevation [m]
ai.Tangent                    -- Direction vector
ai.Cant                       -- Superelevation [mm]
ai.CantRotation               -- "CW" or "CCW"
ai.DistanceToAlignment        -- Lateral distance [m]
ai.Gradient                   -- Grade percentage
ai.CurveRadius                -- Radius of curvature [m]
ai.NormalProjectionExists     -- Can project to alignment?
ai.LinearAddress.DistanceAlong -- Position along alignment
```

### Distance Calculations

```lua
local dist = getDistance(obj1, obj2)            -- along alignment
local dist2d = RC__getDistance2D(point1, point2) -- Euclidean 2D
local dist3d = RC__getDistance3D(point1, point2) -- Euclidean 3D
```

### Collections

```lua
-- Filter (returns new 0-indexed collection)
local signals = collection:filter(function(x)
    return x.RcType == rctype_Signal and x.code:match("Hs")
end)

-- Get length
local count = getCollectionLength(collection)

-- Union multiple collections
local combined = getUnionOfCollections({collection1, collection2})

-- Sort: convert to Lua table, sort, convert back
local t = {}
for i = 0, n - 1 do
    table.insert(t, items[i])
end
table.sort(t, function(a, b) return a.Mileage < b.Mileage end)
local sorted = getCollectionFromTable(t)

-- Order directly
local ordered = orderByAscending(collection, function(x) return x.Mileage end)
```

### Properties

```lua
local value = getPropertyValue("PropertyName")
local value = obj:getPropertyValue("PropertyName")
local displayName = getEnumDisplayName("EnumValue", "EnumTypeName")
local propName = fromDisplayName("Display Name")
```

### Coordinates and Vectors

```lua
local p = getPoint3D(x, y, z)
local p = getPoint3D(x, y)      -- z defaults to 0

-- Vector operations
local modulus = getVectorModulus(v)
local normalized = getVectorNormalized(v)
local dot = getVectorDotProduct(v1, v2)
local angle = getVectorAngleDD(v1, v2)  -- in decimal degrees
```

### Block Images

```lua
local img = get3DBlockImage("BlockName")
local img = getBlockImage("2DBlockName")
local names = getBlockNames("Pattern*")
local bounds = getBlockBounds("BlockName")
```

### Include Files

```lua
includeLuaFile("path/to/file.lua")  -- relative to DNA folder
```

## ACS to WCS Coordinate Conversion

The Alignment Coordinate System (ACS) uses lateral/longitudinal offsets relative to the alignment. World Coordinate System (WCS) uses absolute X/Y coordinates. Conversion is essential for TextPosition formulas.

```lua
-- ACS → WCS
local wcsX, wcsY = RC__AcsToWcs(lateralOffset, longitudinalOffset, angleDeg, obj)

-- WCS → ACS
local acsX, acsY = RC__WcsToAcs(wcsX, wcsY, angleDeg, obj)

-- Flip ACS tuple based on object side and direction
local flippedLat, flippedLong = RC__flipAcsTuple(lateralOffset, longitudinalOffset, obj)
-- Flips lateral based on RightSided/LeftSided
-- Flips longitudinal based on dir (up/down)
```

## Return Values and Status Symbols

### Simple Returns

```lua
return value                   -- just a value
return value, _info("message") -- value with tooltip info
```

### Model Check Returns

Model check functions (bound via `IsModelCheck="true"`) return status indicators:

```lua
return "OK - Everything checks out.", _ok           -- green checkmark
return "WARNING - Marginal distance.", _warning     -- yellow triangle
return "ERROR - Distance too short.", _error        -- red X
return "UNFINISHED - Missing data.", _unfinished    -- orange square

-- With related objects (highlighted in drawing)
return "ERROR - Too close.", {relatedObj}, _error

-- With info tooltip
return message, {relatedObj}, _error, _info("Expected 3.0m minimum")
```

### Symbol Constants

```lua
_ok          -- green checkmark (success)
_warning     -- yellow triangle (warning)
_error       -- red X (error)
_unfinished  -- orange square (incomplete)
_noSymbol    -- no visual indicator
_info(msg)   -- creates a tooltip message
```

## Real Examples

### Simple Property Formula — Signal Name

```xml
<LuaFunction Name="NOBN_sig_getSignalNumber()"
             ReturnType="String"
             Description="Returns the signal number derived from code.">
    <Signature>String NOBN_sig_getSignalNumber()</Signature>
    <Formula>
        function NOBN_sig_getSignalNumber(obj)
            obj = obj or this
            local c = obj:getPropertyValue("code")
            if c == nil or c == "" then
                return "", _info("No code set")
            end
            if c:match("%d+%D+") then
                return c:match("%d+%D+"):match("%d+")
            end
            return c
        end
    </Formula>
</LuaFunction>
```

### Model Check — Balise Distance

```xml
<LuaFunction Name="NOBN_sig_chkBaliseDistance()"
             ReturnType="Tuple"
             Description="Checks distance to next balise in same direction.">
    <Signature>Tuple NOBN_sig_chkBaliseDistance()</Signature>
    <Formula>
        function NOBN_sig_chkBaliseDistance(balise)
            balise = balise or this
            if balise.Alignment == nil then
                return "UNFINISHED - Missing alignment.", _unfinished
            end

            local nextBalise = getDownObject(balise.RcType)
            if nextBalise == nil then
                return "OK - No balise within reach.", _ok
            end

            local dist = getDistance(balise, nextBalise)
            local t = string.format("%.03f", dist)

            if dist < 2.35 then
                return t .. ": ERROR - Fields may merge.", {nextBalise}, _error
            elseif dist < 2.8 then
                return t .. ": WARNING - Outside tolerance.", {nextBalise}, _warning
            else
                return t .. ": OK - Within tolerance.", {nextBalise}, _ok
            end
        end
    </Formula>
</LuaFunction>

<!-- Bound as a model check -->
<LuaExpression Name="mc_BaliseDistance" IsModelCheck="true">
    <Formula>NOBN_sig_chkBaliseDistance()</Formula>
</LuaExpression>
```

### TextPosition — Midpoint Between Two Poles

```xml
<LuaFunction Name="_JBTEH_MAS_SpanlengthTextPosition()"
             ReturnType="Tuple"
             HideFromUser="true"
             Description="Position span length text at midpoint between this pole and the next.">
    <Signature>Tuple _JBTEH_MAS_SpanlengthTextPosition(Double LateralOffset, Double LongitudinalOffset)</Signature>
    <Formula>
        function _JBTEH_MAS_SpanlengthTextPosition(LateralOffset, LongitudinalOffset, obj)
            obj = obj or this
            LateralOffset = LateralOffset or 0
            LongitudinalOffset = LongitudinalOffset or 0

            local r, n = getRelatedObjects(rel_OcsPole_HasNext_OcsPole)
            if n == 0 then
                return 0, 0
            end

            local target = r[0]

            -- WCS coordinates of display points
            local startX = obj.geoCoord.X + obj.SymbolOffset.X
            local startY = obj.geoCoord.Y + obj.SymbolOffset.Y
            local endX = target.geoCoord.X + target.SymbolOffset.X
            local endY = target.geoCoord.Y + target.SymbolOffset.Y

            -- Midpoint in WCS, then convert to ACS
            local wcsX = obj.SymbolOffset.X + (endX - startX) / 2 + LongitudinalOffset
            local wcsY = obj.SymbolOffset.Y + (endY - startY) / 2 - LateralOffset
            local acsX, acsY = -wcsY, wcsX

            return RC__round(acsX, 3), RC__round(acsY, 3)
        end
    </Formula>
</LuaFunction>

<LuaExpression Name="TextAttribute_SPENNLENGDE.Position">
    <Formula>_JBTEH_MAS_SpanlengthTextPosition(20, 0)</Formula>
</LuaExpression>
```

### TextRotation — Angle Between Display Points

```xml
<LuaFunction Name="_JBTEH_MAS_SpanlengthTextRotation()"
             ReturnType="Double"
             HideFromUser="true"
             Description="Rotate span length text to follow the line between poles.">
    <Signature>Double _JBTEH_MAS_SpanlengthTextRotation()</Signature>
    <Formula>
        function _JBTEH_MAS_SpanlengthTextRotation()
            local r, n = getRelatedObjects(rel_OcsPole_HasNext_OcsPole)
            if n == 0 then
                return AlignmentTangent
            end

            local target = r[0]
            local startX = geoCoord.X + SymbolOffset.X
            local startY = geoCoord.Y + SymbolOffset.Y
            local endX = target.geoCoord.X + target.SymbolOffset.X
            local endY = target.geoCoord.Y + target.SymbolOffset.Y

            local angle = math.deg(math.atan(endY - startY, endX - startX))
            return angle + (MileageIncreasesTowardsLeft and 180 or 0)
        end
    </Formula>
</LuaFunction>
```

### Collection Filtering — Balise Position in Group

```xml
<LuaFunction Name="_JBTSA_ETB_positionInGroup()"
             ReturnType="Int"
             HideFromUser="true"
             Description="Returns the ordinal position of this balise within its group.">
    <Signature>Int _JBTSA_ETB_positionInGroup()</Signature>
    <Formula>
        function _JBTSA_ETB_positionInGroup(balise)
            balise = balise or this
            local groups, nGroups = balise:getRelatedObjects(rel_EtcsBalise_BelongsTo_EtcsBaliseGroup)

            if nGroups == 0 then
                return "?", _info("Connect balise to its group.")
            end

            local bg = groups[0]
            local balises, nBalises = bg:getRelatedObjects(rel_EtcsBaliseGroup_Contains_EtcsBalise)

            if nBalises == 1 then
                return 0
            end

            -- Sort by position along alignment
            local t = {}
            for i = 0, nBalises - 1 do
                table.insert(t, {
                    id = balises[i].id,
                    pos = balises[i]:getAlignmentInfo().LinearAddress.DistanceAlong
                })
            end
            table.sort(t, function(a, b)
                local sgn = (bg.dir == "up" and 1 or -1)
                return sgn * a.pos < sgn * b.pos
            end)

            t = getCollectionFromTable(t)
            for i = 0, nBalises - 1 do
                if balise.id == t[i].id then
                    return i, _info("Position #" .. i .. " in direction '" .. bg.dir .. "'")
                end
            end
        end
    </Formula>
</LuaFunction>
```

### Symbol Frame Logic — Variant and Model Check Aggregation

```lua
function NOBN_com_setSymbolFrame()
    local language = 2  -- 1:EN, 2:NO, 3:FR, 4:DE
    local frames = {
        UNFINISHED = {"Unfinished", "Unfinished", "Incomplet", "Unvollstaendig"},
        WARNING    = {"Warning",    "Warning",    "Avertissement", "Warnung"},
        ERROR      = {"Error",      "Error",      "Erreur",    "Fehler"},
        REQUESTED  = {"Requested",  "Requested",  "Demandee",  "Nachgefragt"},
        GRANTED    = {"Granted",    "Granted",    "Accordee",  "Zugesagt"},
    }

    if tostring(Layer):sub(1, 2) == "@@" then
        return frames.HISTORIC[language]
    end

    local modelChecks = LuaExpressions:filter(function(x) return x.IsModelCheck end)

    if modelChecks:filter(function(x) return x.Symbol == "_unfinished" end).Count > 0 then
        return frames.UNFINISHED[language]
    elseif Derogation == "REQUESTED" then
        return frames.REQUESTED[language]
    elseif Derogation == "GRANTED" then
        return frames.GRANTED[language]
    elseif modelChecks:filter(function(x) return x.Symbol == "_error" end).Count > 0 then
        return frames.ERROR[language]
    elseif modelChecks:filter(function(x) return x.Symbol == "_warning" end).Count > 0 then
        return frames.WARNING[language]
    else
        return ""
    end
end
```

## Common Pitfalls

### 0-Indexed Collections
RailCOMPLETE collections from `getRelatedObjects()`, `filter()`, etc. are **0-indexed**. Lua tables created with `table.insert()` are 1-indexed.

```lua
-- WRONG
local items, n = getRelatedObjects(rel)
for i = 1, n do         -- misses index 0!
    local item = items[i]
end

-- CORRECT
for i = 0, n - 1 do
    local item = items[i]
end
```

### Nil Checks Before Property Access
Always check that alignment and related objects exist before accessing properties:

```lua
-- WRONG
local elevation = getAlignmentInfo().Elevation

-- CORRECT
if Alignment == nil then
    return "UNFINISHED - No alignment.", _unfinished
end
local ai = getAlignmentInfo()
if ai == nil or RC__isNan(ai.Elevation) then
    return 0, _info("Missing elevation")
end
```

### Default Parameters
Always default `obj` to `this`:

```lua
function myFunction(obj)
    obj = obj or this    -- essential!
    -- ...
end
```

### String Comparisons
Use `tostring()` when comparing property values that might not be strings:

```lua
if tostring(Layer):sub(1, 2) == "@@" then
```

### String Formatting
Use `string.format()` for numeric formatting:

```lua
string.format("%.3f", value)    -- 3 decimal places
string.format("%04d", number)   -- 4-digit zero-padded
```

### Multi-line Text
Use `\\P` for line breaks in MText display values:

```lua
return "Line 1\\PLine 2\\PLine 3"
```

## Common Object Type Constants

```lua
rctype_Signal                    rctype_Track
rctype_EtcsBalise                rctype_EtcsBaliseGroup
rctype_Balise                    rctype_BaliseGroup
rctype_OcsPole                   rctype_Cantilever
rctype_ContactWire               rctype_Switch
rctype_WireTensioningBalancer    rctype_InnerRailAndInsulation
```

## Common Relation Type Constants

```lua
rel_Signal_Has_BaliseGroup
rel_EtcsBalise_BelongsTo_EtcsBaliseGroup
rel_EtcsBaliseGroup_Contains_EtcsBalise
rel_OcsPole_HasNext_OcsPole
rel_OcsPole_Has_Cantilever
rel_Cantilever_Holds_ContactWire
rel_CantileverOrWtb_HasNext_CantileverOrWtb
rel_Label_AppliesTo_Anything
```

## Utility Functions Reference

```lua
RC__toInt(x)                     -- convert to integer
RC__round(x, decimals)           -- round to N decimal places
RC__isNan(x)                     -- check if NaN
RC__identify(obj)                -- get name, code, or id (best available)
RC__sub(s, i, j)                 -- multi-byte safe substring
RC__flipAcsTuple(lat, long, obj) -- flip offsets by side/direction
RC__AcsToWcs(lat, long, deg, obj)-- ACS to WCS conversion
RC__WcsToAcs(x, y, deg, obj)    -- WCS to ACS conversion
RC__getDistance2D(p1, p2)        -- Euclidean 2D distance
RC__getDistance3D(p1, p2)        -- Euclidean 3D distance
RC__isMemberOf(coll, item)       -- membership test
```

## Documentation Reference

- **Full API reference**: `.claude/documentation/080-luacommands.html` — all ~88 object-level functions with signatures, descriptions, and examples
- **Lua language tutorial**: `.claude/documentation/070-lua.html` — Lua syntax, types, operators, control structures
