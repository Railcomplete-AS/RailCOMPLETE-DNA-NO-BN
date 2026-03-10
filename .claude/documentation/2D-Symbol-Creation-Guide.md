# 2D Symbol Creation Guide

This guide explains how 2D symbols are structured, authored, and compiled in the NO-BN DNA repository. Symbols are AutoLISP-generated AutoCAD blocks that RailCOMPLETE uses to visually represent railway objects in 2D drawings.

---

## Table of Contents

1. [Overview](#overview)
2. [Directory Structure](#directory-structure)
3. [Build Process](#build-process)
4. [Block Types](#block-types)
5. [Naming Conventions](#naming-conventions)
6. [Discipline Codes and Block Prefixes](#discipline-codes-and-block-prefixes)
7. [Anatomy of a Symbol Function](#anatomy-of-a-symbol-function)
8. [Drawing Helpers Reference](#drawing-helpers-reference)
9. [Layer System](#layer-system)
10. [How to Add a New Symbol](#how-to-add-a-new-symbol)
11. [Debugging and VLIDE Tips](#debugging-and-vlide-tips)

---

## Overview

The 2D symbol library is a `.dwg` file containing named AutoCAD block definitions. It is **generated entirely from AutoLISP source code** — there is no manually drawn DWG source file. The LISP code issues AutoCAD commands (`LINE`, `POLYLINE`, `HATCH`, `RECTANGLE`, etc.) to draw geometry, then captures the result as a named block.

The output file is saved as `NO-BN-YYYYMMDD_HHMMSS-2D.dwg` during development, and the version-stamped copy (e.g. `NO-BN-2026.1-2D.dwg`) is the file that RailCOMPLETE loads.

**The golden rule:** every symbol is drawn centred at the origin `(0, 0)`. The block insertion point is always the origin.

---

## Directory Structure

```
NO-BN/2D/
├── NO-BN-2026.1-2D.dwg                      # Compiled output (deployed to users)
├── NO-BN-2026.1-SymbolThumbnails.rc          # Symbol thumbnail resource file
│
├── _SRC/                                      # Source files — NOT deployed (excluded by xcopyignore.txt)
│   │
│   ├── Main/                                  # Symbol definition files
│   │   ├── 00_Administration specific constants.lsp   # Prefixes, gauges, text heights
│   │   ├── 01_CreateStandardLayers.lsp                # All layer definitions
│   │   ├── 10_Thumbnails.lsp                          # Thumbnail symbols
│   │   ├── 11_Annotations.lsp                         # Annotation objects
│   │   ├── 20_Common.lsp                              # Common/shared symbols
│   │   ├── 21_BoardsAndPoles.lsp                      # Loads BoardsAndPoles/ then runs generator
│   │   ├── 22_CivilWorks.lsp
│   │   ├── 23_TrackAndEmbankment.lsp
│   │   ├── 24_HighVoltage.lsp
│   │   ├── 25_LowVoltage.lsp
│   │   ├── 26_Signalling.lsp
│   │   ├── 27_Telecom.lsp
│   │   ├── 99_Main.lsp                                # Entry point — contains C:MAIN
│   │   │
│   │   ├── Annotations/                               # One file per annotation type
│   │   ├── BoardsAndPoles/                            # ~38 files, one per board/pole object
│   │   ├── CivilWorks/
│   │   ├── Common/
│   │   ├── HighVoltage/
│   │   ├── LowVoltage/
│   │   ├── Signalling/                                # ~17 files, one per signalling object group
│   │   ├── Telecom/
│   │   └── TrackAndEmbankment/
│   │
│   ├── Utilities/                                     # Helper functions (loaded first)
│   │   ├── CAD system constants.lsp     # Command aliases, AutoCAD/BricsCAD abstraction
│   │   ├── DrawHelpers.lsp              # Coordinate helpers (PointTL, PointMR, etc.)
│   │   ├── General functions.lsp        # Common utilities
│   │   ├── Trigonometric functions.lsp  # Angle/trig helpers
│   │   └── Unit conversions.lsp         # Unit conversion helpers
│   │
│   └── Fonts/                                         # Font resources
│
└── _BOOTSTRAPS/                                       # Personal build launchers — NOT deployed
    ├── _2D Symbol Library Commands.lsp    # Shared bootstrap commands (MkLib, LdAll, etc.)
    └── _2D Symbol Library Commands_CLFEY.lsp  # Example personal bootstrap (copy and rename)
```

> **Convention:** folders and files prefixed with `_` are source/dev-only and are excluded from the deployed bundle by `xcopyignore.txt`.

---

## Build Process

### Prerequisites

- **AutoCAD** (preferred) or **BricsCAD** with its integrated LISP debugger (VLIDE / BLADE)
- Confirm `_CAD_` is set to `_ACAD_` or `_BCAD_` in `_SRC/Utilities/CAD system constants.lsp`

### One-time setup: personal bootstrap file

Copy `_BOOTSTRAPS/_2D Symbol Library Commands_CLFEY.lsp` to a new file with your own initials, then set `rootFolder` to point at the `_SRC` directory on your machine:

```lisp
(setq rootFolder "C:\\your\\path\\to\\NO-BN\\2D\\_SRC")
```

### Running the build

1. Open AutoCAD and type `_VLIDE` on the command line to open the Visual LISP IDE.
2. Press **Ctrl+Shift+L** and select your personal bootstrap file. This loads the shared commands file automatically.
3. In the VLIDE console, type `(MkLib)` and press Enter.

The build will:
- Load all LISP files from `Fonts/`, `Utilities/`, and `Main/` (including all subfolders)
- Run `INITIALIZE` (sets constants, creates layers)
- Run all `NN_GENERATE-*` routines in order (thumbnails → annotations → common → boards → civil → track → OCS → signalling → telecom)
- Save the result to `NO-BN-YYYYMMDD_HHMMSS-2D.dwg` one level above `_SRC`

### Useful VLIDE commands

| Command | What it does |
|---------|-------------|
| `(MkLib)` | Full build — loads everything and runs `C:MAIN` |
| `(Init)` | Set up constants and layers only (good for incremental testing) |
| `(LdAll)` | Load all LISP source files without executing anything |
| `(LdSub "Signalling")` | Load just the `Main/Signalling/` subfolder |
| `(Hlp)` | Print help with all available commands |

### Build output

Performance is printed to the console at the end:

```
*** PERFORMANCE : 12.345 blocks per second
*** TIME USED   : 0 hour(s), 3 minute(s) and 42 second(s)
150 schematic blocks created
150 annotative blocks created
40 metric blocks created
340 TOTAL blocks created
```

Block counts are also stored as the names of three dummy blocks embedded in the DWG:
- `___Number_of_Schematic_Blocks__NNN`
- `___Number_of_annotative_Blocks___NNN`
- `___Number_of_Metric_Blocks___NNN`

---

## Block Types

For each railway object, the LISP code typically generates **two or three block variants** from the same drawing routine:

| Type | Creation function | Purpose |
|------|------------------|---------|
| **Schematic** | `(CreateSchematicBlockFromCurrentGraphics blockName)` | Fixed display size regardless of drawing scale. Used in schematic diagrams (signalling, track circuit, etc.) |
| **Annotative** | `(CreateAnnotativeBlockFromScaledSchematicBlock blockName scale)` | Scales with the AutoCAD viewport annotation scale. `scale` is normally `_one_` (1:1). |
| **Metric** | `(CreateMetricBlockFromCurrentGraphics blockName)` | Real-world dimensions in metres. Used for clearance zones, metal-free areas, separation distances. |

A symbol function that produces all three types follows this sequence:

```lisp
; 1. Draw schematic-scale graphics
(DrawBox layDef_Zero 4.0 6.0 _noWipeout_)
(CreateSchematicBlockFromCurrentGraphics blockName)
(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

; 2. Draw metric-scale graphics (real-world dimensions)
(DrawMetricBaliseGraphics ...)
(CreateMetricBlockFromCurrentGraphics blockName)
```

---

## Naming Conventions

### Block names

```
NO-BN-2D-JBTxx_CATEGORY-SUBCATEGORY-VARIANT
```

- `NO-BN-2D` — country and output type, always fixed
- `JBTxx` — discipline code (see [Discipline Codes](#discipline-codes-and-block-prefixes))
- Everything after `_` — UPPERCASE, hyphen-separated description and variant

Examples:
```
NO-BN-2D-JBTSA_ETC-ETCS-BALISEGRUPPE-ENKEL
NO-BN-2D-JBTSA_ETC-ETCS-BALISEGRUPPE-DOBBEL
NO-BN-2D-JBTSK_SKK-SKILT-KJOERENDE-SIGNAL-60A-ATC-FORSIGNAL
NO-BN-2D-JBTSA_MSS-SKILT-ERTMS-LEVEL-CROSSING-ANNOUNCE-PORTAL
```

Always build block names using the discipline prefix **variable** (not a hardcoded string), so the prefix stays consistent:

```lisp
; Good — uses the constant
(setq blockName (strcat _SIG_ "ETC-ETCS-BALISEGRUPPE-" variation))

; Bad — hardcoded (fragile if prefix ever changes)
(setq blockName (strcat "NO-BN-2D-JBTSA_ETC-ETCS-BALISEGRUPPE-" variation))
```

### LISP function names

| Level | Pattern | Example |
|-------|---------|---------|
| Discipline generator (top-level) | `NN_GENERATE-DISCIPLINE-OBJECTS` | `26_GENERATE-SIGNALLING-OBJECTS` |
| Object group (mid-level) | `OBJECT-TYPE-NAME` in SCREAMING-KEBAB-CASE | `BALISE-SYSTEM`, `ETCS-BALISE-GROUP` |
| Drawing routine (low-level, NO-BN specific) | `NOBN_DrawDescriptiveName` | `NOBN_DrawEtcsBalise`, `NOBN_DrawLyingPole` |

---

## Discipline Codes and Block Prefixes

Defined in `_SRC/Main/00_Administration specific constants.lsp`:

| LISP variable | Block prefix | JBT code | English name | Norwegian name |
|--------------|-------------|---------|-------------|----------------|
| `_RC_` | `NO-BN-2D-JBTRC_` | RC | RailCOMPLETE internal | — |
| `_COM_` | `NO-BN-2D-JBTFE_` | FE | Common / General | Felles |
| `_SUB_` | `NO-BN-2D-JBTKU_` | KU | Substructure / Civil works | Underbygning |
| `_TRK_` | `NO-BN-2D-JBTKO_` | KO | Track superstructure | Overbygning |
| `_OCS_` | `NO-BN-2D-JBTEH_` | EH | OCS / Catenary | Kontaktledning |
| `_SIG_` | `NO-BN-2D-JBTSA_` | SA | Signalling | Signalanlegg |
| `_TEL_` | `NO-BN-2D-JBTTE_` | TE | Telecommunications | Telekommunikasjon |
| `_POW_` | `NO-BN-2D-JBTEL_` | EL | Low-voltage power | Hjelpekraft |
| `_BNP_` | `NO-BN-2D-JBTSK_` | SK | Boards and poles | Skilt og stolper |

### Other constants from `00_Administration specific constants.lsp`

| Variable | Value | Description |
|----------|-------|-------------|
| `_normalGauge_` | `1.435` | Distance between the two rail heads (m) |
| `_schematicTrackSpacing_` | `21.000` | Centreline spacing in schematic drawings (m) |
| `_geographicTrackSpacing_` | `4.700` | Centreline spacing in real installations (m) |
| `_proxySymbolRadius_` | `1.5` | Radius of the fallback circle when no symbol exists |
| `_descriptionTextHeight_` | `_th020_` | Standard text height for description labels |

---

## Anatomy of a Symbol Function

### Minimal template

```lisp
(defun MY-NEW-SYMBOL ( / blockName description )
    ; 1. Define the block name and description
    (setq blockName  (strcat _SIG_ "XXX-MY-NEW-SYMBOL"))
    (setq description "MY NEW SYMBOL")

    ; 2. Draw geometry, centred at origin (0, 0)
    (DrawBox layDef_Zero 4.0 6.0 _noWipeout_)
    (DrawHatch _solidHatch_)

    ; 3. Optionally add attribute positions for dynamic text labels
    (AddTextAttributeAtPoint layDef_Zero _th250_ (list 0 4.5)
        '("LABEL" "Label prompt" _emptyString_))

    ; 4. Add the standard description text below the symbol
    (AddDescriptionBelowOrigin description 0)

    ; 5. Capture as block(s)
    (CreateSchematicBlockFromCurrentGraphics blockName)
    (CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)

    description  ; Return the description string (used by table generators)
)
```

### Variant pattern

When one object type has multiple variants (single/double, fixed/controlled, etc.):

```lisp
(defun MY-SYMBOL-WITH-VARIANTS ( / blockName description variants variation thisBlockName thisDescription )
    (setq blockName  (strcat _SIG_ "XXX-MY-SYMBOL"))
    (setq description "MY SYMBOL")
    (setq variants '("VARIANT-A" "VARIANT-B"))

    (foreach variation variants
        (setq
            thisBlockName   (strcat blockName   "-" variation)
            thisDescription (strcat description ", " variation)
        )
        (TraceLevel3 thisDescription)

        ; Draw variant-specific geometry
        (cond
            ((= variation "VARIANT-A")
                (DrawBox layDef_Zero 4.0 4.0 _noWipeout_)
            )
            ((= variation "VARIANT-B")
                (DrawBox layDef_Zero 4.0 4.0 _noWipeout_)
                (DrawHatch _solidHatch_)
            )
        )
        (AddDescriptionBelowOrigin thisDescription 0)

        (CreateSchematicBlockFromCurrentGraphics thisBlockName)
        (CreateAnnotativeBlockFromScaledSchematicBlock thisBlockName _one_)
    )
)
```

### Mast-mounted vs portal-mounted signals

Many signal symbols exist in both mounting configurations. The standard epilogue handles repositioning and pole drawing after the signal face geometry has been drawn:

```lisp
(if (= mounting "PORTAL")
    (progn
        ; Move signal face down and add short suspension pole from portal beam
        (command
            _MOVE_ _selectAll_ _ENTER_ (list 0 (+ (/ y 2) portalPole)) _origin_
            _LINE_ _origin_ (list 0 (- portalPole)) _ENTER_
        )
    )
    (progn
        ; Move signal face up and add full mast below it
        (command _MOVE_ _selectAll_ _ENTER_ _origin_ (list 0 (+ (/ y 2) pole)))
        ; Rotate to "lying down" orientation because the pole-drawing helpers expect it
        (command _ROTATE_ _selectAll_ _ENTER_ _origin_ _angleMinus90_)
        (NOBN_DrawLyingPole 0 pole)
        (NOBN_DrawLyingHsBase)
        (command _ROTATE_ _selectAll_ _ENTER_ _origin_ _angle90_) ; rotate back upright
    )
)
(CreateSchematicBlockFromCurrentGraphics blockName)
(CreateAnnotativeBlockFromScaledSchematicBlock blockName _one_)
```

---

## Drawing Helpers Reference

All helpers are defined in `_SRC/Utilities/DrawHelpers.lsp` and `_SRC/Utilities/General functions.lsp`.

### Positions on a centred box (width `x`, height `y`)

```
  PointTL ── PointTC ── PointTR
     │                    │
  PointML    PointMC    PointMR      ← PointMC = (0,0) = origin
     │                    │
  PointBL ── PointBC ── PointBR
```

| Function | Returns |
|----------|---------|
| `(PointTL x y)` | Top-left corner `(-x/2, y/2)` |
| `(PointTC x y)` | Top-centre `(0, y/2)` |
| `(PointTR x y)` | Top-right corner `(x/2, y/2)` |
| `(PointML x y)` | Middle-left `(-x/2, 0)` |
| `(PointMC x y)` | Middle-centre = origin `(0, 0)` |
| `(PointMR x y)` | Middle-right `(x/2, 0)` |
| `(PointBL x y)` | Bottom-left corner `(-x/2, -y/2)` |
| `(PointBC x y)` | Bottom-centre `(0, -y/2)` |
| `(PointBR x y)` | Bottom-right corner `(x/2, -y/2)` |
| `(PosAbove textHeight y)` | Just above the top edge |
| `(PointBelow textHeight y)` | Just below the bottom edge |
| `(PosNR nRows row y)` | Position for row `row` of `nRows` equal-height text rows inside the box |

### Draw functions

| Function | Description |
|----------|-------------|
| `(DrawBox layer x y wipeout)` | Rectangle centred at origin |
| `(DrawBoxAtPoint layer pt x y wipeout)` | Rectangle centred at point `pt` |
| `(DrawLine layer p1 p2)` | Line segment |
| `(DrawCircleAtPoint layer pt r wipeout)` | Circle at `pt` with radius `r` |
| `(DrawHatch hatchType)` | Hatch the last closed entity |
| `(DrawHatchAtPoint hatchType pt angle offset)` | Hatch at a specific point |
| `(DrawArcByCenter layer center start end)` | Arc defined by centre and two endpoints |
| `(MirrorAboutYaxis keep)` | Mirror all current geometry about the Y axis |
| `(MoveUp dist)` | Translate all geometry upward by `dist` |
| `(MoveRight dist)` | Translate all geometry rightward by `dist` |
| `(AddDescriptionBelowOrigin text yOffset)` | Write the standard description label below the symbol |
| `(AddTextAtPointWithJustification layer height pt text just)` | Static text at a point |
| `(AddTextAttributeAtPoint layer height pt attrDef)` | Dynamic block attribute at a point |

### Hatch density constants

| Constant | Density | Semantic meaning |
|----------|---------|-----------------|
| `_solidHatch_` | Solid fill | Black |
| `_denseHatch_` | Dense | Dark / "red" |
| `_mediumHatch_` | Medium | Mid-grey / "blue" |
| `_sparseHatch_` | Sparse | Light grey / "yellow" |

### Text height constants

| Constant | Height | Typical use |
|----------|--------|-------------|
| `_th020_` | 0.20 | Description labels (default: `_descriptionTextHeight_`) |
| `_th100_` | 1.00 | Small labels inside symbols |
| `_th125_` | 1.25 | Medium labels |
| `_th180_` | 1.80 | Larger labels |
| `_th250_` | 2.50 | Attribute text positions above/below a symbol |

---

## Layer System

All layer definitions live in `01_CreateStandardLayers.lsp`. Every layer has a corresponding LISP variable prefixed with `layDef_`.

### Key layers

| Variable | Layer name | Use |
|----------|-----------|-----|
| `layDef_Zero` | `0` | Primary geometry — almost all symbol drawing happens here |
| `layDef_Balise_BaliseSeparation` | `JBTSA$$BALISE-SEPARATION` | Arcs showing separation distance between balises |
| `layDef_Balise_ActuatorSeparation` | `JBTSA$$ACTUATOR-SEPARATION` | Arcs showing actuator separation |
| `layDef_Balise_MetalFreeArea` | `JBTSA$$METAL-FREE-AREA` | Metal-free clearance rectangles |
| `layDef_Balise_GroupSeparation` | `JBTSA$$GROUP-SEPARATION` | Arcs showing group separation |

### The `$$` metadata layer convention

Layer names containing `$$` are **metadata layers** — they hold construction geometry, clearance zones, and separation distances. They are:
- Not plotted
- Typically hidden in normal drawing views
- Used by RailCOMPLETE for spatial analysis and interference checking

### Switching layers in code

```lisp
(SetLayer layDef_Zero)                   ; Primary geometry
(SetLayer layDef_Balise_MetalFreeArea)   ; Switch to metadata layer before drawing clearance box
(command _RECTANGLE_ (list (- mx) (- my)) (list mx my))
(SetLayer layDef_Zero)                   ; Switch back
```

---

## How to Add a New Symbol

### Step 1 — Choose the right discipline file

| What you are adding | Subfolder | Top-level file |
|--------------------|-----------|---------------|
| A signal (train stop, dwarf signal, etc.) | `Signalling/` | `26_Signalling.lsp` |
| A board or pole | `BoardsAndPoles/` | `21_BoardsAndPoles.lsp` |
| A common marker or label | `Common/` | `20_Common.lsp` |
| An OCS component | `HighVoltage/` | `24_HighVoltage.lsp` |
| A track or embankment object | `TrackAndEmbankment/` | `23_TrackAndEmbankment.lsp` |

### Step 2 — Create the LISP file

Create a new `.lsp` file in the appropriate subfolder. Name it descriptively, matching the function it defines:

```
_SRC/Main/Signalling/NO-BN My New Signal.lsp
```

Use the template from [Anatomy of a Symbol Function](#anatomy-of-a-symbol-function). Include the standard copyright header:

```lisp
;=========================================================================================================================
;
; NO-BN My New Signal.lsp
;
; Copyright (c) 2015-2026 Railcomplete AS, Norway, NO916118503
; RailCOMPLETE (R) and the RailCOMPLETE logo are registered trademarks owned by Railcomplete AS.
;
; Change log:
; YYYY-MM-DD INITIALS What was added or changed.
;
;=========================================================================================================================
```

### Step 3 — Register the symbol in the discipline generator

Open the discipline's top-level file (e.g. `26_Signalling.lsp`) and add a `TraceLevel2` + function call pair inside the `26_GENERATE-SIGNALLING-OBJECTS` function:

```lisp
(defun 26_GENERATE-SIGNALLING-OBJECTS ( / )
    (SetCadSystemDefaults)
    ; ... existing calls ...
    (TraceLevel2 "MY-NEW-SIGNAL")    (MY-NEW-SIGNAL)    ; ← add this line
)
```

> For `BoardsAndPoles`, the mechanism is different — add the function name to the list returned by `GetBoardAndPoleNames` in `BoardsAndPoles/getBoardAndPoleNames.lsp` instead.

### Step 4 — Register the block name in the DNA XML

RailCOMPLETE needs to know which DNA object type uses the new block. In `NO-BN/DNA/_SRC/`, find the relevant XML file (e.g. `NO-BN-Signals.xml`) and update the `<ObjectType>` entry:

- Add the new block name to `DisplayBlockNames`
- Add or update `<InsertPointObject>` or `<Variants>` as appropriate

See `DNA-DOCUMENTATION.md` (in this same folder) for a full explanation of the XML structure.

### Step 5 — Build and verify

```
(MkLib)
```

Confirm that:
- No LISP errors appear in the console
- The total block count has increased
- The new block is visible in the generated DWG

---

## Debugging and VLIDE Tips

### Incremental testing without a full rebuild

Use `(Init)` to set up the environment, then call a single symbol function:

```lisp
(Init)              ; Loads all files, sets constants, creates layers
(MY-NEW-SIGNAL)     ; Draws just this one symbol — inspect the result in the drawing
```

### Common errors

| Symptom | Likely cause |
|---------|-------------|
| Block not created | Geometry was empty when `CreateSchematicBlockFromCurrentGraphics` was called — ensure draw commands actually execute before that line |
| Symbol is the wrong size | Dimensions are in metres — double-check you are not using millimetres |
| Description text is missing or misplaced | `AddDescriptionBelowOrigin` called with wrong `yOffset`; try `0` first |
| Block count did not increase | Your new function was not called from the `NN_GENERATE-*` top-level function |
| Hatch is on the wrong layer | `SetLayer` was not called before drawing, or was called after `DrawHatch` |

### VLIDE keyboard shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+Shift+L` | Load a LISP file |
| `Ctrl+Shift+C` | Toggle between AutoCAD command mode and VLIDE mode |
| `Ctrl+Shift+T` | Show call stack trace |
| `Ctrl+R` | Reset from a break loop |
| `F6` | Open the VLIDE console window |
| `F9` | Set / clear a breakpoint at the cursor |
| `Ctrl+W` | Add a watch variable |
| `Ctrl+F9` | Jump to the source location of the last error |

### Controlling AutoCAD feedback during a run

```lisp
(setvar 'CMDECHO 1)  ; Verbose — shows all command echoes (good for debugging)
(setvar 'CMDECHO 0)  ; Silent — faster execution (default in C:MAIN)
```
