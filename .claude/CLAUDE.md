
# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains the **DNA (Definition of Network Assets)** customization for **Bane NOR** (Norwegian railway administration, code `NO-BN`). It is consumed by the RailCOMPLETE plugin for AutoCAD (C# codebase in a separate repo). The DNA defines railway object types, 2D/3D symbols, Lua functions, property sets, and configuration — everything that makes RailCOMPLETE specific to Norway's railway standards.

Current version: **NO-BN 2026.1** (set in `DefineDnaVersion.bat`).

## Build Commands

### Compile the DNA XML
```
MakeDna.bat
```
Calls `DefineDnaVersion.bat` to set version variables, then runs `_TOOLBOX\BatchFiles\CompileDna.bat` which uses **XPPq** (XML preprocessor) to assemble `NO-BN\DNA\_SRC\*.xml` partial files into one compiled DNA file: `NO-BN\DNA\NO-BN-2026.1-DNA.xml`.

### Build and deploy to local test installation
```
MakeDnaAndTransferFilesToAppdata.bat
```
Compiles DNA then copies all resources (2D, 3D, DNA, Lua, Psets, etc.) to `%APPDATA%\Autodesk\ApplicationPlugins\RC.bundle\Adm\NO-BN\`.

### Build and deploy to shared installation
```
MakeDnaAndTransferFilesToProgramdata.bat
```
Same as above but targets `%PROGRAMDATA%`.

### CI/CD
Azure pipeline (`azure-pipelines.yml`) triggers on pushes to `main`. It compiles the DNA, builds an NSIS installer, signs it, and publishes an artifact.

### Build 2D symbol library
Done manually in AutoCAD using VLIDE (Visual LISP IDE). Load a bootstrap file from `NO-BN\2D\_BOOTSTRAPS\`, then run `(MkLib)` to generate the `.dwg` symbol library.

## Repository Structure

```
NO-BN/                          # All Norway Bane NOR content
├── DNA/
│   ├── _SRC/                   # XPPq source XML files (40+ partial files)
│   │   ├── NO-BN-RootFile.xml  # Master file that includes all others
│   │   ├── xppq.exe            # XPPq preprocessor binary
│   │   └── NO-BN-*.xml         # Partial DNA definitions by topic
│   ├── NO-BN-*-DNA.xml         # Compiled DNA output files
│   ├── DnaMappings/            # Version migration mapping files
│   └── Switches/               # Switch geometry definitions
├── 2D/
│   ├── _SRC/                   # AutoLISP source for 2D symbol library
│   │   └── Main/               # LISP files organized by discipline
│   ├── NO-BN-2026.1-2D.dwg    # Compiled 2D symbol library
│   └── NO-BN-2026.1-SymbolThumbnails.rc
├── 3D/
│   ├── _SRC/                   # 3D model source files and LISP generators
│   ├── STD-2025.1/             # 3D model library (2025 version)
│   ├── STD-2026.1/             # 3D model library (2026 version, current)
│   └── LayerMappings/          # Layer mapping configuration
├── Lua/
│   ├── Functions/              # Reusable Lua libraries (lib1.lua, lib2.lua, Pset2021a.lua)
│   ├── Scripts/                # Automation scripts organized by discipline (FE/, SA/)
│   └── LuaTooltipPages/        # Tooltip XML definitions with Lua
├── Psets/                      # Property Set definitions for BIM/Bane NOR data
├── AutoCAD/                    # AutoCAD resources (color tables, fonts)
├── FAQ/                        # FAQ content
├── ReleaseNotes/               # Release notes
├── VectorImages/               # Administration logos etc.
└── WebLinks/                   # Web link definitions

_TOOLBOX/                       # Shared build tools
├── BatchFiles/                 # CompileDna.bat, CopyDnaEtc*.bat
├── AutoCadScripts/
└── XPPq/

Installer/                      # NSIS installer script and resources
_CODING_STYLE_GUIDE_/           # LISP coding style examples
```

### Convention: `_` prefixed folders
Folders starting with `_` (`_SRC`, `_DEPRECATED`, `_SUPPORTINGDOCS`, `_SCRATCHPAD`, `_BOOTSTRAPS`) are **source/dev-only** — excluded from deployment by `xcopyignore.txt`. Only compiled/finalized files are distributed.

## DNA XML Architecture

### XPPq Preprocessing
The DNA is authored as modular XML files assembled by **XPPq** (EPEIOS open-source XML preprocessor). Key directives:
- `<xpp:expand href="FILENAME"/>` — include another file
- `<xpp:define name="NAME">…</xpp:define>` / `<xpp:expand select="NAME"/>` — define and expand macros
- `<xpp:set>` / `<xpp:ifeq>` — variables and conditionals
- `<xpp:bloc>…</xpp:bloc>` — grouping (required for macro definitions)
- `<xpp:cdata>…</xpp:cdata>` — CDATA sections

### DNA XML Elements
The compiled DNA defines:
- **`<ObjectType>`** — Railway object type definitions with attributes like `DataType`, `Class`, `LuaName`, `Name`, `Layer`, `Group`
- **`<LuaFunction>`** — Global Lua functions with `Name`, `ReturnType`, `Signature`, `Formula`
- **`<LuaExpression>`** — Per-object Lua formulas (property calculations, labels, offsets)
- **`<Variants>`** — Object variants with different 2D block images
- **`<InsertPointObject>`** — Object insertion definitions
- **`<AlignmentSystems>`** — Alignment/track definitions
- **`<RelationSpace>`** — Object relationship declarations

### DNA Source File Organization
Partial files in `NO-BN\DNA\_SRC\` are organized by topic:
- `NO-BN-RootFile.xml` — Master assembly, includes all others, contains naming conventions documentation
- `NO-BN-General-Lua.xml` / `NO-BN-National-Lua.xml` — Shared and national Lua functions
- `NO-BN-LuaCode-*.xml` — Lua utility libraries (BasicCADFunctions, Balloon, Cantilever, Portal Beam, Guy Wire)
- `NO-BN-Signals.xml`, `NO-BN-TrackAndWaysideObjects.xml`, `NO-BN-BoardsAndPoles.xml`, etc. — Object definitions by discipline
- `NO-BN-StyleDefinitions.xml`, `NO-BN-ShowLayers.xml`, `NO-BN-Relations.xml` — Configuration
- `NO-BN-ModelChecks.xml` — Data validation rules
- `NO-BN-Tables.xml` — Data tables
- `NO-BN-PositionToolSettings.xml` — Linear referencing (ISO 19148:2012)
- `NO-BN-StandardProperties.xml`, `NO-BN-StandardTextAttributes.xml` — Common property definitions

## Naming Conventions

### Discipline Codes
| Code | Norwegian Name | English Name |
|------|---------------|--------------|
| FE (JBTFE) | Felles | Common/General |
| KU (JBTKU) | Underbygning | Substructure/Civil works |
| KO (JBTKO) | Overbygning | Track superstructure |
| EH (JBTEH) | Elektro Høyspent | High-voltage / OCS (Overhead Catenary System) |
| SA (JBTSA) | Signalanlegg | Signalling |
| TE (JBTTE) | Telekommunikasjon | Telecommunications |
| EL (JBTEL) | Elektro Lavspent | Low-voltage power |
| SK (JBTSK) | Skilt | Signs/Boards and poles |
| RC (JBTRC) | — | RailCOMPLETE internal |

The `JBT` prefix stands for "Jernbaneteknikk" (railway engineering). The 4-letter discipline abbreviation after `JBT` maps to layer names, 2D symbol prefixes, and object type names.

### Lua Function Naming
Defined in the RootFile and enforced across all DNA XML:
- `RC__functionName()` — Generic functions not depending on any DNA-specific content (double underscore after RC)
- `NOBN_discipline_actionObjectName()` — Administration-dependent functions (e.g., `NOBN_sig_getSignalNumber()`)
- `OBJECTTYPE_PropertyName()` — Global, shared by multiple object types
- `_OBJECTTYPE_PropertyName()` — Local to one object type (leading underscore)
- Constants are written as zero-argument functions (Lua has no const keyword): `NOBN_sig_NSS_DEFAULT_DECELERATION()`

### 2D Symbol Block Names
Follow pattern: `NO-BN-2D-JBTxx_DESCRIPTION` where `xx` is the discipline code.

## Lua in this Repository

### Two Lua Contexts (defined in C# plugin)
1. **Object-level formulas** — Short expressions per-object for property values, evaluated in sandboxed `LuaContext`. These appear as `<LuaExpression>` and `<LuaFunction>` in DNA XML.
2. **Scripts** — Longer programs in `NO-BN\Lua\Scripts\` that automate tasks in AutoCAD (insert objects, prompt users, read files). Run in `RunScriptLuaContext`.

### Lua Libraries
- **`lib1.lua`** — Utility functions (point-to-string, rounding, string splitting). Loaded via `includeLuaFile("Lua\\Functions\\lib1.lua")`.
- **`lib2.lua`** — Script-only utilities (writeln, show, stop, selectAll, country-aware messages). Uses `DocumentData.DnaIri` for country detection.
- **`Pset2021a.lua`** — Property set functions for Bane NOR data integration (BaneData IDs, FDV data by discipline).

### Lua Embedded in DNA XML
Most Lua code lives inside the DNA XML files as `<Formula>` elements within `<LuaFunction>` and `<LuaExpression>` tags. The `RC__CAD()` object provides basic CAD operations (createPolyline, createCircle, createLine, etc.) for 3D model generation formulas.

### Lua API
The C# plugin exposes ~88 object-level API functions and ~60 script-only functions. See `.claude/C# Lua skill.md` for the full API reference including categories, attribute system, and how to add new functions.

## 2D Symbol LISP Architecture

AutoLISP source in `NO-BN\2D\_SRC\Main\`:
- `00_Administration specific constants.lsp` — Discipline prefixes, track gauges (1.435m normal), schematic spacing (21m), geographic spacing (4.7m)
- `01_CreateStandardLayers.lsp` — Layer definitions per discipline
- `10-27_*.lsp` — Symbol definitions by discipline (Thumbnails, Annotations, Common, BoardsAndPoles, CivilWorks, TrackAndEmbankment, HighVoltage, LowVoltage, Signalling, Telecom)
- `99_Main.lsp` — Master assembly

Subdirectories contain the actual symbol-drawing LISP routines per discipline.

## Branch Strategy
- **develop**: Active development, may contain work-in-progress
- **main**: Stable releases, triggers Azure CI/CD pipeline

## Key Reference Files
- `DefineDnaVersion.bat` — Single source of truth for DNA version (`ADM=NO-BN`, `RELEASE=2026.1`)
- `NO-BN\DNA\_SRC\NO-BN-RootFile.xml` — Master DNA file, contains comprehensive naming convention documentation
- `.claude/C# agent.md` — CLAUDE.md from the C# RailCOMPLETE plugin repo (build, architecture, dependencies)
- `.claude/C# Lua skill.md` — Detailed Lua subsystem guide (API functions, attributes, registration, testing)
