# Demo-DNA: Condensed & Translated DNA Documentation

> **UPDATE REMINDER:** Every time you process a new file on the `Demo-DNA` branch, update the
> [File Status](#file-status) table and the [Changelog](#changelog) at the bottom of this document.

## Overview

The `Demo-DNA` branch contains a simplified, English-translated version of the full NO-BN DNA.
It is used for:

- **Demonstrations** to international partners and stakeholders
- **Testing** simplified configurations without the full complexity of the production DNA
- **International collaboration** — English names make the DNA legible without Norwegian knowledge

The condensed DNA is not a separate product. It lives on the `Demo-DNA` branch (based on `develop`)
and is maintained in parallel with the main DNA.

---

## Branch

| Item | Value |
|------|-------|
| Branch | `Demo-DNA` |
| Based on | `develop` |
| Remote | `origin/Demo-DNA` |
| Files modified | `NO-BN/DNA/_SRC/NO-BN-*.xml` |

When pulling new changes from `develop` into `Demo-DNA`, re-check that condensed files have
not been reverted or overwritten by merge conflicts.

> **UPDATE REMINDER:** If the base branch changes (e.g. rebased onto `main`), update the table above.

---

## Techniques Used

Four techniques are applied when condensing a DNA file. A single file may use one or all of them.

---

### 1. Variant Reduction

Long lists of variants are trimmed to 1–2 representative examples. The goal is that the object
type remains functional (inserts, displays, has a variant property) without listing every real-world
option.

**Before:**
```xml
<Variants DefaultValue="Kabelkanal, betong, 1-løps, 30 cm">
    <Variant Name="Kabelkanal, betong, 1-løps, 30 cm"/>
    <Variant Name="Kabelkanal, betong, 1-løps, 40 cm"/>
    <Variant Name="Kabelkanal, betong, 2-løps, 60 cm"/>
    <Variant Name="Kabelkanal, betong, 3-løps, 60 cm"/>
</Variants>
```

**After:**
```xml
<Variants DefaultValue="Cable trough, concrete, single-run, 30 cm">
    <Variant Name="Cable trough, concrete, single-run, 30 cm"/>
</Variants>
```

Rules of thumb:
- Keep the default variant (first in list), translated to English.
- Add a second variant only if it exercises a meaningfully different code path (e.g. a different
  linetype or 3D geometry branch).
- Add a comment `<!-- Condensed DNA: keeping N representative variant(s) only -->` above the
  `<Variants>` block when you make this change.

---

### 2. Norwegian → English Translation

All user-visible strings are translated. This includes XML attributes on `<ObjectType>`,
`<Variant>`, `<CustomProperty>`, `<Value>`, and `<AlignmentSystem>`.

**Attributes to translate:**

| Attribute | Example before | Example after |
|-----------|----------------|---------------|
| `Name=` on `<ObjectType>` | `"JBTKU_KFK Kabelkanal"` | `"JBTKU_KFK Cable trough"` |
| `Group=` | `"Underbygning/Føringsveier/Kabelkanal"` | `"Substructure/Cable ducts/Cable trough"` |
| `Variant Name=` | `"Kabelkanal, betong, 1-løps, 30 cm"` | `"Cable trough, concrete, single-run, 30 cm"` |
| `DisplayName=` on `<CustomProperty>` | `"Heving hvis overhøyde"` | `"Raise if superelevation"` |
| `DisplayName=` on `<Value>` | `"Venstrekurve"` | `"Left curve"` |
| `AlignmentSystem Name=` | `"Kabelføring, løftet med overhøyde"` | `"Cable duct, lifted with superelevation"` |
| `DefaultSystemName=` | same as above | same as above |

**Do not translate:**
- `LuaName=` attributes (these are code identifiers used by C#)
- `Layer=` attributes (layer names are fixed in the 2D library)
- `DataType=`, `Class=`, `Color=` — these are enum values, not display strings
- Lua function names (e.g. `NOBN_ku_*`, `_JBTKU_KFK_*`)
- XML comments that are internal developer notes (keep or translate at discretion)

---

### 3. Object Type Removal

Entire object types — or entire sections of a file — may be removed when they are not needed
for the demo scenario. When removing content, **do not delete the file**. Instead:

1. Remove all `<ObjectType>` blocks for the discipline.
2. Update the table-of-contents comment at the top of the file to say the content was removed.
3. Keep the file's header comment and `<xpp:bloc>` wrapper intact so XPPq can still include it.

**Example — `NO-BN-Earthing.xml` after removal:**
```xml
<!--========================================================================================================

    NO-BN-Earthing.xml

	Include in DNA file using XPPq XML preprocessor directive <xpp:expand href="fileName.xml"/>.

	Copyright (c) 2015-2026 Railcomplete AS, Norway, NO916118503

=========================================================================================================-->
<xpp:bloc>

<!--========================================================================================================
	Contents (condensed for Demo-DNA):
	  10		End Of File (all earthing object types removed - they were under Group="Kontaktledning/Jording")
=========================================================================================================-->



<!--========================================================================================================
    End of file
=========================================================================================================-->
</xpp:bloc>
```

Also check `NO-BN-Relations.xml` — if the removed object type participates in relations, those
relation entries may need to be commented out or removed to avoid compile errors.

---

### 4. Lua Formula Adaptation

When variants are renamed (technique 1) or removed (technique 3), Lua formulas that branch on
`Variant` values must be updated to match the new English names.

**Before:**
```lua
function _JBTKU_KFK_Linetype()
    if Variant:lower():match("1%-løps") then return "RC-DASHED-01"
    elseif Variant:lower():match("2%-løps") then return "RC-DASHED-02"
    elseif Variant:lower():match("3%-løps") then return "RC-DASHED-03"
    else return "Continuous", _info("Variant ["..Variant.."] has not been anticipated yet.")
    end
end
```

**After:**
```lua
function _JBTKU_KFK_Linetype()
    if Variant:lower():match("single%-run") then return "RC-DASHED-01"
    elseif Variant:lower():match("2%-run") then return "RC-DASHED-02"
    elseif Variant:lower():match("3%-run") then return "RC-DASHED-03"
    else return "Continuous", _info("Variant ["..Variant.."] has not been anticipated yet.")
    end
end
```

Rules:
- Update every `match()` and `==` comparison that tests a Norwegian variant string.
- Branches for variants that were removed can optionally be kept (translated) so the logic is not silently broken if a variant is re-added later. The `_info(...)` fallback will fire for any unrecognised variant at runtime.
- The `_info(...)` fallback message can stay as-is — it is a debug aid, not displayed to users.

---

## File Status

> **UPDATE REMINDER:** After completing work on any file, change its status in this table and
> add an entry to the [Changelog](#changelog).

### Legend

| Symbol | Meaning |
|--------|---------|
| ✅ Done | Condensed and/or translated, committed to `Demo-DNA` |
| 🌐 Translated only | Norwegian strings translated to English, no variant reduction |
| ❌ Removed | All object types removed; file shell kept |
| ⬜ Pending | Not yet processed |

---

### OCS — High Voltage (`EH`)

| File | Status | Notes |
|------|--------|-------|
| `NO-BN-OcsCantilevers.xml` | ✅ Done | Condensed (commit `bac323b0`) |
| `NO-BN-OcsPoles.xml` | ✅ Done | Condensed; "drop arms under bridge/tunnel" section removed (commit `bac323b0`) |
| `NO-BN-OcsSwitchesAndTransformers.xml` | ✅ Done | Condensed (commit `bac323b0`) |
| `NO-BN-OcsVariousObjects.xml` | ✅ Done | Condensed (commit `bac323b0`) |
| `NO-BN-OcsWireSystem.xml` | ✅ Done | Condensed (commit `b4c55876`) |
| `NO-BN-Earthing.xml` | ❌ Removed | All 4 object types removed (commit `b4c55876`) |

---

### Signalling (`SA`)

| File | Status | Notes |
|------|--------|-------|
| `NO-BN-Signals.xml` | 🌐 Translated only | Translated (commit `71e5a50c`) |
| `NO-BN-SignallingObjects.xml` | 🌐 Translated only | Translated (commit `71e5a50c`) |
| `NO-BN-Balises.xml` | 🌐 Translated only | Translated (commit `71e5a50c`) |
| `NO-BN-Interlocking.xml` | 🌐 Translated only | Translated (commit `71e5a50c`) |
| `NO-BN-SignalSighting.xml` | 🌐 Translated only | Translated (commit `71e5a50c`) |
| `NO-BN-Foulingpoint.xml` | ⬜ Pending | |

---

### Tracks & Substructure (`KO` / `KU`)

| File | Status | Notes |
|------|--------|-------|
| `NO-BN-CivilWorks.xml` | ✅ Done | Condensed + translated (commit `9ecdabc9`) |
| `NO-BN-TrackAndWaysideObjects.xml` | ✅ Done | Condensed + translated (commit `9ecdabc9`) |
| `NO-BN-TrackConnections.xml` | ⬜ Pending | |
| `NO-BN-GaugeHalfProfiles.xml` | ⬜ Pending | |

---

### Boards & Signs (`SK`)

| File | Status | Notes |
|------|--------|-------|
| `NO-BN-BoardsAndPoles.xml` | ✅ Done | Condensed + translated; 7 of 32 object types kept (commit `e47ed101`) |

---

### Telecom & Low Power (`TE` / `EL`)

| File | Status | Notes |
|------|--------|-------|
| `NO-BN-Telecom.xml` | ⬜ Pending | |
| `NO-BN-LowPower.xml` | ⬜ Pending | |

---

### Common & Configuration Files

| File | Status | Notes |
|------|--------|-------|
| `NO-BN-CommonObjects.xml` | ⬜ Pending | |
| `NO-BN-Labels.xml` | ⬜ Pending | |
| `NO-BN-ShowLayers.xml` | ✅ Done | Minor update (commit `9ecdabc9`) |
| `NO-BN-StyleDefinitions.xml` | ✅ Done | Minor update (commit `9ecdabc9`) |
| `NO-BN-Tables.xml` | ✅ Done | Minor update (commit `9ecdabc9`) |
| `NO-BN-Relations.xml` | ⬜ Pending | Check for relations referencing removed object types |
| `NO-BN-ModelChecks.xml` | ⬜ Pending | Check for checks referencing removed object types |

---

## Step-by-Step Process

Use this checklist when condensing a new file.

### Before you start

- [ ] Make sure you are on the `Demo-DNA` branch.
- [ ] Pull latest `develop` into `Demo-DNA` if needed (`git merge develop`).
- [ ] Open the target file and read its table-of-contents comment to understand what's inside.

### Condensing the file

- [ ] **Decide what to keep:** For each object type, decide whether to keep (condensed), translate only,
      or remove entirely. Aim to keep one representative object per discipline group.
- [ ] **Reduce variants:** For each `<Variants>` block, keep 1–2 representative entries.
      Add comment `<!-- Condensed DNA: keeping N representative variant(s) only -->`.
- [ ] **Translate strings:** Go through every `Name=`, `Group=`, `DisplayName=`, `Description=`,
      `Variant Name=`, `AlignmentSystem Name=` and translate Norwegian to English.
- [ ] **Update Lua formulas:** Search for `match(`, `==`, `:lower()` inside `<Formula>` blocks.
      Update any string literals that match renamed variant names.
- [ ] **Remove or keep unused Lua branches:** Optionally delete `elseif` branches for removed variants, or keep them translated as dead code — either is acceptable.
- [ ] **Update file header comment:** Update the table-of-contents line numbers and labels.
- [ ] **Check for removed RcTypes:** If you removed an object type, search `NO-BN-Relations.xml`
      and `NO-BN-ModelChecks.xml` for its `LuaName` (e.g. `rctype_EarthingConductor`) and
      comment out those entries.

### After condensing

- [ ] Run `MakeDna.bat` — the compile must succeed with no XPPq errors.
- [ ] Open RailCOMPLETE in AutoCAD and insert one object from the condensed file to verify it loads.
- [ ] Update the [File Status](#file-status) table in this document.
- [ ] Add an entry to the [Changelog](#changelog).
- [ ] Commit with message: `feat: condensed/translated NO-BN-XxxXxx.xml for Demo-DNA`

---

## Changelog

> **UPDATE REMINDER:** Add a row here for every commit that modifies condensed DNA files.
> Include the commit hash, date, which files were touched, and a brief note.

| Date | Commit | Files | What changed |
|------|--------|-------|-------------|
| 2026-03-16 | `e47ed101` | `NO-BN-BoardsAndPoles.xml` | Condensed + translated Boards & Signs; 7 of 32 object types kept |
| 2026-03-13 | `9ecdabc9` | `NO-BN-CivilWorks.xml`, `NO-BN-TrackAndWaysideObjects.xml`, `NO-BN-ShowLayers.xml`, `NO-BN-StyleDefinitions.xml`, `NO-BN-Tables.xml` | Condensed + translated Tracks & Substructure |
| 2026-03-09 | `71e5a50c` | `NO-BN-Balises.xml`, `NO-BN-Interlocking.xml`, `NO-BN-SignalSighting.xml`, `NO-BN-SignallingObjects.xml`, `NO-BN-Signals.xml` | Translated Signalling files to English |
| 2026-03-05 | `b4c55876` | `NO-BN-Earthing.xml`, `NO-BN-OcsWireSystem.xml` | Finished OCS condensing; removed all Earthing object types |
| 2026-03-03 | `bac323b0` | `NO-BN-OcsCantilevers.xml`, `NO-BN-OcsPoles.xml`, `NO-BN-OcsSwitchesAndTransformers.xml`, `NO-BN-OcsVariousObjects.xml` | Initial OCS condensing |
