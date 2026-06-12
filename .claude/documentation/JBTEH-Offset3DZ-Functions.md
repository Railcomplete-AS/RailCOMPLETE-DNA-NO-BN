# JBTEH Offset3D.Z Functions — Bardun & Strever

**Source file:** `NO-BN/DNA/NO-BN-2021.a-DNA-patch_13.xml`

These two object-local Lua functions compute the `Offset3D.Z` (vertical) coordinate for OCS guy wires (Bardun) and struts (Strever), so the line is drawn at the correct elevation regardless of whether it was drawn mast→footplate or footplate→mast.

---

## `_JBTEH_BAR_Offset3DZ()` — JBTEH_BAR Bardun (guy wire)

| Attribute | Value |
|---|---|
| **Name** | `_JBTEH_BAR_Offset3DZ()` |
| **ReturnType** | `Double` |
| **HideFromUser** | `false` |
| **Signature** | `Double _JBTEH_BAR_Offset3DZ()` |

**Description:**
> Returns the Z coordinate for a guy wire, on condition that it has a relation to its footplate at one end and to its anchoring-to-pole at the catenary mast end. Guy wires are supposed to be drawn from mast to anchoring footplate.

**Associated expression:** `LuaExpression Name="Offset3D.Z"` → `Formula: _JBTEH_BAR_Offset3DZ()`

**Header comment:** `2026-02-13 CLFEY Added after support call with customer. Adapted from DNA 2026.1 WIP.`

---

## `_JBTEH_STR_Offset3DZ()` — JBTEH_STR Strever (strut)

| Attribute | Value |
|---|---|
| **Name** | `_JBTEH_STR_Offset3DZ()` |
| **ReturnType** | `Double` |
| **HideFromUser** | `false` |
| **Signature** | `Double _JBTEH_STR_Offset3DZ()` |

**Description:**
> Returns the Z coordinate for a strut, on condition that it has a relation to its footplate at one end and to its anchoring-to-pole at the catenary mast end. Struts are supposed to be drawn from mast to anchoring footplate.

**Associated expression:** `LuaExpression Name="Offset3D.Z"` → `Formula: _JBTEH_STR_Offset3DZ()`

**Header comment:** `2026-06-01 CLFEY Added for parity with JBTEH_BAR Bardun (PR #276): Z-coordinate for struts drawn in either direction.`

---

## Notes

- Both functions take **no arguments** (empty `()` in the signature); they read `_position`, `RcAlignment`, and the object's relations as globals.
- Both return a `Double` — the interpolated Z value `z0 + _position.Pos * (dZ / L)`.
- The two are deliberately parallel; the only behavioural difference is that `_JBTEH_STR_Offset3DZ()` adds a `nil`-guard (returns `0`, keeping the strut flat) when the mast-console and/or footplate relations are not yet established.
- The Norwegian strings remaining in the function **bodies** (`"Har kraftoverføringskonsoll"`, `"Har forankring"`) are DNA **relation-space identifiers**, not translations — they must match the relation names defined elsewhere in the DNA and cannot be changed without breaking the lookup.
