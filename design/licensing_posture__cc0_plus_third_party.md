# Licensing Posture: CC0 USGS Code + Bundled Third-Party Components

**Date:** November 2, 2026
**Status:** Working determination / best-practice note
**Question addressed:** What do we do about the licenses of third-party code bundled
into an otherwise-CC0 USGS release, and does the SWB2 `THIRD_PARTY_LICENSES.md`
approach work?

---

## 1. Short answer

Yes — the SWB2 approach works and is the sensible best practice:

- USGS-authored code is released as **public domain / CC0 1.0** (the DOI default).
- Each bundled third-party component **stays under its own license** (PROJ.4 MIT-like,
  netCDF/HDF5 BSD-3-Clause, zlib). We do **not** attempt to relicense them under CC0.
- The upstream copyright notices and full license texts are **retained** — in the
  vendored source file headers *and* consolidated in a top-level
  `THIRD_PARTY_LICENSES.md`.
- `LICENSE.md` opens with **"Unless otherwise noted,"** which is the carve-out that
  lets CC0 cover our code while the noted third-party components keep their terms.

This satisfies the only hard obligation the permissive licenses impose (retain the
notice) and aligns with the governing DOI policy. It is a sound posture to defend and
to recommend to colleagues.

---

## 2. Why this is correct (grounded in policy, strongest first)

### 2.1 DOI 2025 Open-Source Software Policy / SHARE IT Act — directly on point

The DOI implementation of the SHARE IT Act (Pub. L. 118-187; DOI OCIO memo signed
Feb 2, 2026) is the authoritative instrument. Its release rules are scoped by authorship:

- **§V.1.c.i** — code "original and developed **solely** by Department of the Interior
  federal employees" is released under the CC0 dedication in **Attachment A**.
- **§V.1.c.iii** — where USGS code is **"based on pre-existing code and/or components,
  release in accordance with the license terms and conditions of the pre-existing
  code."**

Section V.1.c.iii is the direct answer to our question: bundled third-party code is
released under *its* terms, not under CC0. `THIRD_PARTY_LICENSES.md` is the concrete
mechanism for doing exactly that. The policy also states OCIO and the Office of the
Solicitor "will commit to working together to create and share guidance on common
licensing scenarios" — i.e., DOI has openly acknowledged that detailed how-to guidance
for the mixed-license case is still forthcoming. That is why no bureau document
"blesses" a specific file format: the obligation is settled; the prescribed mechanism
is not yet published.

### 2.2 SM 502.11 — adjacent requirement

SM 502.11 (Fundamental Science Practices: Review, Approval, and Release of Scientific
Software; effective May 2026) scopes its §4 release requirements to **Scientific
Software Information Products**. Relevant here:

- **§4.D** — documentation must describe purpose, methods, execution, and
  **dependencies**. Third-party components are dependencies; disclosing them (and their
  licenses) is where `THIRD_PARTY_LICENSES.md` also does double duty.

SM 502.11 does not itself prescribe how to handle third-party license text; it defers,
as the DOI policy does, to the licenses of the incorporated code.

### 2.3 WMA "Code in the Scientific Record" — interpretive confirmation

The WMA DSP Manual interpretation page confirms the CC0 default is scoped to
"work produced **solely by federal employees**," and notes the Unlicense has been
cleared as an acceptable alternative where a venue (e.g., JOSS) rejects CC0 because it
is a dedication rather than a license. The page is silent on the bundled-license
mechanism itself — consistent with the gap DOI flagged above.

### 2.4 The upstream license texts — the actual controlling instruments

The retain-notice obligation comes from the components themselves, and is self-contained:

| Component | License | Core obligation when redistributed (source + binary) |
|-----------|---------|------------------------------------------------------|
| PROJ.4 | MIT-like | Retain copyright + permission notice |
| netCDF | BSD-3-Clause | Retain copyright + conditions + disclaimer; no-endorsement clause |
| HDF5 | BSD-3-Clause | Same as above |
| zlib | zlib license | Don't misrepresent origin; mark altered sources; keep notice |

None of these conflict with CC0, because CC0 governs only USGS-authored code. CC0
cannot and does not purport to relicense third-party code — nor does it need to.

---

## 3. What SWB2 ships today, and the one thing to verify

- `LICENSE.md` — CC0, opening "Unless otherwise noted." ✔
- `THIRD_PARTY_LICENSES.md` — PROJ.4 / netCDF / HDF5 / zlib notices + full texts. ✔
- Vendored `src/proj4/` — per-file upstream copyright headers retained (supplemented,
  not replaced, by the consolidated file). ✔
- `src/proj4/custom_swb_functions.c` — SWB-original (USGS/CC0), not upstream PROJ; fine
  to note as such.

**One item worth confirming:** BSD-3 and zlib carry the notice obligation for **binary**
distribution, not just source. Ensure `THIRD_PARTY_LICENSES.md` actually travels inside
any distributed binary artifact (e.g., the win64 zip), not only in the repo. This is the
most commonly missed detail and the only real gap risk in the current posture.

---

## 4. Recommended best practice (portable to other USGS projects)

1. Release USGS-authored code CC0; keep the "Unless otherwise noted" opening in
   `LICENSE.md`.
2. Do **not** relicense bundled third-party code; leave its headers intact.
3. Consolidate upstream notices + full license texts in `THIRD_PARTY_LICENSES.md`
   (a NOTICE-file-style aggregation). Retaining per-file headers *and* consolidating is
   belt-and-suspenders and the most transparent option.
4. List the components and their licenses in the dependencies documentation (satisfies
   SM 502.11 §4.D).
5. Ship the notices file with binary artifacts, not just source.
6. For Apache-2.0 components (not currently in SWB2), additionally propagate any upstream
   `NOTICE` file contents and note modified files — the one permissive license whose
   obligation a plain text dump doesn't fully cover.
7. For a formal sign-off on a specific release, route the framing (not a request to
   invent a method) to the Center software-release reviewer and/or the Office of the
   Solicitor, per DOI §V.1.c.iii's deferral.

---

## 5. Evidence strength (so future readers know what rests on what)

- **Authoritative, on-point:** DOI 2025 OSS Policy / SHARE IT Act §V.1.c.iii (pre-existing
  components released under their own terms).
- **Authoritative, adjacent:** SM 502.11 §4.D (dependency disclosure).
- **Interpretive:** WMA "Code in the Scientific Record" (CC0-default scoping; Unlicense
  alternative; silent on the mix).
- **Controlling for the notice obligation:** the upstream MIT/BSD/zlib license texts.
- **Not found:** any USGS/DOI document that prescribes a specific file name or format for
  third-party notices. DOI has stated such guidance is still being developed.

---

## 6. Source files (local)

- `usgs_requirements/DOI_SHARE_It_Act_Signed.pdf` (text extract:
  `DOI_SHARE_It_Act_Signed.txt`)
- `usgs_requirements/WMA-code-in-the-scientific-record.pdf` (scanned; read by page render)
- `usgs_requirements/SM_502.11.pdf` (scanned, no text layer) — section content
  corroborated by `usgs_requirements/policy_analysis__SM502.11_vs_operational_guidance.md`
- `THIRD_PARTY_LICENSES.md`, `LICENSE.md`, `src/proj4/` headers
