# AGENTS.md

Guidance for contributors and AI coding agents working on HOPE-CDC-analysis-2026. This is an **R analysis and Quarto website, not an R package**. The default branch is `master`.

## Sources of truth

Read these documents before substantive work:

- [`analysis-plan.md`](analysis-plan.md) — intended scientific questions, definitions, methods, and validation requirements. **Scientific decisions belong here.**
- [`ARCHITECTURE.md`](ARCHITECTURE.md) — a Posit-generated snapshot of **implemented** scripts, data flow, dependencies, and website structure, as of its stated commit. It is descriptive, not a roadmap.
- `AGENTS.md` — repository-wide working conventions, protection of restricted data, and expectations for checks and documentation.

If code and the plan disagree, **report the difference**. Do not quietly redefine the study in code or rewrite architecture to make it appear implemented. Obtain an investigator's decision before changing scientific methods. Do not add package infrastructure (`DESCRIPTION`, `NAMESPACE`, roxygen, pkgdown, CRAN workflows) to this analysis project.

## Experimentation and definitive analysis

Exploration is welcome. Contributors can trial alternative scripts, data structures, workflows, and methods on their own branches or forks without immediately integrating them into `R/main.R` or creating public Quarto pages. Label experimental findings as preliminary; do not present them as validated results.

**Every script adopted into the definitive analytical workflow must have a substantive Quarto explainer.** Explain its purpose, upstream inputs and prerequisites, main transformations and assumptions, downstream objects or outputs, and its place in the workflow. The explainer should display or reference the **actual source script**, not maintain a second, independently edited code copy. Where useful, split the script into labeled sections (`## ---- label`) and use `knitr::read_chunk()`; alternatively show the script using Quarto's `file:` option with execution disabled. Explain *why* choices were made, not only what each line does.

When integrating a definitive step, update orchestration and Quarto navigation as needed. Refresh `ARCHITECTURE.md` with Posit's `describe-design` skill after substantive implementation changes; verify it against the actual code and update its snapshot commit. Do not insert planned features into architecture as though they already exist.

## R workflow and maintainability

- The **current** workflow begins in `R/main.R`, which sources scripts in order. Consult `ARCHITECTURE.md` for existing dependencies and object naming. Preserve established interfaces when modifying existing steps, but do not impose the current global-object design on experiments.
- Shared constants and package setup ordinarily belong in `R/setup.R`; reusable helpers ordinarily belong in `R/functions.R`. Prefer functions with explicit arguments to additional hidden global dependencies.
- Use project-relative paths and portable file handling. Do not embed a contributor's absolute or Windows-specific paths; access restricted SRTR files only through authorized `sRtr` configuration.
- Preserve chunk labels used by Quarto. Check references before renaming or removing labels, and avoid collisions that would confuse `knitr::read_chunk()`.
- Favor readable, maintainable R code; do not perform unrelated stylistic rewrites during scientific changes.

## Scientific and geographic integrity

Apply the **current analysis plan**, not an assumed clinical or surveillance definition. Verify important variables and code values against SRTR SAF, CDC AtlasPlus, ACS/Census, HRSA/OPTN, and routing-provider documentation; cite references used for non-obvious definitions.

When changing analytical behavior, inspect relevant upstream inputs and downstream tables, plots, and Quarto interpretations. In particular:

- **Joins:** Specify keys and expected relationships; check duplicate keys, row multiplication, unmatched records, and geographic identifiers. Keep FIPS/GEOIDs as zero-padded characters.
- **Population:** Distinguish observed, suppressed, missing, and estimated counts. Check denominators, age/stratum compatibility, county-to-tract allocation, and reconciliation at the intended level. Never silently equate missing or suppressed with zero.
- **Centers:** Check organ-specific activity, classification windows, donor/recipient definitions, historical location assumptions, and whether centers are eligible in each year. Avoid duplicate events within organ analyses.
- **Space:** Check CRS and units (the current buffer computations use EPSG:5070/meters), geometry validity, tract inclusion rule, overlapping catchments, and geographic exclusions. Use dissolved catchments for aggregate access estimates and per-center catchments only when overlaps are intentionally allowed.
- **Travel time:** Keep driving isochrones, straight-line buffers, transit access, and travel-time matrices conceptually separate. Check routing profiles, reference times, coverage, API limits, and cached results.
- **Results:** Confirm empty groups are distinguished from missing inputs; inspect denominators and plausibility/order checks without assuming every subgroup necessarily nests unless the agreed definitions require it.

Checks should test scientific consequences and edge cases, not merely reproduce the same calculation. Record which checks ran, which did not, and any unavailable data or credentials.

## Quarto site, outputs, and caches

The Quarto website is configured through `_quarto.yml`, with rendered output in `docs/`. Definitive script pages explain methods; results pages should consume derived outputs rather than rerun restricted-data processing. Verify navigation, chunk labels, figure paths, captions/alt text, tables, citations, and consistency between displayed results and code.

Caches and figures are not automatically invalidated when upstream code, definitions, source data, or routing assumptions change. Identify stale dependencies and describe what needs rebuilding. **Do not assume that an existing cached or rendered result is current.** Before deleting caches, running a full pipeline, or triggering substantial API usage, follow the permissions of the person directing the work. Do not silently clear outputs or launch expensive reruns.

Keep source, generated outputs, and publication distinct. Changes to `docs/`, publishing, deployment, or externally visible actions require authorization under the contributor's workflow. Do not assume a rendered website reproduces results unless its sources and build inputs have been verified.

## Restricted data, confidentiality, and credentials

SRTR patient- and donor-level SAF data are restricted. These requirements apply regardless of collaborator or agent autonomy:

- Do not commit, upload, paste, log, or publish patient-level SRTR data or derived row-level extracts. Respect the governing DUA and institutional approvals; `.gitignore` is not a substitute for disclosure review.
- **AI agents must not inspect real patient-level SRTR data or send them to model providers.** Use synthetic, independently generated fixtures for agent-driven tests. Authorized investigators handle real-data analyses in approved environments.
- Even aggregates derived from restricted data require disclosure review under the applicable DUA, especially small cells and center-specific HIV transplant counts. Do not assume an output is shareable solely because it is aggregated or cached.
- Never commit secrets, credentials, access tokens, or local `.Renviron` files. Keep API keys in an approved external environment or user-level configuration, not in version-controlled source.
- Ask before paid, bulk, or otherwise expensive API calls (notably uncached Mapbox isochrones), full pipeline reruns, or operations requiring restricted data access.

Public ACS, CDC AtlasPlus, HRSA, and Census data can generally be used subject to their own terms; distinguish these from restricted SRTR records.

## Working with agents and reviewing changes

The person directing an agent controls its authorization. Some contributors may allow edits, synthetic-data runs, branches, and pull requests; others may require strictly read-only review. **Do not infer permission to edit, run, commit, push, publish, or deploy from the presence of this file.** Follow the stricter applicable instructions and ask before a consequential or externally visible action when authorization is unclear.

For meaningful changes, report the scientific or technical purpose, affected files and downstream outputs, evidence or documentation consulted, validation performed, limitations, and any divergence from the analysis plan. Distinguish verified defects from issues needing real-data confirmation. Prefer focused reviews and clear GitHub issues over maintaining a second implementation-gap ledger in this file.

## Reference material

Use the SRTR SAF dictionary and documentation, CDC AtlasPlus technical notes, Census ACS and geography documentation, HRSA/OPTN center documentation, relevant routing-provider API specifications, and peer-reviewed scientific literature as appropriate. Record specific references alongside the decisions they support.
