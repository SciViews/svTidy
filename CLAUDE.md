# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package overview

{svTidy} is an R package providing dplyr/tidyr-equivalent functions with two evaluation modes:

- **Standard Evaluation (SE)**: Arguments evaluated in the calling environment (like base R)
- **Formula-based NSE**: Using `~` prefix formulas (e.g., `filter_(~eye_color == "hazel")`)

All functions end with `_` (e.g., `filter_()`, `select_()`, `mutate_()`, `pivot_longer_()`). Built on top of {collapse}, {data.table}, {dplyr}, and {tidyr}.

## Common commands

All commands are run from an R session in the package directory:

```r
# Load package for interactive development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-filtering.R")

# Full package check (as CRAN)
devtools::check()

# Rebuild documentation (roxygen2)
devtools::document()

# Rebuild README.md from README.Rmd
devtools::build_readme()

# Check spelling
spelling::spell_check_package()

# Build pkgdown site
pkgdown::build_site()
```

## Architecture

R source files are organized by data manipulation category:

| File | Functions |
|------|-----------|
| `R/filtering.R` | `filter_()`, `distinct_()`, `slice_*()` |
| `R/selecting.R` | `select_()`, `pull_()`, `rename_*()` |
| `R/mutating.R` | `mutate_()`, `transmute_()` |
| `R/arranging.R` | `arrange_()` |
| `R/grouping.R` | `group_by_()`, `ungroup_()`, group metadata functions |
| `R/summarising.R` | `summarise_()`, `reframe_()`, `count_()`, `tally_()` |
| `R/joining.R` | `*_join_()` functions |
| `R/binding.R` | `bind_rows_()`, `bind_cols_()` |
| `R/pivoting.R` | `pivot_longer_()`, `pivot_wider_()` |
| `R/tidying.R` | `separate_()`, `unite_()`, `complete_()`, `expand_*()` |
| `R/library.R` | `library_dplyr()`, `library_tidyr()` — load packages masking conflicting names |
| `R/aaa-internal.R` | Internal helpers |
| `R/reexport-svBase.R` | Re-exports from {svBase} (including `f_()`) |

### Formula-to-expression conversion

The core mechanism converts formula arguments to expressions evaluated in the data context. The left-hand side (LHS) of a formula becomes the output name; the right-hand side (RHS) is the expression. For example:

```r
mutate_(.data = df, varname ~ height^2)   # LHS = name, RHS = expression
filter_(.data = df, ~eye_color == "hazel") # No LHS = boolean predicate
```

### Key dependencies

- **{collapse}** and **{data.table}**: Performance backend
- **{dplyr}** / **{tidyr}**: Fallback and reference implementations
- **{svBase}**: Shared SciViews utilities, `f_()` function
- **{rlang}** / **{tidyselect}**: Expression handling

## Testing conventions

Tests use testthat 3.0+ (edition 3). Each test file mirrors a source file (e.g., `test-filtering.R` tests `filtering.R`). Tests verify:
- Both SE and formula-NSE modes
- Data type preservation (data.frame, data.table, tibble)
- Grouped and ungrouped behavior
- Consistency with dplyr/tidyr equivalents

## Pending work

See `TODO.md` for missing tidyr functions: `expand()`, `chop()`, `unchop()`, `nest()`, `unnest()` variants, `hoist()`, `pack()`, `unpack()`, and dplyr: `filter_out()`.

## Notes

- `README.md` is generated from `README.Rmd` — edit only the `.Rmd` source.
- `NAMESPACE` is managed by roxygen2 — do not edit manually.
- Minimum R version: 4.2.0.
