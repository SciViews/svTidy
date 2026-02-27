
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ‘SciViews::R’ - Tidy Functions <a href="https://www.sciviews.org/svTidy"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/SciViews/svTidy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SciViews/svTidy/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/SciViews/svTidy/graph/badge.svg?token=1YyLPD4HkZ)](https://app.codecov.io/gh/SciViews/svTidy)
[![CRAN
status](https://www.r-pkg.org/badges/version/svTidy)](https://CRAN.R-project.org/package=svTidy)
[![r-universe
status](https://sciviews.r-universe.dev/badges/svTidy)](https://sciviews.r-universe.dev/svTidy)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

SciViews equivalent functions of {dplyr} and {tidyr}, but faster and
using a standard evaluation of arguments or formulas.

## Installation

You can install the development version of {svTidy} from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("SciViews/svTidy")
```

## Example

This is a basic example which compares a rather classical Tidyverse
pipeline using {dplyr} with the equivalent in {svTidy}. For all humans
in the `starwars` dataset, we want the mean and standard deviation of
their age, two years after the Battle of Yavin (note that `birth_year`
is in years before that battle), grouped by their gender. We also want
the number of non-missing observations in each group.

``` r
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
data(starwars)
ages_dplyr <-
  starwars |>
  filter(species == "Human") |>
  mutate(age = 2 + birth_year) |>
  group_by(gender) |>
  summarise(
    mean_age  = mean(age, na.rm = TRUE),
    sd_age    = sd(age, na.rm = TRUE),
    n_age     = sum(!is.na(age))
  )
ages_dplyr
#> # A tibble: 2 × 4
#>   gender    mean_age sd_age n_age
#>   <chr>        <dbl>  <dbl> <int>
#> 1 feminine      48.4   18.8     5
#> 2 masculine     57.5   25.4    21
```

Here is the {svTidy} version (can you spot the three differences?):

``` r
library(svTidy, quietly = TRUE, warn.conflicts = FALSE)
#> Registered S3 methods overwritten by 'svMisc':
#>   method        from  
#>   print.section svBase
#>   str.section   svBase
ages_svTidy <- {
  .= starwars
  .= filter_(~species == "Human")
  .= mutate_(age = ~2 + birth_year)
  .= group_by_(~gender)
  .= summarise_(
    mean_age  = ~mean(age, na.rm = TRUE),
    sd_age    = ~sd(age, na.rm = TRUE),
    n_age     = ~sum(!is.na(age))
  )
}
identical(ages_dplyr, ages_svTidy)
#> [1] TRUE
```

Differences:

1.  The pipeline is replaced by a so-called **bullet-point** style using
    the `.=` pseudo-operator at the beginning of each step, conveying
    the idea of a list of instructions successively applied to the data.
    These are separate instructions, easier to debug than the single,
    extra-long expression as in the Tidyverse pipeline. The whole is
    grouped together by curly braces `{ }`, which is a common R idiom to
    group several instructions.

2.  {svTidy} functions have same name as the {dplyr} ones, but with an
    **underscore at the end**.

3.  All arguments that are **not** evaluated in a standard way are
    expressed as **formulas** (starting with a tilde `~`) in the
    {svTidy} functions. This conveys more explicitly the idea that they
    are evaluated in a special way… and it has other advantages; Read
    here under…

Now, if you want to generalize this code to other datasets, you could be
tempted to place it in a function. With {dplyr}, you have to take care
of non-standard evaluation in a non trivial way and understand what
**quasi-quotation** is, how to use `:=` for **variables in names**, …
Here is a version of the above code in a function with {dplyr}, then,
with {svTidy}:

``` r
ages_dplyr <- function(data, subset, var, year, group) {
  data |>
    filter({{ subset }}) |>
    mutate({{var}} := .env$year + .data$birth_year) |>
    group_by({{ group }}) |>
    summarise(
      "mean_{{var}}" := mean({{ var }}, na.rm = TRUE),
      "sd_{{var}}"   := sd({{ var }}, na.rm = TRUE),
      "n_{{var}}"    := sum(!is.na({{ var }}))
    )
}
ages_svTidy <- function(data, subset, var, year, group) {
  .__macros__. <- TRUE
  .= data
  .= filter_(subset)
  .= mutate_(var ~ year + birth_year)
  .= group_by_(group)
  .= summarise_(
    "mean_{{var}}" ~ mean(var, na.rm = TRUE),
    "sd_{{var}}"   ~ sd(var, na.rm = TRUE),
    "n_{{var}}"    ~ sum(!is.na(var))
  )
}
```

The {dplyr} version needs substantial edition of the code to include it
in a function and exhibits ultimatelly a less readable result that
{svTidy} that generalizes the code more smoothly. Here is the result of
using these functions:

``` r
ages_dplyr2 <- ages_dplyr(starwars, species == "Human", age, 2, gender)
ages_svTidy2 <- ages_svTidy(starwars, ~species == "Human", ~age, 2, ~gender)
identical(ages_dplyr2, ages_svTidy2)
#> [1] TRUE
ages_dplyr2
#> # A tibble: 2 × 4
#>   gender    mean_age sd_age n_age
#>   <chr>        <dbl>  <dbl> <int>
#> 1 feminine      48.4   18.8     5
#> 2 masculine     57.5   25.4    21
```

Note that the `ages_svTidy()` function somehow “inherits” the properties
of the underlying {svTidy} functions on the arguments that are evaluated
in a non-standard way, that have to be provided as formulas.

Now, some more improvement in `summarise_()` can be done by using
{collapse} **fast statistical functions**:

``` r
library(collapse, quietly = TRUE, warn.conflicts = FALSE)
#> collapse 2.1.0, see ?`collapse-package` or ?`collapse-documentation`
ages_svTidy_fast <- function(data, subset, var, year, group) {
  .__macros__. <- TRUE
  .= data
  .= filter_(subset)
  .= mutate_(var ~ year + birth_year)
  .= group_by_(group)
  .= summarise_(
    "mean_{{var}}" ~ fmean(var),
    "sd_{{var}}"   ~ fsd(var),
    "n_{{var}}"    ~ fnobs(var)
  )
}
ages_svTidy3 <- ages_svTidy_fast(starwars, ~species == "Human", ~age, 2, ~gender)
ages_svTidy3 # Note: there is a slight difference in the last decimals
#> # A tibble: 2 × 4
#>   gender    mean_age sd_age n_age
#>   <chr>        <dbl>  <dbl> <int>
#> 1 feminine      48.4   18.8     5
#> 2 masculine     57.5   25.4    21
```

Now, let’s compare {dplyr} and {svTidy} in terms of speed and memory
use:

``` r
bm <- bench::mark(
  dplyr = ages_dplyr(starwars, species == "Human", age, 2, gender),
  svTidy = ages_svTidy_fast(starwars, ~species == "Human", ~age, 2, ~gender)
)
bm
#> # A tibble: 2 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 dplyr        1.31ms   1.36ms      722.    85.6KB     113.
#> 2 svTidy     155.76µs 163.86µs     5988.    36.6KB     104.
```

With such a small dataset, we essentially measure the overhead of the
two approaches, and we can see that {svTidy} is 8.3 times faster, and it
requires 2.3 times less memory than {dplyr} in this case. Of course,
result vary with the functions used and size of the datasets.

For further instructions, please, refer to the help pages at
<https://www.sciviews.org/svTidy/>.

## Code of Conduct

Please note that the {svTidy} package is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
