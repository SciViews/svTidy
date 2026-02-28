
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

> {svTidy} provides functions equivalent to
> [{dplyr}](https://dplyr.tidyverse.org) and
> [{tidyr}](https://tidyr.tidyverse.org), but using either a standard
> evaluation of arguments or formulas, both being closer to base R
> syntax. Its functions are also usually faster and consume less RAM.

{svTidy} function names are the same as the {dplyr} or {tidyr} ones, but
with an underscore at the end (e.g. `filter_()`, `select_()`,
`mutate_()`, `pivot_longer_()`…). They accept standard evaluation (SE)
of their arguments, similar to most base R functions, making them less
alien to base R users:

``` r
data('starwars', package = 'dplyr')

# Subsetting with base R
starwars[starwars$eye_color == "hazel", c('name', 'height', 'mass')]
#> # A tibble: 3 × 3
#>   name           height  mass
#>   <chr>           <int> <dbl>
#> 1 Wedge Antilles    170    77
#> 2 Ayla Secura       178    55
#> 3 Rey                NA    NA

# Subsetting with {svTidy}, SE mode
starwars |>
  filter_(starwars$eye_color == "hazel") |>
  select_(c('name', 'height', 'mass'))
#> # A tibble: 3 × 3
#>   name           height  mass
#>   <chr>           <int> <dbl>
#> 1 Wedge Antilles    170    77
#> 2 Ayla Secura       178    55
#> 3 Rey                NA    NA
```

They also accept **formula** arguments that implement a non-standard
evaluation (NSE) similar to the [Tidyverse](https://tidyverse.org) DSL
(domain-specific language), which is more familiar to {dplyr} and
{tidyr} users. A tilde `~` is added at the beginning of the arguments
that are evaluated in a non-standard way to turn them into formulas.
*This conveys more explicitly the idea that they are evaluated in a
special way:*

``` r
# {dplyr} code:
starwars |>
  filter(eye_color == "hazel") |> # data masking NSE
  select(name:mass) # tidy selection NSE
#> # A tibble: 3 × 3
#>   name           height  mass
#>   <chr>           <int> <dbl>
#> 1 Wedge Antilles    170    77
#> 2 Ayla Secura       178    55
#> 3 Rey                NA    NA

# {svTidy}, NSE mode
starwars |>
  filter_(~eye_color == "hazel") |> # formula masking NSE
  select_(~name:mass) # idem
#> # A tibble: 3 × 3
#>   name           height  mass
#>   <chr>           <int> <dbl>
#> 1 Wedge Antilles    170    77
#> 2 Ayla Secura       178    55
#> 3 Rey                NA    NA
```

The formula interface, also called **“formula masking”**, is more
similar to many base R functions that also use formulas (e.g. `lm()`)
than data masking or tidy selection in {dplyr}:

``` r
# A base R function from {stats} using a formula interface
res_lm <- lm(data = starwars, mass ~ height^2)

# {svTidy} function used in NSE mode with a formula
varname <- "height2"
res <- mutate_(.data = starwars, varname ~ height^2)
res[1:3, c('name', 'height', 'height2')]
#> # A tibble: 3 × 3
#>   name           height height2
#>   <chr>           <int>   <dbl>
#> 1 Luke Skywalker    172   29584
#> 2 C-3PO             167   27889
#> 3 R2-D2              96    9216
```

There are many similarities between the call to `lm()` and to
`mutate_()` above, with a common template being
`<fun>(data = <df>, <formula>)`. Also note the clean way to define the
name of a new variable with {svTidy} by indicating it at the left-hand
side of the formula (`varname ~ expr` when its name is stored in
`varname`, or directly with `'name' ~ expr`). This is arguably much
smoother than the `:=` and `{{ }}` operators needed in {dplyr} for
**name injection**, resulting in code that is so different to base R
syntax:

``` r
# {dplyr} version
var <- quo(height2) # You cannot just provide a string here!
res2 <- mutate(.data = starwars, {{var}} := height^2) # Name injection
all.equal(res, res2)
#> [1] TRUE
```

We got leaner code thanks to formulas in {svTidy} for many advanced
features. You will learn more here under in the section **more complex
example**.

## Installation

You can install the development version of {svTidy} from the [SciViews
R-universe](https://sciviews.r-universe.dev) with:

``` r
install.packages('svTidy', repos = c('https://sciviews.r-universe.dev',
  'https://cloud.r-project.org'))
```

Alternatively, you can also install it from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("SciViews/svTidy")
```

Being still in the experimental stage, it is not available yet on
[CRAN](https://cran.r-project.org), but it will be submitted as soon as
it matures.

## More complex example

The best way to appreciate {svTidy} is by comparing it to {dplyr} on a
little bit more complex example. Still using the `starwars` dataset,
let’s say we want to calculate the mean and standard deviation of the
age of all humans, two years after the Battle of Yavin (note that
`birth_year` is in years before that battle, so, you got it with
`birth_date + 2` in years), grouped by their gender. We also want the
number of non-missing observations in each group.

``` r
# {dplyr} version
ages_dplyr <-
  starwars |>
  filter(species == "Human") |>
  mutate(age = birth_year + 2) |>
  group_by(gender) |>
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age   = sd(age, na.rm = TRUE),
    n_age    = sum(!is.na(age))
  )
ages_dplyr
#> # A tibble: 2 × 4
#>   gender    mean_age sd_age n_age
#>   <chr>        <dbl>  <dbl> <int>
#> 1 feminine      48.4   18.8     5
#> 2 masculine     57.5   25.4    21
```

Here is the {svTidy} version (can you spot the three sets of
differences?):

``` r
# {svTidy} version
ages_svTidy <- {
  .= starwars
  .= filter_(~species == "Human")
  .= mutate_(age = ~birth_year + 2)
  .= group_by_(~gender)
  .= summarise_(
    mean_age = ~mean(age, na.rm = TRUE),
    sd_age   = ~sd(age, na.rm = TRUE),
    n_age    = ~sum(!is.na(age))
  )
}
all.equal(ages_dplyr, ages_svTidy)
#> [1] TRUE
```

Differences:

1.  {svTidy} functions have the same name as the {dplyr} ones, but with
    an **underscore at the end**.

2.  All **NSE arguments are turned into formulas** by prepending them
    with a tilde `~`.

3.  Optionally, the pipeline is replaced by a so-called **bullet-point**
    style using the `.=` pseudo-operator at the beginning of each step,
    conveying the idea of a list of instructions successively applied to
    the data. These are separate instructions, easier to debug than the
    single, extra-long expression of the Tidyverse pipeline. The whole
    is grouped together by curly braces `{ }`, a common R idiom to group
    several instructions. This style is also possible thanks to the
    **“data-dot”** mechanism of {svTidy} functions that automatically
    insert the dot `.` as default first argument of the function when no
    data frame is provided \[TODO: link to a vignette that explains
    this\].

Now, if you want to generalize this code to other datasets, you could be
tempted to place it in a function. With {dplyr}, you have to take care
of NSE in a non-trivial way and understand what **quasi-quotation** (or
**indirection**) is, how to use `:=` for **name injection**, … (the
[Programming with
dplyr](https://dplyr.tidyverse.org/articles/programming.html) vignette
details all this). Here is a version of the above code in a function
with {dplyr}, then, with {svTidy}:

``` r
ages_dplyr <- function(data, subset, var, year, group) {
  data |>
    filter({{ subset }}) |> # Indirection
    mutate({{var}} := .data$birth_year + .env$year) |> # Name injection + .data/.env
    group_by({{ group }}) |>
    summarise(
      "mean_{{var}}" := mean({{ var }}, na.rm = TRUE),
      "sd_{{var}}"   := sd({{ var }}, na.rm = TRUE),
      "n_{{var}}"    := sum(!is.na({{ var }}))
    )
}
ages_svTidy <- function(data, subset, var, year, group) {
  .= data
  .= filter_(subset) # Just replace the expression with the arg name
  .= mutate_(var ~ birth_year + year) # Name on the left of the formula
  .= group_by_(group)
  .= summarise_(
    "mean_{{var}}" ~ mean(var, na.rm = TRUE),
    "sd_{{var}}"   ~ sd(var, na.rm = TRUE),
    "n_{{var}}"    ~ sum(!is.na(var))
  )
}
```

The {dplyr} version needs a substantial edition of the code to include
it in a function and exhibits ultimately a more complex result that
{svTidy} that generalizes the code more smoothly. Here is the result of
using these two functions:

``` r
ages_dplyr_res <- ages_dplyr(starwars, species == "Human", age, 2, gender)
ages_svTidy_res <- ages_svTidy(starwars, ~species == "Human", ~age, 2, ~gender)
all.equal(ages_dplyr_res, ages_svTidy_res)
#> [1] TRUE
ages_dplyr_res
#> # A tibble: 2 × 4
#>   gender    mean_age sd_age n_age
#>   <chr>        <dbl>  <dbl> <int>
#> 1 feminine      48.4   18.8     5
#> 2 masculine     57.5   25.4    21
```

Note that `ages_svTidy()` arguments somehow naturally “inherit” the
properties of the underlying {svTidy} functions: NSE expressions are to
be provided as formulas. Consistency and clarity are important features
of the code!

Some more improvement in `summarise_()` can be done by using {collapse}
**fast statistical functions**:

``` r
library('collapse')
ages_fast <- function(data, subset, var, year, group) {
  .= data
  .= filter_(subset)
  .= mutate_(var ~ birth_year + year)
  .= group_by_(group)
  .= summarise_(
    "mean_{{var}}" ~ fmean(var), # na.rm = TRUE by default
    "sd_{{var}}"   ~ fsd(var),   # Idem
    "n_{{var}}"    ~ fnobs(var)  # Same as sum(!is.na(var))
  )
}
ages_fast_res <- ages_fast(starwars, ~species == "Human", ~age, 2, ~gender)
all.equal(ages_dplyr_res, ages_fast_res)
#> [1] TRUE
```

Now, let’s compare {dplyr} and {svTidy} in terms of speed and memory
use:

``` r
bm <- bench::mark(
  dplyr  = ages_dplyr(starwars, species == "Human", age, 2, gender),
  svTidy = ages_svTidy(starwars, ~species == "Human", ~age, 2, ~gender),
  fast   = ages_fast(starwars, ~species == "Human", ~age, 2, ~gender)
)
bm
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 dplyr        1.32ms   1.44ms      692.    85.6KB     98.2
#> 2 svTidy     168.47µs 188.48µs     5311.    34.9KB     94.1
#> 3 fast       160.72µs 180.89µs     5526.    34.2KB     37.5
```

With such a small dataset, we essentially measure the overhead of the
different approaches, and we can see that {svTidy} (fast version) is 8
times faster, and it requires 2.5 times less memory than {dplyr} in this
case. This is because {svTidy} uses a more lightweight mechanism thanks
to the formulas, and also (although of limited impact on small datasets)
because it relies on efficient {collapse} or {data.table} code to do the
computation. Of course, results vary with the functions used and the
dataset size.

Not shown here, but if you want to include these functions in an R
package, you will have some trouble with the {dplyr} version and
`R CMD check` will complain unless you take some precautions, like
reexporting the `.data` and `.env` pronouns… The {svTidy} version can be
used in an R package without any special care. But that’s another story,
and another step in the complexity of the Tidyverse mechanisms that is
avoided by {svTidy}.

> Please note that this comparison is not meant to criticize the
> Tidyverse, {dplyr} or {tidyr}. If {svTidy} follows the same API, it is
> because we praise it as an excellent modular set of functions to
> manipulate data frames. We just want to provide an alternative that is
> more consistent with base R syntax and semantics, and that is more
> efficient in terms of speed and memory use when feasible. As great is
> the Tidyverse, it is not the only solution. The R ecosystem is large
> and diverse. It encourages the exploration of new ideas and this is
> precisely what we do here with {svTidy}, in full respect to the huge
> work done by Hadley Wickham and a group of very talented contributors!

For further instructions, refer to the help pages at
<https://www.sciviews.org/svTidy/>.

## Code of Conduct

Please note that the {svTidy} package is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
