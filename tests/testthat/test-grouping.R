# Tests for grouping functions

test_that("group_by_() works with single grouping variable", {
  result <- mtcars |> group_by_(~cyl)

  expect_true(inherits(result, "GRP_df"))
  expect_equal(group_vars_(result), "cyl")
  expect_equal(n_groups_(result), 3)
})

test_that("group_by_() works with multiple grouping variables", {
  result <- mtcars |> group_by_(~cyl, ~gear)

  expect_true(inherits(result, "GRP_df"))
  expect_equal(group_vars_(result), c("cyl", "gear"))
  expect_equal(n_groups_(result), 8)
})

test_that("group_by_() works with character column names", {
  result <- mtcars |> group_by_("cyl", "gear")

  expect_true(inherits(result, "GRP_df"))
  expect_equal(group_vars_(result), c("cyl", "gear"))
})

test_that("group_by_() preserves data frame type", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("data.table")

  # Test with data.frame
  df_result <- mtcars |> group_by_(~cyl)
  expect_s3_class(df_result, "data.frame")

  # Test with tibble
  tbl <- tibble::as_tibble(mtcars)
  tbl_result <- tbl |> group_by_(~cyl)
  expect_s3_class(tbl_result, "tbl_df")

  # Test with data.table
  dt <- data.table::as.data.table(mtcars)
  dt_result <- dt |> group_by_(~cyl)
  expect_s3_class(dt_result, "data.table")
})

test_that("group_by_() with .add parameter adds to existing groups", {
  grouped <- mtcars |> group_by_(~cyl)
  result <- grouped |> group_by_(~gear, .add = TRUE)

  expect_equal(group_vars_(result), c("cyl", "gear"))
  expect_equal(n_groups_(result), 8)
})

test_that("group_by_() without .add replaces existing groups", {
  grouped <- mtcars |> group_by_(~cyl)
  result <- grouped |> group_by_(~gear, .add = FALSE)

  expect_equal(group_vars_(result), "gear")
})

test_that("group_by_() with .drop parameter handles factor levels", {
  df <- data.frame(
    x = factor(c("a", "b", "a"), levels = c("a", "b", "c")),
    y = 1:3
  )

  result_drop <- df |> group_by_(~x, .drop = TRUE)
  result_keep <- df |> group_by_(~x, .drop = FALSE)

  expect_equal(n_groups_(result_drop), 2)
  expect_equal(n_groups_(result_keep), 3)
})

test_that("ungroup_() removes all grouping", {
  grouped <- mtcars |> group_by_(~cyl, ~gear)
  result <- grouped |> ungroup_()

  expect_false(inherits(result, "GRP_df"))
  expect_equal(nrow(result), nrow(mtcars))
  expect_equal(ncol(result), ncol(mtcars))
})

test_that("ungroup_() with specific variables removes those groups", {
  grouped <- mtcars |> group_by_(~cyl, ~gear, ~am)
  expect_warning(result <- grouped |> ungroup_(~gear),
    "Regrouping with"
  )

  expect_true(inherits(result, "GRP_df"))
  expect_equal(group_vars_(result), c("cyl", "am"))
})

test_that("group_vars_() returns grouping variable names", {
  grouped <- mtcars |> group_by_(~cyl, ~gear)
  result <- grouped |> group_vars_()

  expect_equal(result, c("cyl", "gear"))
})

test_that("group_vars_() returns character vector for ungrouped data", {
  result <- mtcars |> group_vars_()

  expect_equal(result, character(0))
})

test_that("n_groups_() returns number of groups", {
  result <- mtcars |> group_by_(~cyl) |> n_groups_()

  expect_equal(result, 3)
})

test_that("n_groups_() returns 1 for ungrouped data", {
  result <- mtcars |> n_groups_()

  expect_equal(result, 1)
})

test_that("group_size_() returns size of each group", {
  grouped <- mtcars |> group_by_(~cyl)
  result <- grouped |> group_size_()

  expect_equal(length(result), 3)
  expect_equal(sum(result), nrow(mtcars))
})

test_that("group_indices_() returns group index for each row", {
  grouped <- mtcars |> group_by_(~cyl)
  result <- grouped |> group_indices_()

  expect_equal(length(result), nrow(mtcars))
  expect_equal(min(result), 1)
  expect_equal(max(result), 3)
})

test_that("group_keys_() returns unique grouping values", {
  grouped <- mtcars |> group_by_(~cyl, ~gear)
  result <- grouped |> group_keys_()

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("cyl", "gear"))
})

test_that("group_rows_() returns row indices for each group", {
  grouped <- mtcars |> group_by_(~cyl)
  result <- grouped |> group_rows_()

  expect_type(result, "list")
  expect_equal(length(result), 3)
  expect_equal(sum(lengths(result)), nrow(mtcars))
})

test_that("group_data_() returns grouping data with row indices", {
  grouped <- mtcars |> group_by_(~cyl, ~gear)
  result <- grouped |> group_data_()

  expect_s3_class(result, "data.frame")
  expect_true(".rows" %in% names(result))
  expect_equal(nrow(result), 8)
})

test_that("groups_() returns grouping variables as symbols", {
  grouped <- mtcars |> group_by_(~cyl, ~gear)
  result <- grouped |> groups_()

  expect_type(result, "list")
  expect_equal(length(result), 2)
})

test_that("group_by_() with empty formula creates no groups", {
  result <- mtcars |> group_by_()

  expect_false(inherits(result, "GRP_df"))
})

test_that("group_by_() works with computed variables", {
  result <- mtcars |> group_by_(cyl_group = ~ifelse(cyl < 6, "small", "large"))

  expect_true(inherits(result, "GRP_df"))
  expect_true("cyl_group" %in% names(result))
  expect_equal(n_groups_(result), 2)
})

test_that("group_by_() with .by parameter creates temporary groups", {
  result <- mtcars |>
    summarise_(
      mean_mpg = ~mean(mpg),
      .by = 'cyl'
    )

  expect_false(inherits(result, "GRP_df"))
  expect_equal(nrow(result), 3)
})

test_that("group_by_() preserves data for grouped operations", {
  grouped <- mtcars |> group_by_(~cyl)

  expect_equal(nrow(grouped), nrow(mtcars))
  expect_equal(ncol(grouped), ncol(mtcars))
})

test_that("group_by_() and ungroup_() roundtrip", {
  grouped <- mtcars |> group_by_(~cyl, ~gear)
  ungrouped <- grouped |> ungroup_()

  expect_false(inherits(ungrouped, "GRP_df"))
  expect_equal(ungrouped, mtcars)
})

test_that("group_by_() with single variable", {
  result <- mtcars |> group_by_(~hp)

  expect_true(inherits(result, "GRP_df"))
  expect_equal(group_vars_(result), "hp")
  expect_equal(n_groups_(result), 22)
})

test_that("group_by_() preserves row order within groups", {
  df <- data.frame(x = c(1, 2, 1, 2), y = c("a", "b", "c", "d"))
  grouped <- df |> group_by_(~x)
  indices <- grouped |> group_rows_()

  expect_equal(indices[[1]], c(1, 3))
  expect_equal(indices[[2]], c(2, 4))
})

test_that("group_size_() with single group", {
  df <- data.frame(x = c(1, 1, 1), y = 1:3)
  result <- df |> group_by_(~x) |> group_size_()

  expect_equal(result, 3)
})

test_that("group_keys_() with single grouping variable", {
  grouped <- mtcars |> group_by_(~cyl)
  result <- grouped |> group_keys_()

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 1)
  expect_equal(names(result), "cyl")
})

test_that("group_indices_() matches group assignment", {
  grouped <- mtcars |> group_by_(~cyl)
  indices <- grouped |> group_indices_()

  # Each group should have consistent indices for same cyl value
  for (i in 1:3) {
    cyl_val <- mtcars$cyl[which(indices == i)[1]]
    idx <- which(mtcars$cyl == cyl_val)
    expect_true(all(indices[idx] == i))
  }
})

test_that("group_by_() handles factors correctly", {
  df <- data.frame(
    x = factor(c("a", "b", "a", "b")),
    y = 1:4
  )
  result <- df |> group_by_(~x)

  expect_true(inherits(result, "GRP_df"))
  expect_equal(n_groups_(result), 2)
})

test_that("group_by_() handles dates correctly", {
  df <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-01")),
    value = 1:3
  )
  result <- df |> group_by_(~date)

  expect_true(inherits(result, "GRP_df"))
  expect_equal(n_groups_(result), 2)
})

test_that("group_by_() with multiple operations", {
  result <- mtcars |>
    group_by_(~cyl) |>
    group_by_(~gear, .add = TRUE) |>
    group_by_(~am, .add = TRUE)

  expect_equal(length(group_vars_(result)), 3)
})

test_that("ungroup_() with character column names", {
  grouped <- mtcars |> group_by_(~cyl, ~gear)
  expect_warning(result <- grouped |> ungroup_("gear"),
    "Regrouping with"
  )

  expect_equal(group_vars_(result), "cyl")
})

test_that("group_rows_() returns correct indices", {
  df <- data.frame(x = c(1, 2, 1, 2, 1), y = letters[1:5])
  grouped <- df |> group_by_(~x)
  result <- grouped |> group_rows_()

  expect_equal(result[[1]], c(1, 3, 5))
  expect_equal(result[[2]], c(2, 4))
})

test_that("group_data_() includes all grouping info", {
  grouped <- mtcars |> group_by_(~cyl)
  result <- grouped |> group_data_()

  expect_true("cyl" %in% names(result))
  expect_true(".rows" %in% names(result))
  expect_equal(nrow(result), 3)
})

test_that("groups_() returns correct symbols", {
  grouped <- mtcars |> group_by_(~cyl)
  result <- grouped |> groups_()

  expect_length(result, 1)
  expect_true(is.symbol(result[[1]]))
})

test_that("group_by_() with NA values in grouping variable", {
  df <- data.frame(x = c(1, 2, NA, 1, 2), y = 1:5)
  result <- df |> group_by_(~x)

  expect_true(inherits(result, "GRP_df"))
  # NA should create its own group
  expect_true(n_groups_(result) >= 2)
})

test_that("group_by_() maintains attributes", {
  df <- mtcars
  attr(df, "custom") <- "test"
  result <- df |> group_by_(~cyl)

  expect_equal(attr(result, "custom"), "test")
})

test_that("n_groups_() after filtering", {
  filtered <- mtcars |> filter_(~cyl > 4)
  grouped <- filtered |> group_by_(~cyl)
  result <- grouped |> n_groups_()

  expect_equal(result, 2)
})

test_that("group_size_() after filtering", {
  filtered <- mtcars |> filter_(~hp > 100)
  grouped <- filtered |> group_by_(~cyl)
  result <- grouped |> group_size_()

  expect_equal(length(result), n_groups_(grouped))
  expect_equal(sum(result), nrow(filtered))
})
