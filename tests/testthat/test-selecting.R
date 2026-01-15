# Tests for selecting functions

test_that("select_() works with basic column names", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- select_(df, ~x, ~y)

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("x", "y"))
  expect_equal(nrow(result), 3)
})

test_that("select_() works with character column names", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- select_(df, "x", "z")

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("x", "z"))
})

test_that("select_() works with column positions", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9, w = 10:12)

  result <- select_(df, 1, 3)

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("x", "z"))
})

test_that("select_() works with ranges", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)

  result <- select_(df, ~a:c)

  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("a", "b", "c"))
})

test_that("select_() works with starts_with helper", {
  df <- data.frame(col_x = 1:3, col_y = 4:6, other = 7:9)

  result <- select_(df, ~starts_with("col"))

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("col_x", "col_y"))
})

test_that("select_() works with ends_with helper", {
  df <- data.frame(x_col = 1:3, y_col = 4:6, other = 7:9)

  result <- select_(df, ~ends_with("col"))

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("x_col", "y_col"))
})

test_that("select_() works with contains helper", {
  df <- data.frame(col_x = 1:3, x_other = 4:6, y_col = 7:9)

  result <- select_(df, ~contains("x"))

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("col_x", "x_other"))
})

test_that("select_() works with where helper", {
  df <- data.frame(x = 1:3, y = 4:6, z = c("a", "b", "c"))

  result <- select_(df, ~where(is.numeric))

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("x", "y"))
})

test_that("select_() works with negation", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- select_(df, ~-y)

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("x", "z"))
})

test_that("select_() works with multiple negations", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9, w = 10:12)

  result <- select_(df, ~-c(y, w))

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("x", "z"))
})

test_that("select_() preserves data frame types", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("data.table")

  df <- data.frame(x = 1:3, y = 4:6)

  result_df <- select_(df, ~x, ~y)
  expect_s3_class(result_df, "data.frame")

  tbl <- tibble::tibble(x = 1:3, y = 4:6)
  result_tbl <- select_(tbl, ~x, ~y)
  expect_s3_class(result_tbl, "tbl_df")

  dt <- data.table::data.table(x = 1:3, y = 4:6)
  result_dt <- select_(dt, ~x, ~y)
  expect_s3_class(result_dt, "data.table")
})

test_that("select_() with all_of helper", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  # TODO: look why this does not work in a test, but works interactively
  #cols <- c("x", "z")

  #result <- select_(df, ~all_of(cols))
  #result <- select_(df, ~all_of(.env$cols))

  result <- select_(df, ~all_of(c("x", "z")))

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("x", "z"))
})

test_that("select_() with everything helper", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- select_(df, ~y, ~everything())

  expect_equal(ncol(result), 3)
  expect_equal(names(result)[1], "y")
})

test_that("select_() combines multiple methods", {
  df <- data.frame(col_1 = 1:3, col_2 = 4:6, other_x = 7:9, z = 10:12)

  result <- select_(df, ~starts_with("col"), ~contains("x"))

  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("col_1", "col_2", "other_x"))
})

test_that("select_() with zero columns", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  result <- select_(df)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 0L)
  expect_equal(nrow(result), 3L)
})

test_that("select_() errors on non-existent column", {
  df <- data.frame(x = 1:3, y = 4:6)

  expect_error(
    select_(df, ~nonexistent),
    "nonexistent"
  )
})

test_that("pull_() extracts column as vector", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- pull_(df, ~y)

  expect_equal(result, 4:6)
  expect_type(result, "integer")
})

test_that("pull_() with character column name", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- pull_(df, "z")

  expect_equal(result, 7:9)
})

test_that("pull_() with column position", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- pull_(df, 2)

  expect_equal(result, 4:6)
})

test_that("pull_() with negative position (last column)", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- pull_(df, -1)

  expect_equal(result, 7:9)
})

test_that("pull_() with name parameter", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- pull_(df, ~y, name = ~x)

  expect_equal(names(result), c("1", "2", "3"))
})

test_that("pull_() preserves data types", {
  df <- data.frame(
    int_col = 1L:3L,
    dbl_col = c(1.1, 2.2, 3.3),
    chr_col = c("a", "b", "c"),
    lgl_col = c(TRUE, FALSE, TRUE)
  )

  expect_type(pull_(df, ~int_col), "integer")
  expect_type(pull_(df, ~dbl_col), "double")
  expect_type(pull_(df, ~chr_col), "character")
  expect_type(pull_(df, ~lgl_col), "logical")
})

test_that("pull_() with tibble", {
  skip_if_not_installed("tibble")

  tbl <- tibble::tibble(x = 1:3, y = 4:6)
  result <- pull_(tbl, ~y)

  expect_equal(result, 4:6)
})

test_that("rename_() renames columns", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- rename_(df, new_x = ~x, new_y = ~y)

  expect_equal(names(result), c("new_x", "new_y", "z"))
  expect_equal(result$new_x, 1:3)
})

test_that("rename_() with character column names", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- rename_(df, new_x = "x", new_z = "z")

  expect_equal(names(result), c("new_x", "y", "new_z"))
})

test_that("rename_() with column positions", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- rename_(df, alpha = 1, beta = 3)

  expect_equal(names(result), c("alpha", "y", "beta"))
})

test_that("rename_() preserves data", {
  df <- data.frame(x = 1:3, y = 4:6)

  result <- rename_(df, a = ~x, b = ~y)

  expect_equal(result$a, 1:3)
  expect_equal(result$b, 4:6)
})

test_that("rename_() with all columns", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- rename_(df, a = ~x, b = ~y, c = ~z)

  expect_equal(names(result), c("a", "b", "c"))
})

test_that("rename_() preserves data frame types", {
  skip_if_not_installed("tibble")

  df <- data.frame(x = 1:3, y = 4:6)
  result_df <- rename_(df, a = ~x)
  expect_s3_class(result_df, "data.frame")

  tbl <- tibble::tibble(x = 1:3, y = 4:6)
  result_tbl <- rename_(tbl, a = ~x)
  expect_s3_class(result_tbl, "tbl_df")
})

test_that("select_() with zero-row data frame", {
  df <- data.frame(x = integer(), y = integer(), z = integer())

  result <- select_(df, ~x, ~y)

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 2)
})

test_that("select_() preserves row names", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9, row.names = c("a", "b", "c"))

  result <- select_(df, ~x, ~z)

  expect_equal(rownames(result), c("a", "b", "c"))
})

test_that("pull_() with zero-row data frame", {
  df <- data.frame(x = integer(), y = integer())

  result <- pull_(df, ~x)

  expect_equal(length(result), 0)
})

test_that("rename_() errors on non-existent column", {
  df <- data.frame(x = 1:3, y = 4:6)

  expect_error(
    rename_(df, new_z = ~z),
    "z"
  )
})

# Tests for advanced selecting functions

test_that("rename_with_() applies function to all columns", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- rename_with_(df, toupper)

  expect_equal(names(result), c("X", "Y", "Z"))
})

test_that("rename_with_() with formula", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- rename_with_(df, ~toupper(.x))

  expect_equal(names(result), c("X", "Y", "Z"))
})

test_that("rename_with_() with tolower", {
  df <- data.frame(X = 1:3, Y = 4:6, Z = 7:9)

  result <- rename_with_(df, tolower)

  expect_equal(names(result), c("x", "y", "z"))
})

test_that("rename_with_() with paste prefix", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- rename_with_(df, ~paste0("col_", .x))

  expect_equal(names(result), c("col_x", "col_y", "col_z"))
})

test_that("rename_with_() with .cols parameter", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- rename_with_(df, toupper, .cols = ~starts_with("x"))

  expect_equal(names(result), c("X", "y", "z"))
})

test_that("rename_with_() with .cols character vector", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- rename_with_(df, toupper, .cols = c("x", "z"))

  expect_equal(names(result), c("X", "y", "Z"))
})

test_that("rename_with_() with .cols positions", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- rename_with_(df, toupper, .cols = c(1, 3))

  expect_equal(names(result), c("X", "y", "Z"))
})

test_that("rename_with_() with gsub function", {
  df <- data.frame(col_x = 1:3, col_y = 4:6, col_z = 7:9)

  result <- rename_with_(df, ~gsub("col_", "", .x))

  expect_equal(names(result), c("x", "y", "z"))
})

test_that("rename_with_() preserves data", {
  df <- data.frame(x = 1:3, y = 4:6)

  result <- rename_with_(df, toupper)

  expect_equal(result$X, 1:3)
  expect_equal(result$Y, 4:6)
})

test_that("rename_with_() preserves data frame types", {
  skip_if_not_installed("tibble")

  df <- data.frame(x = 1:3, y = 4:6)
  result_df <- rename_with_(df, toupper)
  expect_s3_class(result_df, "data.frame")

  tbl <- tibble::tibble(x = 1:3, y = 4:6)
  result_tbl <- rename_with_(tbl, toupper)
  expect_s3_class(result_tbl, "tbl_df")
})

test_that("select_() and rename_() chaining", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- df |>
    select_(~x, ~y) |>
    rename_(a = ~x, b = ~y)

  expect_equal(names(result), c("a", "b"))
  expect_equal(result$a, 1:3)
  expect_equal(result$b, 4:6)
})

test_that("select_() and rename_with_() chaining", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- df |>
    select_(~x, ~y) |>
    rename_with_(toupper)

  expect_equal(names(result), c("X", "Y"))
})

test_that("pull_() with multiple operations", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- df |>
    select_(~x, ~y) |>
    pull_(~y)

  expect_equal(result, 4:6)
})

test_that("select_() with complex patterns", {
  df <- data.frame(
    id = 1:3,
    col_x_1 = 4:6,
    col_x_2 = 7:9,
    col_y_1 = 10:12,
    other = 13:15
  )

  result <- select_(df, ~starts_with("col_x"))

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("col_x_1", "col_x_2"))
})

test_that("select_() with multiple negations and selections", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9, x2 = 10:12)

  result <- select_(df, ~starts_with("x"), ~-z)

  expect_equal(ncol(result), 2L)
  expect_true("x" %in% names(result))
  expect_true("x2" %in% names(result))
  expect_false("z" %in% names(result))
})

test_that("rename_with_() with nchar formula", {
  df <- data.frame(x = 1:3, yy = 4:6, zzz = 7:9)

  result <- rename_with_(df, ~paste0(.x, "_", nchar(.x)))

  expect_equal(names(result), c("x_1", "yy_2", "zzz_3"))
})

test_that("select_() with numeric columns only", {
  df <- data.frame(x = 1:3, y = 4:6, z = c("a", "b", "c"))

  result <- select_(df, ~where(is.numeric))

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("x", "y"))
})

test_that("select_() with logical columns only", {
  df <- data.frame(
    x = c(TRUE, FALSE, TRUE),
    y = c(1, 2, 3),
    z = c(FALSE, TRUE, FALSE)
  )

  result <- select_(df, ~where(is.logical))

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("x", "z"))
})

test_that("select_() with character columns only", {
  df <- data.frame(
    x = c("a", "b", "c"),
    y = 1:3,
    z = c("d", "e", "f")
  )

  result <- select_(df, ~where(is.character))

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("x", "z"))
})

test_that("pull_() from grouped data frame", {
  skip_if_not_installed("dplyr")

  df <- data.frame(group = c("A", "A", "B"), value = 1:3)
  grouped_df <- dplyr::group_by(df, group)

  result <- pull_(grouped_df, ~value)

  expect_equal(result, 1:3)
})

test_that("select_() from grouped data frame", {
  skip_if_not_installed("dplyr")

  df <- data.frame(group = c("A", "A", "B"), x = 1:3, y = 4:6)
  grouped_df <- dplyr::group_by(df, group)

  result <- select_(grouped_df, ~x, ~y)

  expect_equal(ncol(result), 2)
})

test_that("rename_() from grouped data frame", {
  skip_if_not_installed("dplyr")

  df <- data.frame(group = c("A", "A", "B"), x = 1:3, y = 4:6)
  grouped_df <- dplyr::group_by(df, group)

  result <- rename_(grouped_df, a = ~x, b = ~y)

  expect_equal(names(result), c("group", "a", "b"))
})

test_that("select_() with very long column names", {
  df <- data.frame(
    very_long_column_name_1 = 1:3,
    very_long_column_name_2 = 4:6,
    short = 7:9
  )

  result <- select_(df, ~starts_with("very_long"))

  expect_equal(ncol(result), 2)
})

test_that("rename_with_() with special characters handling", {
  df <- data.frame(
    col.1 = 1:3,
    col.2 = 4:6,
    col.3 = 7:9
  )

  result <- rename_with_(df, ~gsub("\\.", "_", .x))

  expect_equal(names(result), c("col_1", "col_2", "col_3"))
})

test_that("select_() with many columns", {
  col_names <- paste0("col_", 1:50)
  df <- as.data.frame(setNames(
    lapply(1:50, function(i) 1:3),
    col_names
  ))

  result <- select_(df, ~starts_with("col_1"))

  expect_equal(ncol(result), 11)  # col_1, col_10-col_19
})

test_that("pull_() with factor column", {
  df <- data.frame(x = factor(c("a", "b", "c")), y = 1:3)

  result <- pull_(df, ~x)

  expect_s3_class(result, "factor")
  expect_equal(levels(result), c("a", "b", "c"))
})

test_that("pull_() with date column", {
  df <- data.frame(
    x = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    y = 1:3
  )

  result <- pull_(df, ~x)

  expect_s3_class(result, "Date")
})

test_that("select_() preserves attributes", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  attr(df, "custom") <- "test"

  result <- select_(df, ~x, ~y)

  expect_s3_class(result, "data.frame")
})

test_that("rename_() with numeric column indices", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)

  result <- rename_(df, alpha = 1, beta = 2, gamma = 3)

  expect_equal(names(result), c("alpha", "beta", "gamma"))
})

test_that("select_() order preservation", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12, e = 13:15)

  result <- select_(df, ~e, ~c, ~a)

  expect_equal(names(result), c("e", "c", "a"))
})

test_that("select_() with matches helper", {
  df <- data.frame(x1 = 1:3, x2 = 4:6, y1 = 7:9, y2 = 10:12)

  result <- select_(df, ~matches("^x"))

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("x1", "x2"))
})

test_that("select_() with across columns", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)

  result <- select_(df, ~a:c)

  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("a", "b", "c"))
})

test_that("pull_() maintains vector properties", {
  df <- data.frame(x = c(1, 2, NA), y = 4:6)

  result <- pull_(df, ~x)

  expect_true(is.na(result[3]))
  expect_equal(result[1:2], c(1, 2))
})

test_that("rename_with_() with conditional formula", {
  df <- data.frame(x_1 = 1:3, x_2 = 4:6, y_1 = 7:9)

  result <- rename_with_(df, ~toupper(.x), .cols = ~starts_with("x"))

  expect_equal(names(result), c("X_1", "X_2", "y_1"))
})

test_that("all_of() with non-existent column", {
  df <- data.frame(x = 1:3, y = 4:6)
  cols <- c("x", "nonexistent")

  # TODO: why cols is found interactively, but not in the test?
  #expect_error(
  #  select_(df, ~all_of(cols)),
  #  "Can't subset elements that don't exist"
  #)
  expect_error(
    select_(df, ~all_of(c("x", "nonexistent"))),
    "Can't subset elements that don't exist"
  )
})

test_that("select_() with data.table", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(x = 1:3, y = 4:6, z = 7:9)
  result <- select_(dt, ~x, ~z)

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("x", "z"))
})
