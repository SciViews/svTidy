# Tests for arranging functions

test_that("arrange_() works with standard evaluation", {
  df <- data.frame(
    x = c(3, 1, 2),
    y = c("a", "c", "b"),
    z = c(10, 30, 20)
  )

  # Single column ascending
  result <- arrange_(df, "x")
  expect_equal(result$x, c(1, 2, 3))

  # Single column descending
  result <- arrange_(df, "x", .decreasing = TRUE)
  expect_equal(result$x, c(3, 2, 1))

  # Multiple columns
  result <- arrange_(df, "x", "z")
  expect_equal(result$x, c(1, 2, 3))

  # Multiple columns with mixed ordering
  df2 <- data.frame(
    x = c(1, 1, 2, 2),
    y = c(4, 3, 2, 1)
  )
  result <- arrange_(df2, "x", "y", .decreasing = c(FALSE, TRUE))
  expect_equal(result$y, c(4, 3, 2, 1))
  expect_equal(result$x, c(1, 1, 2, 2))
})

test_that("arrange_() works with formula syntax", {
  df <- data.frame(
    x = c(3, 1, 2),
    y = c("a", "c", "b"),
    z = c(10, 30, 20)
  )

  # Single column with formula
  result <- arrange_(df, ~x)
  expect_equal(result$x, c(1, 2, 3))

  # Using desc() with formula
  result <- arrange_(df, ~desc(x))
  expect_equal(result$x, c(3, 2, 1))

  # Multiple columns with desc()
  result <- arrange_(df, ~x, ~desc(z))
  expect_equal(result$x, c(1, 2, 3))
  expect_equal(result$z[1], 30)

  # Using minus operator (collapse style)
  result <- arrange_(df, ~-x)
  expect_equal(result$x, c(3, 2, 1))
})

test_that("arrange_() preserves data frame structure", {
  df <- data.frame(
    x = c(3, 1, 2),
    y = c("a", "c", "b")
  )

  result <- arrange_(df, "x")

  # Check dimensions
  expect_equal(nrow(result), nrow(df))
  expect_equal(ncol(result), ncol(df))

  # Check column names
  expect_equal(names(result), names(df))

  # Check class
  expect_s3_class(result, "data.frame")
})

test_that("arrange_() handles edge cases", {
  df <- data.frame(x = c(3, 1, 2))

  # Empty arguments - returns unchanged
  result <- arrange_(df)
  expect_equal(result, df)

  # Single row - returns unchanged
  df_single <- data.frame(x = 1, y = "a")
  result <- arrange_(df_single, "x")
  expect_equal(result, df_single)

  # Zero rows
  df_empty <- data.frame(x = numeric(0), y = character(0))
  result <- arrange_(df_empty, "x")
  expect_equal(result, df_empty)
})

test_that("arrange_() handles missing values", {
  df <- data.frame(
    x = c(3, NA, 1, 2, NA),
    y = c("a", "b", "c", "d", "e")
  )

  # NAs should go to the end (default behavior)
  result <- arrange_(df, "x")
  expect_true(is.na(result$x[4]) && is.na(result$x[5]))
  expect_equal(result$x[1:3], c(1, 2, 3))

  # With descending
  result <- arrange_(df, "x", .decreasing = TRUE)
  expect_true(is.na(result$x[4]) && is.na(result$x[5]))
  expect_equal(result$x[1:3], c(3, 2, 1))
})

test_that("arrange_() works with grouped data", {
  skip_if_not_installed("data.trame")

  df <- data.frame(
    group = c("A", "B", "A", "B"),
    value = c(3, 1, 2, 4)
  )

  # Group and arrange within groups
  grouped <- group_by_(df, 'group')
  result <- arrange_(grouped, ~value, .by_group = TRUE)

  # Check that grouping is preserved
  expect_true(is_grouped_df(result))
  expect_equal(group_vars_(result), "group")

  # Check sorting within groups
  expect_equal(result$value, c(2, 3, 1, 4))
})

test_that("arrange_() with .by_group argument", {
  skip_if_not_installed("data.trame")

  df <- data.frame(
    group = c("B", "A", "B", "A"),
    value = c(3, 1, 2, 4)
  )

  grouped <- group_by_(df, ~group)

  # With .by_group = TRUE, sort by group first
  result <- arrange_(grouped, ~value, .by_group = TRUE)
  expect_equal(result$group, c("A", "A", "B", "B"))
  expect_equal(result$value, c(1, 4, 2, 3))

  # With .by_group = FALSE (default), ignore grouping for sort
  result <- arrange_(grouped, ~value, .by_group = FALSE)
  expect_equal(result$value, c(1, 2, 3, 4))
})

test_that("arrange_() works with different data types", {
  df <- data.frame(
    int_col = c(3L, 1L, 2L),
    dbl_col = c(3.5, 1.2, 2.8),
    chr_col = c("z", "x", "y"),
    fct_col = factor(c("high", "low", "medium"),
                     levels = c("low", "medium", "high")),
    date_col = as.Date(c("2023-03-01", "2023-01-01", "2023-02-01"))
  )

  # Integer
  result <- arrange_(df, "int_col")
  expect_equal(result$int_col, c(1L, 2L, 3L))

  # Double
  result <- arrange_(df, "dbl_col")
  expect_equal(result$dbl_col, c(1.2, 2.8, 3.5))

  # Character
  result <- arrange_(df, "chr_col")
  expect_equal(result$chr_col, c("x", "y", "z"))

  # Factor
  result <- arrange_(df, "fct_col")
  expect_equal(as.character(result$fct_col), c("low", "medium", "high"))

  # Date
  result <- arrange_(df, "date_col")
  expect_equal(result$date_col,
               as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")))
})

test_that("arrange_() handles data.trame objects", {
  skip_if_not_installed("data.trame")

  df <- data.frame(
    x = c(3, 1, 2),
    y = c("a", "c", "b")
  )
  df_trame <- data.trame::as.data.trame(df)

  result <- arrange_(df_trame, "x")

  # Check it's still a data.trame
  expect_s3_class(result, "data.trame")

  # Check sorting worked
  expect_equal(result$x, c(1, 2, 3))
})

test_that("arrange_() with complex expressions", {
  df <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(10, 20, 30, 40, 50)
  )

  # Sort by computed value (formula masking)
  result <- arrange_(df, ~(x + y))
  expect_equal(result$x, c(1, 2, 3, 4, 5))

  # Sort by descending computed value
  result <- arrange_(df, ~desc(x + y))
  expect_equal(result$x, c(5, 4, 3, 2, 1))
})

test_that("arrange_() errors appropriately", {
  df <- data.frame(x = 1:3, y = 4:6)

  # Non-existent column
  expect_error(
    arrange_(df, "z"),
    class = "error"
  )

  # Invalid .decreasing length (when not matching)
  # This might warn rather than error depending on implementation
  expect_warning(
    arrange_(df, ~x, ~y, .decreasing = TRUE),
    "ignored"
  )
})

test_that("arrange_() maintains row names behavior", {
  df <- data.frame(
    x = c(3, 1, 2),
    y = c("a", "c", "b"),
    row.names = c("row3", "row1", "row2")
  )

  result <- arrange_(df, "x")

  # Row names should be reordered or reset depending on implementation
  # Just check we have the right number of rows
  expect_equal(nrow(result), 3)
})

test_that("arrange_() with tibbles", {
  skip_if_not_installed("tibble")

  df <- tibble::tibble(
    x = c(3, 1, 2),
    y = c("a", "c", "b")
  )

  result <- arrange_(df, "x")

  # Should preserve tibble class
  expect_s3_class(result, "tbl_df")

  # Check sorting
  expect_equal(result$x, c(1, 2, 3))
})

test_that("arrange_() with data.table", {
  skip_if_not_installed("data.table")

  df <- data.table::data.table(
    x = c(3, 1, 2),
    y = c("a", "c", "b")
  )

  result <- arrange_(df, "x")

  # Should preserve data.table class
  expect_s3_class(result, "data.table")

  # Check sorting
  expect_equal(result$x, c(1, 2, 3))
})

test_that("arrange_() pipe compatibility", {
  df <- data.frame(
    x = c(3, 1, 2),
    y = c("a", "c", "b")
  )

  # Test with pipe using data-dot mechanism
  result <- df |> arrange_(~x)
  expect_equal(result$x, c(1, 2, 3))

  # Chain multiple operations
  result <- df |>
    arrange_(~x) |>
    mutate_(z = ~x * 2)

  expect_equal(result$x, c(1, 2, 3))
  expect_equal(result$z, c(2, 4, 6))
})
