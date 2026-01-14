test_that("bind_rows_() binds data frames by rows", {
  df1 <- data.frame(x = 1:3, y = letters[1:3])
  df2 <- data.frame(x = 4:6, y = letters[4:6])

  result <- bind_rows_(df1, df2)

  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 2)
  expect_equal(result$x, 1:6)
  expect_equal(result$y, letters[1:6])
})

test_that("bind_rows_() preserves data frame type", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("data.table")
  skip_if_not_installed("data.trame")

  # Test with data.frame
  df1 <- data.frame(x = 1:2)
  df2 <- data.frame(x = 3:4)
  result <- bind_rows_(df1, df2)
  expect_s3_class(result, "data.frame")
  expect_false(inherits(result, "tbl_df"))
  expect_false(inherits(result, "data.table"))

  # Test with tibble
  tbl1 <- tibble::tibble(x = 1:2)
  tbl2 <- tibble::tibble(x = 3:4)
  result <- bind_rows_(tbl1, tbl2)
  expect_s3_class(result, "tbl_df")

  # Test with data.table
  dt1 <- data.table::data.table(x = 1:2)
  dt2 <- data.table::data.table(x = 3:4)
  result <- bind_rows_(dt1, dt2)
  expect_s3_class(result, "data.table")

  # Test with data.trame
  dtr1 <- data.trame::as.data.trame(data.frame(x = 1:2))
  dtr2 <- data.trame::as.data.trame(data.frame(x = 3:4))
  result <- bind_rows_(dtr1, dtr2)
  expect_s3_class(result, "data.trame")
})

test_that("bind_rows_() handles .id parameter", {
  df1 <- data.frame(x = 1:2, y = c("a", "b"))
  df2 <- data.frame(x = 3:4, y = c("c", "d"))

  # Named data frames
  result <- bind_rows_(first = df1, second = df2, .id = "source")

  expect_true("source" %in% names(result))
  expect_equal(result$source, c("first", "first", "second", "second"))
  expect_type(result$source, "character")

  # Unnamed data frames (numeric ids)
  result <- bind_rows_(df1, df2, .id = "id")
  expect_equal(result$id, c("1", "1", "2", "2"))
  expect_type(result$id, "character")
})

test_that("bind_rows_() handles .fill parameter", {
  df1 <- data.frame(x = 1:2, y = letters[1:2])
  df2 <- data.frame(x = 3:4, z = letters[3:4])

  # With .fill = TRUE (default)
  result <- bind_rows_(df1, df2, .fill = TRUE)
  expect_equal(ncol(result), 3)
  expect_true(all(c("x", "y", "z") %in% names(result)))
  expect_true(is.na(result$z[1]))
  expect_true(is.na(result$y[3]))

  # With .fill = FALSE
  expect_error(
    bind_rows_(df1, df2, .fill = FALSE)
  )
})

test_that("bind_rows_() handles .use_names parameter", {
  df1 <- data.frame(a = 1:2, b = 3:4)
  df2 <- data.frame(b = 5:6, a = 7:8)

  # With .use_names = TRUE (default) - match by name
  result <- bind_rows_(df1, df2, .use_names = TRUE)
  expect_equal(result$a, c(1, 2, 7, 8))
  expect_equal(result$b, c(3, 4, 5, 6))

  # With .use_names = FALSE - match by position
  result <- bind_rows_(df1, df2, .use_names = FALSE)
  expect_equal(result$a, c(1, 2, 5, 6))
  expect_equal(result$b, c(3, 4, 7, 8))
})

test_that("bind_rows_() handles list input", {
  df1 <- data.frame(x = 1:2)
  df2 <- data.frame(x = 3:4)
  df3 <- data.frame(x = 5:6)

  # List of data frames
  result <- bind_rows_(list(df1, df2, df3))
  expect_equal(nrow(result), 6)
  expect_equal(result$x, 1:6)

  # Named list
  result <- bind_rows_(list(a = df1, b = df2), .id = "source")
  expect_equal(result$source, c("a", "a", "b", "b"))
})

test_that("bind_rows_() handles empty input", {
  # No arguments
  result <- bind_rows_()
  expect_s3_class(result, "data.trame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 0)

  # Empty list
  result <- bind_rows_(list())
  expect_s3_class(result, "data.trame")
  expect_equal(nrow(result), 0)
})

test_that("bind_rows_() handles single data frame with warning", {
  df <- data.frame(x = 1:3)

  expect_warning(
    bind_rows_(df),
    "Binding only one data frame is a nonsense"
  )
})

test_that("bind_rows_() handles grouped data frames", {
  skip_if_not_installed("dplyr")

  df1 <- data.frame(g = c("a", "a", "b"), x = 1:3)
  df2 <- data.frame(g = c("b", "c", "c"), x = 4:6)

  # Group first data frame
  grouped <- dplyr::group_by(df1, g)

  # bind_rows_() should ungroup
  result <- bind_rows_(grouped, df2)
  expect_false(dplyr::is.grouped_df(result))
  expect_equal(nrow(result), 6)
})

test_that("bind_rows_() validates arguments", {
  df1 <- data.frame(x = 1:2)
  df2 <- data.frame(x = 3:4)

  # Invalid .id
  expect_error(
    bind_rows_(df1, df2, .id = c("a", "b")),
    ".id"
  )

  expect_error(
    bind_rows_(df1, df2, .id = 123),
    ".id"
  )

  # Invalid .fill
  expect_error(
    bind_rows_(df1, df2, .fill = "yes"),
    ".fill"
  )

  # Invalid .use_names
  expect_error(
    bind_rows_(df1, df2, .use_names = "yes"),
    ".use_names"
  )
})

test_that("bind_rows_() handles different column types", {
  df1 <- data.frame(
    int = 1:2,
    dbl = c(1.5, 2.5),
    chr = c("a", "b"),
    lgl = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  df2 <- data.frame(
    int = 3:4,
    dbl = c(3.5, 4.5),
    chr = c("c", "d"),
    lgl = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  result <- bind_rows_(df1, df2)

  expect_type(result$int, "integer")
  expect_type(result$dbl, "double")
  expect_type(result$chr, "character")
  expect_type(result$lgl, "logical")
})

test_that("bind_rows_() handles factors correctly", {
  df1 <- data.frame(x = factor(c("a", "b")), y = 1:2)
  df2 <- data.frame(x = factor(c("c", "d")), y = 3:4)

  result <- bind_rows_(df1, df2)

  expect_s3_class(result$x, "factor")
  expect_equal(as.character(result$x), c("a", "b", "c", "d"))
})

test_that("bind_rows_() handles dates and times", {
  df1 <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-02")),
    time = as.POSIXct(c("2020-01-01 10:00:00", "2020-01-01 11:00:00"))
  )
  df2 <- data.frame(
    date = as.Date(c("2020-01-03", "2020-01-04")),
    time = as.POSIXct(c("2020-01-01 12:00:00", "2020-01-01 13:00:00"))
  )

  result <- bind_rows_(df1, df2)

  expect_s3_class(result$date, "Date")
  expect_s3_class(result$time, "POSIXct")
})

test_that("bind_rows_() handles many data frames", {
  dfs <- lapply(1:10, function(i) {
    data.frame(x = i, y = letters[i])
  })

  result <- do.call(bind_rows_, dfs)

  expect_equal(nrow(result), 10)
  expect_equal(result$x, 1:10)
})

# Tests for binding functions

test_that("bind_cols_() works with basic data frames", {
  df1 <- data.frame(x = 1:3, y = letters[1:3])
  df2 <- data.frame(z = 4:6, w = LETTERS[1:3])

  result <- bind_cols_(df1, df2)

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)
  expect_equal(names(result), c("x", "y", "z", "w"))
  expect_equal(result$x, 1:3)
  expect_equal(result$z, 4:6)
})

test_that("bind_cols_() preserves data frame types", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("data.table")
  skip_if_not_installed("data.trame")

  df1 <- data.frame(x = 1:3)
  df2 <- data.frame(y = 4:6)

  # Test with data.frame
  result_df <- bind_cols_(df1, df2)
  expect_s3_class(result_df, "data.frame")

  # Test with tibble
  tbl1 <- tibble::tibble(x = 1:3)
  tbl2 <- tibble::tibble(y = 4:6)
  result_tbl <- bind_cols_(tbl1, tbl2)
  expect_s3_class(result_tbl, "tbl_df")

  # Test with data.table
  dt1 <- data.table::data.table(x = 1:3)
  dt2 <- data.table::data.table(y = 4:6)
  result_dt <- bind_cols_(dt1, dt2)
  expect_s3_class(result_dt, "data.table")

  # Test with data.trame
  dtrame1 <- data.trame::as.data.trame(data.frame(x = 1:3))
  dtrame2 <- data.trame::as.data.trame(data.frame(y = 4:6))
  result_dtrame <- bind_cols_(dtrame1, dtrame2)
  expect_s3_class(result_dtrame, "data.trame")
})

test_that("bind_cols_() handles .name_repair parameter", {
  df1 <- data.frame(x = 1:3, y = letters[1:3])
  df2 <- data.frame(x = 4:6, z = LETTERS[1:3])

  # Test with unique names
  expect_message(result_unique <- bind_cols_(df1, df2, .name_repair = "unique"),
    "New names:"
  )
  expect_true(all(!duplicated(names(result_unique))))
  expect_equal(ncol(result_unique), 4)

  # Test with minimal names (allows duplicates)
  result_minimal <- bind_cols_(df1, df2, .name_repair = "minimal")
  expect_equal(sum(names(result_minimal) == "x"), 2)

  # Test with universal names
  expect_message(result_universal <- bind_cols_(df1, df2,
    .name_repair = "universal"),
    "New names:"
  )
  expect_true(all(!duplicated(names(result_universal))))
  expect_true(all(make.names(names(result_universal)) == names(result_universal)))
})

test_that("bind_cols_() errors when row counts do not match", {
  df1 <- data.frame(x = 1:3)
  df2 <- data.frame(y = 1:4)

  expect_error(
    bind_cols_(df1, df2),
    "Can't recycle"
  )
})

test_that("bind_cols_() works with empty input", {
  result <- bind_cols_()

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 0)
})

test_that("bind_cols_() works with single data frame", {
  df <- data.frame(x = 1:3, y = letters[1:3])

  expect_warning(
    result <- bind_cols_(df),
    "Binding only one data frame is a nonsense"
  )

  expect_equal(result, df)
})

test_that("bind_cols_() works with multiple data frames", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 4:6)
  df3 <- data.frame(c = 7:9)
  df4 <- data.frame(d = 10:12)

  result <- bind_cols_(df1, df2, df3, df4)

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)
  expect_equal(names(result), c("a", "b", "c", "d"))
  expect_equal(result$a, 1:3)
  expect_equal(result$d, 10:12)
})

test_that("bind_cols_() ungroups grouped data frames", {
  skip_if_not_installed("dplyr")

  df1 <- dplyr::group_by(data.frame(x = 1:3, g = c(1, 1, 2)), g)
  df2 <- data.frame(y = 4:6)

  result <- bind_cols_(df1, df2)

  expect_false(dplyr::is_grouped_df(result))
  expect_equal(ncol(result), 3)
})

test_that("bind_cols_() preserves row order", {
  df1 <- data.frame(x = c(3, 1, 2), row.names = c("c", "a", "b"))
  df2 <- data.frame(y = c(6, 4, 5))

  result <- bind_cols_(df1, df2)

  expect_equal(result$x, c(3, 1, 2))
  expect_equal(result$y, c(6, 4, 5))
})

test_that("bind_cols_() works with zero-row data frames", {
  df1 <- data.frame(x = integer(), y = character())
  df2 <- data.frame(z = integer(), w = logical())

  result <- bind_cols_(df1, df2)

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 4)
  expect_equal(names(result), c("x", "y", "z", "w"))
})

test_that("bind_cols_() preserves column types", {
  df1 <- data.frame(
    int = 1L:3L,
    dbl = c(1.1, 2.2, 3.3),
    chr = c("a", "b", "c"),
    lgl = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  df2 <- data.frame(
    fct = factor(c("x", "y", "z")),
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
  )

  result <- bind_cols_(df1, df2)

  expect_type(result$int, "integer")
  expect_type(result$dbl, "double")
  expect_type(result$chr, "character")
  expect_type(result$lgl, "logical")
  expect_s3_class(result$fct, "factor")
  expect_s3_class(result$date, "Date")
})

# Note: should we do that? Does not work for now.
#test_that("bind_cols_() works with list of data frames", {
#  df_list <- list(
#    data.frame(a = 1:3),
#    data.frame(b = 4:6),
#    data.frame(c = 7:9)
#  )
#
#  result <- bind_cols_(df_list)
#
#  expect_equal(nrow(result), 3)
#  expect_equal(ncol(result), 3)
#  expect_equal(names(result), c("a", "b", "c"))
#})

test_that("bind_cols_() handles data frames with different column names", {
  df1 <- data.frame(alpha = 1:3, beta = 4:6)
  df2 <- data.frame(gamma = 7:9, delta = 10:12)
  df3 <- data.frame(epsilon = 13:15)

  result <- bind_cols_(df1, df2, df3)

  expect_equal(ncol(result), 5)
  expect_equal(names(result), c("alpha", "beta", "gamma", "delta", "epsilon"))
})

test_that("bind_cols_() works with single column data frames", {
  df1 <- data.frame(x = 1:3)
  df2 <- data.frame(y = 4:6)
  df3 <- data.frame(z = 7:9)

  result <- bind_cols_(df1, df2, df3)

  expect_equal(ncol(result), 3)
  expect_equal(result$x, 1:3)
  expect_equal(result$y, 4:6)
  expect_equal(result$z, 7:9)
})

test_that("bind_cols_() works with data frames containing NA values", {
  df1 <- data.frame(x = c(1, NA, 3), y = c(NA, "b", "c"), stringsAsFactors = FALSE)
  df2 <- data.frame(z = c(4, 5, NA), w = c(TRUE, NA, FALSE))

  result <- bind_cols_(df1, df2)

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)
  expect_true(is.na(result$x[2]))
  expect_true(is.na(result$y[1]))
  expect_true(is.na(result$z[3]))
  expect_true(is.na(result$w[2]))
})

test_that("bind_cols_() works with many data frames", {
  df_list <- lapply(1:15, function(i) {
    df <- data.frame(x = 1:3)
    names(df) <- paste0("col", i)
    df
  })

  result <- do.call(bind_cols_, df_list)

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 15)
  expect_equal(names(result), paste0("col", 1:15))
})

test_that("bind_cols_() handles empty data frames mixed with non-empty", {
  df1 <- data.frame(x = 1:3, y = 4:6)
  df2 <- data.frame(z = integer(), w = character())

  expect_error(
    bind_cols_(df1, df2),
    "Can't recycle"
  )

  # But zero-row data frames can be bound together
  df3 <- data.frame(a = integer())
  df4 <- data.frame(b = character())
  result <- bind_cols_(df3, df4)
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 2)
})
