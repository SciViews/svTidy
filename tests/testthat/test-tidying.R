# Tests for tidying functions - Part 1

test_that("separate_() works with basic delimiter", {
  df <- data.frame(x = c("a_b_c", "d_e_f", "g_h_i"), stringsAsFactors = FALSE)

  result <- separate_(df, ~x, into = c("A", "B", "C"), sep = "_")

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("A", "B", "C"))
  expect_equal(result$A, c("a", "d", "g"))
  expect_equal(result$B, c("b", "e", "h"))
  expect_equal(result$C, c("c", "f", "i"))
})

test_that("separate_() works with character column name", {
  df <- data.frame(x = c("a_b", "c_d", "e_f"), stringsAsFactors = FALSE)

  result <- separate_(df, "x", into = c("A", "B"), sep = "_")

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
  expect_equal(result$A, c("a", "c", "e"))
})

test_that("separate_() with remove = FALSE keeps original", {
  df <- data.frame(x = c("a_b_c", "d_e_f"), stringsAsFactors = FALSE)

  result <- separate_(df, ~x, into = c("A", "B", "C"), sep = "_", remove = FALSE)

  expect_equal(ncol(result), 4)
  expect_true("x" %in% names(result))
  expect_equal(result$x, c("a_b_c", "d_e_f"))
})

test_that("separate_() drops columns with NA in into", {
  df <- data.frame(x = c("a_b_c", "d_e_f"), stringsAsFactors = FALSE)

  result <- separate_(df, ~x, into = c("A", NA, "C"), sep = "_")

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("A", "C"))
  expect_equal(result$A, c("a", "d"))
  expect_equal(result$C, c("c", "f"))
})

test_that("separate_() with regex pattern", {
  df <- data.frame(x = c("a-b_c", "d.e f"), stringsAsFactors = FALSE)

  result <- separate_(df, ~x, into = c("A", "B", "C"), sep = "[^[:alnum:]]+")

  expect_equal(ncol(result), 3)
  expect_equal(result$A, c("a", "d"))
})

test_that("separate_() with convert = TRUE", {
  df <- data.frame(x = c("1_a", "2_b", "3_c"), stringsAsFactors = FALSE)

  result <- separate_(df, ~x, into = c("num", "chr"), sep = "_", convert = TRUE)

  expect_type(result$num, "integer")
  expect_type(result$chr, "character")
})

test_that("separate_() handles extra pieces with extra = 'drop'", {
  df <- data.frame(x = c("a_b_c", "d_e_f_g"), stringsAsFactors = FALSE)

  result <- separate_(df, ~x, into = c("A", "B"), sep = "_", extra = "drop")

  expect_equal(ncol(result), 2)
  expect_equal(result$A, c("a", "d"))
})

test_that("separate_() handles extra pieces with extra = 'merge'", {
  df <- data.frame(x = c("a_b_c", "d_e_f_g"), stringsAsFactors = FALSE)

  result <- separate_(df, ~x, into = c("A", "B"), sep = "_", extra = "merge")

  expect_equal(ncol(result), 2)
  expect_equal(result$B, c("b_c", "e_f_g"))
})

test_that("separate_() handles missing pieces with fill = 'right'", {
  df <- data.frame(x = c("a_b_c", "d_e"), stringsAsFactors = FALSE)

  result <- separate_(df, ~x, into = c("A", "B", "C"), sep = "_", fill = "right")

  expect_equal(ncol(result), 3)
  expect_true(is.na(result$C[2]))
})

test_that("separate_() handles missing pieces with fill = 'left'", {
  df <- data.frame(x = c("a_b_c", "d_e"), stringsAsFactors = FALSE)

  result <- separate_(df, ~x, into = c("A", "B", "C"), sep = "_", fill = "left")

  expect_equal(ncol(result), 3)
  expect_true(is.na(result$A[2]))
})

test_that("separate_() preserves data frame type", {
  skip_if_not_installed("tibble")

  df_df <- data.frame(x = c("a_b", "c_d"), stringsAsFactors = FALSE)
  tbl <- tibble::tibble(x = c("a_b", "c_d"))

  result_df <- separate_(df_df, ~x, into = c("A", "B"), sep = "_")
  result_tbl <- separate_(tbl, ~x, into = c("A", "B"), sep = "_")

  expect_s3_class(result_df, "data.frame")
  expect_s3_class(result_tbl, "tbl_df")
})

test_that("unite_() combines columns with delimiter", {
  df <- data.frame(A = c("a", "d", "g"), B = c("b", "e", "h"), C = c("c", "f", "i"), stringsAsFactors = FALSE)

  result <- unite_(df, ~z, ~A, ~B, ~C, sep = "_")

  expect_equal(ncol(result), 1)
  expect_equal(names(result), "z")
  expect_equal(result$z, c("a_b_c", "d_e_f", "g_h_i"))
})

test_that("unite_() with remove = FALSE keeps original columns", {
  df <- data.frame(A = c("a", "d"), B = c("b", "e"), C = c("c", "f"), stringsAsFactors = FALSE)

  result <- unite_(df, ~z, ~A, ~B, ~C, sep = "_", remove = FALSE)

  expect_equal(ncol(result), 4)
  expect_true(all(c("A", "B", "C", "z") %in% names(result)))
})

test_that("unite_() handles NA values by default", {
  df <- data.frame(A = c("a", NA), B = c("b", "e"), stringsAsFactors = FALSE)

  result <- unite_(df, ~z, ~A, ~B, sep = "_")

  expect_equal(result$z, c("a_b", "NA_e"))
})

test_that("unite_() with na.rm = TRUE removes NAs", {
  df <- data.frame(A = c("a", NA), B = c("b", "e"), stringsAsFactors = FALSE)

  result <- unite_(df, ~z, ~A, ~B, sep = "_", na.rm = TRUE)

  expect_equal(result$z, c("a_b", "e"))
})

test_that("unite_() preserves data frame type", {
  skip_if_not_installed("tibble")

  df_df <- data.frame(A = c("a", "d"), B = c("b", "e"), stringsAsFactors = FALSE)
  tbl <- tibble::tibble(A = c("a", "d"), B = c("b", "e"))

  result_df <- unite_(df_df, ~z, ~A, ~B, sep = "_")
  result_tbl <- unite_(tbl, ~z, ~A, ~B, sep = "_")

  expect_s3_class(result_df, "data.frame")
  expect_s3_class(result_tbl, "tbl_df")
})

test_that("fill_() fills missing values downward by default", {
  df <- data.frame(
    group = c(1, 1, 1, 2, 2, 2),
    value = c(10, NA, NA, 20, NA, 30),
    stringsAsFactors = FALSE
  )

  result <- fill_(df, ~value)

  expect_equal(result$value, c(10, 10, 10, 20, 20, 30))
})

test_that("fill_() fills missing values upward", {
  df <- data.frame(
    value = c(10, NA, NA, 20),
    stringsAsFactors = FALSE
  )

  result <- fill_(df, ~value, .direction = "up")

  expect_equal(result$value, c(10, 20, 20, 20))
})

test_that("fill_() with .direction = 'downup'", {
  df <- data.frame(
    value = c(NA, 10, NA, NA, 20, NA),
    stringsAsFactors = FALSE
  )

  result <- fill_(df, ~value, .direction = "downup")

  expect_false(any(is.na(result$value)))
})

test_that("fill_() fills specific columns only", {
  df <- data.frame(
    x = c(1, NA, 3),
    y = c(NA, 2, NA),
    z = c(1, 2, 3),
    stringsAsFactors = FALSE
  )

  result <- fill_(df, ~x, ~y, .direction = "down")

  expect_equal(result$x, c(1, 1, 3))
  expect_equal(result$y, c(NA, 2, 2))
  expect_equal(result$z, c(1, 2, 3))
})

test_that("fill_() with grouped data", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    group = c(1, 1, 1, 2, 2, 2),
    value = c(10, NA, NA, 20, NA, 30),
    stringsAsFactors = FALSE
  )

  df_grouped <- dplyr::group_by(df, group)
  result <- fill_(df_grouped, ~value)

  expect_equal(result$value, c(10, 10, 10, 20, 20, 30))
})

test_that("fill_() preserves data frame type", {
  skip_if_not_installed("tibble")

  df_df <- data.frame(value = c(1, NA, 3))
  tbl <- tibble::tibble(value = c(1, NA, 3))

  result_df <- fill_(df_df, ~value)
  result_tbl <- fill_(tbl, ~value)

  expect_s3_class(result_df, "data.frame")
  expect_s3_class(result_tbl, "tbl_df")
})

test_that("drop_na_() removes rows with any NA", {
  df <- data.frame(
    x = c(1, 2, NA),
    y = c("a", NA, "c"),
    z = 1:3,
    stringsAsFactors = FALSE
  )

  result <- drop_na_(df)

  expect_equal(nrow(result), 1)
  expect_equal(result$x, 1)
})

test_that("drop_na_() checks specific columns only", {
  df <- data.frame(
    x = c(1, 2, NA),
    y = c("a", NA, "c"),
    z = 1:3,
    stringsAsFactors = FALSE
  )

  result <- drop_na_(df, ~x)

  expect_equal(nrow(result), 2)
})

test_that("drop_na_() with multiple columns", {
  df <- data.frame(
    x = c(1, 2, NA),
    y = c("a", NA, "c"),
    z = 1:3,
    stringsAsFactors = FALSE
  )

  result <- drop_na_(df, ~x, ~y)

  expect_equal(nrow(result), 1)
  expect_equal(result$x, 1)
  expect_equal(result$y, "a")
})

test_that("drop_na_() with .prop parameter", {
  df <- data.frame(
    x = c(1, NA, NA),
    y = c(NA, 2, NA),
    z = c(NA, NA, 3),
    stringsAsFactors = FALSE
  )

  result <- drop_na_(df, .prop = 0.5)

  expect_equal(nrow(result), 0)
})

test_that("drop_na_() with .na.attr parameter", {
  df <- data.frame(
    x = c(1, 2, NA),
    y = c("a", NA, "c"),
    stringsAsFactors = FALSE
  )

  result <- drop_na_(df, .na.attr = TRUE)

  expect_equal(nrow(result), 1)
  expect_true(!is.null(attr(result, "na.action")))
})

test_that("drop_na_() preserves data frame type", {
  skip_if_not_installed("tibble")

  df_df <- data.frame(x = c(1, NA), y = c("a", "b"), stringsAsFactors = FALSE)
  tbl <- tibble::tibble(x = c(1, NA), y = c("a", "b"))

  result_df <- drop_na_(df_df)
  result_tbl <- drop_na_(tbl)

  expect_s3_class(result_df, "data.frame")
  expect_s3_class(result_tbl, "tbl_df")
})

# Tests for tidying functions - Part 2

test_that("replace_na_() replaces NAs in data frame", {
  df <- data.frame(
    x = c(1, 2, NA),
    y = c("a", NA, "c"),
    stringsAsFactors = FALSE
  )

  result <- replace_na_(df, list(x = 0, y = "missing"))

  expect_equal(result$x, c(1, 2, 0))
  expect_equal(result$y, c("a", "missing", "c"))
})

test_that("replace_na_() with single value in vector", {
  vec <- c(1, 2, NA, 4, NA)

  result <- replace_na_(v = vec, replace = 0)

  expect_equal(result, c(1, 2, 0, 4, 0))
})

test_that("replace_na_() with character vector", {
  vec <- c("a", NA, "c", NA)

  result <- replace_na_(v = vec, replace = "missing")

  expect_equal(result, c("a", "missing", "c", "missing"))
})

test_that("replace_na_() with single column", {
  df <- data.frame(x = c(1, NA, 3), stringsAsFactors = FALSE)

  result <- replace_na_(df, list(x = 99))

  expect_equal(result$x, c(1, 99, 3))
})

test_that("replace_na_() preserves data frame type", {
  skip_if_not_installed("tibble")

  df_df <- data.frame(x = c(1, NA), y = c("a", NA), stringsAsFactors = FALSE)
  tbl <- tibble::tibble(x = c(1, NA), y = c("a", NA))

  result_df <- replace_na_(df_df, list(x = 0, y = "b"))
  result_tbl <- replace_na_(tbl, list(x = 0, y = "b"))

  expect_s3_class(result_df, "data.frame")
  expect_s3_class(result_tbl, "tbl_df")
})

test_that("replace_na_() works with numeric and logical", {
  df <- data.frame(
    x = c(1, NA, 3),
    y = c(TRUE, NA, FALSE),
    stringsAsFactors = FALSE
  )

  result <- replace_na_(df, list(x = 2, y = FALSE))

  expect_equal(result$x, c(1, 2, 3))
  expect_equal(result$y, c(TRUE, FALSE, FALSE))
})

test_that("replace_na_() with all columns having NAs", {
  df <- data.frame(
    x = c(NA, NA, NA),
    y = as.character(c(NA, NA, NA)),
    stringsAsFactors = FALSE
  )

  expect_warning(result <- replace_na_(df, list(x = 1, y = "a")),
    "coerced"
  )

  expect_equal(result$x, c(TRUE, TRUE, TRUE))
  expect_equal(result$y, c("a", "a", "a"))
})

test_that("replace_na_() with no NAs", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)

  result <- replace_na_(df, list(x = 0, y = "missing"))

  expect_equal(result$x, 1:3)
  expect_equal(result$y, c("a", "b", "c"))
})

test_that("uncount_() duplicates rows by weights", {
  df <- data.frame(x = c("a", "b", "c"), n = c(1, 2, 3), stringsAsFactors = FALSE)

  result <- uncount_(df, ~n)

  expect_equal(nrow(result), 6)
  expect_equal(result$x, c("a", "b", "b", "c", "c", "c"))
})

test_that("uncount_() with numeric weights vector", {
  df <- data.frame(x = c("a", "b", "c"), stringsAsFactors = FALSE)

  result <- uncount_(df, weights = c(2, 1, 3))

  expect_equal(nrow(result), 6)
  expect_equal(result$x, c("a", "a", "b", "c", "c", "c"))
})

test_that("uncount_() with .remove = FALSE keeps weight column", {
  df <- data.frame(x = c("a", "b"), n = c(2, 3), stringsAsFactors = FALSE)

  result <- uncount_(df, ~n, .remove = FALSE)

  expect_true("n" %in% names(result))
  expect_equal(nrow(result), 5)
})

test_that("uncount_() with .remove = TRUE removes weight column", {
  df <- data.frame(x = c("a", "b"), n = c(2, 3), stringsAsFactors = FALSE)

  result <- uncount_(df, ~n, .remove = TRUE)

  expect_false("n" %in% names(result))
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 1)
})

test_that("uncount_() with .id parameter", {
  df <- data.frame(x = c("a", "b", "c"), n = c(1, 2, 1), stringsAsFactors = FALSE)

  result <- uncount_(df, ~n, .id = "id")

  expect_true("id" %in% names(result))
  expect_equal(result$id, c(1, 1, 2, 1))
})

test_that("uncount_() with zero weights", {
  df <- data.frame(x = c("a", "b", "c"), n = c(0, 2, 1), stringsAsFactors = FALSE)

  result <- uncount_(df, ~n)

  expect_equal(nrow(result), 3)
  expect_equal(result$x, c("b", "b", "c"))
})

test_that("uncount_() preserves data frame type", {
  skip_if_not_installed("tibble")

  df_df <- data.frame(x = c("a", "b"), n = c(2, 1), stringsAsFactors = FALSE)
  tbl <- tibble::tibble(x = c("a", "b"), n = c(2, 1))

  result_df <- uncount_(df_df, ~n)
  result_tbl <- uncount_(tbl, ~n)

  expect_s3_class(result_df, "data.frame")
  expect_s3_class(result_tbl, "tbl_df")
})

test_that("uncount_() with all weights = 1", {
  df <- data.frame(x = c("a", "b", "c"), n = c(1, 1, 1), stringsAsFactors = FALSE)

  result <- uncount_(df, ~n)

  expect_equal(nrow(result), 3)
  expect_equal(result$x, c("a", "b", "c"))
})

test_that("uncount_() with character column name", {
  df <- data.frame(x = c("a", "b"), count = c(2, 3), stringsAsFactors = FALSE)

  result <- uncount_(df, "count")

  expect_equal(nrow(result), 5)
})

test_that("uncount_() with multiple columns", {
  df <- data.frame(
    x = c("a", "b"),
    y = c(1, 2),
    n = c(2, 3),
    stringsAsFactors = FALSE
  )

  result <- uncount_(df, ~n)

  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 2)
  expect_equal(result$x, c("a", "a", "b", "b", "b"))
  expect_equal(result$y, c(1, 1, 2, 2, 2))
})

test_that("separate_() and unite_() roundtrip", {
  df1 <- data.frame(x = c("a_b_c", "d_e_f"), stringsAsFactors = FALSE)

  separated <- separate_(df1, ~x, into = c("A", "B", "C"), sep = "_")

  reunited <- unite_(separated, ~x, ~A, ~B, ~C, sep = "_")

  expect_equal(reunited$x, df1$x)
})

test_that("fill_() then drop_na_() removes unfilled NAs", {
  df <- data.frame(
    x = c(1, NA, 3),
    y = c(NA, 2, NA),
    stringsAsFactors = FALSE
  )

  filled <- fill_(df, ~x, .direction = "down")
  result <- drop_na_(filled)

  expect_equal(nrow(result), 1)
})

test_that("separate_() with data.table", {
  skip_if_not_installed("data.table")

  df <- data.table::data.table(x = c("a_b", "c_d"))

  result <- separate_(df, ~x, into = c("A", "B"), sep = "_")

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
})

test_that("unite_() with data.table", {
  skip_if_not_installed("data.table")

  df <- data.table::data.table(A = c("a", "b"), B = c("c", "d"))

  result <- unite_(df, ~z, ~A, ~B, sep = "_")

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2L)
  expect_equal(ncol(result), 1L)
  expect_equal(names(result), "z")
  expect_equal(result$z, c("a_c", "b_d"))
})

test_that("fill_() with data.table", {
  skip_if_not_installed("data.table")

  df <- data.table::data.table(x = c(1, NA, 3), y = c(4, 5, 6))

  result <- fill_(df, ~x)

  expect_s3_class(result, "data.table")
  expect_equal(result$x, c(1, 1, 3))
})

test_that("drop_na_() with data.table", {
  skip_if_not_installed("data.table")

  df <- data.table::data.table(x = c(1, NA, 3), y = c("a", "b", "c"))

  result <- drop_na_(df)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
})

test_that("replace_na_() with data.table", {
  skip_if_not_installed("data.table")

  df <- data.table::data.table(x = c(1, NA, 3), y = c("a", NA, "c"))

  result <- replace_na_(df, list(x = 0, y = "b"))

  expect_s3_class(result, "data.table")
  expect_equal(result$x, c(1, 0, 3))
})

test_that("uncount_() with data.table", {
  skip_if_not_installed("data.table")

  df <- data.table::data.table(x = c("a", "b"), n = c(2, 1))

  result <- uncount_(df, ~n)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)
})

test_that("separate_() with empty string separator", {
  df <- data.frame(x = c("abc", "def"), stringsAsFactors = FALSE)

  # Using fixed = TRUE with empty string
  result <- separate_(df, ~x, into = c("A", "B", "C"), sep = "", fixed = TRUE)

  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
})

test_that("unite_() with zero-length input", {
  df <- data.frame(A = character(0), B = character(0), stringsAsFactors = FALSE)

  result <- unite_(df, ~z, ~A, ~B, sep = "_")

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 1)
})

test_that("fill_() with zero-row data frame", {
  df <- data.frame(x = integer(), y = character(), stringsAsFactors = FALSE)

  result <- fill_(df, ~x)

  expect_equal(nrow(result), 0)
})

test_that("drop_na_() with zero-row data frame", {
  df <- data.frame(x = integer(), y = character(), stringsAsFactors = FALSE)

  result <- drop_na_(df)

  expect_equal(nrow(result), 0)
})

test_that("replace_na_() with zero-row data frame", {
  df <- data.frame(x = integer(), y = character(), stringsAsFactors = FALSE)

  result <- replace_na_(df, list(x = 0, y = "a"))

  expect_equal(nrow(result), 0)
})

test_that("uncount_() with zero-row data frame", {
  df <- data.frame(x = character(0), n = integer(0), stringsAsFactors = FALSE)

  result <- uncount_(df, ~n)

  expect_equal(nrow(result), 0)
})
