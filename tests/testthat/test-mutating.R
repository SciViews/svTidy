# Tests for mutating functions

test_that("mutate_() creates new columns", {
  df <- data.frame(x = 1:3, y = 4:6)
  result <- mutate_(df, z = ~x + y)

  expect_equal(ncol(result), 3)
  expect_true("z" %in% names(result))
  expect_equal(result$z, c(5, 7, 9))
})

test_that("mutate_() creates multiple new columns", {
  df <- data.frame(x = 1:3, y = 4:6)
  result <- mutate_(df, z = ~x + y, w = ~x * y)

  expect_equal(ncol(result), 4)
  expect_true(all(c("z", "w") %in% names(result)))
  expect_equal(result$z, c(5, 7, 9))
  expect_equal(result$w, c(4, 10, 18))
})

test_that("mutate_() modifies existing columns", {
  df <- data.frame(x = 1:3, y = 4:6)
  result <- mutate_(df, x = ~x * 2)

  expect_equal(result$x, c(2, 4, 6))
  expect_equal(result$y, 4:6)
})

test_that("mutate_() references newly created columns", {
  df <- data.frame(x = 1:3, y = 4:6)
  result <- mutate_(
    df,
    z = ~x + y,
    w = ~z * 2
  )

  expect_equal(result$w, c(10, 14, 18))
})

test_that("mutate_() preserves data frame types", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("data.table")

  df <- data.frame(x = 1:3)
  result_df <- mutate_(df, y = ~x * 2)
  expect_s3_class(result_df, "data.frame")

  tbl <- tibble::tibble(x = 1:3)
  result_tbl <- mutate_(tbl, y = ~x * 2)
  expect_s3_class(result_tbl, "tbl_df")

  dt <- data.table::data.table(x = 1:3)
  result_dt <- mutate_(dt, y = ~x * 2)
  expect_s3_class(result_dt, "data.table")
})

test_that("mutate_() with character column names", {
  df <- data.frame(x = 1:3, y = 4:6)
  result <- mutate_(df, "z" = ~x + y)

  expect_true("z" %in% names(result))
  expect_equal(result$z, c(5, 7, 9))
})

test_that("mutate_() with .keep = 'all'", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  result <- mutate_(df, w = ~x + y, .keep = "all")

  expect_equal(ncol(result), 4)
  expect_equal(names(result), c("x", "y", "z", "w"))
})

test_that("mutate_() with .keep = 'used'", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  result <- mutate_(df, w = ~x + y, .keep = "used")

  expect_equal(ncol(result), 3)
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
  expect_true("w" %in% names(result))
  expect_false("z" %in% names(result))
})

test_that("mutate_() with .keep = 'unused'", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  result <- mutate_(df, w = ~x + y, .keep = "unused")

  expect_equal(ncol(result), 2)
  expect_true("z" %in% names(result))
  expect_true("w" %in% names(result))
  expect_false("x" %in% names(result))
})

test_that("mutate_() with .keep = 'none'", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  result <- mutate_(df, w = ~x + y, .keep = "none")

  expect_equal(ncol(result), 1)
  expect_equal(names(result), "w")
})

test_that("mutate_() with .by single grouping variable", {
  df <- data.frame(group = c("A", "A", "B", "B"), val = 1:4)
  result <- mutate_(df, centered = ~val - mean(val), .by = "group")

  expect_equal(ncol(result), 3)
  expect_false(dplyr::is_grouped_df(result))
  expect_equal(result$centered[1], -0.5)
  expect_equal(result$centered[2], 0.5)
  expect_equal(result$centered[3], -0.5)
  expect_equal(result$centered[4], 0.5)
})

test_that("mutate_() with .by multiple grouping variables", {
  df <- data.frame(
    g1 = c("A", "A", "B", "B"),
    g2 = c("X", "Y", "X", "Y"),
    val = 1:4
  )
  result <- mutate_(df, ranked = ~rank(val), .by = c("g1", "g2"))

  expect_equal(ncol(result), 4)
  expect_equal(result$ranked, c(1, 1, 1, 1))
})

# Not yet!
#test_that("mutate_() with formula syntax for .by", {
#  df <- data.frame(group = c("A", "A", "B", "B"), val = 1:4)
#  result <- mutate_(df, centered = ~val - mean(val), .by = ~group)
#
#  expect_equal(ncol(result), 3)
#  expect_equal(result$centered[1], -0.5)
#  expect_equal(result$centered[3], -0.5)
#})

test_that("mutate_() with mathematical functions", {
  df <- data.frame(x = c(1, 4, 9, 16))
  result <- mutate_(df, sqrt_x = ~sqrt(x), log_x = ~log(x))

  expect_equal(result$sqrt_x, c(1, 2, 3, 4))
  expect_true(all(is.numeric(result$log_x)))
})

test_that("mutate_() with conditional expressions", {
  df <- data.frame(x = c(1, 5, 10, 15))
  result <- mutate_(df, category = ~ifelse(x > 7, "high", "low"))

  expect_equal(result$category, c("low", "low", "high", "high"))
})

test_that("mutate_() with NA values", {
  df <- data.frame(x = c(1, NA, 3), y = c(4, 5, NA))
  result <- mutate_(df, z = ~x + y)

  expect_true(is.na(result$z[2]))
  expect_true(is.na(result$z[3]))
  expect_equal(result$z[1], 5)
})

test_that("mutate_() preserves row order", {
  df <- data.frame(
    x = c(3, 1, 2),
    y = c(6, 4, 5),
    row.names = c("c", "a", "b")
  )
  result <- mutate_(df, z = ~x + y)

  expect_equal(result$x, c(3, 1, 2))
  expect_equal(rownames(result), c("c", "a", "b"))
})

test_that("mutate_() with no modifications returns original", {
  df <- data.frame(x = 1:3, y = 4:6)
  result <- mutate_(df)

  expect_equal(result, df)
})

test_that("mutate_() with grouped data frame", {
  skip_if_not_installed("dplyr")

  df <- data.frame(group = c("A", "A", "B", "B"), val = 1:4)
  grouped_df <- dplyr::group_by(df, group)
  result <- mutate_(grouped_df, doubled = ~val * 2)

  expect_true(dplyr::is_grouped_df(result))
  expect_equal(result$doubled, c(2, 4, 6, 8))
})

test_that("mutate_() with column name from variable", {
  df <- data.frame(x = 1:3)
  col_name <- "y"
  result <- mutate_(df, col_name ~ x * 2)

  expect_true("y" %in% names(result))
})

test_that("mutate_() with integer columns", {
  df <- data.frame(x = 1L:3L, y = 4L:6L)
  result <- mutate_(df, z = ~x + y)

  expect_equal(result$z, c(5L, 7L, 9L))
  expect_type(result$z, "integer")
})

test_that("mutate_() with character columns", {
  df <- data.frame(x = c("a", "b", "c"), y = c("d", "e", "f"), stringsAsFactors = FALSE)
  result <- mutate_(df, z = ~paste0(x, y))

  expect_equal(result$z, c("ad", "be", "cf"))
})

test_that("mutate_() with factor columns", {
  df <- data.frame(x = factor(c("a", "b", "c")))
  result <- mutate_(df, y = ~as.character(x))

  expect_equal(result$y, c("a", "b", "c"))
  expect_type(result$y, "character")
})

test_that("mutate_() with date columns", {
  df <- data.frame(date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")))
  result <- mutate_(df, year = ~as.numeric(format(date, "%Y")))

  expect_equal(result$year, c(2020, 2020, 2020))
})

test_that("mutate_() does not error with .keep = 'invalid'", {
  df <- data.frame(x = 1:3)

  expect_error(
    mutate_(df, y = ~x * 2, .keep = "invalid"),
    ".keep"
  )
})

test_that("mutate_() with .before error message", {
  df <- data.frame(x = 1:3, y = 4:6)

  expect_error(
    mutate_(df, z = ~x + y, .before = "x"),
    ".before"
  )
})

test_that("mutate_() with .after error message", {
  df <- data.frame(x = 1:3, y = 4:6)

  expect_error(
    mutate_(df, z = ~x + y, .after = "x"),
    ".after"
  )
})

test_that("mutate_() error with .by on grouped data", {
  skip_if_not_installed("dplyr")

  df <- data.frame(group = c("A", "A", "B"), val = 1:3)
  grouped_df <- dplyr::group_by(df, group)

  expect_error(
    mutate_(grouped_df, new_val = ~val * 2, .by = "group"),
    ".by"
  )
})

test_that("transmute_() creates only new columns", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  result <- transmute_(df, w = ~x + y)

  expect_equal(ncol(result), 1)
  expect_equal(names(result), "w")
  expect_false("x" %in% names(result))
})

test_that("transmute_() with multiple new columns", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  result <- transmute_(df, a = ~x + y, b = ~x * y)

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("a", "b"))
})

test_that("transmute_() preserves data frame types", {
  skip_if_not_installed("tibble")

  df <- data.frame(x = 1:3, y = 4:6)
  result_df <- transmute_(df, z = ~x + y)
  expect_s3_class(result_df, "data.frame")

  tbl <- tibble::tibble(x = 1:3, y = 4:6)
  result_tbl <- transmute_(tbl, z = ~x + y)
  expect_s3_class(result_tbl, "tbl_df")
})

test_that("transmute_() with single column", {
  df <- data.frame(x = 1:3, y = 4:6)
  result <- transmute_(df, z = ~x)

  expect_equal(ncol(result), 1)
  expect_equal(result$z, 1:3)
})

test_that("transmute_() with conditional logic", {
  df <- data.frame(x = c(1, 5, 10))
  result <- transmute_(df, category = ~ifelse(x > 3, "high", "low"))

  expect_equal(result$category, c("low", "high", "high"))
})

test_that("transmute_() returns empty with no columns", {
  df <- data.frame(x = 1:3, y = 4:6)
  result <- transmute_(df)

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 0)
})

# Tests for mutating functions - Part 2: Advanced scenarios

test_that("mutate_() with pipe operator", {
  result <- data.frame(x = 1:3, y = 4:6) |>
    mutate_(z = ~x + y)

  expect_equal(result$z, c(5, 7, 9))
})

test_that("mutate_() with chained operations", {
  result <- data.frame(x = 1:3, y = 4:6) |>
    mutate_(z = ~x + y) |>
    mutate_(w = ~z * 2)

  expect_equal(result$w, c(10, 14, 18))
  expect_equal(ncol(result), 4)
})

test_that("mutate_() preserves all columns with .keep = 'all'", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
  result <- mutate_(df, e = ~a + b, .keep = "all")

  expect_equal(ncol(result), 5)
  expect_equal(names(result), c("a", "b", "c", "d", "e"))
})

test_that("mutate_() with multiple groups and .by", {
  df <- data.frame(
    g1 = c("A", "A", "B", "B"),
    g2 = c(1, 2, 1, 2),
    val = c(10, 20, 30, 40)
  )
  result <- mutate_(
    df,
    mean_val = ~mean(val),
    .by = c("g1", "g2")
  )

  expect_equal(result$mean_val, c(10, 20, 30, 40))
})

test_that("mutate_() with complex arithmetic", {
  df <- data.frame(x = c(2, 4, 8), y = c(3, 5, 9))
  result <- mutate_(df, complex = ~(x^2 + y^2) / (x + y))

  expect_true(all(is.numeric(result$complex)))
  expect_equal(nrow(result), 3)
})

test_that("mutate_() with row-wise operations", {
  df <- data.frame(x = 1:3, y = 2:4, z = 3:5)
  result <- mutate_(df, sum_all = ~x + y + z)

  expect_equal(result$sum_all, c(6, 9, 12))
})

test_that("mutate_() with logical operations", {
  df <- data.frame(x = 1:5)
  result <- mutate_(df, is_even = ~x %% 2 == 0)

  expect_equal(result$is_even, c(FALSE, TRUE, FALSE, TRUE, FALSE))
})

test_that("mutate_() overwrites multiple existing columns", {
  df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
  result <- mutate_(df, x = ~x * 2, y = ~y * 3)

  expect_equal(result$x, c(2, 4, 6))
  expect_equal(result$y, c(12, 15, 18))
  expect_equal(result$z, 7:9)
})

test_that("mutate_() with all NA values in computation", {
  df <- data.frame(x = c(NA, NA, NA))
  result <- mutate_(df, y = ~x * 2)

  expect_true(all(is.na(result$y)))
})

test_that("mutate_() with partial NA values", {
  df <- data.frame(x = c(1, NA, 3, NA, 5))
  result <- mutate_(df, y = ~x * 2)

  expect_equal(result$y[c(1, 3, 5)], c(2, 6, 10))
  expect_true(is.na(result$y[2]))
  expect_true(is.na(result$y[4]))
})

test_that("mutate_() with zero-row data frame", {
  df <- data.frame(x = integer(), y = integer())
  result <- mutate_(df, z = ~x + y)

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 3)
})

test_that("mutate_() with single row", {
  df <- data.frame(x = 1, y = 2)
  result <- mutate_(df, z = ~x + y)

  expect_equal(nrow(result), 1)
  expect_equal(result$z, 3)
})

test_that("mutate_() with very large data frame", {
  df <- data.frame(x = 1:10000, y = 1:10000)
  result <- mutate_(df, z = ~x + y)

  expect_equal(nrow(result), 10000)
  expect_equal(ncol(result), 3)
  expect_equal(result$z[10000], 20000)
})

test_that("mutate_() with duplicate column names after mutation", {
  df <- data.frame(x = 1:3, y = 4:6)
  result <- mutate_(df, x = ~x * 2)

  expect_equal(sum(names(result) == "x"), 1)
  expect_equal(result$x, c(2, 4, 6))
})

test_that("mutate_() with special characters in column names", {
  df <- data.frame(
    "x y" = 1:3,
    "z-w" = 4:6,
    check.names = FALSE)

  result <- mutate_(df, "new col" = ~`x y` + `z-w`)

  expect_true("new col" %in% names(result))
  expect_true(all(result$`new col` == c(5, 7, 9)))
})

test_that("mutate_() with type coercion", {
  df <- data.frame(x = c("1", "2", "3"))
  result <- mutate_(df, y = ~as.numeric(x) * 2)

  expect_equal(result$y, c(2, 4, 6))
})

test_that("transmute_() with grouped data", {
  df <- data.frame(group = c("A", "A", "B", "B"), val = 1:4)
  grouped_df <- group_by_(df, 'group')
  result <- transmute_(grouped_df, sum_val = ~sum(val))

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("group", "sum_val"))

  # Also with dplyr::group_by()
  skip_if_not_installed("dplyr")

  grouped_df <- dplyr::group_by(df, group)
  result <- transmute_(grouped_df, sum_val = ~sum(val))

  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("group", "sum_val"))
})

test_that("transmute_() with .by parameter", {
  df <- data.frame(group = c("A", "A", "B", "B"), val = 1:4)
  result <- transmute_(df, sum_val = ~sum(val), .by = "group")

  expect_equal(ncol(result), 2)
  expect_equal(result$sum_val, c(3, 3, 7, 7))
})

test_that("mutate_() column references across rows", {
  df <- data.frame(x = 1:5)
  result <- mutate_(df, y = ~c(NA, x[-length(x)]))

  expect_true(is.na(result$y[1]))
  expect_equal(result$y[2], 1)
  expect_equal(result$y[5], 4)
})

test_that("mutate_() with trigonometric functions", {
  df <- data.frame(angle = c(0, pi/2, pi))
  result <- mutate_(df, sin_val = ~sin(angle), cos_val = ~cos(angle))

  expect_true(all(is.numeric(result$sin_val)))
  expect_true(all(is.numeric(result$cos_val)))
})

test_that("mutate_() with string functions", {
  df <- data.frame(text = c("hello", "world", "test"))
  result <- mutate_(
    df,
    upper = ~toupper(text),
    length = ~nchar(text)
  )

  expect_equal(result$upper, c("HELLO", "WORLD", "TEST"))
  expect_equal(result$length, c(5, 5, 4))
})

test_that("mutate_() preserves attributes of data frame", {
  df <- data.frame(x = 1:3, y = 4:6)
  attr(df, "custom") <- "value"
  result <- mutate_(df, z = ~x + y)

  expect_s3_class(result, "data.frame")
})

test_that("mutate_() with cumulative functions", {
  df <- data.frame(x = 1:5)
  result <- mutate_(df, cum_sum = ~cumsum(x))

  expect_equal(result$cum_sum, c(1, 3, 6, 10, 15))
})

test_that("mutate_() with aggregation after grouping", {
  df <- data.frame(group = c("A", "A", "B", "B"), val = c(1, 2, 3, 4))
  result <- mutate_(df, total = ~sum(val), .by = "group")

  expect_equal(result$total, c(3, 3, 7, 7))
})

test_that("mutate_() with ranking functions", {
  df <- data.frame(x = c(5, 2, 8, 2, 9))
  result <- mutate_(df, rank_x = ~rank(x))

  expect_true(all(is.numeric(result$rank_x)))
})

test_that("mutate_() with vector recycling", {
  df <- data.frame(x = 1:4)
  result <- mutate_(df, y = ~rep(c(1, 2), 2))

  expect_equal(result$y, c(1, 2, 1, 2))
})

test_that("transmute_() with complex expressions", {
  df <- data.frame(
    a = 1:3,
    b = 4:6,
    c = 7:9,
    d = 10:12
  )
  result <- transmute_(
    df,
    result1 = ~a + b,
    result2 = ~(c - d) / d,
    result3 = ~sqrt(a * b)
  )

  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 3)
})

test_that("mutate_() with nested list columns", {
  df <- data.frame(x = 1:2)
  result <- mutate_(df, nested = ~list(list(a = x, b = x * 2)))

  expect_true(is.list(result$nested))
  expect_true(is.list(result$nested[[1]]))
})

test_that("mutate_() maintains factor levels in grouping", {
  df <- data.frame(
    group = factor(c("A", "B", "A", "B")),
    val = 1:4
  )
  result <- mutate_(df, mean_val = ~mean(val), .by = "group")

  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 3)
})

test_that("mutate_() with date arithmetic", {
  df <- data.frame(date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")))
  result <- mutate_(df, day_num = ~as.numeric(date - min(date)))

  expect_equal(result$day_num, c(0, 1, 2))
})

test_that("mutate_() error when .by used with grouped data", {
  skip_if_not_installed("dplyr")

  df <- data.frame(g = c("A", "A", "B"), v = 1:3)
  grouped <- dplyr::group_by(df, g)

  expect_error(
    mutate_(grouped, new = ~v * 2, .by = "g"),
    ".by"
  )
})

test_that("mutate_() maintains data.frame structure with zero columns", {
  df <- data.frame(x = 1:3, y = 4:6)
  result <- transmute_(df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 0)
})

test_that("mutate_() with very long column names", {
  df <- data.frame(x = 1:3)
  long_name <- paste(rep("very_long_", 10), collapse = "")
  result <- mutate_(df, long_name ~ x * 2)

  expect_true(long_name %in% names(result))
})
