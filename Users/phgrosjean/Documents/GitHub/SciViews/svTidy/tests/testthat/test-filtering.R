# Tests for filtering functions

test_that("filter_() works with basic conditions", {
  data(mtcars)
  result <- filter_(mtcars, ~mpg > 20)
  
  expect_s3_class(result, "data.frame")
  expect_true(all(result$mpg > 20))
  expect_lt(nrow(result), nrow(mtcars))
})

test_that("filter_() works with multiple conditions (AND logic)", {
  data(mtcars)
  result <- filter_(mtcars, ~mpg > 20, ~cyl == 4)
  
  expect_true(all(result$mpg > 20))
  expect_true(all(result$cyl == 4))
  expect_equal(nrow(result), sum(mtcars$mpg > 20 & mtcars$cyl == 4))
})

test_that("filter_() works with character column names", {
  data(mtcars)
  result <- filter_(mtcars, "mpg > 20", "cyl == 4")
  
  expect_true(all(result$mpg > 20))
  expect_true(all(result$cyl == 4))
})

test_that("filter_() handles OR logic with | operator", {
  data(mtcars)
  result <- filter_(mtcars, ~cyl == 4 | cyl == 6)
  
  expect_true(all(result$cyl %in% c(4, 6)))
  expect_equal(nrow(result), sum(mtcars$cyl %in% c(4, 6)))
})

test_that("filter_() handles negation with ! operator", {
  data(mtcars)
  result <- filter_(mtcars, ~!(cyl == 4))
  
  expect_false(any(result$cyl == 4))
  expect_equal(nrow(result), sum(mtcars$cyl != 4))
})

test_that("filter_() preserves data frame type", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("data.table")
  
  data(mtcars)
  
  # Test with data.frame
  result_df <- filter_(mtcars, ~mpg > 20)
  expect_s3_class(result_df, "data.frame")
  
  # Test with tibble
  tbl <- tibble::as_tibble(mtcars)
  result_tbl <- filter_(tbl, ~mpg > 20)
  expect_s3_class(result_tbl, "tbl_df")
  
  # Test with data.table
  dt <- data.table::as.data.table(mtcars)
  result_dt <- filter_(dt, ~mpg > 20)
  expect_s3_class(result_dt, "data.table")
})

test_that("filter_() works with comparison operators", {
  data(mtcars)
  
  # Greater than
  result_gt <- filter_(mtcars, ~mpg > 20)
  expect_true(all(result_gt$mpg > 20))
  
  # Less than
  result_lt <- filter_(mtcars, ~mpg < 15)
  expect_true(all(result_lt$mpg < 15))
  
  # Equal
  result_eq <- filter_(mtcars, ~cyl == 4)
  expect_true(all(result_eq$cyl == 4))
  
  # Not equal
  result_ne <- filter_(mtcars, ~cyl != 4)
  expect_false(any(result_ne$cyl == 4))
})

test_that("filter_() works with %in% operator", {
  data(mtcars)
  result <- filter_(mtcars, ~cyl %in% c(4, 6))
  
  expect_true(all(result$cyl %in% c(4, 6)))
  expect_equal(nrow(result), sum(mtcars$cyl %in% c(4, 6)))
})

test_that("filter_() works with is.na()", {
  df <- data.frame(x = c(1, 2, NA, 4), y = c(NA, "b", "c", "d"))
  result <- filter_(df, ~!is.na(x))
  
  expect_false(any(is.na(result$x)))
  expect_equal(nrow(result), 3)
})

test_that("filter_() works with missing(...) - returns full data", {
  data(mtcars)
  result <- filter_(mtcars)
  
  expect_equal(result, mtcars)
})

test_that("filter_() works with grouped data frames", {
  data(mtcars)
  mtcars_grouped <- group_by_(mtcars, ~cyl)
  result <- filter_(mtcars_grouped, ~mpg > mean(~mpg))
  
  expect_true(is_grouped_df(result))
  # Each group should have rows where mpg > group mean
  for (cyl_val in unique(result$cyl)) {
    group_data <- result[result$cyl == cyl_val, ]
    group_mean <- mean(mtcars[mtcars$cyl == cyl_val, "mpg"]$mpg)
    expect_true(all(group_data$mpg > group_mean))
  }
})

test_that("filter_() works with .by parameter", {
  data(mtcars)
  result <- filter_(mtcars, ~mpg > mean(~mpg), .by = ~cyl)
  
  expect_false(is_grouped_df(result))
  # Check that grouping was applied correctly
  for (cyl_val in unique(result$cyl)) {
    group_data <- result[result$cyl == cyl_val, ]
    group_mean <- mean(mtcars[mtcars$cyl == cyl_val, "mpg"]$mpg)
    expect_true(all(group_data$mpg > group_mean))
  }
})

test_that("filter_() works with string patterns using grepl", {
  df <- data.frame(name = c("apple", "banana", "apricot", "blueberry"))
  result <- filter_(df, ~grepl("^a", name))
  
  expect_equal(nrow(result), 2)
  expect_true(all(startsWith(result$name, "a")))
})

test_that("filter_() works with mathematical functions", {
  data(mtcars)
  result <- filter_(mtcars, ~sqrt(hp) > 10)
  
  expect_true(all(sqrt(result$hp) > 10))
})

test_that("filter_() works with logical operators", {
  data(mtcars)
  result <- filter_(mtcars, ~(cyl == 4 & mpg > 25) | (cyl == 6 & mpg > 20))
  
  expected_mask <- (mtcars$cyl == 4 & mtcars$mpg > 25) | (mtcars$cyl == 6 & mtcars$mpg > 20)
  expect_equal(nrow(result), sum(expected_mask))
})

test_that("filter_() returns empty data frame when no rows match", {
  data(mtcars)
  result <- filter_(mtcars, ~mpg > 1000)
  
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), ncol(mtcars))
})

test_that("filter_() preserves column order", {
  data(mtcars)
  result <- filter_(mtcars, ~mpg > 20)
  
  expect_equal(names(result), names(mtcars))
})

test_that("filter_() preserves row names", {
  data(mtcars)
  result <- filter_(mtcars, ~mpg > 20)
  
  expect_equal(rownames(result), rownames(mtcars)[mtcars$mpg > 20])
})

test_that("filter_() works with between()", {
  data(mtcars)
  result <- filter_(mtcars, ~dplyr::between(mpg, 15, 25))
  
  expect_true(all(result$mpg >= 15 & result$mpg <= 25))
})

test_that("filter_() works with multiple grouping variables", {
  data(mtcars)
  mtcars_grouped <- group_by_(mtcars, ~cyl, ~gear)
  result <- filter_(mtcars_grouped, ~mpg > median(~mpg))
  
  expect_true(is_grouped_df(result))
  expect_equal(length(group_vars(result)), 2)
})

test_that("filter_() works with NA in conditions", {
  df <- data.frame(
    x = c(1, 2, NA, 4, 5),
    y = c("a", "b", "c", NA, "e")
  )
  result <- filter_(df, ~!is.na(y))
  
  expect_false(any(is.na(result$y)))
  expect_equal(nrow(result), 3)
})

test_that("filter_() works with complex expressions", {
  data(mtcars)
  result <- filter_(mtcars, ~log(mpg) > 3, ~sqrt(hp) < 15)
  
  expect_true(all(log(result$mpg) > 3))
  expect_true(all(sqrt(result$hp) < 15))
})

test_that("filter_() errors on invalid column references", {
  data(mtcars)
  
  expect_error(
    filter_(mtcars, ~nonexistent_column > 5),
    "object.*not found|unknown.*variable"
  )
})

test_that("filter_() preserves attributes", {
  data(mtcars)
  mtcars$comment <- "test"
  attr(mtcars, "custom_attr") <- "value"
  
  result <- filter_(mtcars, ~mpg > 20)
  
  expect_true(inherits(result, class(mtcars)[1]))
})

test_that("filter_() works with data.trame objects", {
  skip_if_not_installed("data.trame")
  
  data(mtcars)
  dtrame <- data.trame::as.data.trame(mtcars)
  result <- filter_(dtrame, ~mpg > 20)
  
  expect_s3_class(result, "data.trame")
  expect_true(all(result$mpg > 20))
})

test_that("filter_() works with pipe operator", {
  data(mtcars)
  result <- mtcars |> filter_(~mpg > 20)
  
  expect_true(all(result$mpg > 20))
})

test_that("filter_() works with single row result", {
  data(mtcars)
  result <- filter_(mtcars, ~hp > 330)
  
  expect_equal(nrow(result), 1)
  expect_true(result$hp > 330)
})

test_that("filter_() preserves grouping structure after filtering", {
  data(mtcars)
  mtcars_grouped <- group_by_(mtcars, ~cyl)
  result <- filter_(mtcars_grouped, ~mpg > 20)
  
  expect_true(is_grouped_df(result))
  expect_equal(group_vars(result), "cyl")
})

test_that("filter_() works with between() and multiple conditions", {
  data(mtcars)
  result <- filter_(
    mtcars,
    ~dplyr::between(mpg, 15, 25),
    ~hp > 100
  )
  
  expect_true(all(result$mpg >= 15 & result$mpg <= 25))
  expect_true(all(result$hp > 100))
})

test_that("filter_() works with ifelse() in condition", {
  data(mtcars)
  result <- filter_(mtcars, ~ifelse(cyl == 4, mpg > 25, mpg > 15))
  
  for (i in seq_len(nrow(result))) {
    if (result$cyl[i] == 4) {
      expect_true(result$mpg[i] > 25)
    } else {
      expect_true(result$mpg[i] > 15)
    }
  }
})

test_that("filter_() returns same structure with all rows when condition always true", {
  data(mtcars)
  result <- filter_(mtcars, ~mpg > 0)
  
  expect_equal(nrow(result), nrow(mtcars))
  expect_equal(ncol(result), ncol(mtcars))
})

test_that("distinct_() removes duplicate rows", {
  data(mtcars)
  result <- distinct_(mtcars, ~cyl, ~gear)
  
  expect_lt(nrow(result), nrow(mtcars))
  expect_equal(nrow(result), nrow(unique(mtcars[, c("cyl", "gear")])))
})

test_that("distinct_() with .keep_all preserves all columns", {
  data(mtcars)
  result <- distinct_(mtcars, ~cyl, .keep_all = TRUE)
  
  expect_equal(ncol(result), ncol(mtcars))
  expect_lt(nrow(result), nrow(mtcars))
})

test_that("slice_() selects rows by position", {
  data(mtcars)
  result <- slice_(mtcars, 1, 5, 10)
  
  expect_equal(nrow(result), 3)
  expect_equal(rownames(result), rownames(mtcars)[c(1, 5, 10)])
})

test_that("slice_head_() selects first n rows", {
  data(mtcars)
  result <- slice_head_(mtcars, n = 5)
  
  expect_equal(nrow(result), 5)
  expect_equal(result, mtcars[1:5, ])
})

test_that("slice_tail_() selects last n rows", {
  data(mtcars)
  result <- slice_tail_(mtcars, n = 3)
  
  expect_equal(nrow(result), 3)
  expect_equal(result, mtcars[(nrow(mtcars) - 2):nrow(mtcars), ])
})

test_that("slice_head_() with prop parameter", {
  data(mtcars)
  result <- slice_head_(mtcars, prop = 0.1)
  
  expected_n <- ceiling(nrow(mtcars) * 0.1)
  expect_equal(nrow(result), expected_n)
})

test_that("slice_tail_() with prop parameter", {
  data(mtcars)
  result <- slice_tail_(mtcars, prop = 0.2)
  
  expected_n <- ceiling(nrow(mtcars) * 0.2)
  expect_equal(nrow(result), expected_n)
})

test_that("slice_() works with negative indices", {
  data(mtcars)
  result <- slice_(mtcars, -1, -2, -3)
  
  expect_equal(nrow(result), nrow(mtcars) - 3)
  expect_false(1 %in% as.numeric(rownames(result)))
})

