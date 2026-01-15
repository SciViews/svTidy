# Tests for summarising functions - Part 1: summarise_() basics

test_that("summarise_() creates single row summary", {
  df <- data.frame(x = 1:10, y = 11:20)

  result <- summarise_(df, mean_x = ~mean(x), sum_y = ~sum(y))

  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("mean_x", "sum_y"))
  expect_equal(result$mean_x, 5.5)
  expect_equal(result$sum_y, 155)
})

test_that("summarise_() works with formula syntax", {
  df <- data.frame(value = c(10, 20, 30, 40, 50))

  result <- summarise_(df, median_val = ~median(value))

  expect_equal(result$median_val, 30)
})

test_that("summarise_() works with standard evaluation", {
  df <- data.frame(x = 1:5, y = 6:10)

  result <- summarise_(df, mean_x = mean(df$x), sum_y = sum(df$y))

  expect_equal(nrow(result), 1)
  expect_equal(result$mean_x, 3)
})

test_that("summarise_() with multiple summary statistics", {
  df <- data.frame(x = 1:10)

  result <- summarise_(df,
    mean_x = ~mean(x),
    sd_x   = ~sd(x),
    min_x  = ~min(x),
    max_x  = ~max(x),
    n_obs  = ~length(x)
  )

  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 5)
  expect_equal(result$mean_x, 5.5)
  expect_equal(result$min_x, 1)
  expect_equal(result$max_x, 10)
  expect_equal(result$n_obs, 10)
})

test_that("summarise_() with grouped data", {
  df <- data.frame(
    group = c("A", "A", "A", "B", "B", "B"),
    value = c(1, 2, 3, 4, 5, 6)
  )
  df_grouped <- group_by_(df, ~group)

  result <- summarise_(df_grouped, mean_val = ~mean(value))

  expect_equal(nrow(result), 2)
  expect_equal(result$mean_val, c(2, 5))
})

test_that("summarise_() with .by parameter", {
  df <- data.frame(
    group = c("A", "A", "B", "B"),
    value = c(1, 2, 3, 4)
  )

  result <- summarise_(df,
    mean_val = ~mean(value),
    .by = 'group'
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$group, c("A", "B"))
  expect_equal(result$mean_val, c(1.5, 3.5))
})

test_that("summarise_() with .by and multiple grouping variables", {
  df <- data.frame(
    g1 = c("A", "A", "B", "B"),
    g2 = c("X", "Y", "X", "Y"),
    val = c(1, 2, 3, 4)
  )

  result <- summarise_(df,
    mean_val = ~mean(val),
    .by = c('g1', 'g2')
  )

  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 3)
})

test_that("summarise_() with .groups parameter", {
  df <- data.frame(
    g1 = c("A", "A", "B", "B"),
    g2 = c("X", "X", "Y", "Y"),
    val = c(1, 2, 3, 4)
  )
  df_grouped <- group_by_(df, ~g1, ~g2)

  # Drop last level
  result_drop_last <- summarise_(df_grouped, mean_val = ~mean(val),
    .groups = "drop_last")
  expect_true(is.grouped_df(result_drop_last))
  expect_equal(length(groups_(result_drop_last)), 1)

  # Drop all groups
  result_drop <- summarise_(df_grouped, mean_val = ~mean(val), .groups = "drop")
  expect_false(is.grouped_df(result_drop))

  # Keep all groups
  result_keep <- summarise_(df_grouped, mean_val = ~mean(val), .groups = "keep")
  expect_true(is.grouped_df(result_keep))
  expect_equal(length(groups_(result_keep)), 2)
})

test_that("summarise_() preserves data frame type", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("data.table")

  # data.frame
  df <- data.frame(x = 1:5)
  result_df <- summarise_(df, mean_x = ~mean(x))
  expect_s3_class(result_df, "data.frame")

  # tibble
  tbl <- tibble::tibble(x = 1:5)
  result_tbl <- summarise_(tbl, mean_x = ~mean(x))
  expect_s3_class(result_tbl, "tbl_df")

  # data.table
  dt <- data.table::data.table(x = 1:5)
  result_dt <- summarise_(dt, mean_x = ~mean(x))
  expect_s3_class(result_dt, "data.table")
})

test_that("summarise_() with NA handling", {
  df <- data.frame(x = c(1, 2, NA, 4, 5))

  result <- summarise_(df,
    mean_with_na = ~mean(x),
    mean_no_na   = ~mean(x, na.rm = TRUE)
  )

  expect_true(is.na(result$mean_with_na))
  expect_equal(result$mean_no_na, 3)
})

test_that("summarise_() with zero-row data frame", {
  df <- data.frame(x = numeric(), y = numeric())

  result <- summarise_(df, mean_x = ~mean(x))

  expect_equal(nrow(result), 1)
  expect_true(is.nan(result$mean_x))
})

test_that("summarise_() with single row data frame", {
  df <- data.frame(x = 42)

  result <- summarise_(df, val = ~x * 2)

  expect_equal(nrow(result), 1)
  expect_equal(result$val, 84)
})

test_that("summarise_() with conditional aggregation", {
  df <- data.frame(x = 1:10, group = c("A", "B"))

  result <- summarise_(df,
    sum_all = ~sum(x),
    sum_a   = ~sum(x[group == "A"])
  )

  expect_equal(result$sum_all, 55)
  expect_equal(result$sum_a, 25)
})

test_that("summarise_() error with .by on grouped data", {
  df <- data.frame(g = c("A", "B"), x = 1:2)
  df_grouped <- group_by_(df, ~g)

  expect_error(
    summarise_(df_grouped, mean_x = ~mean(x), .by = 'g'),
    "can't supply `.by` "
  )
})

test_that("summarise_() with logical columns", {
  df <- data.frame(flag = c(TRUE, FALSE, TRUE, TRUE, FALSE))

  result <- summarise_(df,
    n_true   = ~sum(flag),
    pct_true = ~mean(flag) * 100
  )

  expect_equal(result$n_true, 3)
  expect_equal(result$pct_true, 60)
})

test_that("summarise_() with character columns", {
  df <- data.frame(x = c("a", "b", "c", "d", "e"))

  result <- summarise_(df,
    n_unique  = ~length(unique(x)),
    first_val = ~x[1]
  )

  expect_equal(result$n_unique, 5)
  expect_equal(result$first_val, "a")
})

test_that("summarise_() with date columns", {
  df <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03", "2020-01-04"))
  )

  result <- summarise_(df,
    min_date = ~min(date),
    max_date = ~max(date),
    n_days   = ~length(date)
  )

  expect_equal(result$min_date, as.Date("2020-01-01"))
  expect_equal(result$max_date, as.Date("2020-01-04"))
  expect_equal(result$n_days, 4)
})

test_that("summarise_() with factor columns", {
  df <- data.frame(
    fct = factor(c("low", "high", "low", "medium", "high"))
  )

  result <- summarise_(df,
    n_levels = ~nlevels(fct),
    first_level = ~as.character(fct[1])
  )

  expect_equal(result$n_levels, 3)
  expect_equal(result$first_level, "low")
})

test_that("summarise_() with pipe operator", {
  result <- data.frame(x = 1:10, y = 11:20) |>
    summarise_(
      mean_x = ~mean(x),
      sum_y  = ~sum(y)
    )

  expect_equal(nrow(result), 1)
  expect_equal(result$mean_x, 5.5)
})


# Tests for summarising functions - Part 2: reframe_() and count_()

test_that("reframe_() returns ungrouped data", {
  df <- data.frame(
    group = c("A", "A", "B", "B"),
    value = c(1, 2, 3, 4)
  )
  df_grouped <- group_by_(df, ~group)

  result <- reframe_(df_grouped, quantile_val = ~quantile(value, c(0.25, 0.75)))

  expect_false(is.grouped_df(result))
  expect_equal(nrow(result), 4)
})

test_that("reframe_() with multiple rows per group", {
  df <- data.frame(
    group = c("A", "A", "A", "B", "B", "B"),
    value = c(1, 2, 3, 4, 5, 6)
  )
  df_grouped <- group_by_(df, ~group)

  result <- reframe_(df_grouped,
    quantiles = ~quantile(value, c(0.25, 0.5, 0.75))
  )

  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 2)
})

test_that("reframe_() with .by parameter", {
  df <- data.frame(
    group = c("A", "A", "B", "B"),
    value = c(1, 2, 3, 4)
  )

  result <- reframe_(df,
    range_val = ~range(value),
    .by = 'group'
  )

  expect_equal(nrow(result), 4)
  expect_false(is.grouped_df(result))
})

test_that("reframe_() with computed columns", {
  df <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(2, 4, 6, 8, 10)
  )

  result <- reframe_(df,
    ratios = ~x/y
  )

  expect_equal(nrow(result), 5)
  expect_equal(result$ratios, c(0.5, 0.5, 0.5, 0.5, 0.5))
})

test_that("count_() basic counting", {
  df <- data.frame(x = c("a", "b", "a", "c", "b", "a"))

  result <- count_(df, ~x)

  expect_equal(nrow(result), 3)
  expect_equal(result$n, c(3, 2, 1))
})

test_that("count_() with multiple variables", {
  df <- data.frame(
    g1 = c("A", "A", "B", "B", "A"),
    g2 = c("X", "Y", "X", "Y", "X")
  )

  result <- count_(df, ~g1, ~g2)

  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 3)
})

test_that("count_() with sort parameter", {
  df <- data.frame(x = c("a", "b", "a", "c", "b", "a"))

  result <- count_(df, ~x, sort = TRUE)

  expect_equal(result$x, c("a", "b", "c"))
  expect_equal(result$n, c(3, 2, 1))
})

test_that("count_() with decreasing parameter", {
  df <- data.frame(x = c("a", "b", "a", "c", "b", "a"))

  result <- count_(df, ~x, sort = TRUE, decreasing = FALSE)

  expect_equal(result$n[1], 1)
  expect_equal(result$n[3], 3)
})

test_that("count_() with weights", {
  df <- data.frame(
    x = c("a", "b", "a", "b"),
    weight = c(1, 2, 3, 4)
  )

  result <- count_(df, ~x, wt = ~weight)

  expect_equal(nrow(result), 2)
  expect_equal(result$n, c(4, 6))
})

test_that("count_() with custom name parameter", {
  df <- data.frame(x = c("a", "b", "a"))

  result <- count_(df, ~x, name = "frequency")

  expect_true("frequency" %in% names(result))
  expect_false("n" %in% names(result))
})

test_that("count_() with computed grouping variable", {
  df <- data.frame(x = 1:6)

  result <- count_(df, even = ~x %% 2 == 0)

  expect_equal(nrow(result), 2)
  expect_true(any(result$even == TRUE))
  expect_true(any(result$even == FALSE))
})

test_that("count_() with NA values", {
  df <- data.frame(x = c("a", "b", NA, "a", "b", NA))

  result <- count_(df, ~x)

  expect_equal(nrow(result), 3)
  expect_true(any(is.na(result$x)))
})

test_that("count_() on grouped data", {
  df <- data.frame(
    group = c("A", "A", "B", "B"),
    x = c("a", "b", "a", "a")
  )
  df_grouped <- group_by_(df, ~group)

  result <- count_(df_grouped, ~x)

  expect_equal(nrow(result), 3L)
})

test_that("count_() with zero-row data frame", {
  df <- data.frame(x = character())

  result <- count_(df, ~x)

  expect_equal(nrow(result), 0)
})

test_that("count_() with single row", {
  df <- data.frame(x = "a")

  result <- count_(df, ~x)

  expect_equal(nrow(result), 1)
  expect_equal(result$n, 1)
})

test_that("count_() preserves data frame type", {
  skip_if_not_installed("tibble")

  df <- tibble::tibble(x = c("a", "b", "a"))
  result <- count_(df, ~x)

  expect_s3_class(result, "tbl_df")
})

test_that("count_() with pipe operator", {
  result <- data.frame(x = c("a", "b", "a", "b", "b")) |>
    count_(~x, sort = TRUE)

  expect_equal(nrow(result), 2)
  expect_equal(result$x[1], "b")
})

test_that("count_() with multiple computed variables", {
  df <- data.frame(x = 1:4)

  result <- count_(df,
    even = ~x %% 2 == 0,
    gt2  = ~x > 2
  )

  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 3)
})

test_that("count_() with character column name", {
  df <- data.frame(group = c("A", "B", "A", "B"))

  result <- count_(df, "group")

  expect_equal(nrow(result), 2)
  expect_equal(result$n, c(2, 2))
})

# Tests for summarising functions - Part 3: tally_() and add_count_()

test_that("tally_() counts total rows", {
  df <- data.frame(x = 1:10, y = 11:20)

  result <- tally_(df)

  expect_equal(nrow(result), 1)
  expect_equal(result$n, 10)
})

test_that("tally_() with grouped data", {
  df <- data.frame(
    group = c("A", "A", "B", "B", "B"),
    value = 1:5
  )
  df_grouped <- group_by_(df, ~group)

  result <- tally_(df_grouped)

  expect_equal(nrow(result), 2)
  expect_equal(result$n, c(2, 3))
})

test_that("tally_() with weights", {
  df <- data.frame(
    group = c("A", "A", "B", "B"),
    weight = c(1, 2, 3, 4)
  )
  df_grouped <- group_by_(df, ~group)

  result <- tally_(df_grouped, wt = ~weight)

  expect_equal(nrow(result), 2)
  expect_equal(result$n, c(3, 7))
})

test_that("tally_() with custom name", {
  df <- data.frame(x = 1:5)

  result <- tally_(df, name = "count")

  expect_true("count" %in% names(result))
  expect_false("n" %in% names(result))
})

test_that("tally_() with sort parameter", {
  df <- data.frame(
    group = c("A", "A", "B", "B", "B", "C"),
    value = 1:6
  )
  df_grouped <- group_by_(df, ~group)

  result <- tally_(df_grouped, sort = TRUE)

  expect_equal(result$group, c("B", "A", "C"))
})

test_that("tally_() with zero-row data frame", {
  df <- data.frame(x = numeric())

  result <- tally_(df)

  expect_equal(nrow(result), 1L)
  expect_equal(result$n, 0)
})

test_that("tally_() with single row", {
  df <- data.frame(x = 42)

  result <- tally_(df)

  expect_equal(result$n, 1)
})

test_that("tally_() preserves data frame type", {
  skip_if_not_installed("tibble")

  df <- tibble::tibble(x = 1:5)
  result <- tally_(df)

  expect_s3_class(result, "tbl_df")
})

test_that("tally_() with pipe operator", {
  result <- data.frame(x = 1:8, y = 9:16) |>
    tally_()

  expect_equal(result$n, 8)
})

test_that("add_count_() adds count column", {
  df <- data.frame(x = c("a", "b", "a", "c", "b", "a"))

  result <- add_count_(df, ~x)

  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 2)
  expect_true("n" %in% names(result))
  expect_equal(result$n, c(3, 2, 3, 1, 2, 3))
})

test_that("add_count_() with multiple grouping variables", {
  df <- data.frame(
    g1 = c("A", "A", "B", "B"),
    g2 = c("X", "Y", "X", "Y"),
    val = 1:4
  )

  result <- add_count_(df, ~g1, ~g2)

  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 4)
  expect_true(all(result$n == 1))
})

test_that("add_count_() with custom name", {
  df <- data.frame(x = c("a", "b", "a"))

  result <- add_count_(df, ~x, name = "freq")

  expect_true("freq" %in% names(result))
  expect_false("n" %in% names(result))
})

test_that("add_count_() with weights", {
  df <- data.frame(
    x = c("a", "b", "a"),
    weight = c(2, 3, 4)
  )

  result <- add_count_(df, ~x, wt = ~weight)

  expect_equal(result$n, c(6, 3, 6))
})

test_that("add_count_() preserves all columns", {
  df <- data.frame(
    id = 1:4,
    group = c("A", "B", "A", "B"),
    value = c(10, 20, 30, 40)
  )

  result <- add_count_(df, ~group)

  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 4)
  expect_true(all(c("id", "group", "value", "n") %in% names(result)))
})

test_that("add_count_() with sort parameter", {
  df <- data.frame(x = c("a", "b", "a", "c", "b", "a"))

  result <- add_count_(df, ~x, sort = TRUE)

  expect_equal(nrow(result), 6)
  expect_equal(result$n[1], 3)
})

test_that("add_count_() on grouped data frame", {
  df <- data.frame(
    outer = c("X", "X", "Y", "Y"),
    inner = c("A", "B", "A", "A")
  )
  df_grouped <- group_by_(df, ~outer)

  result <- add_count_(df_grouped, ~inner)

  expect_equal(nrow(result), 4)
  expect_true("n" %in% names(result))
})

test_that("add_count_() with NA values", {
  df <- data.frame(x = c("a", NA, "a", NA))

  result <- add_count_(df, ~x)

  expect_equal(result$n[1], 2)
  expect_equal(result$n[2], 2)
})

test_that("add_count_() with pipe operator", {
  result <- data.frame(x = c("a", "b", "a", "b", "b")) |>
    add_count_(~x)

  expect_equal(nrow(result), 5)
  expect_equal(result$n[1], 2)
  expect_equal(result$n[2], 3)
})

test_that("add_count_() with zero-row data frame", {
  df <- data.frame(x = character())

  result <- add_count_(df, ~x)

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 2)
})

test_that("add_count_() preserves data frame type", {
  skip_if_not_installed("tibble")

  df <- tibble::tibble(x = c("a", "b", "a"))
  result <- add_count_(df, ~x)

  expect_s3_class(result, "tbl_df")
})

# Tests for summarising functions - Part 4: add_tally_() and advanced scenarios

test_that("add_tally_() adds count to each row", {
  df <- data.frame(x = 1:5, y = 6:10)

  result <- add_tally_(df)

  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 3)
  expect_true("n" %in% names(result))
  expect_true(all(result$n == 5))
})

test_that("add_tally_() with grouped data", {
  df <- data.frame(
    group = c("A", "A", "B", "B", "B"),
    value = 1:5
  )
  df_grouped <- group_by_(df, ~group)

  result <- add_tally_(df_grouped)

  expect_equal(nrow(result), 5)
  expect_equal(result$n, c(2, 2, 3, 3, 3))
})

test_that("add_tally_() with weights", {
  df <- data.frame(
    group = c("A", "A", "B", "B"),
    weight = c(1, 2, 3, 4)
  )
  df_grouped <- group_by_(df, ~group)

  result <- add_tally_(df_grouped, wt = ~weight)

  expect_equal(nrow(result), 4)
  expect_equal(result$n, c(3, 3, 7, 7))
})

test_that("add_tally_() with custom name", {
  df <- data.frame(x = 1:5)

  result <- add_tally_(df, name = "total")

  expect_true("total" %in% names(result))
  expect_false("n" %in% names(result))
})

test_that("add_tally_() with sort parameter", {
  df <- data.frame(
    group = c("A", "A", "B", "B", "B", "C"),
    value = 1:6
  )
  df_grouped <- group_by_(df, ~group)

  result <- add_tally_(df_grouped, sort = TRUE)

  expect_equal(nrow(result), 6)
  expect_true(all(diff(result$n[result$group != "A"]) <= 0))
})

test_that("add_tally_() preserves all columns", {
  df <- data.frame(
    id = 1:4,
    name = c("a", "b", "a", "b"),
    value = c(10, 20, 30, 40)
  )

  result <- add_tally_(df)

  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 4)
  expect_true(all(c("id", "name", "value", "n") %in% names(result)))
})

test_that("add_tally_() with zero-row data frame", {
  df <- data.frame(x = numeric())

  result <- add_tally_(df)

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 2)
})

test_that("add_tally_() with single row", {
  df <- data.frame(x = 42)

  result <- add_tally_(df)

  expect_equal(nrow(result), 1)
  expect_equal(result$n, 1)
})

test_that("add_tally_() with pipe operator", {
  result <- data.frame(x = 1:8) |>
    add_tally_()

  expect_equal(nrow(result), 8)
  expect_true(all(result$n == 8))
})

test_that("add_tally_() preserves data frame type", {
  skip_if_not_installed("tibble")

  df <- tibble::tibble(x = 1:5)
  result <- add_tally_(df)

  expect_s3_class(result, "tbl_df")
})

test_that("chained count operations", {
  df <- data.frame(
    g1 = c("A", "A", "B", "B"),
    g2 = c("X", "Y", "X", "Y")
  )

  result <- df |>
    count_(~g1, ~g2) |>
    rename_(freq = ~n)

  expect_equal(nrow(result), 4)
  expect_true("freq" %in% names(result))
  expect_false("n" %in% names(result))
})

test_that("summarise and count combination", {
  df <- data.frame(
    group = c("A", "A", "B", "B"),
    value = c(1, 2, 3, 4)
  )

  result <- df |>
    count_(~group) |>
    mutate_(pct = ~n / sum(n) * 100)

  expect_equal(nrow(result), 2)
  expect_true("pct" %in% names(result))
})

# Cannot do that yet!
#test_that("reframe with multiple row output", {
#  df <- data.frame(
#    group = c("A", "A", "A", "B", "B", "B"),
#    value = c(1, 2, 3, 4, 5, 6)
#  )
#  df_grouped <- group_by_(df, ~group)

#  result <- reframe_(df_grouped,
#    stat = ~c("min", "mean", "max"),
#    val = ~c(min(value), mean(value), max(value))
#  )
# # Should give:
# #group stat    val
# #1 A     min       1
# #2 A     mean      2
# #3 A     max       3
# #4 B     min       4
# #5 B     mean      5
# #6 B     max       6
#  expect_equal(nrow(result), 6)
#})

test_that("summarise with n() alternative using fn()", {
  df <- data.frame(
    group = c("A", "A", "B", "B"),
    value = 1:4
  )
  df_grouped <- group_by_(df, ~group)

  result <- summarise_(df_grouped,
    n_obs    = ~length(value),
    mean_val = ~mean(value)
  )

  expect_equal(result$n_obs, c(2, 2))
})

test_that("count with computed grouping", {
  df <- data.frame(x = 1:10)

  result <- count_(df, category = ~x %% 3)

  expect_equal(nrow(result), 3)
  expect_true("category" %in% names(result))
})

test_that("add_count and filter combination", {
  df <- data.frame(
    group = c("A", "A", "B", "B", "C"),
    value = 1:5
  )

  result <- df |>
    add_count_(~group) |>
    filter_(~n > 1)

  expect_equal(nrow(result), 4)
  expect_true(all(result$group %in% c("A", "B")))
})

# Cannot do that yet!
#test_that("summarise with grouped and ungrouped operations", {
#  df <- data.frame(
#    group = c("A", "A", "B", "B"),
#    value = c(1, 2, 3, 4)
#  )
#  df_grouped <- group_by_(df, ~group)#
#
#  result <- summarise_(df_grouped,
#    group_mean  = ~mean(value),
#    global_mean = ~mean(df$value),
#    diff        = ~mean(value) - mean(df$value)
#  )
#  # Should give
# #group group_mean global_mean  diff
# #1 A            1.5         2.5    -1
# #2 B            3.5         2.5     1
# # ... but probably can do that with a mix of fmean() and mean() !
#
#  expect_equal(nrow(result), 2)
#  expect_equal(result$global_mean, c(2.5, 2.5))
#})

test_that("count with multiple weights", {
  df <- data.frame(
    group = c("A", "B", "A", "B"),
    weight1 = c(1, 2, 3, 4),
    weight2 = c(10, 20, 30, 40)
  )

  result1 <- count_(df, ~group, wt = ~weight1)
  result2 <- count_(df, ~group, wt = ~weight2)

  expect_equal(result1$n, c(4, 6))
  expect_equal(result2$n, c(40, 60))
})

test_that("grouped summarise with drop_last", {
  df <- data.frame(
    g1 = c("A", "A", "B", "B"),
    g2 = c("X", "Y", "X", "Y"),
    val = 1:4
  )
  df_grouped <- group_by_(df, ~g1, ~g2)

  result <- summarise_(df_grouped, sum_val = ~sum(val), .groups = "drop_last")

  expect_true(is.grouped_df(result))
})

test_that("add_count preserves row order", {
  df <- data.frame(
    x = c(3, 1, 2, 3, 1),
    y = c("a", "b", "c", "a", "b")
  )

  result <- add_count_(df, ~x)

  expect_equal(result$y, df$y)
  expect_equal(result$x, df$x)
})
