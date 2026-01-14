# Tests for pivoting functions - Part 1: pivot_longer_() basics

test_that("pivot_longer_() works with basic wide to long conversion", {
  wide_data <- data.frame(
    id = 1:3,
    q1 = c(100, 110, 120),
    q2 = c(105, 115, 125),
    q3 = c(110, 120, 130)
  )

  result <- pivot_longer_(wide_data, ~q1:q3, names_to = "quarter", values_to = "sales")

  expect_equal(nrow(result), 9)
  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("id", "quarter", "sales"))
  expect_equal(result$quarter, rep(c("q1", "q2", "q3"), 3))
})

test_that("pivot_longer_() works with character column specification", {
  wide_data <- data.frame(
    id = 1:2,
    x = c(1, 2),
    y = c(3, 4),
    z = c(5, 6)
  )

  result <- pivot_longer_(wide_data, c("x", "y", "z"), names_to = "var", values_to = "val")

  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 3)
  expect_true("var" %in% names(result))
  expect_true("val" %in% names(result))
})

test_that("pivot_longer_() preserves data frame type", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("data.table")

  # Test with data.frame
  df <- data.frame(id = 1:2, a = c(1, 2), b = c(3, 4))
  result_df <- pivot_longer_(df, ~a:b, names_to = "var", values_to = "val")
  expect_s3_class(result_df, "data.frame")

  # Test with tibble
  tbl <- tibble::tibble(id = 1:2, a = c(1, 2), b = c(3, 4))
  result_tbl <- pivot_longer_(tbl, ~a:b, names_to = "var", values_to = "val")
  expect_s3_class(result_tbl, "tbl_df")

  # Test with data.table
  dt <- data.table::data.table(id = 1:2, a = c(1, 2), b = c(3, 4))
  result_dt <- pivot_longer_(dt, ~a:b, names_to = "var", values_to = "val")
  expect_s3_class(result_dt, "data.table")
})

test_that("pivot_longer_() handles tidy-select helpers", {
  wide_data <- data.frame(
    id = 1:2,
    q1 = c(1, 2),
    q2 = c(3, 4),
    q3 = c(5, 6),
    other = c(7, 8)
  )

  result <- pivot_longer_(wide_data, ~starts_with("q"), names_to = "quarter",
    values_to = "sales")

  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 4)
  expect_equal(unique(result$quarter), c("q1", "q2", "q3"))
  expect_true("other" %in% names(result))
})

test_that("pivot_longer_() removes names_prefix", {
  wide_data <- data.frame(
    id = 1:2,
    q1 = c(1, 2),
    q2 = c(3, 4),
    q3 = c(5, 6)
  )

  result <- pivot_longer_(
    wide_data,
    ~q1:q3,
    names_to = "quarter",
    values_to = "sales",
    names_prefix = "q"
  )

  expect_equal(unique(result$quarter), c("1", "2", "3"))
})

test_that("pivot_longer_() handles cols_vary parameter", {
  wide_data <- data.frame(
    id = 1:3,
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )

  result_fastest <- pivot_longer_(
    wide_data,
    ~x:y,
    cols_vary = "fastest",
    names_to = "var",
    values_to = "val"
  )

  result_slowest <- pivot_longer_(
    wide_data,
    ~x:y,
    cols_vary = "slowest",
    names_to = "var",
    values_to = "val"
  )

  expect_equal(nrow(result_fastest), 6)
  expect_equal(nrow(result_slowest), 6)
  expect_equal(ncol(result_fastest), 3)
  expect_equal(ncol(result_slowest), 3)
})

test_that("pivot_longer_() drops NA values when values_drop_na = TRUE", {
  wide_data <- data.frame(
    id = 1:3,
    x = c(1, NA, 3),
    y = c(4, 5, NA)
  )

  result <- pivot_longer_(
    wide_data,
    ~x:y,
    names_to = "var",
    values_to = "val",
    values_drop_na = TRUE
  )

  expect_false(any(is.na(result$val)))
  expect_equal(nrow(result), 4)
})

test_that("pivot_longer_() keeps NA values when values_drop_na = FALSE", {
  wide_data <- data.frame(
    id = 1:3,
    x = c(1, NA, 3),
    y = c(4, 5, NA)
  )

  result <- pivot_longer_(
    wide_data,
    ~x:y,
    names_to = "var",
    values_to = "val",
    values_drop_na = FALSE
  )

  expect_true(any(is.na(result$val)))
  expect_equal(nrow(result), 6)
})

test_that("pivot_longer_() converts to factors when factor = TRUE", {
  wide_data <- data.frame(
    id = 1:2,
    x = c(1, 2),
    y = c(3, 4)
  )

  result <- pivot_longer_(
    wide_data,
    ~x:y,
    names_to = "var",
    values_to = "val",
    factor = TRUE
  )

  expect_s3_class(result$var, "factor")
  expect_type(result$val, "double")
})

test_that("pivot_longer_() works with all data types", {
  wide_data <- data.frame(
    id = 1:2,
    int_col = c(1L, 2L),
    dbl_col = c(1.5, 2.5),
    chr_col = c("a", "b"),
    lgl_col = c(TRUE, FALSE)
  )

  result <- pivot_longer_(
    wide_data,
    ~int_col:lgl_col,
    names_to = "variable",
    values_to = "value"
  )

  expect_equal(nrow(result), 8)
  expect_equal(ncol(result), 3)
})

test_that("pivot_longer_() preserves id column order", {
  wide_data <- data.frame(
    id = c(3, 1, 2),
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )

  result <- pivot_longer_(wide_data, ~x:y, names_to = "var", values_to = "val")

  expect_equal(result$id, c(3, 3, 1, 1, 2, 2))
})

test_that("pivot_longer_() works with single column to pivot", {
  wide_data <- data.frame(
    id = 1:3,
    x = c(1, 2, 3)
  )

  result <- pivot_longer_(wide_data, ~x, names_to = "var", values_to = "val")

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)
})

test_that("pivot_longer_() works with zero rows", {
  wide_data <- data.frame(
    id = integer(),
    x = integer(),
    y = integer()
  )

  result <- pivot_longer_(wide_data, ~x:y, names_to = "var", values_to = "val")

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("id", "var", "val"))
})

test_that("pivot_longer_() with multiple id columns", {
  wide_data <- data.frame(
    id1 = c(1, 1, 2),
    id2 = c("a", "b", "a"),
    x = c(10, 20, 30),
    y = c(40, 50, 60)
  )

  result <- pivot_longer_(wide_data, ~x:y, names_to = "var", values_to = "val")

  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 4)
  expect_true("id1" %in% names(result))
  expect_true("id2" %in% names(result))
})

test_that("pivot_longer_() with all columns to pivot", {
  wide_data <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    z = c(5, 6)
  )

  result <- pivot_longer_(wide_data, ~x:z, names_to = "var", values_to = "val")

  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 2)
})

test_that("pivot_longer_() with character names vector", {
  wide_data <- data.frame(
    id = 1:2,
    x = c(1, 2),
    y = c(3, 4),
    z = c(5, 6)
  )

  result <- pivot_longer_(wide_data, c("x", "y", "z"), names_to = "var", values_to = "val")

  expect_equal(nrow(result), 6)
  expect_equal(unique(result$var), c("x", "y", "z"))
})

test_that("pivot_longer_() with numeric column positions", {
  wide_data <- data.frame(
    id = 1:2,
    x = c(1, 2),
    y = c(3, 4),
    z = c(5, 6)
  )

  result <- pivot_longer_(wide_data, 2:4, names_to = "var", values_to = "val")

  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 3)
})

test_that("pivot_longer_() preserves NA in id columns", {
  wide_data <- data.frame(
    id = c(1, NA, 3),
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  )

  result <- pivot_longer_(wide_data, ~x:y, names_to = "var", values_to = "val")

  expect_true(any(is.na(result$id)))
  expect_equal(nrow(result), 6)
})

# Tests for pivoting functions - Part 2: pivot_wider_() basics

test_that("pivot_wider_() works with basic long to wide conversion", {
  long_data <- data.frame(
    id = rep(1:3, each = 3),
    quarter = rep(c("q1", "q2", "q3"), 3),
    sales = c(100, 105, 110, 110, 115, 120, 120, 125, 130)
  )

  result <- pivot_wider_(long_data, names_from = "quarter", values_from = "sales")

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)
  expect_true("q1" %in% names(result))
  expect_true("q2" %in% names(result))
  expect_true("q3" %in% names(result))
})

test_that("pivot_wider_() with character column names", {
  long_data <- data.frame(
    id = c(1, 1, 2, 2),
    var = c("x", "y", "x", "y"),
    val = c(1, 2, 3, 4)
  )

  result <- pivot_wider_(long_data, names_from = "var", values_from = "val")

  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
  expect_equal(result$x, c(1, 3))
  expect_equal(result$y, c(2, 4))
})

test_that("pivot_wider_() preserves data frame type", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("data.table")

  long_data <- data.frame(
    id = c(1, 1, 2, 2),
    var = c("x", "y", "x", "y"),
    val = c(1, 2, 3, 4)
  )

  # Test with data.frame
  result_df <- pivot_wider_(long_data, names_from = "var", values_from = "val")
  expect_s3_class(result_df, "data.frame")

  # Test with tibble
  tbl <- tibble::as_tibble(long_data)
  result_tbl <- pivot_wider_(tbl, names_from = "var", values_from = "val")
  expect_s3_class(result_tbl, "tbl_df")

  # Test with data.table
  dt <- data.table::as.data.table(long_data)
  result_dt <- pivot_wider_(dt, names_from = "var", values_from = "val")
  expect_s3_class(result_dt, "data.table")
})

test_that("pivot_wider_() with values_fill parameter", {
  long_data <- data.frame(
    id = c(1, 1, 2),
    var = c("x", "y", "x"),
    val = c(1, 2, 3)
  )

  result <- pivot_wider_(
    long_data,
    names_from = "var",
    values_from = "val",
    values_fill = 0
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$y[2], 0)
})

test_that("pivot_wider_() with values_fn aggregation", {
  long_data <- data.frame(
    id = c(1, 1, 1, 2, 2),
    var = c("x", "x", "y", "x", "y"),
    val = c(1, 2, 3, 4, 5)
  )

  result <- pivot_wider_(
    long_data,
    names_from = "var",
    values_from = "val",
    values_fn = "sum"
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$x[1], 3)
  expect_equal(result$x[2], 4)
})

test_that("pivot_wider_() with values_fn = 'last'", {
  long_data <- data.frame(
    id = c(1, 1, 1),
    var = c("x", "x", "y"),
    val = c(1, 2, 3)
  )

  result <- pivot_wider_(
    long_data,
    names_from = "var",
    values_from = "val",
    values_fn = "last"
  )

  expect_equal(result$x[1], 2)
})

test_that("pivot_wider_() with values_fn = 'first'", {
  long_data <- data.frame(
    id = c(1, 1, 1),
    var = c("x", "x", "y"),
    val = c(1, 2, 3)
  )

  result <- pivot_wider_(
    long_data,
    names_from = "var",
    values_from = "val",
    values_fn = "first"
  )

  expect_equal(result$x[1], 1)
})

test_that("pivot_wider_() with values_fn as formula", {
  long_data <- data.frame(
    id = c(1, 1, 1, 2, 2),
    var = c("x", "x", "y", "x", "y"),
    val = c(1, 2, 3, 4, 5)
  )

  result <- pivot_wider_(
    long_data,
    names_from = "var",
    values_from = "val",
    values_fn = ~mean(.x)
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$x[1], 1.5)
})

test_that("pivot_wider_() with explicit id_cols", {
  long_data <- data.frame(
    id1 = c(1, 1, 2, 2),
    id2 = c("a", "a", "b", "b"),
    var = c("x", "y", "x", "y"),
    val = c(1, 2, 3, 4)
  )

  result <- pivot_wider_(
    long_data,
    id_cols = c("id1", "id2"),
    names_from = "var",
    values_from = "val"
  )

  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 4)
})

test_that("pivot_wider_() with NULL id_cols", {
  long_data <- data.frame(
    id = c(1, 1, 2, 2),
    var = c("x", "y", "x", "y"),
    val = c(1, 2, 3, 4)
  )

  result <- pivot_wider_(
    long_data,
    id_cols = NULL,
    names_from = "var",
    values_from = "val"
  )

  expect_equal(nrow(result), 2)
})

test_that("pivot_wider_() with multiple values_from", {
  long_data <- data.frame(
    id = c(1, 1, 2, 2),
    var = c("x", "y", "x", "y"),
    sales = c(100, 200, 300, 400),
    cost = c(50, 100, 150, 200)
  )

  result <- pivot_wider_(
    long_data,
    names_from = "var",
    values_from = c("sales", "cost")
  )

  expect_equal(nrow(result), 2)
  expect_true("sales_x" %in% names(result) || "x_sales" %in% names(result))
})

# Not yet!
#test_that("pivot_wider_() with names_prefix", {
#  long_data <- data.frame(
#    id = c(1, 1, 2, 2),
#    var = c("x", "y", "x", "y"),
#    val = c(1, 2, 3, 4)
#  )
#
#  result <- pivot_wider_(
#    long_data,
#    names_from = "var",
#    values_from = "val",
#    names_prefix = "value_"
#  )
#
#  expect_true("value_x" %in% names(result))
#  expect_true("value_y" %in% names(result))
#})

test_that("pivot_wider_() with sort parameter", {
  long_data <- data.frame(
    id = c(3, 1, 2, 3, 1, 2),
    var = c("x", "x", "x", "y", "y", "y"),
    val = c(1, 2, 3, 4, 5, 6)
  )

  result_sorted <- pivot_wider_(
    long_data,
    names_from = "var",
    values_from = "val",
    sort = TRUE
  )

  result_unsorted <- pivot_wider_(
    long_data,
    names_from = "var",
    values_from = "val",
    sort = FALSE
  )

  expect_equal(nrow(result_sorted), 3)
  expect_equal(nrow(result_unsorted), 3)
})

# Not yet! Should return a df with 0x1 columns (id as integer and no rows)
#test_that("pivot_wider_() with zero rows", {
#  long_data <- data.frame(
#    id = integer(),
#    var = character(),
#    val = integer()
#  )
#
#  result <- pivot_wider_(long_data, names_from = "var", values_from = "val")
#
#  expect_equal(nrow(result), 0)
#})

test_that("pivot_wider_() with single row", {
  long_data <- data.frame(
    id = 1,
    var = c("x", "y", "z"),
    val = c(1, 2, 3)
  )

  result <- pivot_wider_(long_data, names_from = "var", values_from = "val")

  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 4)
})

test_that("pivot_wider_() with NA in names_from", {
  long_data <- data.frame(
    id = c(1, 1, 2),
    var = c("x", NA, "x"),
    val = c(1, 2, 3)
  )

  result <- pivot_wider_(long_data, names_from = "var", values_from = "val")

  expect_equal(nrow(result), 2)
  expect_true(is.na(names(result)[3]) || "NA" %in% colnames(result))
})

test_that("pivot_wider_() with NA in values_from", {
  long_data <- data.frame(
    id = c(1, 1, 2),
    var = c("x", "y", "x"),
    val = c(1, NA, 3)
  )

  result <- pivot_wider_(long_data, names_from = "var", values_from = "val")

  expect_equal(nrow(result), 2)
  expect_true(is.na(result$y[1]))
})

test_that("pivot_wider_() with factor columns", {
  long_data <- data.frame(
    id = c(1, 1, 2, 2),
    var = factor(c("x", "y", "x", "y")),
    val = c(1, 2, 3, 4)
  )

  result <- pivot_wider_(long_data, names_from = "var", values_from = "val")

  expect_equal(nrow(result), 2)
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
})

test_that("pivot_wider_() with date columns", {
  long_data <- data.frame(
    id = c(1, 1, 2, 2),
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-01", "2020-01-02")),
    var = c("x", "x", "y", "y"),
    val = c(1, 2, 3, 4)
  )

  result <- pivot_wider_(long_data, names_from = "var", values_from = "val")

  expect_equal(nrow(result), 4)
})

test_that("pivot_wider_() preserves column order", {
  long_data <- data.frame(
    id = c(1, 1, 2, 2),
    var = c("x", "y", "x", "y"),
    val = c(1, 2, 3, 4)
  )

  result <- pivot_wider_(long_data, names_from = "var", values_from = "val")

  expect_equal(names(result)[1], "id")
})

# Tests for pivoting functions - Part 3: Advanced scenarios and round-trips

test_that("pivot_longer_() and pivot_wider_() roundtrip", {
  original <- data.frame(
    id = 1:3,
    x = c(1, 2, 3),
    y = c(4, 5, 6),
    z = c(7, 8, 9)
  )

  result <- original |>
    pivot_longer_(~x:z, names_to = "var", values_to = "val") |>
    pivot_wider_(names_from = "var", values_from = "val")

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)
  expect_equal(result$x, original$x)
  expect_equal(result$y, original$y)
  expect_equal(result$z, original$z)
})

test_that("pivot_longer_() with multiple id columns roundtrip", {
  original <- data.frame(
    id1 = c(1, 1, 2),
    id2 = c("a", "b", "a"),
    x = c(10, 20, 30),
    y = c(40, 50, 60)
  )

  result <- original |>
    pivot_longer_(~x:y, names_to = "var", values_to = "val") |>
    pivot_wider_(names_from = "var", values_from = "val")

  expect_equal(nrow(result), 3)
  expect_equal(result$x, original$x)
  expect_equal(result$y, original$y)
})

test_that("pivot_longer_() with multiple names_from and values_from", {
  long_data <- data.frame(
    id = rep(1:2, each = 4),
    metric = rep(c("sales", "profit"), 4),
    quarter = rep(c("q1", "q2"), each = 2, times = 2),
    value = c(100, 20, 105, 22, 110, 24, 115, 26)
  )

  result <- pivot_wider_(
    long_data,
    names_from = c("quarter", "metric"),
    values_from = "value"
  )

  expect_equal(nrow(result), 2)
  expect_true(ncol(result) > 2)
})

test_that("pivot_wider_() with id_expand parameter", {
  long_data <- data.frame(
    id = c(1, 1, 2),
    var = c("x", "y", "x"),
    val = c(1, 2, 3)
  )

  # With id_expand = TRUE, should create all combinations
  result_expand <- pivot_wider_(
    long_data,
    names_from = "var",
    values_from = "val",
    id_expand = TRUE
  )

  expect_equal(nrow(result_expand), 2)
})

test_that("pivot_longer_() with contains() helper", {
  wide_data <- data.frame(
    id = 1:2,
    total_q1 = c(1, 2),
    total_q2 = c(3, 4),
    total_q3 = c(5, 6),
    other = c(7, 8)
  )

  result <- pivot_longer_(
    wide_data,
    ~contains("total"),
    names_to = "quarter",
    values_to = "sales"
  )

  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 4)
  expect_true("other" %in% names(result))
})

test_that("pivot_longer_() with ends_with() helper", {
  wide_data <- data.frame(
    id = 1:2,
    sales_2020 = c(1, 2),
    sales_2021 = c(3, 4),
    profit_2020 = c(5, 6),
    profit_2021 = c(7, 8)
  )

  result <- pivot_longer_(
    wide_data,
    ~ends_with("2020"),
    names_to = "metric",
    values_to = "amount",
    names_prefix = "_"
  )

  expect_equal(nrow(result), 4)
  expect_true("sales_2021" %in% names(result))
})

# Not yet!
#test_that("pivot_longer_() with all nested levels", {
#  wide_data <- data.frame(
#    id = 1:2,
#    a_x = c(1, 2),
#    a_y = c(3, 4),
#    b_x = c(5, 6),
#    b_y = c(7, 8)
#  )
#
#  result <- pivot_longer_(
#    wide_data,
#    ~a_x:b_y,
#    names_to = c("group", "var"),
#    names_sep = "_",
#    values_to = "val"
#  )
#
#  expect_equal(nrow(result), 8)
#  expect_equal(ncol(result), 4)
#  expect_true("group" %in% names(result))
#  expect_true("var" %in% names(result))
#})

test_that("pivot_wider_() with multiple columns and fill", {
  long_data <- data.frame(
    id = c(1, 1, 1, 2, 2),
    var = c("x", "y", "z", "x", "z"),
    val = c(1, 2, 3, 4, 5)
  )

  result <- pivot_wider_(
    long_data,
    names_from = "var",
    values_from = "val",
    values_fill = 0
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$y[2], 0)
})

test_that("pivot_longer_() with where() selector", {
  wide_data <- data.frame(
    id = 1:3,
    val1 = c(1, 2, 3),
    val2 = c(4, 5, 6),
    val3 = c(7, 8, 9),
    text = c("a", "b", "c")
  )

  result <- pivot_longer_(
    wide_data,
    ~where(is.numeric) & id,
    names_to = "variable",
    values_to = "value"
  )

  expect_equal(nrow(result), 3)
  expect_true("text" %in% names(result))
})

test_that("pivot_wider_() preserves row and column order with sort = FALSE", {
  long_data <- data.frame(
    id = c(3, 2, 1, 3, 2, 1),
    var = c("x", "x", "x", "y", "y", "y"),
    val = c(1, 2, 3, 4, 5, 6)
  )

  result <- pivot_wider_(
    long_data,
    names_from = "var",
    values_from = "val",
    sort = FALSE
  )

  expect_equal(result$id, c(3, 2, 1))
})

test_that("pivot_longer_() with multiple id columns and -pattern", {
  wide_data <- data.frame(
    id1 = 1:2,
    id2 = c("a", "b"),
    x = c(1, 2),
    y = c(3, 4),
    z = c(5, 6)
  )

  result <- pivot_longer_(
    wide_data,
    ~-c(id1, id2),
    names_to = "var",
    values_to = "val"
  )

  expect_equal(nrow(result), 6)
  expect_true("id1" %in% names(result))
  expect_true("id2" %in% names(result))
})

test_that("pivot_wider_() with character-to-factor conversion", {
  long_data <- data.frame(
    id = c(1, 1, 2, 2),
    var = c("x", "y", "x", "y"),
    val = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )

  result <- pivot_wider_(
    long_data,
    names_from = "var",
    values_from = "val"
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$x, c("a", "c"))
  expect_equal(result$y, c("b", "d"))
})

test_that("pivot_longer_() with large data frame", {
  wide_data <- data.frame(
    id = 1:1000,
    x = rnorm(1000),
    y = rnorm(1000),
    z = rnorm(1000)
  )

  result <- pivot_longer_(wide_data, ~x:z, names_to = "var", values_to = "val")

  expect_equal(nrow(result), 3000)
  expect_equal(ncol(result), 3)
})

test_that("pivot_wider_() with many groups", {
  long_data <- data.frame(
    id = rep(1:100, each = 5),
    var = rep(c("a", "b", "c", "d", "e"), 100),
    val = rnorm(500)
  )

  result <- pivot_wider_(long_data, names_from = "var", values_from = "val")

  expect_equal(nrow(result), 100)
  expect_equal(ncol(result), 6)
})

test_that("pivot_longer_() with integer and numeric mixing", {
  wide_data <- data.frame(
    id = 1:2,
    int_col = c(1L, 2L),
    dbl_col = c(1.5, 2.5)
  )

  result <- pivot_longer_(
    wide_data,
    ~int_col:dbl_col,
    names_to = "var",
    values_to = "val"
  )

  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 3)
})

test_that("pivot_wider_() error with duplicate keys", {
  long_data <- data.frame(
    id = c(1, 1, 1, 1),
    var = c("x", "x", "y", "y"),
    val = c(1, 2, 3, 4)
  )

  # Should handle multiple values per cell
  result <- pivot_wider_(
    long_data,
    names_from = "var",
    values_from = "val",
    values_fn = "last"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$x, 2)
  expect_equal(result$y, 4)
})

test_that("pivot_longer_() with repeated column names", {
  wide_data <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    z = c(5, 6)
  )
  names(wide_data) <- c("value", "value", "id")

  # Should handle (though may create issues)
  expect_no_error({
    result <- pivot_longer_(
      wide_data,
      1:2,
      names_to = "var",
      values_to = "val"
    )
  })
})

test_that("pivot_wider_() with all NA values in column", {
  long_data <- data.frame(
    id = c(1, 1, 2, 2),
    var = c("x", "y", "x", "y"),
    val = c(NA, NA, NA, NA)
  )

  result <- pivot_wider_(long_data, names_from = "var", values_from = "val")

  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result$x)))
  expect_true(all(is.na(result$y)))
})

test_that("pivot_longer_() with tidy-select combinations", {
  wide_data <- data.frame(
    id = 1:2,
    q1_sales = c(1, 2),
    q2_sales = c(3, 4),
    q1_profit = c(5, 6),
    q2_profit = c(7, 8),
    other = c(9, 10)
  )

  result <- pivot_longer_(
    wide_data,
    ~contains("q") & contains("sales"),
    names_to = "period",
    values_to = "amount"
  )

  expect_equal(nrow(result), 4)
  expect_true("other" %in% names(result))
  expect_true("q1_profit" %in% names(result))
})

test_that("pivot_wider_() with values_fn as mean", {
  long_data <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    var = c("x", "x", "y", "x", "x", "y"),
    val = c(1, 3, 5, 2, 4, 6)
  )

  result <- pivot_wider_(
    long_data,
    names_from = "var",
    values_from = "val",
    values_fn = "mean"
  )

  expect_equal(nrow(result), 2)
  expect_equal(result$x[1], 2)
  expect_equal(result$x[2], 3)
})
