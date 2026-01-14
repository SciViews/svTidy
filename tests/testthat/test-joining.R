# Tests for joining functions - Part 1

test_that("left_join_() works with basic data frames", {
  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = c(1, 2, 4), score = c(85, 90, 95))

  result <- left_join_(df1, df2, by = "id")

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)
  expect_equal(names(result), c("id", "name", "score"))
  expect_equal(result$score, c(85, 90, NA))
})

test_that("right_join_() works with basic data frames", {
  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = c(1, 2, 4), score = c(85, 90, 95))

  result <- right_join_(df1, df2, by = "id")

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)
  expect_true(4 %in% result$id)
  expect_equal(result$score, c(85, 90, 95))
})

test_that("inner_join_() keeps only matching rows", {
  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = c(1, 2, 4), score = c(85, 90, 95))

  result <- inner_join_(df1, df2, by = "id")

  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
  expect_equal(result$id, c(1, 2))
  expect_equal(result$name, c("Alice", "Bob"))
})

test_that("full_join_() keeps all rows from both data frames", {
  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = c(1, 2, 4), score = c(85, 90, 95))

  result <- full_join_(df1, df2, by = "id")

  expect_equal(nrow(result), 4)
  expect_equal(ncol(result), 3)
  expect_true(all(c(1, 2, 3, 4) %in% result$id))
  expect_true(is.na(result$score[3]))
})

test_that("semi_join_() filters x by y without adding columns", {
  df1 <- data.frame(id = 1:5, name = c("Alice", "Bob", "Charlie", "David", "Eve"))
  df2 <- data.frame(id = c(1, 2, 4), score = c(85, 90, 95))

  result <- semi_join_(df1, df2, by = "id")

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("id", "name"))
  expect_equal(result$id, c(1, 2, 4))
})

test_that("anti_join_() filters x to rows not in y", {
  df1 <- data.frame(id = 1:5, name = c("Alice", "Bob", "Charlie", "David", "Eve"))
  df2 <- data.frame(id = c(1, 2, 4), score = c(85, 90, 95))

  result <- anti_join_(df1, df2, by = "id")

  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 2)
  expect_equal(result$id, c(3, 5))
  expect_equal(result$name, c("Charlie", "Eve"))
})

test_that("joining by character names works", {
  df1 <- data.frame(person_id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = c(1, 2, 4), score = c(85, 90, 95))

  result <- left_join_(df1, df2, by = c("person_id" = "id"))

  expect_equal(nrow(result), 3)
  expect_equal(names(result), c("person_id", "name", "score"))
})

test_that("joining by multiple columns works", {
  df1 <- data.frame(
    year = c(2020, 2020, 2021),
    month = c(1, 2, 1),
    value = c(10, 20, 30)
  )
  df2 <- data.frame(
    year = c(2020, 2020, 2021),
    month = c(1, 2, 1),
    target = c(15, 25, 35)
  )

  result <- left_join_(df1, df2, by = c("year", "month"))

  expect_equal(nrow(result), 3)
  expect_equal(result$target, c(15, 25, 35))
})

test_that("suffix parameter handles duplicate column names", {
  df1 <- data.frame(id = 1:3, value = c(10, 20, 30))
  df2 <- data.frame(id = c(1, 2, 4), value = c(100, 200, 400))

  result <- left_join_(df1, df2, by = "id", suffix = c("_x", "_y"))

  expect_equal(ncol(result), 3)
  expect_true("value_x" %in% names(result))
  expect_true("value_y" %in% names(result))
  expect_equal(result$value_x, c(10, 20, 30))
  expect_equal(result$value_y, c(100, 200, NA))
})

test_that("join preserves data frame types", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("data.table")

  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = c(1, 2, 4), score = c(85, 90, 95))

  # Test with tibble
  tbl1 <- tibble::tibble(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  tbl2 <- tibble::tibble(id = c(1, 2, 4), score = c(85, 90, 95))
  result_tbl <- left_join_(tbl1, tbl2, by = "id")
  expect_s3_class(result_tbl, "tbl_df")

  # Test with data.table
  dt1 <- data.table::data.table(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  dt2 <- data.table::data.table(id = c(1, 2, 4), score = c(85, 90, 95))
  result_dt <- left_join_(dt1, dt2, by = "id")
  expect_s3_class(result_dt, "data.table")
})

# Not yet!
#test_that("join works with formula syntax for by parameter", {
#  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
#  df2 <- data.frame(id = c(1, 2, 4), score = c(85, 90, 95))
#
#  result <- left_join_(df1, df2, by = ~id)
#
#  expect_equal(nrow(result), 3)
#  expect_equal(names(result), c("id", "name", "score"))
#})

test_that("join with NULL by uses all common columns", {
  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"), dept = c("A", "B", "A"))
  df2 <- data.frame(id = c(1, 2, 4), dept = c("A", "B", "C"), score = c(85, 90, 95))

  result <- left_join_(df1, df2, by = NULL)

  expect_equal(nrow(result), 3)
  expect_equal(result$score[1], 85)
  expect_equal(result$score[3], NA_real_)
})

test_that("join works with empty data frames", {
  df1 <- data.frame(id = integer(), name = character())
  df2 <- data.frame(id = 1:3, score = c(85, 90, 95))

  result <- left_join_(df1, df2, by = "id")

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 3)
})

test_that("join preserves row order from x in left_join", {
  df1 <- data.frame(id = c(3, 1, 2), name = c("Charlie", "Alice", "Bob"))
  df2 <- data.frame(id = c(1, 2, 3), score = c(85, 90, 95))

  result <- left_join_(df1, df2, by = "id")

  expect_equal(result$id, c(3, 1, 2))
  expect_equal(result$score, c(95, 85, 90))
})

test_that("join with character column types", {
  df1 <- data.frame(key = c("a", "b", "c"), val1 = 1:3, stringsAsFactors = FALSE)
  df2 <- data.frame(key = c("a", "b", "d"), val2 = c(10, 20, 40), stringsAsFactors = FALSE)

  result <- left_join_(df1, df2, by = "key")

  expect_equal(nrow(result), 3)
  expect_equal(result$val2, c(10, 20, NA))
})

test_that("join with factor columns", {
  df1 <- data.frame(grp = factor(c("X", "Y", "Z")), val = 1:3)
  df2 <- data.frame(grp = factor(c("X", "Y")), score = c(100, 200))

  result <- left_join_(df1, df2, by = "grp")

  expect_equal(nrow(result), 3)
  expect_equal(result$score, c(100, 200, NA))
})

test_that("join with date columns", {
  df1 <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    val = 1:3
  )
  df2 <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-02")),
    score = c(85, 90)
  )

  result <- left_join_(df1, df2, by = "date")

  expect_equal(nrow(result), 3)
  expect_equal(result$score, c(85, 90, NA))
})

# Tests for joining functions - Part 2: Advanced scenarios

test_that("join with multiple matches (one-to-many)", {
  df1 <- data.frame(id = c(1, 2), name = c("Alice", "Bob"))
  df2 <- data.frame(id = c(1, 1, 2), score = c(85, 88, 90))

  result <- left_join_(df1, df2, by = "id", multiple = "all")

  expect_equal(nrow(result), 3)
  expect_equal(result$id, c(1, 1, 2))
  expect_equal(result$name, c("Alice", "Alice", "Bob"))
})

test_that("join with multiple parameter - first", {
  df1 <- data.frame(id = c(1, 2), name = c("Alice", "Bob"))
  df2 <- data.frame(id = c(1, 1, 2, 2), score = c(85, 88, 90, 92))

  result <- left_join_(df1, df2, by = "id", multiple = "first")

  expect_equal(nrow(result), 2)
  expect_equal(result$score, c(85, 90))
})

test_that("join with NA values in key", {
  df1 <- data.frame(id = c(1, 2, NA), name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = c(1, 2), score = c(85, 90))

  result <- left_join_(df1, df2, by = "id")

  expect_equal(nrow(result), 3)
  expect_true(is.na(result$score[3]))
})

test_that("join with sort parameter", {
  df1 <- data.frame(id = c(3, 1, 2), name = c("Charlie", "Alice", "Bob"))
  df2 <- data.frame(id = c(1, 2, 3), score = c(85, 90, 95))

  result <- left_join_(df1, df2, by = "id", sort = TRUE)

  expect_equal(result$id, c(1, 2, 3))
})

test_that("join with copy parameter", {
  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = c(1, 2, 4), score = c(85, 90, 95))

  result <- left_join_(df1, df2, by = "id", copy = TRUE)

  expect_equal(nrow(result), 3)
  expect_equal(result$score, c(85, 90, NA))
})

test_that("join() generic function with how parameter", {
  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = c(1, 2, 4), score = c(85, 90, 95))

  result_inner <- join_(df1, df2, by = "id", how = "inner")
  result_left <- join_(df1, df2, by = "id", how = "left")
  result_right <- join_(df1, df2, by = "id", how = "right")
  result_full <- join_(df1, df2, by = "id", how = "full")
  result_semi <- join_(df1, df2, by = "id", how = "semi")
  result_anti <- join_(df1, df2, by = "id", how = "anti")

  expect_equal(nrow(result_inner), 2)
  expect_equal(nrow(result_left), 3)
  expect_equal(nrow(result_right), 3)
  expect_equal(nrow(result_full), 4)
  expect_equal(nrow(result_semi), 2)
  expect_equal(nrow(result_anti), 1)
})

test_that("join with unmatched parameter", {
  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = c(1, 2, 4), score = c(85, 90, 75))

  result <- inner_join_(df1, df2, by = "id", unmatched = "drop")
  expect_equal(nrow(result), 2)

  expect_error(inner_join_(df1, df2, by = "id", unmatched = "error"),
    "Matched")

  expect_warning(inner_join_(df1, df2, by = "id",
    unmatched = list(x = 1, y = 0.5, fail = "warning")),
    "Matched")
})

test_that("join with na_matches parameter", {
  df1 <- data.frame(id = c(1, 2, NA), name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = c(1, 2), score = c(85, 90))

  result_na <- left_join_(df1, df2, by = "id", na_matches = "na")
  result_never <- left_join_(df1, df2, by = "id", na_matches = "never")

  expect_equal(nrow(result_na), 3)
  expect_equal(nrow(result_never), 3)
})

test_that("join with keep parameter", {
  df1 <- data.frame(id = 1:2, name = c("Alice", "Bob"))
  df2 <- data.frame(id = c(1, 2), score = c(85, 90))

  result_keep_true <- left_join_(df1, df2, by = "id", keep = TRUE)
  result_keep_false <- left_join_(df1, df2, by = "id", keep = FALSE)

  expect_true("id.x" %in% names(result_keep_true))
  expect_true("id" %in% names(result_keep_false))
})

test_that("multiple joins in sequence", {
  df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(id = c(1, 2, 3), dept = c("Sales", "IT", "HR"))
  df3 <- data.frame(id = c(1, 2, 3), salary = c(50000, 60000, 55000))

  result <- df1 |>
    left_join_(df2, by = "id") |>
    left_join_(df3, by = "id")

  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)
  expect_true("dept" %in% names(result))
  expect_true("salary" %in% names(result))
})

test_that("join preserves column order", {
  df1 <- data.frame(a = 1:2, b = 3:4, c = 5:6)
  df2 <- data.frame(a = 1:2, d = 7:8, e = 9:10)

  result <- left_join_(df1, df2, by = "a")

  expect_equal(names(result)[1:3], c("a", "b", "c"))
  expect_equal(names(result)[4:5], c("d", "e"))
})

test_that("anti_join with multiple conditions", {
  df1 <- data.frame(year = c(2020, 2020, 2021), month = c(1, 2, 1), val = 1:3)
  df2 <- data.frame(year = c(2020, 2021), month = c(1, 1), blocked = TRUE)

  result <- anti_join_(df1, df2, by = c("year", "month"))

  expect_equal(nrow(result), 1)
  expect_equal(result$month[1], 2)
})

test_that("semi_join with multiple matches", {
  df1 <- data.frame(id = c(1, 1, 2, 3), name = c("A", "A", "B", "C"))
  df2 <- data.frame(id = c(1, 2, 4), approved = TRUE)

  result <- semi_join_(df1, df2, by = "id")

  expect_equal(nrow(result), 3)
  expect_equal(result$id, c(1, 1, 2))
})

test_that("join with numeric column types", {
  df1 <- data.frame(id = c(1L, 2L, 3L), name = c("A", "B", "C"))
  df2 <- data.frame(id = c(1, 2, 4), score = c(85, 90, 95))

  result <- left_join_(df1, df2, by = "id")

  expect_equal(nrow(result), 3)
  expect_equal(result$score, c(85, 90, NA))
})

test_that("join with logical columns", {
  df1 <- data.frame(flag = c(TRUE, FALSE, TRUE), val = 1:3)
  df2 <- data.frame(flag = c(TRUE, FALSE), score = c(100, 200))

  result <- left_join_(df1, df2, by = "flag")

  expect_equal(nrow(result), 3)
  expect_equal(result$score, c(100, 200, 100))
})

test_that("join with many columns", {
  df1 <- data.frame(
    id = 1:2,
    a = 3:4, b = 5:6, c = 7:8, d = 9:10,
    e = 11:12, f = 13:14, g = 15:16
  )
  df2 <- data.frame(
    id = 1:2,
    h = 17:18, i = 19:20, j = 21:22, k = 23:24
  )

  result <- left_join_(df1, df2, by = "id")

  expect_equal(ncol(result), 12)
  expect_equal(nrow(result), 2)
})

test_that("join preserves data.frame structure", {
  df1 <- data.frame(id = 1:3, name = c("A", "B", "C"))
  df2 <- data.frame(id = 1:3, score = c(85, 90, 95))

  result <- left_join_(df1, df2, by = "id")

  expect_s3_class(result, "data.frame")
})

test_that("join with similar column names", {
  df1 <- data.frame(person_id = 1:3, person_name = c("Alice", "Bob", "Charlie"))
  df2 <- data.frame(person_id = c(1, 2, 4), person_score = c(85, 90, 95))

  result <- left_join_(df1, df2, by = "person_id")

  expect_equal(ncol(result), 3)
  expect_true("person_name" %in% names(result))
  expect_true("person_score" %in% names(result))
})

test_that("full_join with all NA in one side", {
  df1 <- data.frame(id = c(NA, NA), name = c("A", "B"))
  df2 <- data.frame(id = c(1, 2), score = c(85, 90))

  result <- full_join_(df1, df2, by = "id")

  expect_true(nrow(result) >= 2)
})

test_that("join with order preservation across multiple calls", {
  df1 <- data.frame(id = c(3, 1, 2), val = 10:12)
  df2 <- data.frame(id = c(2, 1, 3), x = 100:102)
  df3 <- data.frame(id = c(1, 3, 2), y = 1000:1002)

  result <- df1 |>
    left_join_(df2, by = "id") |>
    left_join_(df3, by = "id")

  expect_equal(result$id, c(3, 1, 2))
})

test_that("inner_join returns intersection of keys", {
  df1 <- data.frame(id = 1:5, grp = letters[1:5])
  df2 <- data.frame(id = c(2, 3, 4, 5, 6), val = 20:24)

  result <- inner_join_(df1, df2, by = "id")

  expect_equal(nrow(result), 4)
  expect_true(all(result$id %in% c(2, 3, 4, 5)))
})

test_that("right_join preserves all rows from y", {
  df1 <- data.frame(id = 1:3, name = c("A", "B", "C"))
  df2 <- data.frame(id = c(2, 3, 4, 5), score = c(85, 90, 95, 100))

  result <- right_join_(df1, df2, by = "id")

  expect_equal(nrow(result), 4)
  expect_true(all(c(2, 3, 4, 5) %in% result$id))
  expect_true(is.na(result$name[result$id == 4]))
})

test_that("join with different row order in both data frames", {
  df1 <- data.frame(id = c(5, 3, 1, 4, 2), val = 10:14)
  df2 <- data.frame(id = c(2, 4, 1, 5, 3), x = 100:104)

  result <- left_join_(df1, df2, by = "id")

  expect_equal(nrow(result), 5)
  expect_equal(result$id, c(5, 3, 1, 4, 2))
})
