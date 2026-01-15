test_that("list_sciviews_functions() returns a list a function names", {
  result <- list_sciviews_functions()

  expect_true(length(result) > 0)
  expect_type(result, "character")
  expect_true(all(grepl("^[a-zA-Z0-9_]+$", result)))
})
