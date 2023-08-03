test_that("lib_summary returns expected results", {
  expect_silent(result <- lib_summary())
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("library", "n_packages"))
  expect_type(result$library, "character")
  expect_type(result$n_packages, "integer")
  expect_gt(sum(result$n_packages), 0)
  expect_setequal(result$library,.libPaths())
})

test_that("lib_summary fails appropriately", {
  expect_error(lib_summary("foo"), "not interpretable as logical")
})

test_that("sizes argument works",{
  result <- lib_summary(sizes = TRUE)
  expect_equal(names(result), c("library", "n_packages", "lib_size"))
  expect_type(result$lib_size, "double")
})

