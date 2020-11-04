
test_that("load test data DBS.rda works", {
  load("testdata/DBS.rda")

  expect_equal(nrow(DBS), 11)
  expect_equal(ncol(DBS), 12)
  expect_true(is(DBS, "SummarizedExperiment"))
})
