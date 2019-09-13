testthat::skip("get_paths() is expected to work only at KWB")

test_that("get_paths() works", {

  expect_error(kwb.flusshygiene:::get_paths())

})
