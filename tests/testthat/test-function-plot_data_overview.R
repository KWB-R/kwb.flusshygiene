#
# This test file has been generated by kwb.test::create_test_files()
# launched by user micha on 2019-09-10 18:15:03.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("plot_data_overview() works", {

  expect_error(
    kwb.flusshygiene:::plot_data_overview()
    # argument "riverdata" is missing, with no default
  )

})

