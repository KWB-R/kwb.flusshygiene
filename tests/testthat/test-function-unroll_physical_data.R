#
# This test file has been generated by kwb.test::create_test_files()
# launched by user micha on 2019-09-10 18:15:05.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("unroll_physical_data() works", {

  expect_error(
    kwb.flusshygiene:::unroll_physical_data()
    # argument "physical_data" is missing, with no default
  )

})

