context("Test unroll_vector2lagmatrix and unroll_lagdays")

test_that("Dimensions of lagmatrix are c(length(x), n)", {
  expect_equal(dim(unroll_vector2lagmatrix(1:10, 5)), c(10, 5))
  expect_equal(dim(unroll_vector2lagmatrix(1:5, 3)), c(5, 3))
  expect_equal(dim(unroll_vector2lagmatrix(rep("fa",5), 5)), c(5, 5))
})

test_that("Number of NAs in lagmatrix are n*(n+1)/2", {
  expect_equal(sum(is.na(unroll_vector2lagmatrix(1:10, 5))), 15)
  expect_equal(sum(is.na(unroll_vector2lagmatrix(1:10, 4))), 10)
  expect_equal(sum(is.na(unroll_vector2lagmatrix(1:10, 3))), 6)
})

df1 <- data.frame(datum = rep("foo",10), x1 = 1:10, x2 = 21:30, x3 = 41:50)
df2 <- data.frame(datum = rep("bar",25), x1 = 1:25, x2 = 26:50)

test_that("Length of unroll_lagdays is length(df)+(length(df)-1)*n*(n+1)/2", {
  expect_length(unroll_lagdays(df1), 49)
  expect_length(unroll_lagdays(df2), 33)
})
