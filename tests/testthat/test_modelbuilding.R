context("Testing of all functions related to modelbuilding")

variables1 <- c("e.coli","q_a","ka_a","r_a","i_a","i_b")
variables2 <- c("e.coli","q_a","q_b","ka_a","ka_b","r_a")
variables3 <- c("log_e.coli","q_a","r_a","q_a:r_a")

riverdata <- list(hygiene  = data.frame(datum = 1:10, e.coli = 1:10 + rnorm(n=10)),
                  q_river  = data.frame(datum = 1:10, q_a = 20:11, q_b = 21:30),
                  ka_river = data.frame(datum = 1:10, ka_a = 31:40, ka_b = 51:60),
                  r_river  = data.frame(datum = 1:10, r_a = 121:130),
                  i_river  = data.frame(datum = 1:10, i_a = 1001:1010, i_b = 21:30))

test_that("create_formula does not create formula", {
  expect_is(create_formula(variables1, T), "formula")
  expect_is(create_formula(variables2), "formula")
  expect_is(create_formula(variables3), "formula")
})

test_that("create_formula does not create correct formulas", {
  expect_equal(length(create_formula(variables1, T)), 3)
  expect_equal(length(create_formula(variables2)), 3)
  expect_equal(length(create_formula(variables3)), 3)
})

test_that("process_model_riverdata does not create correct data.frames", {
  expect_is(process_model_riverdata(riverdata, variables1), "data.frame")
  expect_is(process_model_riverdata(riverdata, variables3), "data.frame")
  expect_length(process_model_riverdata(riverdata, variables1), 7)
  expect_length(process_model_riverdata(riverdata, variables2), 7)
  expect_length(process_model_riverdata(riverdata, variables3), 4)
})

test_that("everything cool until build_model throws an error", {
  expect_error(build_model(riverdata, variables = variables2))
})

test_that("lm does not work with create formula and process_model_riverdata", {
  expect_is(lm(formula = create_formula(variables1, T),
               data = process_model_riverdata(riverdata, variables1)), "lm")
  expect_is(lm(formula = create_formula(variables2),
               data = process_model_riverdata(riverdata, variables2)), "lm")
  expect_is(lm(formula = create_formula(variables3),
               data = process_model_riverdata(riverdata, variables3)), "lm")
})
