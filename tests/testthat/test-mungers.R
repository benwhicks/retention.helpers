# Testing the data munging / wrangling type functions

test_that("add_year_from_session works", {
  expect_equal(add_year_from_session(
    tibble::tibble(
      session = c(201930, 202090)
    )),
    tibble::tibble(
      session = c(201930, 202090),
      year = c(2019, 2020)
    ))
})

test_that("add_year_from_offering works", {
  expect_equal(add_year_from_offering(
    tibble::tibble(
      offering = c("ABC123_201930_OA_I", "BFG101_202090_B_D")
    )),
    tibble::tibble(
      offering = c("ABC123_201930_OA_I", "BFG101_202090_B_D"),
      year = c(2019, 2020)
    ))
})


test_that("add_subject_from_offering works", {
  expect_equal(add_subject_from_offering(
    tibble::tibble(
      offering = c("ABC123_201930_OA_I", "BFG101_202090_B_D")
    )),
    tibble::tibble(
      offering = c("ABC123_201930_OA_I", "BFG101_202090_B_D"),
      subject = c("ABC123", "BFG101")
    ))
})


test_that("add_session_from_offering works", {
  expect_equal(add_session_from_offering(
    tibble::tibble(
      offering = c("ABC123_201930_OA_I", "BFG101_202090_B_D")
    )),
    tibble::tibble(
      offering = c("ABC123_201930_OA_I", "BFG101_202090_B_D"),
      session = c(201930, 202090)
    ))
})

