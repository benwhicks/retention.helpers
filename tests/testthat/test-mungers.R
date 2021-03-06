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
      offering = c("ABC123_201930_OA_I", "BFG101_202090_B_D", "XLV0000_202060_M_D")
    )),
    tibble::tibble(
      offering = c("ABC123_201930_OA_I", "BFG101_202090_B_D", "XLV0000_202060_M_D"),
      year = c(2019, 2020, 2020)
    ))
})


test_that("add_subject_from_offering works", {
  expect_equal(add_subject_from_offering(
    tibble::tibble(
      offering = c("ABC123_201930_OA_I", "BFG101_202090_B_D", "XLV0000_202060_M_D")
    )),
    tibble::tibble(
      offering = c("ABC123_201930_OA_I", "BFG101_202090_B_D", "XLV0000_202060_M_D"),
      subject = c("ABC123", "BFG101", "XLV0000")
    ))
})


test_that("add_session_from_offering works", {
  expect_equal(add_session_from_offering(
    tibble::tibble(
      offering = c("ABC123_201930_OA_I", "BFG101_202090_B_D", "XLV0000_202060_M_D")
    )),
    tibble::tibble(
      offering = c("ABC123_201930_OA_I", "BFG101_202090_B_D", "XLV0000_202060_M_D"),
      session = c(201930, 202090, 202060)
    ))
})



test_that("add_grade_finalised works", {
  expect_equivalent(add_grade_finalised(
    tibble::tibble(
      grade = c("WD", "TA", "AW",
                "PS", "CR", "DI", "HD", "FW", "FL") %>%
        forcats::fct_relevel("AW", "FW", "FL", "PS", "CR", "DI", "HD")
    )),
    tibble::tibble(
      grade = c("WD", "TA", "AW",
                "PS", "CR", "DI", "HD", "FW", "FL") %>%
        forcats::fct_relevel("AW", "FW", "FL", "PS", "CR", "DI", "HD"),
      grade_finalised = c(rep(F, 3), rep(T, 6))
    ))
})

test_that("add_grade_helpers works", {
  expect_equivalent(add_grade_helpers(
    tibble::tibble(
      grade = c("WD", "TA", "AW",
                "FW", "FL",
                "PS", "CR", "DI", "HD") %>%
        forcats::fct_relevel("AW", "FW", "FL", "PS", "CR", "DI", "HD"))
    ),
    tibble::tibble(
      grade = c("WD", "TA", "AW",
                "FW", "FL",
                "PS", "CR", "DI", "HD") %>%
        forcats::fct_relevel("AW", "FW", "FL", "PS", "CR", "DI", "HD"),
      grade_finalised = c(rep(F, 3), rep(T, 6)),
      grade_pass = c(rep(F, 5), rep(T, 4)),
      grade_fail = c(rep(F, 3), T, T, rep(F, 4)),
      grade_fw = c(rep(F, 3), T, rep(F, 5)))
    )
})

