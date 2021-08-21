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



test_that("add_grade_substantive works", {
  expect_equivalent(add_grade_substantive(
    tibble::tibble(
      grade = c("WD", "TA",
                "AW", "PS", "CR", "DI", "HD", "FW", "FL") %>%
        forcats::fct_relevel("AW", "FW", "FL", "PS", "CR", "DI", "HD")
    )),
    tibble::tibble(
      grade = c("WD", "TA",
                "AW", "PS", "CR", "DI", "HD", "FW", "FL") %>%
        forcats::fct_relevel("AW", "FW", "FL", "PS", "CR", "DI", "HD"),
      grade_substantive = c(rep(F, 2), rep(T, 7))
    ))
})

test_that("add_grade_success works", {
  expect_equivalent(add_grade_success(
    tibble::tibble(
      grade = c("WD", "TA", "AW",
                "PS", "CR", "DI", "HD",
                "FW", "FL") %>%
        forcats::fct_relevel("AW", "FW", "FL", "PS", "CR", "DI", "HD")
    )),
    tibble::tibble(
      grade = c("WD", "TA", "AW",
                "PS", "CR", "DI", "HD", "FW", "FL") %>%
        forcats::fct_relevel("AW", "FW", "FL", "PS", "CR", "DI", "HD"),
      grade_success = c(rep(F, 3), rep(T, 4), rep(F, 2))
    ))
})

test_that("add_grade_gpa works", {
  expect_equivalent(add_grade_gpa(
    tibble::tibble(
      grade = c("WD", "TA", "AW",
                "PS", "CR", "DI", "HD", "FW", "FL") %>%
        forcats::fct_relevel("AW", "FW", "FL", "PS", "CR", "DI", "HD")
    )),
    tibble::tibble(
      grade = c("WD", "TA", "AW",
                "PS", "CR", "DI", "HD", "FW", "FL") %>%
        forcats::fct_relevel("AW", "FW", "FL", "PS", "CR", "DI", "HD"),
      grade_substantive = c(rep(F, 3), rep(T, 6))
    ))
})

test_that("add_gpa_by works", {
  expect_equivalent(
    add_gpa_by(
      tibble::tibble(
        id = c(rep("1A", 4 %>% %>% %>% %>% %>% %>% %>% %>% ), rep("34", 5)),
        grade = c("AW", "PS", "HD", "FL",
                  "PS", "CR", "DI", "HD", "FW") %>%
          forcats::fct_relevel("AW", "FW", "FL", "PS", "CR", "DI", "HD")
      ),
      id),

    tibble::tibble(
      id = c("1A", "34"),
      gpa = c(
        (4 + 7 + 0) / 3,
        (4 + 5 + 6 + 7 + 0) / 5)
    ))
})

