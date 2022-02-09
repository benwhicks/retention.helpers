
# test_path <- file.path()

#' reads grade book download
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Reads in a greade centre download csv, as taken from the i2 site.
#' File should be saved in it's default name: gc_...
#'
#' @param path Path to the gc csv file
#'
#' @export read_gb_csv
read_gb_csv <- function(path) {
  # TODO: Handle SY/US grades somehow. Currently they are dropped
  off <-  stringr::str_remove(path, "^.*gc_S-") %>% stringr::str_remove("_fullgc_.*$")
  ts <- lubridate::as_date(stringr::str_remove(path, "^.*_fullgc_") %>%
                             stringr::str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}"))

  if (stringr::str_detect(path, "csv$")) {
    d <- readr::read_csv(path)
  } else {
    # probably xlsx
    d <- readxl::read_excel(path)
  }
  d %>%
    janitor::remove_empty("cols") %>%
    dplyr::select(
      id = `Student ID`,
      firstname = `First Name`,
      lastname = `Last Name`,
      user_id = Username,
      (dplyr::contains("Score]") & where(is.numeric))) %>%
    dplyr::mutate(id = as.character(id)) %>%  # making sure
    tidyr::pivot_longer(cols = dplyr::contains("Score]"),
                        names_to = "assessment_string",
                        values_to = "raw_mark") %>%
    dplyr::mutate(
      offering = off, date_updated = ts,
      assessment_name = stringr::str_extract(assessment_string, "^.* \\[" ) %>%
        stringr::str_remove(" \\["),
      possible = as.numeric(stringr::str_extract(assessment_string, " [0-9]* Score") %>%
                              stringr::str_remove(" Score")),
      gradebook_id = as.numeric(stringr::str_extract(assessment_string, "[0-9]*$"))
    ) %>%
    dplyr::select(-assessment_string) %>%
    dplyr::mutate(
      mark = raw_mark / possible,
      subject = stringr::str_extract(offering, ".{6}")) %>%
    dplyr::select(subject, offering, assessment_name, id, mark, everything())
}
