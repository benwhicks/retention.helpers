
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

#' reads grade book download with cleaned header
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Reads in a greade centre download csv, as taken from the i2 site.
#' The top 4 rows have the metadata: week due, value, eai & threshold mark
#' The labels of those columns should be in cells A1:A4
#'
#' @param path Path to the gc csv file
#'
#' @export read_cleaned_gc_xlsx
read_cleaned_gc_xlsx <- function(f) {
  # TODO: exclude offering, only subject and session

  # data in any of the headers, include,
  # Also include cumulative mark
  # Also include calculated grade, and admin override
  # need total points (taken from header) as well as the full header title, and the extracted title
  # probably impute 0's for missing marks
  # will need to check 'child_subject_id' for offering
  file_offering <- stringr::str_extract(f, "[A-Z]{3}[0-9]{3}_[0-9]{6}_[A-Z]{1,2}_[A-Z]{1}")
  file_subject <- stringr::str_extract(file_offering, "^[A-Z]{3}[0-9]{3}")
  file_session <- as.numeric(stringr::str_extract(file_offering, "_[0-9]{6}_") %>% stringr::str_remove_all("_"))
  message(stringr::str_c("Extracting: ", file_offering, "; from: "))
  message(f) # helps debugging, remove later
  file_download <- file.info(f)$mtime %>%
    lubridate::as_date()

  header_data_raw <- suppressMessages(readxl::read_excel(f, col_names = FALSE, n_max = 5))
  header_data <- suppressWarnings(tibble(
    gc_title = header_data_raw %>%
      dplyr::slice(5) %>%
      dplyr::select(2:ncol(header_data_raw)) %>%
      as.character(),
    week_due = header_data_raw %>%
      dplyr::filter(`...1` == "week due") %>%
      dplyr::select(2:ncol(header_data_raw)) %>%
      as.numeric(),
    value = header_data_raw %>%
      dplyr::filter(`...1` == "value") %>%
      dplyr::select(2:ncol(header_data_raw)) %>%
      as.character(),
    eai = as.logical(!is.na(header_data_raw %>%
                              dplyr::filter(`...1` == "eai") %>%
                              dplyr::select(2:ncol(header_data_raw)))),
    threshold_mark = header_data_raw %>%
      dplyr::filter(`...1` == "threshold mark") %>%
      dplyr::select(2:ncol(header_data_raw)) %>%
      as.character()
  )) %>%
    dplyr::filter(
      !is.na(week_due) | !is.na(value) | !is.na(eai) | !is.na(threshold_mark)
    )

  # diagnostic - marks add to 100
  mark_sum <- sum(as.numeric(header_data$value), na.rm = T)
  if (mark_sum != 100) {cat(
    paste("\n!!!! GC sheet for", file_offering, "adds to", mark_sum, "and not 100 !!!\n"))}

  # ===== extracting main data ===== //
  main_data_raw <- readxl::read_excel(f, skip = 4, col_types = "text") %>%
    dplyr::select(id = `Student ID`, dplyr::any_of(header_data$gc_title),
                  dplyr::contains("Cumulative Mark"),
                  dplyr::contains("Calculated Grade"),
                  dplyr::contains("Administrative Override"))

  # overall performance data
  if (any(stringr::str_detect(names(main_data_raw), "Cumulative Mark"))) {
    cumulative_marks <-
      main_data_raw %>%
      dplyr::select(id, contains("Cumulative Mark")) %>%
      tidyr::pivot_longer(-id, names_to = "gc_title", values_to = "score") %>%
      dplyr::mutate(score = as.numeric(score)) %>%
      dplyr::mutate(
        title =  stringr::str_extract(gc_title, "^.* \\[") %>%  stringr::str_remove(" \\["),
        metric = stringr::str_extract(gc_title, "\\[.*\\]") %>% stringr::str_remove_all("\\[|\\]"),
        bb_pk1 = stringr::str_extract(gc_title, "\\|.*$") %>%   stringr::str_remove("\\|")) %>%
      dplyr::mutate(out_of = as.numeric(stringr::str_extract(metric, " [0-9]* "))) %>%
      dplyr::mutate(score_p = score / out_of)
  } else {
    cumulative_marks <-
      main_data_raw %>%
      dplyr::select(id) %>%
      dplyr::slice(0)
    cat("\n!!! No cumulative marks !!!\n")
  }

  grades <-
    main_data_raw %>%
    dplyr::select(id,
           dplyr::contains("Calculated Grade"),
           dplyr::contains("Administrative Override")) %>%
    tidyr::pivot_longer(-id, names_to = "gc_title", values_to = "grade",
                 values_drop_na = TRUE) %>%
    dplyr::mutate(
      title =  stringr::str_extract(gc_title, "^.* \\[") %>% string::str_remove(" \\["),
      metric = stringr::str_extract(gc_title, "\\[.*\\]") %>% string::str_remove_all("\\[|\\]"),
      bb_pk1 = stringr::str_extract(gc_title, "\\|.*$") %>% string::str_remove("\\|"))

  if (any(grades$grade == "SY")) {
    cat("\n  ---  SY subject --- \n")
  }

  # will need to deal with SY/US seperately, so dealing with assessment seperately
  numeric_assessment_names <-
    header_data %>%
    dplyr::filter(!is.na(value), !is.na(as.numeric(value))) %>%
    dplyr::pull(gc_title)

  sy_assessment_names <-
    header_data %>%
    dplyr::filter(!is.na(value), is.na(as.numeric(value))) %>%
    dplyr::pull(gc_title)

  # break into assessments, and other important data, then join
  numeric_assessments <-
    main_data_raw %>%
    dplyr::select(id, dplyr::any_of(numeric_assessment_names)) %>%
    tidyr::pivot_longer(
      cols = -id,
      names_to = "gc_title",
      values_to = "score") %>%
    dplyr::mutate(score = dplyr::replace_na(as.numeric(score), 0)) %>%
    dplyr::mutate(
      title =  stringr::str_extract(gc_title, "^.* \\[") %>% stringr::str_remove(" \\["),
      metric = stringr::str_extract(gc_title, "\\[.*\\]") %>%
        stringr::str_remove_all("\\]|\\["),
      bb_pk1 = stringr::str_extract(gc_title, "\\|.*$") %>% stringr::str_remove("\\|")) %>%
    dplyr::left_join(header_data, by = "gc_title") %>%
    dplyr::mutate(out_of =
             dplyr::coalesce(
               as.numeric(stringr::str_extract(metric, " [0-9|\\.]* ")),
               as.numeric(value))) %>%
    dplyr::mutate(score_p = score / out_of)

  # diagnostics on numeric assessment % scores

  if (any(is.na(numeric_assessments$score_p))) {
    warning(stringr::str_c("Some assessment of unknown % for ", file_offering))}
  if (max(numeric_assessments$score_p) > 1) {
    warning(stringr::str_c("Some assessment marks exceed 100% for ", file_offering))}
  if (min(numeric_assessments$score_p) < 0) {
    warning(stringr::str_c("Some assessment marks negative for ", file_offering))}

  if (length(sy_assessment_names) > 0) {
    sy_assessments <-
      main_data_raw %>%
      dplyr::select(id, dplyr::any_of(sy_assessment_names)) %>%
      tidyr::pivot_longer(
        cols = -id,
        names_to = "gc_title",
        values_to = "sy_us"
      ) %>%
      dplyr::mutate(sy_us = replace_na(sy_us, "US")) %>%
      dplyr::mutate(
        title = stringr::str_extract(gc_title, "^.* \\[") %>%
          stringr::str_remove(" \\["),
        metric = stringr::str_extract(gc_title, "\\[.*\\]") %>%
          stringr::str_remove_all("\\[|\\]"),
        bb_pk1 = stringr::str_extract(gc_title, "\\|.*$") %>%
          stringr::str_remove("\\|")) %>%
      dplyr::left_join(header_data, by = "gc_title")
    assessments <- dplyr::bind_rows(numeric_assessments, sy_assessment_names)
  } else {
    assessments <- numeric_assessments
  }

  # ====== joining all together ==== //
  df_out <-
    grades %>%
    dplyr::bind_rows(cumulative_marks) %>%
    dplyr::bind_rows(assessments) %>%
    dplyr::select(-gc_title) %>%
    dplyr::mutate(subject = file_subject, session = file_session) %>%
    dplyr::select(subject, session, id, title, week_due, value, eai, threshold_mark, metric,
           everything())
  cat(crayon::green("Complete"), "\n\n")
  return(df_out)
}

