# Data munging and wrangling functions

#' str_c_sentence_list
#'
#' Takes a character vector and returns a string of
#' the elements in that character vector separated by
#' commas, with the last two elements separated with " and "
#'
#' @param s a character vector
#' @return a string
#'
#' @export str_c_sentence_list
str_c_sentence_list <- function(s) {
  if (length(s) <= 1) {
    return(s)
  } else if (length(s) == 2) {
    return(paste0(s[[1]], " and ", s[[2]]))
  } else {
    return(paste0(
      c(s[1:(length(s) - 2)]
        ,
        paste0(s[[length(s) - 1]], " and ", s[[length(s)]])
      ),
      collapse = ", "
    ))
  }
}

#' add year from session
#'
#' Takes a data frame with \strong{session} variable and returns the
#' same data frame with a \strong{year} variable. Requires the \strong{session}
#' to be in the format \emph{201930}
#'
#' @param d a data frame
#' @return a data frame
#' @family mungers
#'
#' @export add_year_from_session
add_year_from_session <- function(d) {
  if ("session" %in% names(d)) {
    d %>%
      dplyr::mutate(year = floor(session / 100))
  } else {
    stop("Variable 'session' not found")
  }
}

#' add year from offering
#'
#' Takes a data frame with \strong{offering} variable and returns the
#' same data frame with a \strong{year} variable. Requires the \strong{offering}
#' to be in the format ABC123_201930_W_D which would return \emph{2019}
#'
#' @param d a data frame
#' @return a data frame
#' @family mungers
#'
#' @export add_year_from_offering
add_year_from_offering <- function(d) {
  d %>%
    dplyr::mutate(year = offering %>%
                    stringr::str_remove("^\\w*?_") %>%
                    stringr::str_extract("^.{4}") %>%
                    as.numeric())
}

#' add subject from offering
#'
#' Takes a data frame with \strong{offering} variable and returns the
#' same data frame with a \strong{subject} variable. Requires the \strong{offering}
#' to be in the format ABC123_201930_W_D, which would then return
#' \emph{ABC123} as the subject.
#'
#' @param d a data frame
#' @return a data frame
#'
#' @export add_subject_from_offering
add_subject_from_offering <- function(d) {
  if ("offering" %in% names(d)) {
    d %>%
      dplyr::mutate(
        subject = offering %>%
          stringr::str_extract("^\\w*?_") %>%
          stringr::str_remove("_")
      )
  } else {
    stop("Variable 'offering' not found")
  }
}

#' add session from offering
#'
#' Takes a data frame with \strong{offering} variable and returns the
#' same data frame with a \strong{session} variable. Requires the \strong{offering}
#' to be in the format ABC123_201930_W_D, which would then return
#' \emph{201930} as the session.
#'
#' @param d a data frame
#' @return a data frame
#'
#' @export add_session_from_offering
add_session_from_offering <- function(d) {
  if ("offering" %in% names(d)) {
    d %>%
      dplyr::mutate(
        session = offering %>%
          stringr::str_remove("^\\w*?_") %>%
          stringr::str_extract("^[:digit:]{6}") %>%
          as.numeric()
      )
  } else {
    stop("Variable 'offering' not found")
  }
}

#' add grade finalised
#'
#' Takes a data frame with \strong{grade} variable and returns the
#' same data frame with a \strong{grade_finalised} variable, which is
#' true if grade is FW, FL, PS, CR, DI or HD; and false otherwise.
#'
#' @param d a data frame
#' @return a data frame
#'
#' @export add_grade_finalised
add_grade_finalised <- function(d) {
    d %>%
      dplyr::mutate(
        grade_finalised = stringr::str_detect(grade, "FNS|FW|FL|PS|CR|DI|HD")
      )
}

#' add grade pass
#'
#' Takes a data frame with \strong{grade} variable and returns the
#' same data frame with a \strong{grade_pass} variable, which is
#' true if grade is PS, CR, DI or HD; and false otherwise.
#'
#' @param d a data frame
#' @return a data frame
#'
#' @export add_grade_pass
add_grade_pass <- function(d) {
  d %>%
    dplyr::mutate(
      grade_pass = stringr::str_detect(grade, "PS|CR|DI|HD")
    )
}

#' add grade fail
#'
#' Takes a data frame with \strong{grade} variable and returns the
#' same data frame with a \strong{grade_fail} variable, which is
#' true if grade is FW, FL; and false otherwise.
#'
#' @param d a data frame
#' @return a data frame
#'
#' @export add_grade_fail
add_grade_fail <- function(d) {
  d %>%
    dplyr::mutate(
      grade_fail = stringr::str_detect(grade, "FW|FL|FNS")
    )
}


#' add grade Non-Participating Enrolment (NPE)
#'
#' Takes a data frame with \strong{grade} variable and returns the
#' same data frame with a \strong{grade_npe} variable, which is
#' true if grade is FW or FNS; and false otherwise.
#'
#' @param d a data frame
#' @return a data frame
#'
#' @export add_grade_npe
add_grade_npe <- function(d) {
  d %>%
    dplyr::mutate(
      grade_npe = stringr::str_detect(grade, "FW|FNS")
    )
}


#' add grade helpers
#'
#' Takes a data frame with \strong{grade} variable and returns the
#' same data frame with a series of grade_* variables to assist in
#' analysing grade data.
#'
#' @param d a data frame
#' @return a data frame
#'
#' @export add_grade_helpers
add_grade_helpers <- function(d) {
  d %>%
    add_grade_finalised() %>%
    add_grade_pass() %>%
    add_grade_fail() %>%
    add_grade_npe()
}


# TODO: Create fct_ style functions that replicate the factor structure in the retention package.
#       Probably best to steal the levels from the _academic_ table rather than specifying...maybe...but that
#       creates a dependency

# TODO: adjust_* style functions
# For example, adjust_minutes would try to sort the errors in the activity table (some have minutes
# for a day over 22 days worth)

#' nice count
#'
#' Counts a field, then uses the janitor functions to add % and total.
#' Works for a single field only
#'
#' @param col Column to be counted
#'
#' @export nice_count
nice_count <- function(d, col) {
  d %>%
    dplyr::count({{col}}) %>%
    janitor::adorn_percentages() %>%
    janitor::adorn_pct_formatting() %>%
    janitor::adorn_ns()
}


#' add subject context
#'
#' Requires a data frame with the *subject* (six character code),
#' *n_remained* (if this is NA the subject wasn't in HEPPP last year), *genuine_pass_rate*,
#' *n_passed*,  *same_trigger* (boolean) and also
#' three fields describing when the subject runs, *s1*, *s2* and *s3*.
#' The *sn* fields should be set to __internal__, __distance__ or __both__.
#' If they are all blank (NA) then it is deemed that it is unknown when the subject is run next.
#'
#' @param d data frame, as specified above
#' @examples
#' d <- tibble::tribble(
#'    ~subject, ~genuine_pass_rate, ~n_remained, ~n_passed, ~same_trigger, ~s1, ~s2, ~s3,
#'    "MEH111", NA_real_, NA, NA, NA, "both", "internal", "internal",
#'    "NUP200", 0.88, 18, 3, TRUE, NA_character_, "both", NA_character_,
#'    "LOL101", 0.71, 15, 0, FALSE, "distance", "distance", "distance"
#' )
#' d |> add_subject_context()
#'
#' @export add_subject_context
add_subject_context <- function(d) {

  d %>%
    dplyr::mutate(
      subject_context = stringr::str_c(

        subject,
        dplyr::if_else(!is.na(n_remained), " was in last years campaign. ", " was not in last years campaign."),

        dplyr::if_else(is.na(genuine_pass_rate), "",
                       stringr::str_c(
                         " Around ", round(genuine_pass_rate / 0.05) * 5, "% of students who submit at least 1 assessment end up passing."
                       )),

        # performance of last years flagged students who remained past census date
        dplyr::if_else(is.na(n_remained), "",
                       dplyr::if_else(n_remained == 0, "No students in a similar position last year remained past census date.",
                       stringr::str_c(
                         " Last year ", n_remained, if_else(n_remained == 1, " student", " students"),
                         " who were in ", dplyr::if_else(same_trigger, "the same", "a similar"),
                         " position remained enrolled past census and ",
                         dplyr::if_else(
                           n_passed == 0,
                           "none of them passed.",
                           stringr::str_c(
                             n_passed,
                             " of them passed (",
                             round(100*n_passed/n_remained),"%)."))
                       ))),

        # When the subject is run
        dplyr::if_else(
          (is.na(s1) & is.na(s2) & is.na(s3)),
          "It is unclear when this subject is run next.",
          stringr::str_c(
            " This subject runs in:",
            dplyr::case_when(
              is.na(s1) ~ "",
              s1 == "both" ~     " Session 1 (internal and distance)",
              s1 == "internal" ~ " Session 1 (internal only)",
              s1 == "distance" ~ " Session 1 (distance only)"
            )
            ,
            dplyr::case_when(
              is.na(s2) ~ "",
              s2 == "both" ~     " Session 2 (internal and distance)",
              s2 == "internal" ~ " Session 2 (internal only)",
              s2 == "distance" ~ " Session 2 (distance only)"
            ),
            dplyr::case_when(
              is.na(s3) ~ "",
              s3 == "both" ~     " Session 3 (internal and distance)",
              s3 == "internal" ~ " Session 3 (internal only)",
              s3 == "distance" ~ " Session 3 (distance only)"
            )
          )),
        "."
      )
    )
}
