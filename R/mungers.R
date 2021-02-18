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
        grade_finalised = stringr::str_detect(grade, "FW|FL|PS|CR|DI|HD")
      )
}

# TODO: Create fct_ style functions that replicate the factor structure in the retention package.
#       Probably best to steal the levels from the _academic_ table rather than specifying...maybe...but that
#       creates a dependency
