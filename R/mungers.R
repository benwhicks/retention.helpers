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
#' Takes a data frame with **session** variable and returns the
#' same data frame with a **year** variable. Requires the **session**
#' to be in the format *201930*
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
    stop("Varable 'session' not found")
  }
}

#' add year from offering
#'
#' Takes a data frame with **offering** variable and returns the
#' same data frame with a **year** variable. Requires the **offering**
#' to be in the format ABC123_201930_W_D which would return *2019*
#'
#' @param d a data frame
#' @return a data frame
#' @family mungers
#'
#' @export add_year_from_offering
add_year_from_offering <- function(d) {
  if ("offering" %in% names(d)) {
    d %>%
      dplyr::mutate(year = offering %>%
                      stringr::str_remove("^.{7}") %>%
                      stringr::str_extract(".{4}") %>%
                      as.numeric())
  } else {
    stop("Varable 'offering' not found")
  }
}

#' add subject from offering
#'
#' Takes a data frame with **offering** variable and returns the
#' same data frame with a **subject** variable. Requires the **offering**
#' to be in the format ABC123_201930_W_D, which would then return
#' *ABC123* as the subject.
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
          stringr::str_extract("^.{6}")
      )
  } else {
    stop("Varable 'offering' not found")
  }
}

#' add session from offering
#'
#' Takes a data frame with **offering** variable and returns the
#' same data frame with a **session** variable. Requires the **offering**
#' to be in the format ABC123_201930_W_D, which would then return
#' *201930* as the session.
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
          stringr::str_remove("^.{7}") %>%
          stringr::str_extract("^.{6}") %>%
          as.numeric()
      )
  } else {
    stop("Varable 'offering' not found")
  }
}
