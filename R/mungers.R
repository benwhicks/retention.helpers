# Data munging and wrangling functions


#' str_c_sentence_list
#'
#'  @description `r lifecycle::badge('maturing')`
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
#'  @description `r lifecycle::badge('stable')`
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
    d |>
      dplyr::mutate(year = floor(session / 100))
  } else {
    stop("Variable 'session' not found")
  }
}

#' add year from offering
#'
#'  @description `r lifecycle::badge('stable')`
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
  d |>
    dplyr::mutate(year = offering |>
                    stringr::str_remove("^\\w*?_") |>
                    stringr::str_extract("^.{4}") |>
                    as.numeric())
}

#' add subject from offering
#'
#'  @description `r lifecycle::badge('stable')`
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
    d |>
      dplyr::mutate(
        subject = offering |>
          stringr::str_extract("^\\w*?_") |>
          stringr::str_remove("_")
      )
  } else {
    stop("Variable 'offering' not found")
  }
}

#' add session from offering
#'
#' @description `r lifecycle::badge('stable')`
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
    d |>
      dplyr::mutate(
        session = offering |>
          stringr::str_remove("^\\w*?_") |>
          stringr::str_extract("^[:digit:]{6}") |>
          as.numeric()
      )
  } else {
    stop("Variable 'offering' not found")
  }
}

grade_classifications <- tibble::tribble(
  ~grade, ~grade_substantive, ~grade_success, ~grade_gpa,
  "AA",                 0L,             0L,         0L,
  "AE",                 0L,             0L,         0L,
  "AW",                 1L,             0L,         0L,
  "C",                 1L,             1L,         1L,
  "CI",                 0L,             0L,         0L,
  "CP",                 1L,             1L,         1L,
  "CR",                 1L,             1L,         1L,
  "D",                 1L,             1L,         1L,
  "DI",                 1L,             1L,         1L,
  "FF",                 1L,             0L,         1L,
  "FI",                 1L,             0L,         1L,
  "FL",                 1L,             0L,         1L,
  "FW",                 1L,             0L,         1L,
  "GP",                 0L,             0L,         0L,
  "H1",                 1L,             1L,         0L,
  "H2a",                 1L,             1L,         0L,
  "H2b",                 1L,             1L,         0L,
  "H3",                 1L,             1L,         0L,
  "HD",                 1L,             1L,         1L,
  "IP",                 0L,             0L,         0L,
  "IS",                 0L,             0L,         0L,
  "NA",                 0L,             0L,         0L,
  "P",                 1L,             1L,         1L,
  "PS",                 1L,             1L,         1L,
  "PT",                 1L,             1L,         1L,
  "S",                 1L,             1L,         0L,
  "SX",                 0L,             0L,         0L,
  "SY",                 1L,             1L,         0L,
  "TA",                 0L,             0L,         0L,
  "LV",                 0L,             0L,         0L,
  "US",                 1L,             0L,         1L,
  "W",                 1L,             0L,         0L,
  "TCR",                 0L,             0L,         0L,
  "PCR",                 0L,             0L,         0L,
  "FCR",                 0L,             0L,         0L,
  "WD",                 0L,             0L,         0L,
  "DX",                 0L,             0L,         0L,
  "FNS",                 1L,             0L,         1L,
  "ZZ",                 0L,             0L,         0L
)

#' grade classifications
#'
#' What each of the grade codes count towards
#'
#' @export grade_classifications
grade_classifications <- grade_classifications |>
  dplyr::mutate(
    grade_substantive = as.logical(grade_substantive),
    grade_success = as.logical(grade_success),
    grade_gpa = as.logical(grade_gpa),
    grade = forcats::fct_relevel(grade, "AW", "FW", "FL", "PS", "CR", "DI", "HD")
  )

#' add grade finalised
#'
#' @description `r lifecycle::badge('deprecated')`
#'
#' Takes a data frame with \strong{grade} variable and returns the
#' same data frame with a \strong{grade_finalised} variable, which is
#' true if grade is FW, FL, PS, CR, DI or HD; and false otherwise.
#'
#' @param d a data frame
#' @return a data frame
#' @keywords internal
#'
#' @export add_grade_finalised
add_grade_finalised <- function(d) {
  lifecycle::deprecate_warn(
    "0.2.0",
    "add_grade_finalised()",
    "add_grade_substantive()",
    details = "Substantive is the correct terminology for CSU and definitions changed to include AW."
  )
    d |>
      dplyr::mutate(
        grade_finalised = stringr::str_detect(grade, "FNS|FW|FL|PS|CR|DI|HD")
      )
}

#' add grade substantive
#'
#' @description  `r lifecycle::badge("experimental")`
#'
#' Adds *grade_substantive* column to data frame, based on
#' *grade* column. A substantive grade is included in the denominator
#' when calculating progress rates.
#'
#' @param d a data frame
#' @return a data frame
#'
#' @export add_grade_substantive
add_grade_substantive <- function(d) {
  d |>
    dplyr::left_join(
      grade_classifications |>
        dplyr::select(grade, grade_substantive),
      by = "grade")
}

#' add grade success
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Adds *grade_success* column to data frame, based on
#' *grade* column.
#'
#' @param d a data frame
#' @return a data frame
#'
#' @export add_grade_success
add_grade_success <- function(d) {
  d |>
    dplyr::left_join(
      grade_classifications |>
        dplyr::select(grade, grade_success),
      by = "grade")
}

#' add grade gpa
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Adds *grade_gpa* column to data frame, based on
#' *grade* column. Grades that count towards a GPA
#' are not the same as substantive grades (a key difference
#' being AW grades)
#'
#' @param d a data frame
#' @return a data frame
#'
#' @export add_grade_gpa
add_grade_gpa <- function(d) {
  d |>
    dplyr::left_join(
      grade_classifications |>
        dplyr::select(grade, grade_gpa),
      by = "grade")
}

#' add grade pass
#'
#' @description  `r lifecycle::badge('superseded')`
#'
#' Takes a data frame with \strong{grade} variable and returns the
#' same data frame with a \strong{grade_pass} variable, which is
#' true if grade is PS, CR, DI or HD; and false otherwise.
#'
#' @param d a data frame
#' @return a data frame
#' @keywords internal
#'
#' @export add_grade_pass
add_grade_pass <- function(d) {
  lifecycle::deprecate_warn(
    "0.2.0",
    "add_grade_pass()",
    "add_grade_success()",
    details = "Success is the correct terminology for CSU and some definitions changed."
  )
  d |>
    dplyr::mutate(
      grade_pass = stringr::str_detect(grade, "PS|CR|DI|HD")
    )
}

#' add grade fail
#'
#'  @description `r lifecycle::badge('stable')`
#'
#' Takes a data frame with \strong{grade} variable and returns the
#' same data frame with a \strong{grade_fail} variable, which is
#' true if grade is FW, FL or FNS; and false otherwise.
#'
#' @param d a data frame
#' @return a data frame
#'
#' @export add_grade_fail
add_grade_fail <- function(d) {
  d |>
        dplyr::left_join(
            grade_classifications |>
                dplyr::select(grade,
                              g_gpa = grade_gpa,
                              g_success = grade_success),
            by = "grade") |>
        dplyr::mutate(
            grade_fail = g_gpa & !g_success
        ) |>
        dplyr::select(-g_gpa, -g_success)
}


#' add grade Non-Participating Enrolment (NPE)
#'
#'  @description `r lifecycle::badge('maturing')`
#'
#' Takes a data frame with \strong{grade} variable and returns the
#' same data frame with a \strong{grade_npe} variable, which is
#' true if grade is FW or FNS; and false otherwise. Identical to
#' add_grade_zf.
#'
#' @param d a data frame
#' @return a data frame
#'
#' @export add_grade_npe
add_grade_npe <- function(d) {
  d |>
    dplyr::mutate(
      grade_npe = stringr::str_detect(grade, "FW|FNS")
    )
}

#' add grade Zero Fail (ZF)
#'
#'  @description `r lifecycle::badge('maturing')`
#'
#' Takes a data frame with \strong{grade} variable and returns the
#' same data frame with a \strong{grade_zf} variable, which is
#' true if grade is FW or FNS; and false otherwise. Identical to
#' add_grade_npe.
#'
#' @param d a data frame
#' @return a data frame
#'
#' @export add_grade_zf
add_grade_zf <- function(d) {
  d |>
    dplyr::mutate(
      grade_zf = stringr::str_detect(grade, "FW|FNS")
    )
}


#' add grade helpers
#'
#'  @description `r lifecycle::badge('maturing')`
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
  d |>
    add_grade_substantive() |>
    add_grade_success() |>
    add_grade_gpa() |>
    add_grade_fail() |>
    add_grade_zf() |>
    add_grade_npe()
}

#' add GPA
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Takes a data frame with *grade* and other variables for grouping and
#' returns the a data frame with the grouping variables and *gpa*.
#'
#' @param d a data frame
#' @param ... grouping variables, passed to group_by
#' @return a data frame
#'
#' @export add_gpa_by
add_gpa_by <- function(d, ...) {
  d |>
    dplyr::group_by(...) |>
    retention.helpers::add_grade_gpa() |> # throws error if grade_gpa already there
    dplyr::summarise(
      gpa = dplyr::if_else(
        sum(grade_gpa, na.rm = TRUE) == 0,
        NA_real_,
        sum(
          dplyr::case_when(
            grade == "HD" ~ 7,
            grade == "A+" ~ 7,
            grade == "DI" ~ 6,
            grade == "D"  ~ 6,
            grade == "CR" ~ 5,
            grade == "C"  ~ 5,
            grade == "PS" ~ 4,
            grade == "P"  ~ 4,
            grade == "CP" ~ 3,
            grade == "PT" ~ 3,
            grade == "CT" ~ 3,
            grade == "SR" ~ 3,
            grade == "T"  ~ 3,
            grade == "XP" ~ 3,
            TRUE ~ 0,
          )
        ) / sum(grade_gpa, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}


#' nice count
#'
#'  @description `r lifecycle::badge('experimental')`
#'
#' Counts a field, then uses the janitor functions to add % and total.
#' Works for a single field only
#'
#' @param col Column to be counted
#'
#' @export nice_count
nice_count <- function(d, col) {
  d |>
    dplyr::count({{col}}) |>
    janitor::adorn_percentages() |>
    janitor::adorn_pct_formatting() |>
    janitor::adorn_ns()
}


#' add subject context
#'
#'  @description `r lifecycle::badge('maturing')`
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

  d |>
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


#' add enrolled
#'
#'  @description `r lifecycle::badge('experimental')`
#'
#' Adds boolean field if student is enrolled
#'
#' @param d a data frame with id, and offering or withdraw_date
#' @return a data frame
#'
#' @export add_grade_fail
add_enrolled <- function(d) {
  if ("withdraw_date" %in% names(d)) {
    d |>
      dplyr::mutate(enrolled = is.na(withdraw_date))
  } else {
    d |>
      dplyr::inner_join(enrolments |> dplyr::select(id, offering, withdraw_date)) |>
      dplyr::mutate(enrolled = is.na(withdraw_date)) |>
      dplyr::select(-withdraw_date)
  }
}

#' add remained past census
#'
#'  @description `r lifecycle::badge('experimental')`
#'
#' Adds boolean field if student is past census. This will remove
#'
#' @param d a data frame with id, session and offering
#' @return a data frame
#'
#' @export add_grade_fail
add_remained_past_census <- function(d) {
  d |>
    dplyr::inner_join(enrolments |> dplyr::select(id, offering, withdraw_date)) |>
    dplyr::inner_join(sessions |> dplyr::select(session, census_date))
    dplyr::mutate(
      remained_past_census = is.na(withdraw_date) |
        withdraw_date > census_date) |>
    dplyr::select(-withdraw_date, -census_date)
}

#' GPA
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Calculates GPA from a list of grades
#'
#' @param grades a list of grades
#' @return a number, grade point average
#'
#' @export gpa
gpa <- function(grades) {
  sum(
    dplyr::case_when(
      grades == "HD" ~ 7,
      grades == "A+" ~ 7,
      grades == "DI" ~ 6,
      grades == "D"  ~ 6,
      grades == "CR" ~ 5,
      grades == "C"  ~ 5,
      grades == "PS" ~ 4,
      grades == "P"  ~ 4,
      grades == "CP" ~ 3,
      grades == "PT" ~ 3,
      grades == "CT" ~ 3,
      grades == "SR" ~ 3,
      grades == "T"  ~ 3,
      grades == "XP" ~ 3,
      TRUE ~ 0
    )) /
      sum(
        grades %in% c("HD", "A+", "DI", "D", "CR", "C", "PS", "P", "CP", "PT", "CT", "SR", "T", "XP"))

}


#' summarise academic
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Calculates a summary of grades.
#'
#' @param d a data frame with grades in it, grouped by relevant
#' @param include_grades true / false: do you want to include a list of the grades as well?
#' @param zf_text The text for ZF / NPE / FNS grades
#' @param zf_long_text The longer text for 'zero fail' or 'non-participating enrolment' or 'FNS'
#' @return a summarised data frame
#'
#' @export summarise_academic
summarise_academic <- function(d, include_grades = FALSE,
                               zf_text = "ZF", zf_long_text = "zero fail") {
    df_out <-
        d |>
        add_grade_helpers() |>
        dplyr::filter(grade_substantive) |>
        dplyr::summarise(
            grades = stringr::str_c(sort(grade), collapse = ", "),
            result_long = factor(
                dplyr::case_when(
                    all(grade_zf) ~ paste0("Fail all (total ", zf_text, ")"),
                    any(grade_zf) & mean(grade_success) >= 0.5 ~ paste0("Passing (some ", zf_text, ")"),
                    all(grade_fail) & any(grade_zf) ~ paste0("Fail all (some non-", zf_long_text, "s)"),
                    any(grade_zf) & mean(grade_success) < 0.5 ~ paste0("Failing (some ", zf_text, ")"),
                    all(grade_success) ~ "Pass all",
                    mean(grade_success) >= 0.5 ~ paste0("Passing (some non-", zf_long_text,"s)"),
                    mean(grade_success) < 0.5 ~ paste0("Failing (non-", zf_long_text,"s)"))),
            result = factor(
                dplyr::case_when(
                    all(grade_zf) ~ paste("Total", zf_text),
                    any(grade_zf)  ~ paste("Partial", zf_text),
                    mean(grade_success) >= 0.5 ~ "Pass",
                    mean(grade_success)  < 0.5 ~ paste0("Non-", zf_long_text))),
            .groups = "drop") |>
        mutate(
            result_long = suppressWarnings(forcats::fct_relevel(result_long,
                "Pass all",
                paste0("Passing (some non-", zf_long_text,"s)"),
                paste0("Passing (some ", zf_text, ")"),
                paste0("Failing (non-", zf_long_text,"s)"),
                paste0("Failing (some ", zf_text, ")"),
                paste0("Fail all (some non-", zf_long_text, "s)"),
                paste0("Fail all (total ", zf_text,")"))),
            result = suppressWarnings(forcats::fct_relevel(result, # why would this work better than above??
                "Pass",
                paste0("Non-", zf_long_text),
                paste("Partial", zf_text),
                paste("Total", zf_text))))

    if (include_grades) {
        return(df_out)
    } else {
        return(df_out |> select(-grades))
    }
}
