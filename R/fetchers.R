# Functions around fetching augmented tables of data

#' Fetches as much student data as possible, preferencing the most recent
#'
#' @description `r lifecycle::badge('maturing')`
#'
#' Grabs the most data possible on as many students as possible.
#' For time dependent data (as in the student_progress table)
#' it grabs the latest non NA data. Note that this \emph{only} works
#' if the \code{student_ids}, \code{student_demographics} and \code{student_progress}
#' tables are loaded, either through the \strong{retention.data} package
#' or the .rda files.
#'
#' @export fetch_students
fetch_students <- function() {
  student_ids %>%
    dplyr::select(-user_id, -email) %>%
    dplyr::full_join(
      student_demographics %>%
        dplyr::select(-firstname, -lastname)
      , by = "id"
    ) %>%
    dplyr::full_join(
      student_progress %>%
        dplyr::arrange(desc(session), desc(timestamp)) %>%
        dplyr::select(-session, -timestamp) %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(across(everything(), ~.x[!is.na(.x)][1])) %>%
        dplyr::ungroup()
      , by = "id"
    ) %>%
    unique()
}


#' Gets a beefed up table of student data by session based on course
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Given a course filtering this returns a data table with one row
#' per student per session with as much demographic, retention and academic
#' data as possible. By default it only includes flags with the concerns:
#' \emph{course requirement, low activity, non submission} and \emph{prior performance}.
#' You \emph{must} have the \code{retention.data} package loaded for this
#' to work, or the equivalent tables.
#' Some of the calculated fields are worth explaining:
#' * \strong{progress_rate} is the ratio of passing grades (PS, CR, DI, HD) to all grades
#' * \strong{pass_rate} is the ratio of passing grades to all finalised grades
#' * \strong{fail_rate} is the ratio of failing grades (FL, FW) to all grades
#' * \strong{fw_rate} is the ratio of FW (fail by non submission) to all grades
#'
#' @param course_filter_string A regex expression used to filter on the course
#' @param concerns A list of options from for the concern field in the flags table.
#' @return a data frame, one row per student per session
#'
#' @export fetch_student_session_summary_from_course
fetch_student_session_summary_from_course <- function(course_filter_string = ".",
                                              concerns = c("course requirement",
                                                           "low activity",
                                                           "non submission",
                                                           "prior performance")) {
  stu <- student_progress %>%
    dplyr::filter(
      stringr::str_detect(course, course_filter_string),
      course_enrolment_status == "Active Student") %>%
    dplyr::distinct(id, session, course) %>%
    dplyr::inner_join(
      student_demographics %>%
        dplyr::select(-firstname, -lastname),
      by = "id") %>%
    add_year_from_session()

  fla <- flags %>%
    dplyr::filter(concern %in% concerns) %>%
    dplyr::inner_join(stu %>% dplyr::distinct(id, session), by = c("id", "session"))

  aca <- academic %>%
    dplyr::filter(grade != "TA") %>%
    dplyr::inner_join(stu %>% dplyr::distinct(id, session), by = c("id", "session")) %>%
    dplyr::left_join(
      offerings %>%
        dplyr::group_by(subject, session) %>%
        dplyr::summarise(heppp = any(pre_census_focus)),
      by = c("subject", "session")) %>%
    dplyr::mutate(heppp = tidyr::replace_na(heppp, FALSE))

  aca_summary <- aca %>%
    retention.helpers::add_grade_helpers() %>%
    dplyr::group_by(id, session) %>%
    dplyr::summarise(
      progress_rate = sum(grade_success, na.rm = T) / sum(grade_substantive, na.rm = T),
      npe_rate = sum(grade_npe, na.rm = T) / sum(grade_substantive, na.rm = T),
      n_fails = sum(grade_fail, na.rm = T),
      n_substantive_grades = sum(grade_substantive),
      grades = paste0(sort(grade), collapse = ", ")) |>
    left_join(
      aca |>
        retention.helpers::add_gpa_by(id, session),
      by = c('id', 'session')
    ) |>
    dplyr::mutate(dplyr::across(.fns = ~ifelse(is.infinite(.), NA, .))) |>
    ungroup()

  missed_by_heppp <- aca %>%
    dplyr::group_by(id, session) %>%
    dplyr::summarise(any_heppp = any(heppp)) %>%
    dplyr::filter(!any_heppp) %>%
    dplyr::mutate(not_watched = "No tracked subjects") %>%
    dplyr::select(-any_heppp)

  dat <- stu %>%
    dplyr::left_join(
      fla %>%
        group_by(id, session) %>%
        summarise(concern = str_c(concern, collapse = ", "),
                  flags = n()) %>%
        mutate(flagged = "At risk"),
      by = c("id", "session")) %>%
    dplyr::mutate(
      concern = tidyr::replace_na(concern, "not flagged"),
      flags = tidyr::replace_na(flags, 0),
      flagged = tidyr::replace_na(flagged, "Not at risk")) %>%
    dplyr::left_join(
      missed_by_heppp,
      by = c("id", "session")) %>%
    dplyr::mutate(flagged = coalesce(not_watched, flagged)) %>%
    dplyr::select(-not_watched) %>%
    dplyr::left_join(aca_summary, by = c("id", "session"))

  dat
}


#' Gets a beefed up table of student data by session based on a list of student ids
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Given a list of student ids this returns a data table with one row
#' per student per session with as much demographic, retention and academic
#' data as possible. By default it only includes flags with the concerns:
#' \emph{course requirement, low activity, non submission} and \emph{prior performance}
#'
#' You \emph{must} have the \code{retention.data} package loaded for this
#' to work, or the equivalent tables.
#'
#' Some of the calculated fields are worth explaining:
#'
#' * \strong{progress_rate} is the ratio of successful grades to substantive grades
#' * \strong{npe_rate} is the ratio of FW / FNS (fail by non submission) to all grades
#'
#' @param ids A character vector of student ids
#' @param concerns A list of options from for the concern field in the flags table.
#' @return a data frame, one row per student per session
#'
#' @export fetch_student_session_summary_from_ids
fetch_student_session_summary_from_ids <- function(ids,
                                              concerns = c("course requirement",
                                                           "low activity",
                                                           "non submission",
                                                           "prior performance")) {
  stu <- student_progress %>%
    dplyr::filter(
      id %in% ids,
      course_enrolment_status == "Active Student") %>%
    dplyr::distinct(id, session, course) %>% # fetches anyone who was active at some point in the session
    dplyr::inner_join(
      student_demographics %>%
        dplyr::select(-firstname, -lastname),
      by = "id") %>%
    add_year_from_session()

  fla <- flags %>%
    dplyr::filter(concern %in% concerns) %>%
    dplyr::inner_join(stu %>% dplyr::distinct(id, session), by = c("id", "session"))

  aca <- academic %>%
    dplyr::filter(grade != "TA") %>%
    dplyr::inner_join(stu %>% dplyr::distinct(id, session), by = c("id", "session")) %>%
    dplyr::left_join(
      offerings %>%
        dplyr::group_by(subject, session) %>%
        dplyr::summarise(heppp = any(pre_census_focus)),
      by = c("subject", "session")) %>%
    dplyr::mutate(heppp = tidyr::replace_na(heppp, FALSE))

  aca_summary <- aca %>%
    retention.helpers::add_grade_helpers() %>%
    dplyr::group_by(id, session) %>%
    dplyr::summarise(
      progress_rate = sum(grade_success, na.rm = T) / sum(grade_substantive, na.rm = T),
      npe_rate = sum(grade_npe, na.rm = T) / sum(grade_substantive, na.rm = T),
      n_fails = sum(grade_fail, na.rm = T),
      n_substantive_grades = sum(grade_substantive),
      grades = paste0(sort(grade), collapse = ", ")) |>
    left_join(
      aca |>
        retention.helpers::add_gpa_by(id, session),
      by = c('id', 'session')
    ) |>
    dplyr::mutate(dplyr::across(.fns = ~ifelse(is.infinite(.), NA, .))) |>
    ungroup()

  missed_by_heppp <- aca %>%
    dplyr::group_by(id, session) %>%
    dplyr::summarise(any_heppp = any(heppp)) %>%
    dplyr::filter(!any_heppp) %>%
    dplyr::mutate(not_watched = "No tracked subjects") %>%
    dplyr::select(-any_heppp)

  dat <- stu %>%
    dplyr::left_join(
      fla %>%
        group_by(id, session) %>%
        summarise(concern = str_c(concern, collapse = ", "),
                  flags = n()) %>%
        mutate(flagged = "At risk"),
      by = c("id", "session")) %>%
    dplyr::mutate(
      concern = tidyr::replace_na(concern, "not flagged"),
      flags = tidyr::replace_na(flags, 0),
      flagged = tidyr::replace_na(flagged, "Not at risk")) %>%
    dplyr::left_join(
      missed_by_heppp,
      by = c("id", "session")) %>%
    dplyr::mutate(flagged = coalesce(not_watched, flagged)) %>%
    dplyr::select(-not_watched) %>%
    dplyr::left_join(aca_summary, by = c("id", "session"))

  dat
}


#' Gets a beefed up table of student data by subject
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Given a subject filtering this returns a data table with one row
#' per student per subject per session with as much demographic, retention and academic
#' data as possible. By default it only includes flags with the concerns:
#' \emph{course requirement, low activity, non submission} and \emph{prior performance}
#'
#' You \emph{must} have the \code{retention.data} package loaded for this
#' to work, or the equivalent tables.
#'
#' @param subject_filter_string A regex expression used to filter on the course
#' @param concerns A list of options from for the concern field in the flags table.
#' @return a data frame, one row per student per subject per session
#'
#' @export fetch_student_subject_summary_from_subject
fetch_student_subject_summary_from_subject <- function(subject_filter_string = ".",
                                              concerns = c("course requirement",
                                                           "low activity",
                                                           "non submission",
                                                           "prior performance")) {
  stu <- enrolments %>%
    add_subject_from_offering() %>%
    dplyr::filter(
      stringr::str_detect(subject, subject_filter_string)) %>%
    # getting only the latest enrolment activity in the session
    dplyr::group_by(id, subject, session) %>%
    dplyr::filter(enrol_date == max(enrol_date)) %>%
    dplyr::distinct(id, session, subject) %>%
    dplyr::inner_join(
      student_demographics %>%
        dplyr::select(-firstname, -lastname),
      by = "id") %>%
    add_year_from_session()

  fla <- flags %>%
    dplyr::filter(concern %in% concerns) %>%
    dplyr::inner_join(
      stu %>%
        dplyr::distinct(id, session, subject),
      by = c("id", "session", "subject"))

  aca <- academic %>%
    dplyr::filter(grade != "TA") %>%
    dplyr::inner_join(
      stu %>%
        dplyr::distinct(id, session, subject),
      by = c("id", "session", "subject")) %>%
    dplyr::left_join(
      offerings %>%
        dplyr::group_by(subject, session) %>%
        dplyr::summarise(pre_census_focus = any(pre_census_focus)),
      by = c("subject", "session")) %>%
    dplyr::mutate(pre_census_focus = tidyr::replace_na(pre_census_focus, FALSE))


  dat <- stu %>%
    dplyr::left_join(
      fla %>%
        group_by(id, session, subject) %>%
        summarise(concern = str_c(concern, collapse = ", "),
                  flags = n()) %>%
        mutate(flagged = "At risk"),
      by = c("id", "session", "subject")) %>%
    dplyr::mutate(
      concern = tidyr::replace_na(concern, "not flagged"),
      flags = tidyr::replace_na(flags, 0),
      flagged = tidyr::replace_na(flagged, "Not at risk")) %>%
    dplyr::left_join(aca, by = c("id", "session", "subject"))

  dat
}



#' Gets a beefed up table of student data by subject from a list of ids
#'
#' @description `r lifecycle::badge('questioning')`
#'
#' Given list of student ids this returns a data table with one row
#' per student per subject per session with as much demographic, retention and academic
#' data as possible. By default it only includes flags with the concerns:
#' \emph{course requirement, low activity, non submission} and \emph{prior performance}
#'
#' You \emph{must} have the \code{retention.data} package loaded for this
#' to work, or the equivalent tables.
#'
#' @param ids A character vector of student ids
#' @param concerns A list of options from for the concern field in the flags table.
#' @return a data frame, one row per student per subject per session
#' @export fetch_student_subject_summary_from_ids
fetch_student_subject_summary_from_ids <- function(ids,
                                              concerns = c("course requirement",
                                                           "low activity",
                                                           "non submission",
                                                           "prior performance")) {
  # TODO: Need to adjust this to match student id filtering
  # TODO: Add export
  stu <- enrolments %>%
    add_subject_from_offering() %>%
    dplyr::filter(
      stringr::str_detect(subject, subject_filter_string)) %>%
    # getting only the latest enrolment activity in the session
    dplyr::group_by(id, subject, session) %>%
    dplyr::filter(enrol_date == max(enrol_date)) %>%
    dplyr::distinct(id, session, subject) %>%
    dplyr::inner_join(
      student_demographics %>%
        dplyr::select(-firstname, -lastname, -postcode, -yob_approx),
      by = "id") %>%
    add_year_from_session()

  fla <- flags %>%
    dplyr::filter(concern %in% concerns) %>%
    dplyr::inner_join(
      stu %>%
        dplyr::distinct(id, session, subject),
      by = c("id", "session", "subject"))

  aca <- academic %>%
    dplyr::filter(grade != "TA") %>%
    dplyr::inner_join(
      stu %>%
        dplyr::distinct(id, session, subject),
      by = c("id", "session", "subject")) %>%
    dplyr::left_join(
      offerings %>%
        dplyr::group_by(subject, session) %>%
        dplyr::summarise(pre_census_focus = any(pre_census_focus)),
      by = c("subject", "session")) %>%
    dplyr::mutate(pre_census_focus = tidyr::replace_na(pre_census_focus, FALSE))


  dat <- stu %>%
    dplyr::left_join(
      fla %>%
        group_by(id, session, subject) %>%
        summarise(concern = str_c(concern, collapse = ", "),
                  flags = n()) %>%
        mutate(flagged = "At risk"),
      by = c("id", "session", "subject")) %>%
    dplyr::mutate(
      concern = tidyr::replace_na(concern, "not flagged"),
      flags = tidyr::replace_na(flags, 0),
      flagged = tidyr::replace_na(flagged, "Not at risk")) %>%
    dplyr::left_join(aca, by = c("id", "session", "subject"))

  dat
}

#' Gets a overall demographic summary by subject
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Returns a summary of basic key demographics breakdown based on
#' a selection of subjects. Currently the key demographics selected by
#' this function are Low SES, Australian Indigenous, Regional and First in Family
#'
#' @param subject_string a regex expression used to filter the subjects
#' @param by_session if TRUE then returns one row per subject per session
#' @param sessions a numeric vector with the sessions that the data should be based on, greedy if left as NULL
#' @param include_withdrawn if set to FALSE then students that withdraw are removed from the results
#' @param domestic_only if TRUE then only domestic enrolments are used
#' @return a data frame, one row per subject (and per session if by session is TRUE)
#' @export fetch_subject_demographic_summary
fetch_subject_demographic_summary <- function(subject_string,
                                              by_session = TRUE,
                                              sessions = NULL,
                                              include_withdrawn = FALSE,
                                              domestic_only = TRUE) {
  stu_dat <- enrolments %>%
    add_subject_from_offering() %>%
    dplyr::filter(
      stringr::str_detect(subject, subject_string),
      ifelse(is.null(sessions), TRUE, session %in% sessions),
      ifelse(include_withdrawn, TRUE, is.na(withdraw_date))
      ) %>%
    dplyr::distinct(id, subject, session) %>%
    dplyr::inner_join(
      student_demographics %>%
        dplyr::select(id, domesticity, atsi, nesb, ses, remoteness, parental_education) %>%
        dplyr::filter(ifelse(domestic_only, domesticity == "Domestic", TRUE)),
      by = "id")

  if (by_session) {
    grp_dat <- stu_dat %>%
      dplyr::group_by(subject, session)
  } else {
    grp_dat <- stu_dat %>%
      dplyr::group_by(subject)
  }

   grp_dat %>%
    dplyr::summarise(
      low_ses = sum(ses == "Low SES"),
      low_ses_pc =
        sum(ses == "Low SES") /
        sum(stringr::str_detect(ses, "SES")),
      indigenous = sum(atsi == "Australian Indigenous"),
      indigenous_pc =
        sum(atsi == "Australian Indigenous") /
        sum(stringr::str_detect(atsi, "Indigenous")),
      regional = sum(stringr::str_detect(remoteness, "Regional|Remote")),
      regional_pc =
        sum(stringr::str_detect(remoteness, "Regional|Remote")) /
        sum(stringr::str_detect(remoteness, "Regional|Remote|Major")),
      first_in_family = sum(parental_education == "Not University Level"),
      first_in_family_pc =
        sum(parental_education == "Not University Level") /
        sum(stringr::str_detect(parental_education, "University"))
    )
}



#' Fetch prior attempts
#'
#' Given a subject and session finds current enrolments who have
#' previously attempted the subject
#'
#' @param enr data frame of enrolments
#' @param aca data frame of academic results
#' @param sub subject
#' @param sesh session
#'
#' @export fetch_prior_attempts
fetch_prior_attempts <- function(enr, aca, sub, sesh) {
  enr |>
    dplyr::filter(
      subject == sub,
      session == sesh,
      is.na(withdraw_date)) |>
    dplyr::distinct(id, subject) |>
    dplyr::inner_join(
      aca |>
        dplyr::select(id, subject, session, grade),
      by = c("id", "subject")) |>
    dplyr::filter(session < sesh) |>
    dplyr::arrange(id, session) |>
    dplyr::distinct()

}

