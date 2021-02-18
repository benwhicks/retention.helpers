# Functions around fetching augmented tables of data

#' Fetches as much student data as possible, preferencing the most recent
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
#' Given a course filtering this returns a data table with one row
#' per student per session with as much demographic, retention and academic
#' data as possible.
#'
#' You \emph{must} have the \strong{retention.data} package loaded for this
#' to work, or the equivalent tables.
#'
#' Some of the calculated fields are worth explaining:
#'
#' * \strong{progress_rate} is the ratio of passing grades (PS, CR, DI, HD) to all grades
#' * \strong{pass_rate} is the ratio of passing grades to all finalised grades
#' * \strong{fail_rate} is the ratio of failing grades (FL, FW) to all grades
#' * \strong{fw_rate} is the ratio of FW (fail by non submission) to all grades
#'
#' @param course_filter_string A regex expression used to filter on the course
#' @return a data frame, one row per student per session
#'
#' @export fetch_student_summary_from_course
fetch_student_summary_from_course <- function(course_filter_string = ".") {
  stu <- student_progress %>%
    dplyr::filter(
      stringr::str_detect(course, course_filter_string),
      course_enrolment_status == "Active Student") %>%
    dplyr::distinct(id, session, course) %>%
    dplyr::inner_join(
      student_demographics %>%
        dplyr::select(-firstname, -lastname, -postcode, -yob_approx),
      by = "id") %>%
    add_year_from_session()

  fla <- flags %>%
    dplyr::inner_join(stu %>% dplyr::distinct(id, session), by = c("id", "session"))

  # con <- contact %>%
  #   inner_join(stu %>% distinct(id, session), by = c("id", "session"))
  #
  # # imputing flags - is this the other issue?? Early low engagement missing??
  # fla <- fla %>%
  #   bind_rows(con %>% anti_join(fla, by = c("id", "session")))

  aca <- academic %>%
    dplyr::inner_join(stu %>% dplyr::distinct(id, session), by = c("id", "session")) %>%
    dplyr::left_join(
      offerings %>%
        dplyr::group_by(subject, session) %>%
        dplyr::summarise(heppp = any(pre_census_focus)),
      by = c("subject", "session")) %>%
    dplyr::mutate(heppp = tidyr::replace_na(heppp, FALSE))

  aca_summary <- aca %>%
    dplyr::mutate(
      pass = stringr::str_detect(grade, "PS|CR|DI|HD"),
      fail = stringr::str_detect(grade, "FL|FW"),
      fw = stringr::str_detect(grade, "FW"),
      finalised = stringr::str_detect(grade, "FW|FL|PS|CR|DI|HD")) %>%
    dplyr::group_by(id, session) %>%
    dplyr::summarise(
      progress_rate = mean(pass, na.rm = T),
      pass_rate = if_else(sum(finalised) == 0, NA_real_, sum(pass) / sum(finalised)),
      fail_rate = mean(fail, na.rm = T),
      fw_rate = mean(fw, na.rm = T),
      finalised_grades = sum(finalised),
      grades = paste0(sort(grade), collapse = ", "))

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
