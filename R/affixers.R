# Functions that affix / join additional data to an existing data frame.
# They take in a data frame as the input, and the variables in the data
# frame help define the level of aggregation (grouping) in the output.

#' Attaches demographic summaries to a data frame
#'
#' @description `r lifecycle::badge('maturing')`
#'
#' Joins in aggregated key demographic data to an existing data frame.
#' The grouping of the aggregation is based on the existing data frame.
#' As it is aggregated the categorical variables are reduced to a count and
#' a relevant percentage of the key categorical value of interest.
#'
#' @param d a data frame
#' @export affix_demographics
#'
#' @examples
#' single_subject <- tibble(subject = "ACC100")
#' single_subject %>%
#'   affix_demographics()
#'
#' subject_and_session <- tibble(
#'                         subject = rep("ACC100", 2),
#'                         session = c(202030, 202060))
#' subject_and_session %>%
#'   affix_demographics()
#'
#' course_campus <- tibble(
#'    course = c("Bachelor of Occupational Therapy", "Bachelor of Occupational Therapy"),
#'    campus = c("Port Macquarie", "Albury-Wodonga"))
#'
#' course_campus %>%
#'   affix_demographics()
affix_demographics <- function(d) {
  d %>%
    dplyr::inner_join(
      dplyr::inner_join(
        enrolments %>%
          dplyr::filter(is.na(withdraw_date)) %>%
          add_subject_from_offering() %>%
          dplyr::select(id, session, subject, offering) %>%
          dplyr::left_join(
            subjects,
            by = "subject") %>%
          dplyr::left_join(
            student_progress %>%
              group_by(id, session) %>%
              filter(timestamp == max(timestamp)) %>%
              ungroup() %>%
              select(id, session, course, course_faculty, course_level, commencing, campus),
            by = c("id", "session"))
          ,
        student_demographics %>%
          dplyr::select(id, domesticity, atsi, parental_education, ses, remoteness),
        by = "id")
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(names(d)))) %>%
    dplyr::summarise(
      n = dplyr::n(),
      domestic = sum(domesticity == "Domestic"),
      domestic_p =
        sum(domesticity == "Domestic") /
        sum(domesticity != "Unknown"),
      first_nations = sum(atsi == "Australian Indigenous"),
      first_nations_p =
        sum(atsi == "Australian Indigenous") /
        sum(stringr::str_detect(atsi, "Australian")),
      low_ses = sum(ses == "Low SES"),
      low_ses_p =
        sum(ses == "Low SES") /
        sum(stringr::str_detect(ses, "SES")),
      regional_or_remote = sum(stringr::str_detect(remoteness, "Regional|Remote")),
      regional_or_remote_p =
        sum(stringr::str_detect(remoteness, "Regional|Remote")) /
        sum(stringr::str_detect(remoteness, "Regional|Remote|Cities")),
      parents_no_uni = sum(parental_education == "Not University Level"),
      parents_no_uni_p =
        sum(parental_education == "Not University Level") /
        sum(stringr::str_detect(parental_education, "University"))
    )
}

# TODO: affix_names
# Very basic, just adds firstname lastname based on id

# TODO: affix_subject
# Adds subject info joined on offering. Will need to handle session and subject field being present or absent
# Maybe use a set diff on names to set a 'by' character vector

# TODO: affix_academic

# TODO: affix_flags

# TODO: affix_contact

# TODO: augment_* functions
# These will package together affixers to and adds to bulk out specific tables (activity being the main
# as it is minimal to save on storage)
