## code to prepare `sample data` dataset goes here
library(tidyverse)
grade_fct_levels <- c("AW", "FNS", "FL", "PS", "CR", "DI", "HD")

toy_student_course <-
    readxl::read_excel('data-raw/sample_data.xlsx',
                       sheet = 'student_course')
toy_enrolments <-
    readxl::read_excel('data-raw/sample_data.xlsx',
                       sheet = 'enrolments')
toy_academic <-
    readxl::read_excel('data-raw/sample_data.xlsx',
                       sheet = 'academic') |>
    mutate(grade = grade |>
               fct_relevel(grade_fct_levels))
toy_academic_for_interventions <-
    readxl::read_excel('data-raw/sample_data.xlsx',
                       sheet = 'academic_for_interventions') |>
    mutate(grade = grade |>
               fct_relevel(grade_fct_levels))
toy_flags <-
    readxl::read_excel('data-raw/sample_data.xlsx',
                       sheet = 'flags')
toy_interventions <-
    readxl::read_excel('data-raw/sample_data.xlsx',
                       sheet = 'interventions')

usethis::use_data(toy_student_course, overwrite = TRUE)
usethis::use_data(toy_enrolments, overwrite = TRUE)
usethis::use_data(toy_academic, overwrite = TRUE)
usethis::use_data(toy_academic_for_interventions, overwrite = TRUE)
usethis::use_data(toy_flags, overwrite = TRUE)
usethis::use_data(toy_interventions, overwrite = TRUE)
