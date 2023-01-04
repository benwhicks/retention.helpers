## code to prepare `sample data` dataset goes here

toy_student_course <-
    readxl::read_excel('data-raw/sample_data.xlsx',
                       sheet = 'student_course')
toy_enrolments <-
    readxl::read_excel('data-raw/sample_data.xlsx',
                       sheet = 'enrolments')
toy_academic <-
    readxl::read_excel('data-raw/sample_data.xlsx',
                       sheet = 'academic')

usethis::use_data(toy_student_course, overwrite = TRUE)
usethis::use_data(toy_enrolments, overwrite = TRUE)
usethis::use_data(toy_academic, overwrite = TRUE)
