% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchers.R
\name{fetch_students}
\alias{fetch_students}
\title{Fetches as much student data as possible, preferencing the most recent}
\usage{
fetch_students()
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#maturing}{\figure{lifecycle-maturing.svg}{options: alt='[Maturing]'}}}{\strong{[Maturing]}}

Grabs the most data possible on as many students as possible.
For time dependent data (as in the student_progress table)
it grabs the latest non NA data. Note that this \emph{only} works
if the \code{student_ids}, \code{student_demographics} and \code{student_progress}
tables are loaded, either through the \strong{retention.data} package
or the .rda files.
}
