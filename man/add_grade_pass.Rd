% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mungers.R
\name{add_grade_pass}
\alias{add_grade_pass}
\title{add grade pass}
\usage{
add_grade_pass(d)
}
\arguments{
\item{d}{a data frame}
}
\value{
a data frame
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}}

Takes a data frame with \strong{grade} variable and returns the
same data frame with a \strong{grade_pass} variable, which is
true if grade is PS, CR, DI or HD; and false otherwise.
}
\keyword{internal}
