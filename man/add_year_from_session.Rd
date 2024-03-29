% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mungers.R
\name{add_year_from_session}
\alias{add_year_from_session}
\title{add year from session}
\usage{
add_year_from_session(d)
}
\arguments{
\item{d}{a data frame}
}
\value{
a data frame
}
\description{
@description \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
}
\details{
Takes a data frame with \strong{session} variable and returns the
same data frame with a \strong{year} variable. Requires the \strong{session}
to be in the format \emph{201930}
}
\seealso{
Other mungers: 
\code{\link{add_year_from_offering}()}
}
\concept{mungers}
