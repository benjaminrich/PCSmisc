\name{age.in.years}
\alias{age.in.years}
\title{Compute Age From Date-of-Birth}
\description{
  Computes the age of a person (in years) on a specified date from the
  person's date of birth.
}
\usage{
age.in.years(on.this.date, birth.date)
}
\arguments{
  \item{on.this.date}{A vector of \code{\link{Date}} objects.  The
  date on which the age is to be computed.}
  \item{birth.date}{A vector of \code{\link{Date}} objects.  The
  date-of-birth.}
}
\value{
  An integer vector containing the age in years.
}
\author{Ben Rich <benjamin.rich@certara.com>}

\seealso{
  \code{\link{difftime}}
}
\examples{
age.in.years(
  as.Date(c("2012-03-01", "2012-02-29", "2012-02-28")),
  as.Date(c("1980-02-03", "1980-02-29", "1980-05-28")))
}
\keyword{ utilities }

% vim: tw=70 sw=2 ts=2 et :
