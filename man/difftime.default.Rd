\name{difftime.default}
\alias{difftime.default}
\alias{difftime.hours}
\title{Convenience Function for Computing Time Differences}
\description{
    Subtracts time2 from time1.  If both are of class POSIXt, gives the result in hours.
}
\usage{
difftime.default(time1, time2)
difftime.hours(time1, time2)
}
\arguments{
    \item{time1, time2}{Two time vectors of the same length, either both numeric of both of class POSIXt.}
}
\details{
  If time1 and time2 are both numeric then regular subtraction is used, but if
  both are of class POSIXt, then \code{\link{difftime}} is used and the result is
  converted into hours.

  \code{difftime.hours} does the same, but only operates on POSIXt objects.
}
\value{
    A numeric vector containing the time difference.
}
\author{Ben Rich <benjamin.rich@certara.com>}

\seealso{
    \code{\link{difftime}}
    \code{\link{blk.tad}}
    \code{\link{blk.relativeTime}}
}
\examples{
time1 <- strptime("2011-01-12 16:58", "\%Y-\%m-\%d \%H:\%M")
time2 <- strptime("2011-01-11 09:13", "\%Y-\%m-\%d \%H:\%M")
difftime.default(time1, time2)  # In hours
}
\keyword{ utilities }
