\name{difftime.days}
\alias{difftime.days}
\title{Compute Time Difference In Days}
\description{
  Computes the number of days between two vectors of class Date or 
  POSIXt.  In the latter case, the time portion is ignored.
}
\usage{
difftime.days(time1, time2)
}
\arguments{
  \item{time1, time2}{Two vectors of the same length, both either of class Date or POSIXt.}
}
\details{
  This function considers the calendar date portion of a POSIXt object only.  Thus, a day need not correspond
  to a 24 hour period (see example).
}
\value{
  An integer vector containing the number of days between time1 and time2.
}
\author{Ben Rich <benjamin.rich@certara.com>}

\seealso{
  \code{\link{difftime.default}}
  \code{\link{difftime}}
}
\examples{
\dontrun{
time1 <- strptime("2011-12-22 00:03", "%Y-%m-%d %H:%M")
time2 <- strptime("2011-12-22 00:01", "%Y-%m-%d %H:%M")
time3 <- strptime("2011-12-21 23:59", "%Y-%m-%d %H:%M")
time4 <- strptime("2011-12-20 00:01", "%Y-%m-%d %H:%M")
difftime.days(time1, time2)  # 2 minutes difference, on the same day so considered 0 days 
difftime.days(time2, time3)  # Also 2 minutes difference but on different days, so considered one day
difftime.days(time3, time4)  # Although only 47 hours and 58 minutes difference, still considered one day
}
}
\keyword{ utilities }

% vim: tw=70 sw=2 ts=2 et :
