\name{blk.changeFromBaseline}
\alias{blk.changeFromBaseline}
\alias{blk.relativeTime}
\title{Compute Change from Baseline and Relative Time}
\description{
  These functions compute change from baseline in a \code{\link{block-format}} data set.
}
\usage{
blk.changeFromBaseline(x, id, baseline = blk.singleValue(x, id, ind = bl.ind, select = bl.select),
                       bl.ind = NULL, bl.select = "first", diff.op = percentChange, ...)

blk.relativeTime(time, id, baseline = blk.singleValue(time, id, ind=bl.ind, select=bl.select),
                 bl.ind = NULL, bl.select = "first", diff.op = difftime.default, ...)
}
\arguments{
  \item{x, time}{A vector in \code{\link{block-format}} with respect to \code{id}.}
  \item{id}{A valid \code{\link{block-format}} ID.}
  \item{baseline}{A vector of basline values or times, one for each level of \code{id}.}
  \item{bl.ind}{A logical vector that indicates elements of \code{x} or \code{time} used for baseline specification.}
  \item{bl.select}{How to select a single baseline from a vector of possibilities.}
  \item{diff.op}{A function that computes differences in \code{x} or \code{time}.}
  \item{...}{Further arguments passed to \code{diff.op}.}
}
\details{

  A unique baseline value for each level of \code{id} is required.  The
  baseline values can be specified in two ways: directly as a vector of
  length \code{nlevels{id}}, or indirectly by the arguments \code{bl.ind} and
  \code{bl.select} which are passed to \code{\link{blk.singleValue}}.

  Relative time refers to change from baseline in a time
  variable, so there are two functions which are essentially identical,
  except for the default value of \code{diff.op}.  By default,
  \code{blk.changeFromBaseline} computes the \emph{percent} change from baseline.
  When applied to date/time objects (class \code{lind{POSIXt}}), the default
  \code{diff.op} for \code{blk.relativeTime} computes relative time in hours.

}
\value{
  A vector in \code{\link{block-format}} with respect to \code{id} containing
  the change from baseline (or relative time) for \code{x} (or \code{time}).
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\seealso{
  \itemize{
  \item \code{\link{block-format}}
  \item \code{\link{blk.singleValue}}
  \item \code{\link{difftime.default}}
  }
}
\examples{
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects


cbind(dat, PCT.CHG=blk.changeFromBaseline(dat$conc, asID(dat$Subject), bl.ind=(!is.na(dat$conc))))


# Relative time from calendar date and clock time
# -----------------------------------------------

# This data already has relative time, so for this example we will
# re-create a calendar date and clock time from made-up baseline times.

baseline <- strptime(c("2005-04-13 8:05:00", "2005-04-16 7:59:00", "2005-03-31 9:00:00", "2005-04-19 8:35:00"), format="\%F \%T")

# Back-calcuate the calendar date and clock time
dat$time2 <- blk.repeatValue(baseline, id2=asID(dat$Subject)) + as.difftime(dat$time, units="hours")

# Now, recalcuate the relative time assuming that only time2 was specified
cbind(dat, REL.TIME=blk.relativeTime(dat$time2, asID(dat$Subject), bl.ind=(!is.na(dat$dose))))


}
\keyword{ utilities }

% vim: tw=70 sw=2
