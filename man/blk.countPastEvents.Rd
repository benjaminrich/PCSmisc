\name{blk.countPastEvents}
\alias{blk.countPastEvents}
\title{Count Past Events}
\description{

  Counts how many events occurred before the specified times in a
  \code{\link{block-format}} data set.

}
\usage{
blk.countPastEvents(time, id, event.t = time, id2 = id)
}
\arguments{
  \item{time}{A vector of times at which to determine the number of past events, in \code{\link{block-format}} with respect to \code{id}.}
  \item{id,id2}{A valid \code{\link{block-format}} ID.}
  \item{event.t}{A vector of times at which events occur, in \code{\link{block-format}} with respect to \code{id2}.}
}
\details{

    This function operates on data sets in \code{\link{block-format}}.
    The times at which ``events'' occur is specified.  For a given
    vecotr of times, this function counts how many events occurred
    prior to each of those times.

    For instance, one may wish to compute how many episodes of a given disease
    occurred prior to baseline time (\eqn{t=0}) for each individual.
}
\value{
    An integer vector in \code{\link{block-format}} with respect to
    \code{id}.  The value indicates how events occurred prior to the
    corresponding \code{time}.
}
\author{Ben Rich <benjamin.rich@certara.com>}

\seealso{
  \itemize{
  \item \code{\link{block-format}}
  \item \code{\link{blk.concomitant}}
  }
}
\examples{
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects
attach(dat)

cbind(dat, LAST.DOSE=blk.locf(dose, asID(Subject)))

detach(dat)
}
\keyword{ utilities }

% vim: tw=70 sw=2
