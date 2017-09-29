\name{blk.locf}
\alias{blk.locf}
\alias{blk.locf2}
\alias{blk.nocb}
\title{Last Observation Carried Forward}
\description{
    Performs last observation carried forward (LOCF) imputation on a column of data.
}
\usage{
blk.locf(x, id, na.action = c("fill", "carry.back"), fill = NA)
blk.locf2(time, id, observ.x, observ.t, id2 = id, na.action = c("fill", "carry.back"), fill = NA)
blk.nocb(x, id, na.action = c("fill", "carry.forward"), fill = NA)
}
\arguments{
  \item{x}{A vector in \code{\link{block-format}} with respect
  to \code{id}, containing both observed values and missing values,
  which must be ordered chronologically within each block.}
  \item{id,id2}{A valid \code{\link{block-format}} ID.}
  \item{time}{A vector of times at which to impute values, in \code{\link{block-format}} with respect to \code{id}.}
  \item{observ.x}{A vector of obseved values in \code{\link{block-format}} with respect to \code{id2}.}
  \item{observ.t}{A vector of observation times corresponding to \code{observ.x}, in \code{\link{block-format}} with respect to \code{id2}.}
  \item{na.action}{How to handle times prior to the first observation.  See details.}
  \item{fill}{A value to use when no other value is appropriate.}
}
\details{

  These functions operate on data sets in \code{\link{block-format}}.
  The simpler \code{blk.locf} fills in
  the missing values in \code{x} using the observed values.  Time is
  implicit; the time-ordering of \code{x} (within blocks of
  \code{id}) is required.

  For \code{blk.locf2} observations and corresponding times are
  specified separately from the times at which imputation is
  desired.  This function is essentially equivalent to
  \code{blk.nearestMatch} with \code{direction = "forward"} and
  \code{tol = Inf}.

  The parameter \code{na.action} is used to control the result
  when there is no previous non-missing value available.  With
  \code{na.action = "fill"} the valued specified with
  \code{fill} is used.  With \code{na.action = "carry.back"},
  the earliest available non-missing \emph{future} value is used.
  If there is no non-missing observed values for a level of
  \code{id}, the result is \code{NA} for that \code{id} at all time points.

  Less frequently used is next observation carried back (NOCB).
  Here, future observations are used to fill in the missing values
  at earlier times.

}
\value{
  A vector in \code{\link{block-format}} with respect to \code{id}.
}
\author{Ben Rich <benjamin.rich@certara.com>}

\seealso{
  \itemize{
  \item \code{\link{block-format}}
  \item \code{\link{blk.nearestMatch}}
  }
}
\examples{
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,] # First 4 subjects
dat <- dat[order(dat$Subject, dat$time),]
attach(dat)

cbind(dat, LOCF=blk.locf(conc, Subject))
cbind(dat, LOCF=blk.locf(conc, Subject, na.action="carry.back"))

detach(dat)
}
\keyword{ utilities }

% vim: tw=70 sw=2
