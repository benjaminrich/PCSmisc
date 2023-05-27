\name{blk.tad}
\alias{blk.tad}
\alias{blk.tad2}
\title{Compute Time After Dose}
\description{
  These functions compute the time after dose (TAD) for a PK data set in \code{\link{block-format}}.
}
\usage{
blk.tad(time, id, dose.ind, na.action = c("neg.time", "fill"), fill = NA, diff.op = difftime.default)

blk.tad2(time, id, dose.t, id2, na.action = c("neg.time", "fill"), fill = NA, diff.op = difftime.default)
}
\arguments{
  \item{time}{A vector of event times in \code{\link{block-format}} with respect to \code{id}.}
  \item{id,id2}{A valid \code{\link{block-format}} ID.}
  \item{dose.ind}{A logical vector in \code{\link{block-format}} with respect to \code{id}.
      Indicates rows corresponding to dose events.}
  \item{dose.t}{A vector of dose times in \code{\link{block-format}} with respect to \code{id2}.}
  \item{na.action}{How to handle events prior to the first dose.  See details.}
  \item{fill}{A value to use when no other value is appropriate.}
  \item{diff.op}{A function that subtracts one time from another.}
}
\details{

  These functions compute the TAD, the difference between the time of a PK
  observation and the time of the dosing event that most nearly precedes it.  
  
  There are two variants.  In the first variant, all event times are in a
  single vector and an indicator is used to distiguish between dose events and
  observation events.  In the second variant, dose times and observation times
  are in separate vectors, each with an associated \code{\link{block-format}}
  ID vector.  In the case where a dose time and observation time match exactly,
  in the first variant events occur in the order specified, while in the second
  variant it is always assumed that the observation comes first (i.e.
  observations precede doses at the same time).
  
  There are two ways of handling observations that occurs before the first
  dose.  If \code{na.action == "neg.time"} (the default), then the TAD values for
  these observations are negative, and their magnitudes are the time
  \emph{until} the first dose.  If \code{na.action == "fill"} then the value of
  \code{fill} is used to `fill in' the TAD for all such observations.

}
\value{
  A numeric vector of TAD values in \code{\link{block-format}} with respect to \code{id}.
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\seealso{
  \code{\link{tapply}}
  \code{\link{difftime}}
}
\examples{
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects
attach(dat)

cbind(dat, TAD=blk.tad(time, asID(Subject), !is.na(dose)))

detach(dat)

id <- gl(4, 6)
id2 <- gl(4, 2)
time <- rep(c(0, 1, 6, 12, 18, 24), 4)
dose.t <- rep(c(0, 12), 4)
data.frame(id=id, time=time, tad=blk.tad2(time, id, dose.t, id2))

}
\keyword{ utilities }

% vim: tw=70 sw=2

