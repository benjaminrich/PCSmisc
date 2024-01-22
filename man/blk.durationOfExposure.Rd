\name{blk.durationOfExposure}
\alias{blk.durationOfExposure}
\title{Find Duration of Exposure}
\description{

  Finds the total (i.e., cumulative) duration of ``exposure''
  in a \code{\link{block-format}} data set.

}
\usage{
blk.durationOfExposure(begin.exposure, end.exposure, id, id2 = id, diff.op = "-", fill = 0)
}
\arguments{
  \item{begin.exposure}{A vectors of times delineating when the exposure begins, in \code{\link{block-format}} with respect to \code{id}.}
  \item{end.exposure}{Vectors of times delineating when the exposure ends, in \code{\link{block-format}} with respect to \code{id}.}
  \item{id,id2}{A valid \code{\link{block-format}} ID.}
  \item{diff.op}{A function that computes differences between times (i.e., \code{begin.exposure} and \code{end.exposure}).}
  \item{fill}{A value to use when no other value is appropriate.}
}
\details{

  The time intervals during which exposure occurs are specified by begin/end
  pairs (which cannot contain missing values).  This function determines
  whether the total (cumulative) time of exposure for each individual in a
  \code{\link{block-format}} data set.  Note that the time intervals may
  overlap.

}
\value{
  If \code{id2} is \code{NULL},
  a vector containing one value for
  each level of \code{id}.  Otherwise,
  a vectors in \code{\link{block-format}} with respect to \code{id2}.
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\seealso{
  \itemize{
  \item \code{\link{block-format}}
  \item \code{\link{blk.concomitant}}
  }
}
\examples{
}
\keyword{ utilities }

% vim: tw=70 sw=2

