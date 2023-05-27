\name{blk.concomitant}
\alias{blk.concomitant}
\title{Find Concomitant Events}
\description{

    Finds ``events'' (i.e. times) that are ``concomitant'' with (i.e. occur at
    the same time as) some given state (or ``exposure'') in a
    \code{\link{block-format}} data set.

}
\usage{
blk.concomitant(time, id, begin.exposure, end.exposure, id2 = id)
}
\arguments{
  \item{time}{A vector of times at which to determine the state, in \code{\link{block-format}} with respect to \code{id}.}
  \item{id,id2}{A valid \code{\link{block-format}} ID.}
  \item{begin.exposure}{A vectors of times delineating when the exposure begins, in \code{\link{block-format}} with respect to \code{id2}.}
  \item{end.exposure}{Vectors of times delineating when the exposure ends, in \code{\link{block-format}} with respect to \code{id2}.}
}
\details{

    Exposure is determined by start-time/end-time pairs.  This function
    determines whether the exposure is present or absent at specific times in a
    \code{\link{block-format}} data set.

    Sometimes, the time intervals may overlap.  The behaviour in that instance
    is to count the number of overlapping intevals.  For instance,

\preformatted{
Inteval 1:      |-----------------|
Inteval 2:             |------------------|
Time:       ^      ^       ^         ^        ^
Value:      0      1       2         1        0
}
    
    The end times may be missing, indicating open-ended intervals.  If all end
    times are missing, then the same result can be achieved with
    \code{\link{blk.countPastEvents}}.

}
\value{
    An integer vector in \code{\link{block-format}} with respect to \code{id}.
    The value indicates how many of the time intervals overlap with the corresponding \code{time}.
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\seealso{
  \itemize{
  \item \code{\link{block-format}}
  \item \code{\link{blk.countPastEvents}}
  }
}
\examples{
}
\keyword{ utilities }

% vim: tw=70 sw=2
