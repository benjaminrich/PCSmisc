\name{blk.nearestMatch}
\alias{blk.nearestMatch}
\title{Impute Values Using the Nearest Match}
\description{

  This function imputes the value of a quantity that would have been
  observed at a given time using the value that was observed nearest
  in time in a \code{\link{block-format}} data set.

  The imputation can be done forwards of backwards in time, and can be
  constrained such that the value is only imputed if the observation
  occurred within a certain time window.

}
\usage{
blk.nearestMatch(time, id, observ.x, observ.t, id2 = id, direction = c("forward", "backward", "both"), tol = Inf, fill = NA, na.rm = FALSE, diff.op = difftime.default)
}
\arguments{
  \item{time}{A vector of times at which to impute values, in \code{\link{block-format}} with respect to \code{id}.}
  \item{id,id2}{A valid \code{\link{block-format}} ID.}
  \item{observ.x}{A vector of obseved values in \code{\link{block-format}} with respect to \code{id2}.}
  \item{observ.t}{A vector of observation times corresponding to \code{observ.x}, in \code{\link{block-format}} with respect to \code{id2}.}
  \item{direction}{Should value be carried forward in time, backward in time, or both?}
  \item{tol}{A numeric vector of length 1 or 2.  The time window or
  tolerance for matching.  See details.}
  \item{fill}{A value to use when no other value is appropriate.}
  \item{na.rm}{Should missing value in \code{observ.x} be removed?}
  \item{diff.op}{A function that subtracts one time from another.}
}
\details{
  For a given list of times, finds the observed values that are closest in
  time, searching either backward in time or forward in time.  This is
  done for each block of \code{time} in a \code{\link{block-format}}
  data set.

  For a given time, a match is declared if an observation occurs
  within a specific time window.  The size of the time window is
  specified with the tolerance parameter \code{tol}.  If \code{tol}
  has length 2 then the first number is the tolerance for matching in
  the ``forward'' direction and the second number is the tolerance for
  matching in the ``backward'' direction; if \code{tol} has length 1
  then this is the tolerance for matching in both directions.

  If two or more observations that are equidistant
  from a \code{time} value according to \code{diff.op}, then one of
  the values is selected arbitrarily.  Forward matching value are
  selected in preference to backward matching values (when
  \code{direction == "both"}).
}
\value{
  A vector in \code{\link{block-format}} with respect to \code{id}
  containig the nearest match imputed values at times \code{time} from
  \code{observ.x}.
}
\author{Ben Rich <benjamin.rich@certara.com>}

\seealso{
  \itemize{
  \item \code{\link{block-format}}
  \item \code{\link{blk.locf2}}
  }
}
\examples{
}
\keyword{ utilities }

% vim: tw=70 sw=2
