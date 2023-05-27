\name{mapping}
\alias{mapping}
\title{Generate a Mapping Function}
\description{
  This function returns a function that does a simple mapping from one set of value to another.
  It is a function-generating function.
}
\usage{
mapping(from, to, ch.as.fact = TRUE)
}
\arguments{
  \item{from}{A vector.  This is the domain of the function.}
  \item{to}{A vector of the same length as \code{from}.}
  \item{ch.as.fact}{A logical.  Should the mapping return a
  \code{factor} instead of \code{character}?}
}
\details{

  This function returns a function.  When called with a vector
  argument \code{x}, this function will return a vector \code{y} of
  the same length as \code{x} and such that each element \code{y[i]}
  is equal to \code{to[j]} where \code{j} is the smallest integer such
  that \code{from[j] == x[i]}, and \code{NA} if no such \code{j}
  exists.

}
\value{
  A function that translates from \code{from} to \code{to}.  The
  function also has a attribute \code{"inverse"} which is a function
  that performs the inverse mapping.
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\examples{

sex.mapping <- mapping(c("Female", "F", "Male", "M"), c(0, 0, 1, 1))
sex.mapping(c("Female", "Female", "Male", "F"))

sex.inverse.mapping <- attr(sex.mapping, "inverse")
sex.inverse.mapping(c(0, 0, 1, 0))

race.mapping <- mapping(c(1, 2, 5), c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE"))
race.mapping(1:5)

}
\keyword{ utilities }

% vim: tw=70 sw=2
