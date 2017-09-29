\name{nmissing}
\alias{nmissing}
\title{How Many Missing Values?}
\description{
  Returns the number of \code{NA}'s in a vector.
}
\usage{
nmissing(x)
}
\arguments{
  \item{x}{A vector.}
}
\value{
  An integer equal to \code{sum(is.na(x))}.
}
\author{Ben Rich <benjamin.rich@certara.com>}

\keyword{ utilities }

% vim: tw=70 sw=2 ts=2 et :
