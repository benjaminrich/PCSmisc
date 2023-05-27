\name{percentChange}
\alias{percentChange}
\title{Compute Percent Change}
\description{
    A convenience function for computing percent difference of percent change.
}
\usage{
percentChange(x, y)
}
\arguments{
    \item{x, y}{Numeric vectors.}
}
\value{
    \code{100*(x - y) / y}
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\keyword{ utilities }

% vim: tw=70 sw=2
