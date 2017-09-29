\name{is.sorted}
\alias{is.sorted}
\title{Check Whether Vectors are Sorted}
\description{
  Check whether one or more vectors are sorted.  In the case of more than one vector,
  the function checks if each vector is sorted within blocks of the preceeding vector(s).
}
\usage{
is.sorted(...)
}
\arguments{
  \item{...}{One or more atomic vectors, all of the same length.}
}
\details{
  If called with a single argument \code{x}, this function simply returns \code{!\link{is.unsorted}(x)}.  If called with
  more than one argument, then \code{\link{order}} is used on all arguments, and the result is compared to \code{1:n}
  where \code{n} is the common length of all the arguments.

  Thus, in the multiple vector case, the ordering is defined by the nested structure of the vectors (See example).
}
\value{
A logical.
}
\author{Ben Rich <benjamin.rich@certara.com>}

\seealso{
  \code{\link{is.unsorted}}
  \code{\link{order}}
}
\examples{
require(nlme)
data(Phenobarb)
dat <- Phenobarb[1:56,]  # First 4 subjects
dat$id <- asID(dat$Subject)

attach(dat)

is.sorted(id)        # TRUE  - data are sorted by id
is.sorted(time)      # FALSE - data are NOT sorted by time
is.sorted(id, time)  # TRUE  - within id blocks, data are sorted by time

detach(dat)
}

\keyword{ utilities }

% vim: tw=70 sw=2 ts=2 et :
