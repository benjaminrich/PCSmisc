\name{mixedData}
\alias{mixedData}
\alias{unique.mixedData}
\alias{as.character.mixedData}
\alias{as.double.mixedData}
\alias{as.data.frame.mixedData}
\alias{print.mixedData}
\title{A Class for Mixed Character and Numeric Data}
\description{
  The purpose of this class is to represent data that is essentially
  numeric, but also included some `special' values that are
  non-numeric and hence the object cannot
  cannot be represented by a standard \code{numeric}
  class.  A typical example would be an assay result
  that used a symbol like \code{"BQL<(1.00)"} to represent
  left-censored values.
}
\usage{
mixedData(x)
\method{unique}{mixedData}(obj)
\method{as.character}{mixedData}(obj)
\method{as.double}{mixedData}(obj, fill = NA)
\method{as.data.frame}{mixedData}(obj, ...)
\method{print}{mixedData}(obj)
}
\arguments{
  \item{x}{A vector containing mixed data (typically \code{character} or \code{factor}).}
  \item{obj}{An object of class \code{mixedData}.}
  \item{fill}{A value to use when no other value is appropriate. See details.}
  \item{...}{Additional arguments to be passed to or from methods.}
}
\details{
  The class implements just a few methods.  The
  main uses are:
  \itemize{
  \item \code{as.numeric} to avoid the warning issued when calling
  this method on type \code{character} and the danger of calling it on
  type \code{factor} (see Examples).
  \item \code{unique}, which considers all numeric values as
  \code{"<Number>"} and hence allows one to quickly identify all
  the `special' (non-numeric) values are.
  }
  The \code{print} method just prints the character representation but
  with \code{quote = FALSE} for readability.
}
\value{
  \code{mixedData} returns a new object of class \code{mixedData}.

  The methods return what one would expect: \code{as.charater},
  \code{unique} and \code{print} return \code{character},
  \code{as.double} returns \code{numeric}.
}
\author{Benjamin Rich <mail@benjaminrich.net>}

\examples{
# Generate a vector with approx. 5% left-censoring:
set.seed(123)
x <- rexp(350, 0.05129329)
x <- ifelse(x < 10, "BQL<(1.00)", as.character(signif(x, 4)))
# Another 'special' result, 'quantity not sufficient':
x[129] <- "QNS"
x <- factor(x)

unique(x)  # Not very useful

# Warning! not the desired result when x is a factor:
as.numeric(x)

# This works, but you get a warning:
as.numeric(as.character(x))

y <- mixedData(x)
unique(y)
print(y)
as.numeric(y)
as.character(y)

}

\keyword{ utilities }

% vim: tw=70 sw=2 ts=2 et :
