\name{write.nonmem.csv}
\alias{write.nonmem.csv}
\alias{read.nonmem.csv}
\alias{read.nonmem.table}
\title{Read/Write NONMEM Data}
\description{
  These convenience functions are simple wrappers to the standard read/write
  functions that make reading and writing NONMEM data files and tables
  easier and more consistent.
}
\usage{
  write.nonmem.csv(x, file, digits = 6, colnames.toupper = TRUE)
  read.nonmem.csv(file, colnames.tolower = TRUE)
  read.nonmem.table(file, colnames.tolower = TRUE)
}
\arguments{
  \item{x}{A \code{\link{data.frame}}.}
  \item{file}{A character string naming a file.}
  \item{digits}{An integer specifying the number of decimal digits to keep for number variables.}
  \item{colnames.toupper}{Should column names be converted to uppercase before writing?}
  \item{colnames.tolower}{Should column names be converted to lowercase after reading?}
}
\details{

  \code{write.nonmem.csv} calls \code{\link{write.csv}} after: (1)
  rounding all numeric columns according to \code{digits}; (2)
  converting all column names to uppercase
  (if \code{colnames.toupper = TRUE}, the default); and (3) pre-pending
  `\code{#}' to the name of the first column (so that NONMEM will
  ignore the line containing the variable names).

  \code{read.nonmem.csv} is for reading NONMEM datasets.  It call
  \code{\link{read.csv}} with \code{na.strings = c(".", "", "NA")} and
  then: (1) removing `\code{X.}' from the name of the first column if
  present (by default, \code{R} converts the `\code{#}' prefix to
  `\code{X.}'); and (2) converting all column named to lowercase (if
  \code{colnames.tolower = TRUE}, the default).

  \code{read.nonmem.table} is for reading table files produced by
  NONMEM `\code{$TABLE}' statements.  These tables typically look something like:
\preformatted{
TABLE NO.  1
 ID          DV          MDV         ETA1        ETA2        ETA3        ETA4        IPRED       CWRES       DV          PRED        RES         WRES
  1.0001E+04  0.0000E+00  1.0000E+00 -1.3065E-01 -5.8225E-01  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
  1.0001E+04  0.0000E+00  1.0000E+00 -1.3065E-01 -5.8225E-01  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00
  1.0001E+04  2.5200E+03  0.0000E+00 -1.3065E-01 -5.8225E-01  0.0000E+00  0.0000E+00  1.6347E+03  2.5442E+00  2.5200E+03  5.4237E+02  1.9776E+03  3.5474E+00
  1.0001E+04  0.0000E+00  1.0000E+00 -1.3065E-01 -5.8225E-01  0.0000E+00  0.0000E+00  4.1236E+02  0.0000E+00  0.0000E+00  5.3607E+02  0.0000E+00  0.0000E+00
  ...
}
  \code{read.nonmem.table} calls \code{\link{read.table}} with \code{skip = 1}, \code{header
  = TRUE} and \code{na.strings = c(".", "")} before converting all
  column named to lowercase (if \code{colnames.tolower = TRUE}, the
  default) and changing `\code{ID}' to a factor (with
  \code{\link{asID}}.

}
\value{
  \code{write.nonmem.csv} is called for its side-effects (returns an
  invisible `\code{NULL}').

  \code{read.nonmem.csv} and \code{read.nonmem.table} both return a
  \code{\link{data.frame}}.
}
\author{Ben Rich <benjamin.rich@certara.com>}

\seealso{
  \itemize{
  \item \code{\link{write.csv}}
  \item \code{\link{read.csv}} \code{\link{read.table}}
  \item \code{\link{tolower}} \code{\link{toupper}}
  \item \code{\link{asID}}
  }
}

\keyword{ utilities }

% vim: tw=70 sw=2 ts=2 et :
