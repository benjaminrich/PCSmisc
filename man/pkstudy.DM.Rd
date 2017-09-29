\name{pkstudy.DM}
\alias{pkstudy.DM}
\docType{data}
\title{
Demographic Data from an Imaginary PK Study
}
\description{
This \code{data.frame} contains data on subject demographics from an imaginary PK study.
}
\usage{data(pkstudy.DM)}
\format{
  A data frame with 12 observations on the following 8 variables.
  \describe{
    \item{\code{studyid}}{a factor with levels \code{XXX-YYY-ZZZ}}
    \item{\code{domain}}{a factor with levels \code{DM}}
    \item{\code{subjid}}{a factor with levels \code{001-001} \code{001-002} \code{001-003} \code{001-004} \code{001-005} \code{001-006} \code{002-001} \code{002-002} \code{002-003} \code{002-004} \code{002-005} \code{002-006}}
    \item{\code{rfdtc}}{a factor.  The reference date/time in \code{yyyy-mm-ddTHH:MM} format.}
    \item{\code{age}}{a numeric vector}
    \item{\code{ageu}}{a factor with levels \code{YEARS}}
    \item{\code{sex}}{a factor with levels \code{F} \code{M}}
    \item{\code{race}}{a factor with levels \code{ASIAN/PACIFIC ISLANDER} \code{BLACK OR AFRICAN AMERICAN} \code{WHITE}}
  }
}
\details{
This data is part of a toy database from an imaginary PK study.  The
data is meant to resemble a standard CDISC format.  Briefly,
\code{pkstudy.DM} contains demographic data, \code{pkstudy.PC}
contains pharmacokinetic concentrations, \code{pkstudy.EX} contains
exposure data, \code{pkstudy.VS} contains vitals signs,
\code{pkstudy.LB} contains laboratory tests, and \code{pkstudy.CM}
contains data on concomitant medications.  All the data are linked by
a \code{subjid} field, the ID that identifies the individual subjects
in the study.
}
\seealso{
pkstudy.PC
pkstudy.EX
pkstudy.VS
pkstudy.LB
pkstudy.CM
}
\examples{
data(pkstudy.DM)
}
\keyword{datasets}
