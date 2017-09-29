\name{pkstudy.EX}
\alias{pkstudy.EX}
\docType{data}
\title{
Drug Exposure Data from an Imaginary PK Study
}
\description{
This \code{data.frame} contains data on drug exposure from an imaginary PK study.
}
\usage{data(pkstudy.EX)}
\format{
  A data frame with 66 observations on the following 10 variables.
  \describe{
    \item{\code{studyid}}{a factor with levels \code{XXX-YYY-ZZZ}}
    \item{\code{domain}}{a factor with levels \code{EX}}
    \item{\code{subjid}}{a factor with levels \code{001-001} \code{001-002} \code{001-003} \code{001-004} \code{001-005} \code{001-006} \code{002-001} \code{002-002} \code{002-003} \code{002-004} \code{002-006}}
    \item{\code{extrt}}{a factor with levels \code{DRUG-X}}
    \item{\code{exdose}}{a numeric vector.  The dose given.}
    \item{\code{exdosu}}{a factor with levels \code{mg}}
    \item{\code{visit}}{a factor with levels \code{Treatment Period I} \code{Treatment Period II}}
    \item{\code{exstdtc}}{a factor.  The date/time of start of exposure in \code{yyyy-mm-ddTHH:MM} format.}
    \item{\code{exendtc}}{a factor.  The date/time of end of exposure in \code{yyyy-mm-ddTHH:MM} format.}
    \item{\code{exdy}}{a numeric vector.  The study day of drug exposure.}
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
pkstudy.DM
pkstudy.PC
pkstudy.VS
pkstudy.LB
pkstudy.CM
}
\examples{
data(pkstudy.EX)
}
\keyword{datasets}
