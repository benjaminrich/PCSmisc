\name{pkstudy.VS}
\alias{pkstudy.VS}
\docType{data}
\title{
Vital Statistics Data from an Imaginary PK Study
}
\description{
This \code{data.frame} contains data on vital statistics from an imaginary PK study.
}
\usage{data(pkstudy.VS)}
\format{
  A data frame with 127 observations on the following 10 variables.
  \describe{
    \item{\code{studyid}}{a factor with levels \code{XXX-YYY-ZZZ}}
    \item{\code{domain}}{a factor with levels \code{VS}}
    \item{\code{subjid}}{a factor with levels \code{001-001} \code{001-002} \code{001-003} \code{001-004} \code{001-005} \code{001-006} \code{002-001} \code{002-002} \code{002-003} \code{002-004} \code{002-005} \code{002-006}}
    \item{\code{vstest}}{a factor with levels \code{DIASTOLIC BLOOD PRESSURE} \code{HEIGHT} \code{PULSE RATE} \code{SYSTOLIC BLOOD PRESSURE} \code{TEMPERATURE} \code{WEIGHT}}
    \item{\code{vsorres}}{a logical vector}
    \item{\code{vsorresu}}{a factor with levels \code{bpm} \code{C} \code{cm} \code{kg} \code{mmHg}}
    \item{\code{vsstresc}}{a numeric vector.  The result in standard units.}
    \item{\code{vsstresn}}{a numeric vector.  The result in standard units.}
    \item{\code{vsstresu}}{a factor with levels \code{bpm} \code{C} \code{cm} \code{kg} \code{mmHg}}
    \item{\code{vsdtc}}{a factor.  The time of collection in \code{yyyy-mm-ddTHH:MM} format.}
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
pkstudy.EX
pkstudy.LB
pkstudy.CM
}
\examples{
data(pkstudy.VS)
}
\keyword{datasets}

% vim: tw=70 sw=2
