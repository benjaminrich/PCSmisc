\name{pkstudy.PC}
\alias{pkstudy.PC}
\docType{data}
\title{
Pharmacokinetic Concentration Data from an Imaginary PK Study
}
\description{
This \code{data.frame} contains data on pharmacokinetic concentration from an imaginary PK study.
}
\usage{data(pkstudy.PC)}
\format{
  A data frame with 112 observations on the following 12 variables.
  \describe{
    \item{\code{studyid}}{a factor with levels \code{XXX-YYY-ZZZ}}
    \item{\code{domain}}{a factor with levels \code{PC}}
    \item{\code{subjid}}{a factor with levels \code{001-001} \code{001-002} \code{001-003} \code{001-004} \code{001-005} \code{001-006} \code{002-001} \code{002-002} \code{002-003} \code{002-004} \code{002-005} \code{002-006}}
    \item{\code{pctest}}{a factor with levels \code{DRUG-X} \code{METABOLITE-Y}}
    \item{\code{pcorres}}{a factor. The result in original units.}
    \item{\code{pcorresu}}{a factor with levels \code{ng/mL}}
    \item{\code{pcstresc}}{a factor.  The result in standard units.}
    \item{\code{pcstresn}}{a numeric vector}
    \item{\code{pcstresu}}{a factor with levels \code{ng/mL}}
    \item{\code{visit}}{a factor with levels \code{PERIOD I} \code{PERIOD II}}
    \item{\code{pctpt}}{a factor with levels \code{0.5 HOURS POST-DOSE} \code{1.0 HOURS POST-DOSE} \code{12 HOURS POST-DOSE} \code{2 HOURS POST-DOSE} \code{4 HOURS POST-DOSE} \code{6 HOURS POST-DOSE} \code{8 HOURS POST-DOSE} \code{PRE-DOSE}}
    \item{\code{pcdtc}}{a factor.  The date/time of collection in \code{yyyy-mm-ddTHH:MM} format.}
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
pkstudy.EX
pkstudy.VS
pkstudy.LB
pkstudy.CM
}
\examples{
data(pkstudy.PC)
}
\keyword{datasets}

% vim: tw=70 sw=2
