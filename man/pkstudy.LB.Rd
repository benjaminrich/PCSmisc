\name{pkstudy.LB}
\alias{pkstudy.LB}
\docType{data}
\title{
Laboratory Test Data from an Imaginary PK Study
}
\description{
This \code{data.frame} contains data on laboratory tests from an imaginary PK study.
}
\usage{data(pkstudy.LB)}
\format{
  A data frame with 1038 observations on the following 11 variables.
  \describe{
    \item{\code{studyid}}{a factor with levels \code{XXX-YYY-ZZZ}}
    \item{\code{domain}}{a factor with levels \code{LB}}
    \item{\code{subjid}}{a factor with levels \code{001-001} \code{001-002} \code{001-003} \code{001-004} \code{001-005} \code{001-006} \code{002-001} \code{002-002} \code{002-003} \code{002-004} \code{002-005} \code{002-006}}
    \item{\code{lborres}}{a logical vector}
    \item{\code{lborresu}}{a factor with levels \code{\%} \code{/HPF} \code{/LPF} \code{g/dL} \code{mEq/L} \code{mg/dL} \code{U/L} \code{x10^3/uL} \code{x10^6/uL}}
    \item{\code{lbstresc}}{a numeric vector.  The result in standard units.}
    \item{\code{lbstresn}}{a numeric vector.  The result in standard units.}
    \item{\code{lbstresu}}{a factor with levels \code{\%} \code{/HPF} \code{/LPF} \code{g/L} \code{GI/L} \code{mmol/L} \code{TI/L} \code{U/L} \code{umol/L}}
    \item{\code{lbtest}}{a factor with levels \code{Albumin} \code{Alkaline Phosphatase} \code{ALT (SGPT)} \code{AST (SGOT)} \code{Atypical Lymphocytes} \code{Basophils} \code{Basophils (\%)} \code{Calcium (EDTA)} \code{Creatinine} \code{Eosinophils} \code{Eosinophils (\%)} \code{GGT} \code{Hematocrit} \code{Hemoglobin} \code{LDH} \code{Lymphocytes} \code{Lymphocytes (\%)} \code{Magnesium} \code{Monocytes} \code{Monocytes (\%)} \code{Neutrophils} \code{Neutrophils (\%)} \code{Phosphorus} \code{Platelets} \code{RBC} \code{Serum Bicarbonate} \code{Serum Chloride} \code{Serum Glucose} \code{Serum Potassium} \code{Serum Sodium} \code{Serum Uric Acid} \code{Total Abs. Neutrophil Count} \code{Total Bilirubin} \code{Total Protein} \code{Ur Blood} \code{Ur Glucose} \code{Ur Hyaline Casts/LPF} \code{Ur pH} \code{Ur Protein} \code{Ur RBC/HPF} \code{Ur Specific Gravity} \code{Ur Squamous Epithelial Cells} \code{Ur WBC/HPF} \code{Urea Nitrogen} \code{WBC}}
    \item{\code{visit}}{a factor with levels \code{PERIOD I} \code{PERIOD II}}
    \item{\code{lbdtc}}{a factor.  The date/time of collection in \code{yyyy-mm-ddTHH:MM} format.}
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
pkstudy.VS
pkstudy.CM
}
\examples{
data(pkstudy.LB)
}
\keyword{datasets}

% vim: tw=70 sw=2
