% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{filterRareCommon}
\alias{filterRareCommon}
\title{Filter rare response variables from the data}
\usage{
filterRareCommon(X, lower = lower, higher = higher)
}
\arguments{
\item{X}{is a data.frame with rows as sites or individuals or populations and
columns as loci or species OTUs.}

\item{lower}{is the lower threshold value  in which response varialkes are
removed from the data.frame.}

\item{higher}{is the upper threshold value  in which response varialkes are
removed from the data.frame.}
}
\value{
A filtered tibble.
}
\description{
Filter rare response variables from the data
}
\details{
This function allows you to remove response units (OTUs or SNPs or
species) from your response data as a preprocessing step. Suitable when the
response is a binary outcome.
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
X <- filterRareCommon(Responsedata, lower = 0.4, higher = 0.7)
\dontshow{\}) # examplesIf}
}
