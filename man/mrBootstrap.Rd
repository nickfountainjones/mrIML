% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mrBootstrap.R
\name{mrBootstrap}
\alias{mrBootstrap}
\title{Bootstrap mrIML model predictions}
\usage{
mrBootstrap(mrIMLobj, num_bootstrap = 10, downsample = FALSE)
}
\arguments{
\item{mrIMLobj}{A list object output by \code{\link[=mrIMLpredicts]{mrIMLpredicts()}}.}

\item{num_bootstrap}{The number of bootstrap samples to generate
(default: 10).}

\item{downsample}{Logical. Should the bootstrap samples be downsampled?
(default: FALSE).}
}
\value{
A list containing bootstrap samples of variable profiles for each
response variable.
}
\description{
This function bootstraps model predictions and generates variable profiles
for each response variable.
}
\examples{
\dontshow{if (identical(Sys.getenv("NOT_CRAN"), "true")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# Specify a random forest tidy model
mrIML_rf <- mrIML::mrIML_bird_parasites_RF

#future::plan(multisession, workers = 4)

mrIML_bootstrap <- mrIML_rf \%>\%
  mrBootstrap(
    num_bootstrap = 50
  )
\dontshow{\}) # examplesIf}
}
