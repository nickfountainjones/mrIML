% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mrVip.R
\name{mrVipPCA}
\alias{mrVipPCA}
\title{Principal Component Analysis of mrIML variable importance}
\usage{
mrVipPCA(mrVip_obj)
}
\arguments{
\item{mrVip_obj}{A list returned by \code{\link[=mrVip]{mrVip()}}.}
}
\value{
A list of PCA results:
\itemize{
\item \verb{$PCA_plot}: Side-by-side plots of the different response
models on the first two principal components (PCs) and a
Scree plot.
\item \verb{$PC_outliers}: A list of the models flagged as outliers
on at least one of the PCs.
\item \verb{$eigenvalues}: The eigenvalues associated with the
principal components.
\item \verb{$PC_scores}: The PC scores of each response model.
}
}
\description{
Principal Component Analysis of mrIML variable importance
}
\examples{
# Without bootstrap
mrIML_rf <- mrIML::mrIML_bird_parasites_RF

mrIML_rf_vip <- mrVip(mrIML_rf, taxa = "Plas")

vipPCA_results <- mrIML_rf_vip \%>\%
 mrVipPCA()
}
