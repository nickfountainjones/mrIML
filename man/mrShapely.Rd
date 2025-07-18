% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mrShapely.R
\name{mrShapely}
\alias{mrShapely}
\title{Generate SHAP (SHapley Additive exPlanations) Plots for Multiple Models
and Responses}
\usage{
mrShapely(
  mrIML_obj,
  taxa = NULL,
  kind = "beeswarm",
  max_display = 15L,
  plot_feature_effects = TRUE,
  plot_dependencies = TRUE,
  plot_2D_dependencies = TRUE
)
}
\arguments{
\item{mrIML_obj}{A list object output by \code{\link[=mrIMLpredicts]{mrIMLpredicts()}}.}

\item{taxa}{An optional character vector specifying which responses to include.}

\item{kind}{A character string passed to \code{\link[shapviz:sv_importance]{shapviz::sv_importance()}} specifying
the type of plot parameter (e.g., "beeswarm" for feature effect plot, "bar" for
variable importance plot, or "both").}

\item{max_display}{An integer passed to \code{\link[shapviz:sv_importance]{shapviz::sv_importance()}} specifying
the maximum number of features to display.}

\item{plot_feature_effects}{A logical indicating whether to generate feature
effect plots (default is TRUE).}

\item{plot_dependencies}{A logical indicating whether to generate dependency
plots (default is TRUE).}

\item{plot_2D_dependencies}{A logical indicating whether to generate
interaction plots (default is TRUE).}
}
\value{
A list object where the first element returns the SHAP results, and the
following elements contain the feature-effect, 1D-dependencies, and 2D-dependencies
if they were set to TRUE in the input.
}
\description{
This function generates SHAP (SHapley Additive exPlanations) plots for
multiple models and responses.
}
\examples{
\dontshow{if (identical(Sys.getenv("NOT_CRAN"), "true")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
mrIML_model <- mrIML::mrIML_bird_parasites_RF

shapely_plots_list <- mrShapely(mrIML_model, plot_2D_dependencies = FALSE)
\dontshow{\}) # examplesIf}
}
