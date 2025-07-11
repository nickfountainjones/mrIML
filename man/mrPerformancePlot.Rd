% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mrPerformancePlot.R
\name{mrPerformancePlot}
\alias{mrPerformancePlot}
\title{Plot Model Performance Comparison}
\usage{
mrPerformancePlot(
  ModelPerf1 = NULL,
  ModelPerf2 = NULL,
  mode = "classification"
)
}
\arguments{
\item{ModelPerf1, ModelPerf2}{Two data frames of model performance metrics to
compare. The data frames are created by \link{mrIMLperformance}, see \strong{Examples}.}

\item{mode}{A character string describing the mode of the models. Should be
either "regression" or "classification". The default is "classification".}
}
\value{
A list containing:
\itemize{
\item \verb{$performance_plot}: A box plot of model performance metrics.
\item \verb{$performance_diff_plot}: A bar plot of the differences in
performance metrics.
\item \verb{$performance_diff_df}: A data frame in wide format containing model
performance metrics and their differences.
}
}
\description{
Create visualizations to compare the performance of two models based on their
performance metrics generated by \link{mrIMLperformance}.
}
\examples{
library(parsnip)
library(dplyr)

data <- MRFcov::Bird.parasites
Y <- data \%>\%
  select(-scale.prop.zos) \%>\%
  select(order(everything()))
X <- data \%>\%
  select(scale.prop.zos)

# Specify a random forest tidy model
model_rf <- rand_forest(
  trees = 10, # 10 trees are set for brevity. Aim to start with 1000
  mode = "classification",
  mtry = tune(),
  min_n = tune()
) \%>\%
  set_engine("randomForest")
model_lm <- logistic_reg()

MR_perf_rf <- mrIMLpredicts(
  X = X,
  Y = Y,
  Model = model_rf,
  prop = 0.7,
  k = 2,
  racing = FALSE
) \%>\%
  mrIMLperformance()
MR_perf_lm <- mrIMLpredicts(
  X = X,
  Y = Y,
  Model = model_lm,
  prop = 0.7,
  k = 2,
  racing = FALSE
) \%>\%
  mrIMLperformance()

perf_comp <- mrPerformancePlot(
  ModelPerf1 = MR_perf_rf,
  ModelPerf2 = MR_perf_lm
)

}
