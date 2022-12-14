% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bars.R
\name{draw_bars}
\alias{draw_bars}
\title{Draw horizontal bar plot, possibly with error bars.}
\usage{
draw_bars(
  val,
  val_sd = NULL,
  val_lower = NULL,
  val_upper = NULL,
  var_names = NULL,
  x_lab = "",
  color = list(pos = "steelblue", neg = "grey"),
  title = NULL,
  subtitle = NULL
)
}
\arguments{
\item{val}{A numeric vector of model reliance.}

\item{val_sd}{A numeric vector of standard deviations of model reliance (with
the same length as \code{val}), if available. Alternatively, provide
\code{val_lower} and \code{val_upper} instead of \code{val_sd} to reflect
uncertainty in \code{val}.}

\item{val_lower}{A numeric vector of the lower bound of 95\% confidence
interval of model reliance (with the same length as \code{val}), if
available.}

\item{val_upper}{A numeric vector of the upper bound of 95\% confidence
interval of model reliance (with the same length as \code{val}), if
available.}

\item{var_names}{A factor or string vector of variable names (with the same
length as \code{val}). If a factor is provided, variables will be plotted
as ordered in \code{levels(var_names)}, otherwise variables will be ordered
by \code{val}. If unspecified, variables will be named 'X1', 'X2', etc.}

\item{x_lab}{Title of x-axis (optional).}

\item{color}{Color for bars with positive and negative values. Default is
steel blue for positive and grey for negative.}

\item{title}{Title of the bar plot (optional).}

\item{subtitle}{Subtitle of the bar plot (optional).}
}
\value{
Returns a ggplot object for the bar plot.
}
\description{
Draw horizontal bar plot, possibly with error bars.
}
\examples{
tests/test_bars.R
}
