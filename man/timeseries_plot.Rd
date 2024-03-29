% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeseries_plot.R
\name{timeseries_plot}
\alias{timeseries_plot}
\title{Timeseries Plot with standard formatting}
\usage{
timeseries_plot(
  data,
  date_size = 12,
  date_break = "month",
  date_label = "\%b-\%y",
  x_angle = 90,
  legend_text_size = 10,
  y_title_size = 10,
  y_unit = "mg/L",
  dates_range = date_range,
  ymin = 0,
  ymax = NA
)
}
\arguments{
\item{data}{tibble}

\item{date_size}{size of x-axis date labels}

\item{date_break}{date breaks to be used ("2 weeks")}

\item{date_label}{format of date label (\%b-\%y)}

\item{x_angle}{angle of x axis text}

\item{legend_text_size}{size of the text in the legend}

\item{y_title_size}{size of y_axis label}

\item{y_unit}{unit to display on y_axis heading}

\item{dates_range}{Range of dates to appear on x-axis}

\item{ymin}{Minimum value on y-axis}

\item{ymax}{Maximum value on y-axis}
}
\value{
a timeseries plot based on the scale and date range of the dataset
}
\description{
Timeseries Plot with standard formatting
}
\examples{
timeseries_plot(data)
}
