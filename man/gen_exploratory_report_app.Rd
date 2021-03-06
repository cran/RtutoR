% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_report_app.R
\name{gen_exploratory_report_app}
\alias{gen_exploratory_report_app}
\title{Launch a Shiny App for Exploratory Analysis}
\usage{
gen_exploratory_report_app(df)
}
\arguments{
\item{df}{Dataframe object for exploratory analysis}
}
\value{
Launches a Shiny App. Please refer to the ReadMe page of the App to
understand how the app works
}
\description{
Launch a Shiny App for Exploratory Analysis
}
\examples{
 \dontrun{
diamonds_sample <- ggplot2::diamonds[sample(1:nrow(ggplot2::diamonds),size=1000),]
gen_exploratory_report_app(diamonds_sample)}
}
