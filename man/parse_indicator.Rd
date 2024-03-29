% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save.R
\name{parse_indicator}
\alias{parse_indicator}
\title{Parse a UKFSR indicator id into its components}
\usage{
parse_indicator(indicator_id)
}
\arguments{
\item{indicator_id}{A valid UKFSR indicator id}
}
\value{
A named list of the indicator id components
}
\description{
Parse a UKFSR indicator id into its components
}
\examples{
i <- parse_indicator("3.2.1b")
i$theme
i$section
i$indicator
i$variant
}
\seealso{
\code{\link[=check_indicator]{check_indicator()}} tests whether an indicator id is valid
}
