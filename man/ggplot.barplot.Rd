\name{ggplot.barplot}
\alias{ggplot.barplot}
\title{Create a nice plot for Bar Plots...}
\usage{ggplot.barplot(panel, spine=FALSE, ...)}
\description{Create a nice plot for Bar Plots
Create a nice looking plot complete with axes using ggplot.}
\author{Barret Schloekre schloerke [at] gmail.com}
\keyword{hplot}
\arguments{\item{data}{plot to display, object created by \code{dd_load()}}
\item{spine}{(not implemented currently) whether to display the barchart as a spine plot}
\item{other}{arguments passed through to the ggplot function}}
\examples{ggplot(dd_example("barchart-species"))}
