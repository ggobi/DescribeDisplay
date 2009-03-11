\name{ggplot.histogram}
\alias{ggplot.histogram}
\title{Create a nice plot...}
\usage{ggplot.histogram(data, spine=FALSE, ...)}
\description{Create a nice plot
Create a nice looking plot complete with axes using ggplot.}
\author{Barret Schloekre schloerke [at] gmail.com}
\keyword{hplot}
\arguments{\item{data}{plot to display, object created by \code{dd_load()}}
\item{spine}{whether the display the barchart as a spine}
\item{other}{arguments passed to the grob function}}
\examples{ggplot(dd_example("barchart"))}
