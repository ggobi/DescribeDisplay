\name{ggplot.ddplot}
\alias{ggplot.ddplot}
\title{Create a nice plot...}
\usage{ggplot.ddplot(data, axis.location=c(0.2, 0.2), ...)}
\description{Create a nice plot
Create a nice looking plot complete with axes using ggplot.}
\author{Hadley Wickham h.wickham [at] gmail.com}
\keyword{hplot}
\arguments{\item{data}{plot to display, object created by \code{dd_load()}}
\item{axis.location}{grob function to use for drawing}
\item{other}{arguments passed to the grob function}}
\examples{ggplot(dd_example("edges"))
ggplot(dd_example("xyplot"))
ggplot(dd_example("edges")) + xlab(NULL) + ylab(NULL)}
