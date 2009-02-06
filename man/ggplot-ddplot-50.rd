\name{ggplot.ddplot}
\alias{ggplot.ddplot}
\alias{ggplot.dd}
\title{Create a nice plot}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Create a nice looking plot complete with axes using ggplot.
}
\usage{ggplot.ddplot(data, axis.location = c(0.2, 0.2), ...)}
\arguments{
\item{data}{plot to display}
\item{axis.location}{grob function to use for drawing}
\item{...}{other arguments passed to the grob function}
}

\details{}

\examples{ggplot(dd_example("edges"))
ggplot(dd_example("xyplot"))
ggplot(dd_example("edges")) + xlab(NULL) + ylab(NULL)}
\keyword{hplot}
