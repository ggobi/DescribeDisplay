\name{ggplot.parcoords}
\alias{ggplot.parcoords}
\title{Create a nice plot for parallel coordinates plot...}
\usage{ggplot.parcoords(data, absoluteX=FALSE, absoluteY=FALSE, lines=TRUE, ...)}
\description{Create a nice plot for parallel coordinates plot
Create a nice looking plot complete with axes using ggplot.}
\author{Barret Schloerke schloerke [at] gmail.com}
\keyword{hplot}
\arguments{\item{plot}{to display}
\item{absoluteX}{make the sections proportional horizontally to eachother}
\item{absoluteY}{make the sections proportional vertically to eachother}
\item{other}{arguments passed to the grob function}}
\examples{ggplot(dd_example("pcp"))
ggplot(dd_example("pcp"), size = 1)
ggplot(dd_example("pcp-ash"))
ggplot(dd_example("pcp-ash"),lines = FALSE)
ggplot(dd_example("pcp-ash"),absoluteX = TRUE)
ggplot(dd_example("pcp-ash"),absoluteY = TRUE)
ggplot(dd_example("pcp-ash"),absoluteX = TRUE, absoluteY = TRUE)
ggplot(dd_example("pcp-texture"))
ggplot(dd_example("pcp-texture"),lines=FALSE)
ggplot(dd_example("pcp-texture"),absoluteY=TRUE,lines=FALSE)}
