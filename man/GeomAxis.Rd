\docType{data}
\name{GeomAxis}
\alias{geom_axis}
\alias{GeomAxis}
\title{GeomAxis
A special ggplot2 geom for drawing the tour axes}
\format{proto object 
 $ draw        :function (., data, scales, coordinates, location = c(0.2, 0.2), size = 0.9,  
    colour = "black", ...)   
 $ default_stat:function (.)   
 $ icon        :function (.)   
 $ objname     : chr "axis" 
 $ default_aes :function (.)   
 $ desc        : chr "Projection axes" 
 $ required_aes: NULL 
 $ new         :function (., data = NULL, stat = NULL, position = NULL, ...)   
 parent: proto object 
 .. parent: proto object}
\usage{
  GeomAxis
}
\arguments{
  \item{Geom}{previous information}

  \item{new}{function to set up the axis}
}
\description{
  GeomAxis A special ggplot2 geom for drawing the tour axes
}
\examples{
print(ggplot(dd_example("tour2d")))
}
\author{
  Hadley Wickham \email{h.wickham@gmail.com}
}
\keyword{datasets}
\keyword{internal}

