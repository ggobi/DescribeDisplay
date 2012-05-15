#' @importFrom proto proto
GeomAxis <- proto(ggplot2:::Geom, {
  new <- function(., data=NULL, stat=NULL, position=NULL, ...){
    do.call("layer", list(mapping=NULL, data=data, stat=stat, geom=., position=position, ..., inherit.aes = FALSE))
  }
  
  draw <- function(., data, scales, coordinates, location = c(0.2, 0.2), size=0.9, colour = "black", ...) {
    axesVp <- axesViewport(data, location)
    axes <- axesGrob(data, gp=gpar(col = colour))
    
    gTree(
      children = gList(axes), 
      childrenvp = axesVp
    )    
  }

  objname <- "axis"
  icon <- function(.) {}
  desc <- "Projection axes"
  
  default_stat <- function(.) StatIdentity
  required_aes <- c()
  default_aes <- function(.) aes()
  
})

#' Geom Axis
#' A special ggplot2 geom for drawing the tour axes
#' 
#' @param ... should include data, location, aes\_string information
#' @author Hadley Wickham \email{h.wickham@@gmail.com}
#' @keywords internal
#' @aliases GeomAxis
#' @export
#' @examples
#' library(ggplot2)
#' print(ggplot(dd_example("tour2d")))
geom_axis <- function(...) GeomAxis$new(...)