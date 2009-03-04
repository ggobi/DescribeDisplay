#' Add brush to plot
#' This adds a rectangle to a ggplot plot indicating the brush position
#' 
#' @param plot object
#' @param x position of brush
#' @param y position of brush
#' @param width of brush
#' @param height of brush
#' @param which corner of brush should be determined by x and y position
#' @param fill colour for brush (use ggplot-alpha for alpha blending)
#' @param outline colour of brush
#' @keywords hplot 
#' @returns NULL
addbrush <- function(plot, x,y, width=0.5, height=0.5, just=c("left", "top"), fill=NA, col="black") {
  brush <- data.frame(x=x, y=y, width=width, height=height)
  geom_rect(
    aes_string("x", "y", width = "width", height = "height"),
    data = brush, 
    justification=just, fill=fill, colour=col
  )
}

#' Remove hidden points
#' Will remove all hidden points from the plot.
#'
#' @param ddplot object
#' @keywords manip
#' @returns NULL
removehiddens <- function(d) {
  d$plots <- lapply(d$plots, function(dd) {
    dd$points <- dd$points[!dd$points$hidden, ]
    dd
  })
  
  d
}