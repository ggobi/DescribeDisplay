# Add brush to plot
# This adds a rectangle to a ggplot plot indicating the brush position
# 
# @arguments plot object
# @arguments x position of brush
# @arguments y position of brush
# @arguments width of brush
# @arguments height of brush
# @arguments which corner of brush should be determined by x and y position
# @arguments fill colour for brush (use \code{\link[ggplot]{alpha}} for alpha blending)
# @arguments outline colour of brush
# @keyword hplot 
addbrush <- function(plot, x,y, width=0.5, height=0.5, just=c("left", "top"), fill=NA, col="black") {
  brush <- data.frame(x=x, y=y, width=width, height=height)
  geom_rect(
    aes_string("x", "y", width = "width", height = "height"),
    data = brush, 
    justification=just, fill=fill, colour=col
  )
}

# Remove hidden points
# Will remove all hidden points from the plot.
#
# @argument ddplot object
# @keyword manip
removehiddens <- function(d) {
  d$plots <- lapply(d$plots, function(dd) {
    dd$points <- dd$points[!dd$points$hidden, ]
    dd
  })
  
  d
}