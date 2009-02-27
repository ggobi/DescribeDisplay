# Create a nice plot
# Create a nice looking plot complete with axes using ggplot.
# 
# @arguments plot to display
# @arguments grob function to use for drawing
# @arguments other arguments passed to the grob function
# @keyword hplot
# @alias ggplot.dd
#X ggplot(dd_example("edges"))
#X ggplot(dd_example("xyplot"))
#X ggplot(dd_example("edges")) + xlab(NULL) + ylab(NULL)
ggplot.ddplot <- function(data, axis.location = c(0.2, 0.2), ...) {
  print(head(data$points))
  p <- ggplot(data$points, 
    aes_string(x = "x", y = "y", shape = "pch", size = "cex * 6", colour = "col")) +
    scale_colour_identity() + 
    scale_size_identity() + 
    scale_shape_identity() + 
    scale_linetype_identity() +
    scale_x_continuous(
    if(TRUE %in% (c("2dtour", "1dtour") %in% class(data) ) )
      name = "" 
    else 
      name = data$params$xlab,
    limits = data$xscale) + 
    scale_y_continuous(
    if(TRUE %in% (c("2dtour", "1dtour") %in% class(data) ) )
      name = "" 
    else
      name = data$params$ylab,
    limits = data$yscale) + 
    geom_point() 

  
  axes <- dd_tour_axes(data)
  if (!is.null(axes)) {
    vars <- names(axes)
    names(vars) <- vars

    p <- p + geom_axis(
      data = axes, location = axis.location, 
      do.call(aes_string, as.list(vars)) 
    ) +
    opts(aspect.ratio = 1)
  }

  edges <- data$edges
  if (!is.null(edges)) {
    p <- p + geom_segment(
      aes_string(x = "src.x", y = "src.y", xend = "dest.x", yend = "dest.y",
      linetype = "lty", shape = "NULL", size = "lwd"), data = edges
    )
  }

  
  if (!is.null(data$labels)) {
    p <- p + geom_text(aes_string(label = "label"), data=data$labels, justification=c(data$labels$left[1], data$labels$top[1]))
  }  

  p
}

ggplot.dd <- function(data, ...) { 
  panel <- data$plots[[1]]
  ggplot(panel, ...) + opts(title = data$title)
}








