# Create a nice plot
# Create a nice looking plot complete with axes using ggplot.
# 
# @arguments plot to display
# @arguments grob function to use for drawing
# @arguments other arguments passed to the grob function
# @keyword hplot
# @alias ggplot.dd
#X points <- dd_load(system.file("examples", "test-xyplot.r", package="DescribeDisplay"))
#X ggplot(points)
#X edges <- dd_load(system.file("examples", "test-edges.r", package="DescribeDisplay"))
#X ggplot(edges)
ggplot.ddplot <- function(data, axis.location = c(0.2, 0.2), ...) {
  p <- ggplot(data$points, aes(x, y, shape=pch, size=cex * 6, colour=col)) +
    scale_colour_identity() + 
    scale_size_identity() + 
    scale_shape_identity() + 
    scale_linetype_identity() +
    scale_x_continuous(data$params$xlab, limits = data$xscale) + 
    scale_y_continuous(data$params$ylab, limits = data$yscale) + 
    geom_point() 
  
  axes <- dd_tour_axes(data)
  if (!is.null(axes)) {
    vars <- names(axes)
    names(vars) <- vars
    p <- p + geom_axis(data=axes, location = axis.location, do.call(aes_string, as.list(vars)))
  }

  edges <- data$edges
  if (!is.null(edges))  
    p <- p + geom_segment(aes(x = src.x, y = src.y, xend = dest.x, yend = dest.y, linetype=lty, shape=NULL, size=lwd), data=edges)
    
  
  if (!is.null(data$labels))
    p <- p + geom_text(aes(label = label), data=data$labels, justification=c(data$labels$left[1], data$labels$top[1]))
    
  p
}

ggplot.dd <- function(data, ...) { 
  panel <- data$plots[[1]]
  ggplot(panel, ...) + opts(title = data$title)
}

# Compact pcp data
# A parallel coordinates is written out as a series of 1D dotplots.  This function
# compacts it back into one dataset.
# 
# @arguments data
# @keyword internal 
compact_pcp <- function(data) {
  df <- do.call(rbind, lapply(data$plots, function(p) data.frame(p$points[, c("col", "pch","cex")], value=p$points$x, variable=p$params$label, id=1:nrow(p$points))))
  cast(df, id + ... ~ variable)
}

# Create a nice plot for parallel coordinates plot
# Create a nice looking plot complete with axes using ggplot.
# 
# @arguments plot to display
# @arguments other (currently) unused arguments
# @keyword hplot 
#X pcp <- dd_load 
#
ggplot.parcoords <- function(data, ...) { 
  df <- as.data.frame(compact_pcp(data))
  p <- ggpcp(df, vars = setdiff(names(df), c("cex","pch","col", "id")), scale="range") +
    scale_colour_identity() + 
    scale_size_identity() + 
    scale_shape_identity() + 
    scale_linetype_identity() + 
    opts(title = data$title) +
    scale_y_continuous("", breaks = seq(0, 1, length=4), labels = "") + 
    scale_x_discrete("")
   
  if (data$showPoints) {
    p <- p + geom_point(aes(colour=col, shape=pch, size=cex * 4.5), ...)
  }
  
  p <- p + geom_line(aes(colour=col, size=cex * 2, order=as.numeric(col)), ...)

  ggopt(axis.colour = "black")
  p
}
