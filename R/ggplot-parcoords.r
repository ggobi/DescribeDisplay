#' Compact pcp data
#' A parallel coordinates is written out as a series of 1D dotplots.  This function
#' compacts it back into one dataset.
#' 
#' @param data data to pull points from
#' @param x.values pull the x or y values
#' @author Hadley Wickham \email{h.wickham@@gmail.com}
#' @keywords internal 
compact_pcp <- function(data) {
  ldply(data$plots, function(p) {
    data.frame(
      id = 1:nrow(p$points),
      variable = p$params$label, 
      p$points[c("col", "pch", "cex")], 
      x = nulldefault(p$points$y, 1),
      y = p$points$x
    )
  })
}


range01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) {
    rep(0, length(x))
  } else {
    (x - rng[1]) / diff(rng)
  }
}

#' Create a nice plot for parallel coordinates plot
#' Create a nice looking plot complete with axes using ggplot.
#' 
#' @param plot to display
#' @param absoluteX make the sections proportional horizontally to eachother
#' @param absoluteY make the sections proportional vertically to eachother
#' @param other arguments passed to the grob function
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot 
#' @examples
#' ggplot(dd_example("pcp"))
#' ggplot(dd_example("pcp-ash"))
#' ggplot(dd_example("pcp-ash"),lines = FALSE)
#' ggplot(dd_example("pcp-ash"),absoluteY = TRUE, lines = FALSE)
#' ggplot(dd_example("pcp-texture"))
#' ggplot(dd_example("pcp-texture"),lines=FALSE)
#' ggplot(dd_example("pcp-texture"),absoluteY=TRUE,lines=FALSE)
ggplot.parcoords <- function(
  data,
  absoluteX = FALSE, 
  absoluteY = FALSE, 
  lines = TRUE,
  ...
) { 

  df <- compact_pcp(data)
  browser()
  
  if (absoluteX) {
    std <- transform(df, x = as.numeric(variable) + range01(x) / 2)  
  } else {
    # Scale variables individually
    std <- ddply(df, .(variable), transform, 
      x = as.numeric(variable) + range01(x) / 2)    
  }
  
  if (!absoluteY) {
    std <- ddply(std, .(variable), transform, y = range01(y))
  }

  ybreaks <- seq(min(df$y), max(df$y), length = 5)
  vars <- levels(df$variable)

  ### Make a pretty picture
  p <- ggplot(std, aes(x, y, group = id, colour = col, order = col)) +
    scale_colour_identity() + 
    scale_size_identity() + 
    scale_shape_identity() + 
    scale_linetype_identity() + 
    opts(title = data$title) +
    scale_y_continuous("", breaks = ybreaks, labels = "") + 
    scale_x_continuous("", breaks = 1:length(vars), 
      labels = vars, minor_breaks = FALSE)
  
  if (lines) {
    p <- p + geom_line(aes(size = cex * 2))
  }

  # Plot points on top
  if (data$showPoints) {
    p <- p + geom_point(aes(shape = pch, size = cex * 4.5))
  }

  p
}
