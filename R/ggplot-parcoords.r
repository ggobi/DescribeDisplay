#' Compact pcp data
#' A parallel coordinates is written out as a series of 1D dotplots.  This function
#' compacts it back into one dataset.
#' 
#' @param data
#' @keywords internal 
compact_pcp <- function(data) {
  df <- do.call(rbind, lapply(data$plots, function(p) {
    aes <- p$points[, c("col", "pch", "cex")]
    data.frame(
      aes, 
      value = p$points$x, 
      variable = p$params$label, 
      id = 1:nrow(p$points)
    )
  }))
  cast(df, id + ... ~ variable)
}



#' Create a nice plot for parallel coordinates plot
#' Create a nice looking plot complete with axes using ggplot.
#' 
#' @param plot to display
#' @param other (currently) unused arguments
#' @keywords hplot 
#' @examples
#' ggplot(dd_example("pcp"))
#' ggplot(dd_example("pcp"), size = 1)
ggplot.parcoords <- function(data, ...) { 
  df <- as.data.frame(compact_pcp(data))

	
	## Reorder according to occurances of color (causes it to plot the ids who appear the least, last)
	df2 <- NULL
	colTable <- table(df[,"col"])
	for(i in names(colTable[order(colTable, decreasing = TRUE)])){
		df2 <- rbind(df2, df[df[,"col"] == i, ])
	}
	
	print(head(df))
	print(head(df2))
	df <- df2
	

  p <- ggpcp(df, vars = setdiff(names(df), c("cex","pch","col", "id")), scale="range") +
    scale_colour_identity() + 
    scale_size_identity() + 
    scale_shape_identity() + 
    scale_linetype_identity() + 
    opts(title = data$title) +
    scale_y_continuous("", breaks = seq(0, 1, length = 5), labels = "") + 
    scale_x_discrete("")

  
  p <- p + geom_line(
    aes_string(colour="col", size="cex * 2", order="as.numeric(col)")
  )

  ## Moved to have points plotted last
  if (data$showPoints) {
    p <- p + geom_point(
      aes_string(colour="col", shape="pch", size="cex * 4.5")
    )
  }

  p
}
