#' Create a nice plot
#' Create a nice looking plot complete with axes using ggplot.
#' 
#' @param data plot to display, object created by \code{dd_load()}
#' @param spine whether the display the barchart as a spine
#' @param other arguments passed to the grob function
#' @author Barret Schloekre schloerke [at] gmail.com
#' @keywords hplot
#' @examples
#' ggplot(dd_example("barchart"))
ggplot.histogram <- function(data, spine = FALSE,...) {
  cat("\nggplot.barchart\n")

	print(head(data$points))
	#print(unique(data$points$col))
	
  p <- ggplot(data$points, 
    	aes(x = x, colour = col, fill = col,...)) +
    scale_size_identity() + 
    scale_shape_identity() + 
    scale_linetype_identity() +
	scale_colour_identity()
	
	
	if(spine){
		## not correct yet
		p <- p + geom_bar( position = "fill", binwidth = diff(data$params$breaks[1:2]) , ...)
		cat("\nspine\n")
	}else{
		p <- p + geom_bar(binwidth = diff(data$params$breaks[1:2]),...)
	}

	
	    


  p
}
