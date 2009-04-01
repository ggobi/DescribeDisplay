#' Create a nice plot for Histograms
#' Create a nice looking plot complete with axes using ggplot.
#' 
#' @param data plot to display, object created by \code{dd_load()}
#' @param spine (not implemented currently) whether to display the barchart as a spine plot
#' @param other arguments passed through to the ggplot function
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggplot(dd_example("barchart"))
ggplot.histogram <- function(data, spine = FALSE,...) {
  #cat("\nggplot.histogram\n")
  
  p <- ggplot(data$points, 
    aes(x = x, fill = col,...)) + 
      xlab(data$params$label) +
      coord_flip() +
      scale_size_identity() + 
      scale_shape_identity() + 
      scale_linetype_identity() +
      scale_fill_identity()
  
  
#  if(spine){
#    ## not correct yet
#    p <- p + geom_bar( position = "fill", binwidth = diff(data$params$breaks[1:2]) , ...)
#    cat("\nspine\n")
#  }else{
    allBreaks <- c(data$params$breaks,data$params$breaks[length(data$params$breaks)] + diff(data$params$breaks[1:2]))

    p <- p + geom_histogram(breaks = allBreaks,...)
#  }

  p
}




#' Create a nice plot for Bar Plots
#' Create a nice looking plot complete with axes using ggplot.
#' 
#' @param data plot to display, object created by \code{dd_load()}
#' @param spine (not implemented currently) whether to display the barchart as a spine plot
#' @param other arguments passed through to the ggplot function
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggplot(dd_example("barchart-species"))
ggplot.barplot <- function(panel,spine=FALSE,...){
  #cat("\nggplot.barplot\n")
  levelnames <- panel$params$levelnames
  levelNameOrder <- panel$params$levelvalues + 1
  xVals <- panel$points$x
  for(i in 1:length(levelnames)){
    xVals[xVals==i] <- levelnames[levelNameOrder[i]]
  }

  panel$points$splitBy <- xVals
#print(panel$points)

  p <- ggplot(panel$points, aes_string(x = "splitBy", fill = "col", ...)) + 
    geom_bar() + xlab(panel$params$label) +
    coord_flip() +
    scale_size_identity() + 
    scale_shape_identity() + 
    scale_linetype_identity() +
    scale_fill_identity() + 
    xlim(unique(panel$points$splitBy))
    
  p
}      