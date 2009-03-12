#' Create a nice plot
#' Create a nice looking plot complete with axes using ggplot.
#' 
#' @param data plot to display, object created by \code{dd_load()}
#' @param spine (not implements currently) whether to display the barchart as a spine plot
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
      aes(x = x, fill = col,...)) +
    scale_size_identity() + 
    scale_shape_identity() + 
    scale_linetype_identity()# +
  #scale_fill_identity()
  
  
#  if(spine){
#    ## not correct yet
#    p <- p + geom_bar( position = "fill", binwidth = diff(data$params$breaks[1:2]) , ...)
#    cat("\nspine\n")
#  }else{
    p <- p + geom_bar(binwidth = diff(data$params$breaks[1:2]),...)
#  }

  
      


  p
}


#' Create a nice plot
#' Create a nice looking plot complete with axes using ggplot.
#' 
#' @param data plot to display, object created by \code{dd_load()}
#' @param spine (not implements currently) whether to display the barchart as a spine plot
#' @param other arguments passed to the grob function
#' @author Barret Schloekre schloerke [at] gmail.com
#' @keywords hplot
#' @examples
#' ggplot(dd_example("barchart-species"))
ggplot.barchart <- function(data, spine = FALSE,...) {
  panel = data$plots[[1]]
print(panel$points)
  xCount = table(panel$points$col,panel$points$x)

  colnames(xCount) = panel$params$levelnames
print(xCount)  
  foregroundColors = data$colormap$foreground
print(foregroundColors)
  
  uniColors = as.character(unique(panel$points$col));
cat("\nuniColors\n");print(uniColors)
  
cat("\nnames\n");print(colnames(xCount))
  

  barplot(xCount,col=uniColors,names.arg=colnames(xCount),main = data$title)

  
#  if (all(!is.null(data$params$levelnames) && !is.null(data$params$levelvalues) {
 #     p <- p + geom_text(aes_string(label = "label"), data=data$labels, justification=c(data$labels$left[1], data$labels$top[1]))
#  }  

  
}      