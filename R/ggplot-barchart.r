#' Create a nice plot
#' Create a nice looking plot complete with axes using ggplot.
#' 
#' @param data plot to display, object created by \code{dd_load()}
#' @param spine (not implements currently) whether to display the barchart as a spine plot
#' @param other arguments passed to the grob function
#' @author Barret Schloekre schloerke [at] gmail.com
#' @keywords internal
#' @examples
#' ggplot(dd_example("barchart"))
#' ggplot(dd_example("barchart-spine"))
#' ggplot(dd_example("barchart-species"))
ggplot.barchart <- function(data,...){

cat("\nggplot.barchart\n")

  panel <- data$plots[[1]]
  
  if("histogram" %in% class(panel)){
    ggplot.dd(data,...)
  }else{
    ggplot(panel,title = data$title,...)
  }

}



#' Create a nice plot
#' Create a nice looking plot complete with axes using ggplot.
#' 
#' @param data plot to display, object created by \code{dd_load()}
#' @param spine (not implements currently) whether to display the barchart as a spine plot
#' @param other arguments passed to the grob function
#' @author Barret Schloekre schloerke [at] gmail.com
#' @keywords internal
#' @examples
#' ggplot(dd_example("barchart"))
ggplot.histogram <- function(data, spine = FALSE,...) {
  cat("\nggplot.histogram\n")

#  print(head(data$points))
  #print(unique(data$points$col))
  
  p <- ggplot(data$points, 
      aes(x = x, fill = col,...)) + xlab(data$params$label) +
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
#' @param spine (not implemented currently) whether to display the barchart as a spine plot
#' @param other arguments passed to the grob function
#' @author Barret Schloekre schloerke [at] gmail.com
#' @keywords hplot
#' @examples
#' ggplot(dd_example("barchart-species"))
ggplot.barplot <- function(panel,title,...){

  cat("\n ggplot.barplot\n")

#print(panel$points)
  xCount = table(panel$points$col,panel$points$x)

  colnames(xCount) = panel$params$levelnames
#print(xCount)  
  
  uniColors = as.character(unique(panel$points$col));
#cat("\nuniColors\n");print(uniColors)
  
#cat("\nnames\n");print(colnames(xCount))
  

  #Make a fake graph
  plot(c(0,0.2+1.2*(ncol(xCount))),c(0,max(xCount)),col="white",axes=FALSE,xlab="",ylab="")

  #Add a grey background
  rect(-max(xCount)*0.1,-max(xCount)*.1,1.1*(0.2+1.2*(ncol(xCount))),1.1*max(xCount), col="grey90")

  # Add a darker box
  box(col="grey70")

  #Ablines don't match the tick marks
#  abline(h=c(0,0.2*max(xCount),0.4*max(xCount),0.6*max(xCount),0.8*max(xCount),max(xCount)),col="white") 

  # Make tick marks with ugly formatting, aka 2.45 when it should be 2
#  axis(
#    2,
#    at = c(0,0.2*max(xCount),0.4*max(xCount),0.6*max(xCount),0.8*max(xCount),max(xCount)), 
#    labels = as.numeric(c(0,0.2*max(xCount),0.4*max(xCount),0.6*max(xCount),0.8*max(xCount),max(xCount)) )
#  )

  barplot(
    xCount,
    col=uniColors,
    names.arg=colnames(xCount),
    main = title,
    axes=TRUE,
    axis.lty=0,
    add=TRUE
  )


}      