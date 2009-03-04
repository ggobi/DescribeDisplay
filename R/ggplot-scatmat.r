#' Compact scatmat data
#' A scatter plot matrix is made from the information from the 1Dplots. This function collects only that data.
#' 
#' @param data
#' @keywords internal 
compact_scatmat <- function(data) {
	
	df <- do.call(rbind, lapply(data$plots, function(p){
		if("1dplot" %in% class(p)){
		    aes <- p$points[, c("col", "pch", "cex")]
    		data.frame(
    		  aes, 
    		  value = p$points$x, 
    		  variable = p$params$label, 
    		  id = 1:nrow(p$points)
    		)
		}
	}))
	
	cast(df, id + ... ~ variable)
}

#' Create a nice plots in a scatter plot matrix
#' Create a nice looking plots in a matrix.  The 1d sections along
#' the diagonal have a smooth density while the values are compared 
#' to eachother within the matrix.
#' 
#' @param plot to display
#' @param other (currently) unused arguments
#' @examples
#' ggplot(dd_example("scattermat"))
#' @keywords hplot 
ggplot.scatmat <- function(data,...){
	df <- compact_scatmat(data)

	p <- plotmatrix( df[,setdiff(names(df), c("cex","pch","col", "id")) ] ) +
	    scale_colour_identity() + 
	    scale_size_identity() + 
	    scale_shape_identity() + 
	    scale_linetype_identity() + 
	    opts(title = data$title) +
	    scale_y_continuous("") +
	    scale_x_continuous("")

	p
}