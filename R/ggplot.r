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

compact_timeseries <- function(data){
	dfx <- data.frame(
		data$plots[[1]]$points[,c("col","pch","cex")], 
		value = data$plots[[1]]$points$x, 
		variable = data$plots[[1]]$params$xlabel, 
		id = 1:nrow(data$plots[[1]]$points) 
	)
	
	df <- do.call(rbind, lapply(data$plots, function(p) {
	    aes <- p$points[, c("col", "pch", "cex")]
	    data.frame(
	      aes, 
		  value = p$points$y, 
	      variable = p$params$ylabel , 
	      id = 1:nrow(p$points)
	    )
	  }))

	df <- cast(df, id + ... ~ variable)
	dfx <- cast(dfx, id + ... ~ variable)
	
	df <- cbind(
		df[,names(df) %in% c("cex","pch","col", "id") ],
	 	dfx[,setdiff(names(dfx), c("cex","pch","col", "id")) ],
		df[,setdiff(names(df), c("cex","pch","col", "id")) ] 
	)
	colnames(df)[sum(names(df) %in% c("cex","pch","col", "id")) + 1] <- data$plots[[1]]$params$xlabel
	
	return(df)
	
}

ggplot.timeseries <- function(data, edges = FALSE,...){
	df <- compact_timeseries(data)

	data.par <- df[,colnames(df) %in% c("cex","pch","col","id") ]

	df <- df[,setdiff(colnames(df),colnames(data.par))]
	
	## time series, one column no 1d plots
	grid <- expand.grid(x=1, y=1:ncol(df))
		
  	grid <- subset(grid, x != y)

	all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
		xcol <- grid[i, "x"]
		ycol <- grid[i, "y"]

		data.frame(
			xvar = names(df)[ycol], 
			yvar = names(df)[xcol],
			x = df[, xcol], y = df[, ycol], df,data.par
		)
	}))



	  all$xvar <- factor(all$xvar, levels=names(df))
	  all$yvar <- factor(all$yvar, levels=names(df))
	
    aesString <- aes_string(x="x", y="y",group="xvar")
	class(aesString) <- "uneval"

	p <- ggplot(all, aesString) + facet_grid(xvar ~ yvar, scales = "free") + 
		scale_colour_identity() + 
	    scale_size_identity() + 
	    scale_shape_identity() + 
	    scale_linetype_identity() + 
	    opts(title = data$title) +
	    scale_x_continuous(all[1,"yvar"]) +
	    scale_y_continuous("")+
		geom_point(data = all, aes_string(size = "cex * 4",colour="col", shape = "pch"))
	
	if(data$showDirectedEdges | data$showUndirectedEdges | edges == TRUE)
		p <- p + geom_path(data = all, aes_string(x = "x", y = "y", size = "cex", colour = "col"))

	p
}



compact_scatmat <- function(data, scatmat = FALSE, x.values = TRUE) {
	
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


# Compact pcp data
# A parallel coordinates is written out as a series of 1D dotplots.  This function
# compacts it back into one dataset.
# 
# @arguments data
# @keyword internal 
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


# Create a nice plot for parallel coordinates plot
# Create a nice looking plot complete with axes using ggplot.
# 
# @arguments plot to display
# @arguments other (currently) unused arguments
# @keyword hplot 
#X ggplot(dd_example("pcp"))
#X ggplot(dd_example("pcp"), size = 1)
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
