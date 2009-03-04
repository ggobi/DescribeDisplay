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