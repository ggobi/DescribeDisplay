#' Compact pcp data
#' A parallel coordinates is written out as a series of 1D dotplots.  This function
#' compacts it back into one dataset.
#' 
#' @param data data to pull points from
#' @param x.values pull the x or y values
#' @author Hadley Wickham h.wickham [at] gmail.com
#' @keywords internal 
compact_pcp <- function(data,x.values=TRUE) {
  df <- do.call(rbind, lapply(data$plots, function(p) {
    aes <- p$points[, c("col", "pch", "cex")]
    data.frame(
      aes, 
      value = if(x.values){ p$points$x }else{ p$points$y}, 
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
#' @param absoluteX make the sections proportional horizontally to eachother
#' @param absoluteY make the sections proportional vertically to eachother
#' @param other arguments passed to the grob function
#' @author Barret Schloerke schloerke [at] gmail.com
#' @keywords hplot 
#' @examples
#' ggplot(dd_example("pcp"))
#' ggplot(dd_example("pcp"), size = 1)
#' ggplot(dd_example("pcp-ash")
#' ggplot(dd_example("pcp-ash",lines = FALSE)
#' ggplot(dd_example("pcp-ash",absoluteX = TRUE)
#' ggplot(dd_example("pcp-ash",absoluteY = TRUE)
#' ggplot(dd_example("pcp-ash",absoluteX = TRUE, absoluteY = TRUE)
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

	cat("\nggplot.parcoords\n")

	df.x <- compact_pcp(data)
	df.y <- compact_pcp(data,x.values=FALSE)
	
	vars.par <- c("cex","pch","col", "id")
	vars.using <- setdiff(names(df.x), vars.par)
	
	df.y <- df.y[,vars.using]
	
	names(df.y) <- vars.using.Y <- paste(names(df.y),".y",sep="")

	df <- cbind(df.x,df.y)

		
	### Actually the X part of data
	if(!absoluteY){
		## Make everything scaled to the same range
		
		for(i in 1:length(vars.using)){
			df[,vars.using[i]] <- (df[,vars.using[i]] - min(df[,vars.using[i]]))/diff(range(df[,vars.using[i]])) 
		}
	}
	
	### Actually the Y part of data
	maxRangeX <- diff(range(df[,vars.using.Y[1]]))
	if(absoluteX){
		## find the max range to do correct scaling
		
		for(i in 1:length(vars.using.Y)){
			tmp <- diff(range(df[,vars.using.Y[i]]))
#			print(tmp)
			if(maxRangeX < tmp) maxRangeX <- tmp
		}
#		cat("\nmaxRangeX = ", maxRangeX,"\n")
	}
	
	
	for(i in 1:length(vars.using.Y)){
		if(diff(range(df[,vars.using.Y[i]])) == 0){
			## put values on a straight line
			df[,vars.using.Y[i]] <- i
		}else{
			## give values a range of 2/3 the area to the right of the break line
			if(absoluteX){
				## Area used is relative to the other sections
				df[,vars.using.Y[i]] <- (df[,vars.using.Y[i]] - min(df[,vars.using.Y[i]])) / maxRangeX * 2/3 + i
#				print(range(df[,vars.using.Y[i]]))
			}else{
				## Each section fills up 2/3 area
				df[,vars.using.Y[i]] <- (df[,vars.using.Y[i]] - min(df[,vars.using.Y[i]]))/diff(range(df[,vars.using.Y[i]]))* 2/3 + i
			}
		}
	}

	
	### Reformat data
	Y <- c(as.matrix(df[,vars.using]))
	X <- c(as.matrix(df[,vars.using.Y]))

	df.vars.par <- NULL
	for(i in 1:length(vars.using)){
		df.vars.par <- rbind(df.vars.par, df.x[,vars.par])
	}	

	df.final <- cbind(X,Y,df.vars.par)

	## Reorder according to occurances of color (causes it to plot the ids who appear the least, last)
	df2 <- NULL
	colTable <- table(df.final[,"col"])
	for(i in names(colTable[order(colTable, decreasing = TRUE)])){
		df2 <- rbind(df2, df.final[df.final[,"col"] == i, ])
	}
	
#	print(head(df.final))
#	print(head(df2))
	df.final <- df2
	df2 <- NULL


	### Make a pretty picture
	aesString <- aes_string(x="X", y="Y",group="id")
	p <- ggplot(data = df.final, aesString)+
    	scale_colour_identity() + 
    	scale_size_identity() + 
    	scale_shape_identity() + 
    	scale_linetype_identity() + 
    	opts(title = data$title) +
    	scale_y_continuous(
			"", 
			breaks = seq(
				min(df.final[,"Y"]),
				max(df.final[,"Y"]), 
				length = 5
			), 
			labels = ""
		) + 
    		scale_x_continuous(
			name = "", 
				#limits=c(2/3,(length(vars.using)+1)), 
			breaks = 1:length(vars.using), 
			labels = vars.using, 
			minor_breaks = FALSE
		)

  if(lines)
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
