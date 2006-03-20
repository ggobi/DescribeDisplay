# Plot describe display output.
# This uses base graphics and is rather limited in terms of layout.
# 
# You probably shouldn't need to ever use this, but it is convenient 
# for quickly checking that the datastructures are correct.
# 
# @arguments describe display object
# @arguments plot 
plot.dd <- function(x, y=1, ...) {
  dd <- x; n <- 1
  arguments <- defaults(list(...), c(dd_points(dd, n), dd_defaults(dd, n)))
  
  do.call(plot, arguments)
  edges <- dd_edges(dd,n)
  if (!is.null(edges)) {
    segments(edges$src.x, edges$src.y, edges$dest.x, edges$dest.y, lwd=edges$lwd, col=edges$col)
    
  }
  
  box(col="grey")
}


# Panel grob
# Construct grob for single panel.
# 
# @arguments describe display object
# @arguments plot 
# @arguments axis location, x and y position
#X ash <- system.file("examples", "test-ash.r", package="DescribeDisplay")
#X ash[[1]]$drawlines = TRUE
#X grid.newpage(); grid.draw(panelGrob(ash))
panelGrob <- function(panel,axislocation = c(0.1, 0.1)) {
  points <- panel$points
  edges <- panel$edges
  
  axesVp <- viewport(xscale=c(-1,1), yscale=c(-1,1), name="axes", width=0.2, height=0.2, x=axislocation[1], y=axislocation[2])
  grobs <- list(
    rectGrob(gp=gpar(col="grey")),
    pointsGrob(points$x, points$y, pch=points$pch, gp=gpar(col=points$col), size=unit(points$cex, "char")),
    textGrob(nulldefault(panel$params$xlab, ""), 0.99, 0.01, just = c("right","bottom")),
    textGrob(nulldefault(panel$params$ylab, ""), 0.01, 0.99, just = c("left", "top")),
    axesGrob(panel)
  )

  if (length(panel$params$label) == 1)
    grobs <- append(grobs, list(textGrob(nulldefault(panel$params$label, ""), 0.5, 0.01, just = c("centre", "bottom"))))

  if (panel$drawlines) {
    grobs <- append(grobs, segmentsGrob(0, points$y, points$x, points$y), , gp=gpar(col=points$col))
    
  }

  if (!is.null(edges))  
    grobs <- append(grobs, list(segmentsGrob(edges$src.x, edges$src.y, edges$dest.x, edges$dest.y, default.units="native", gp=gpar(lwd=edges$lwd, col=edges$col))))
  
  gTree(
    children = do.call(gList, grobs), 
    vp = dataViewport(
      xscale = expand_range(range(points$x), 0.1),
      yscale = expand_range(range(points$y), 0.1)
    ),
    childrenvp = axesVp
  )
}

# Plot a dd plot
# Convenient method to draw a single panel.
# 
# This is mainly used for bug testing so that you can pull out a single 
# panel quickly and easily.
# 
# @arguments object to plot
# @arguments axis location, x and y position
plot.dd_plot <- function(x, ..., axislocation = c(0.1, 0.1)) {
  grid.newpage()
  grid.draw(panelGrob(x, axislocation=axislocation))  
}

# Draw dd plot
# Draw a complete describe display.
# 
# If you want to layout multiple dd plots on the same page, you can
# use \code{\link[grid]{grid.layout}}.  If you need even more control,
# set \code{draw = FALSE} and then \code{\link[grid]{grid.draw}} the 
# resulting grob yourself.
# 
# @arguments dd object to plot
# @arguments (unused)
# @arguments draw plot, or just return grob
# @value frame grob containing all panels, note that this does not contain the title or border
plot.dd <- function(x, ..., draw = TRUE, axislocation = c(0.1, 0.1)) {
  d <- x$dim
  layout <- grid.layout(nrow=d[1], ncol=d[2])
  panels <- frameGrob(layout = layout)
  
  for(i in 1:x$nplot) {
    panels <- placeGrob(panels, panelGrob(x$plots[[i]], axislocation=axislocation), col = (i - 1) %/% d[1] + 1
    , row = (i - 1) %% d[1] + 1)
  }

  pg <- frameGrob(grid.layout(nrow=2, ncol=1))
  pg <- packGrob(pg, textGrob(x$title, gp=gpar(cex=1.5)), row=1, height=unit(2,"lines"))
  pg <- packGrob(pg, panels, row=2)

  if (draw) {
    grid.newpage()
    pushViewport(viewport(w = 0.9, h = 0.9))
    grid.draw(pg)
  }
  
  invisible(panels)
  
}

# Axes grob
# Construct grob for axes.
# 
# @arguments describe display object
# @arguments plot 
axesGrob <- function(panel) {
  axes <- dd_tour_axes(panel)
  if (is.null(axes)) return()

  gTree(children=gList(
    circleGrob(0, 0, 1, default.units="native", gp=gpar(fill="transparent", col="black")),
    segmentsGrob(0,0, axes$x, axes$y, default.units="native"),
    textGrob(axes$label, 1.1 * cos(axes$theta), 1.1 * sin(axes$theta), default.units="native", gp=gpar(fontsize=10))
  ), vp=vpPath("axes"))  
}