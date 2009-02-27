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