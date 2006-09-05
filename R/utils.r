# Null default
# Convienece function for setting defaults for null values
# 
# @arguments value  
# @arguments default to use if value is null
# @keyword internal 
nulldefault <- function(x, default) {
  if (is.null(x)) return(default)
  x
}


addbrush <- function(plot, x,y, width=0.5, height=0.5, just=c("left", "top")) {
	brush <- data.frame(x=x, y=y, width=width, height=height)
	ggrect(plot, data=brush, aes=list(x=x, y=y, width=width, height=height), justification=just)
}