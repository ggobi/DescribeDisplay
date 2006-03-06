# Expand range
# Convenience function for expanding a range with a multiplicative 
# or additive constant.
# 
# @arguments range of data
# @arguments multiplicative constract
# @arguments additive constant
# @keyword manip 
expand_range <- function(range, mul=0, add=0) {
	range + c(-1, 1) * (diff(range) * mul + add)
}

# Defaults
# Convience method for combining a list of values with their defaults.
# 
# @arguments list of values
# @arguments defaults
# @keyword manip 
defaults <- function(x, y)  {
	c(x, y[setdiff(names(y), names(x))])
}

# Null default
# Convienece function for setting defaults for null values
# 
# @arguments value  
# @arguments default to use if value is null
nulldefault <- function(x, default) {
  if (is.null(x)) return(default)
  x
}