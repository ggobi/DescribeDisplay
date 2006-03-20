library(grid)
sapply(dir("R", "\\.r$",full=TRUE), source)

if (!exists("tests")) {
  paths <- dir("tests/", "\\.[rR]$", full=T)
  tests <- lapply(paths, dd_load)
  names(tests) <- sub("\.[rR]$", "", dir("tests/", "\\.[rR]$"))  
}
