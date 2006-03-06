source("data.r")
source("plots.r")
source("utils.r")

paths <- dir("tests/", "\\.[rR]$", full=T)
#tests <- lapply(paths, dd_load)
#names(tests) <- sub("\.[rR]$", "", dir("tests/", "\\.[rR]$"))