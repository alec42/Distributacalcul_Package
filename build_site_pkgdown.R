setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(pkgdown)
build_site()
