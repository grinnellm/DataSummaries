# Load the saved image
load(file = file.path(paste("Temp", ".RData", sep = "")))
# Load required packages
UsePackages(pkgs = c(
  "tidyverse", "RODBC", "zoo", "Hmisc", "scales", "sp", "maptools", "rgdal",
  "rgeos", "raster", "xtable", "cowplot", "grid", "colorRamps", "RColorBrewer",
  "stringr", "lubridate", "readxl", "plyr", "ggforce", "viridis", "ggthemes",
  "SpawnIndex", "tidyselect", "ggrepel"
))
# Make the animation (twice to remove empty first page)
PlotLocationsYear(dat = filter(siYearLoc, Year %in% yrRange[1:2]))
PlotLocationsYear(dat = siYearLoc)
