##### Header #####
#
# Author:       Matthew H. Grinnell
# Affiliation:  Pacific Biological Station, Fisheries and Oceans Canada (DFO)
# Group:        Quantitative Assessment Methods Section
# Address:      3190 Hammond Bay Road, Nanaimo, BC, Canada, V9T 6N7
# Contact:      e-mail: Matthew.Grinnell@dfo-mpo.gc.ca | tel: (250) 756.7055
# Project:      Herring
# Code name:    RunSummaries.R
# Version:      1.0
# Date started: Jun 16, 2017
# Date edited:  Feb 12, 2018
#
# Overview:
# Run all the data summaries in turn.
#
# Requirements:
# The data summary script, 'Summary.R'.
#
# Notes:
# Comment-out the 'clear workspace' line in 'Summary.R'.

##### Housekeeping #####

# General options
rm(list = ls()) # Clear the workspace
sTimeAll <- Sys.time() # Start the timer
graphics.off() # Turn graphics off

# # Make packages available
# UsePackages( pkgs=c("foreach", "doSNOW") )

##### Controls #####

# Region names: major, minor, special
regionNames <- c("HG", "PRD", "CC", "SoG", "WCVI", 
                 "A27", "A2W", "A10",
                 "All")

# Initialize some cores
# clust <- makeCluster( 4 )
# registerDoSNOW( cluster )

##### Main #####

# Load helper functions
source(file = file.path("..", "HerringFunctions", "Functions.R"))

# Message re data summaries
cat(
  "Summarising data for", length(regionNames), "regions:",
  PasteNicely(regionNames), "\n\n"
)

# Loop over regions
for (r in 1:length(regionNames)) {
  # Get list of objects to keep
  lsKeep <- c("sTimeAll", "regionNames", "r")
  # Remove everything else
  rm(list = ls()[!ls() %in% lsKeep])
  # Turn off graphics
  graphics.off()
  # Get the region
  region <- regionNames[r]
  # Run the data summary depending on the region
  source(file = "Summary.R")
  # Put a space
  cat("\n")
} # End loop over regions

# Function to run the summary
RunSummary <- function(reg, ...) {
  #  # Get list of objects to keep
  #  lsKeep <- c( "sTimeAll", "regionNames", "r", "reg" )
  #  # Remove everything else
  #  rm( list=ls()[!ls() %in% lsKeep] )
  #  # Turn off graphics
  #  graphics.off( )
  # Get the region
  region <- reg
  # Run the data summary depending on the region
  source(file = "Summary.R")
  return(1)
} # End RunSummary function

## Number of cores
# nCores <- detectCores( ) - 1
#
## Initiate the cluster
# clust <- makeCluster( nCores )
#
## Export variables to the cluster
# clusterExport( clust, varlist=c("regionNames") )
#
## Apply over regions
## clusterApply( cl=clust, x=regionNames, fun=RunSummary )
## parSapply( cl=clust, X=regionNames, FUN=RunSummary, reg=regionNames )
# parApply( cl=clust, X=array(regionNames), FUN=RunSummary, MARGIN=1, reg=regionNames )
#
## Close the cluster
# stopCluster( cl=clust )

##### End #####

# Stop the cluster
# stopCluster( clust )

# Print end of file message and elapsed time
cat("End of file RunSummaries.R: ", sep = "")
print(Sys.time() - sTimeAll)
