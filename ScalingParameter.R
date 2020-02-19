# Clear the workspace
rm( list=ls() )

# Packages
require( tidyverse )

# Regions
regions <- c( "HG", "PRD", "CC", "SoG", "WCVI" )

# Load the spawn indices
LoadIndices <- function( r ) {
  # Start a list to hold data
  res <- list( )
  # Loop over regions
  for( i in 1:length(r) ) {
    # Import the data
    res[[i]] <- read_csv( file=paste("Spawn", r[i], ".csv", sep=""),
                     col_types=cols() ) %>%
    mutate( Region=r[i] )
  }  # End i loop over regions
  # Bind tibbles
  out <- bind_rows( res ) %>%
    replace_na( replace=list(Surf=0, Macro=0, Under=0) ) %>%
    group_by( Region, Year ) %>%
    summarise( Surface=Surf/Total*100, Dive=(Macro+Under)/Total*100 ) %>%
    ungroup( ) %>%
    mutate( Region=factor(Region, levels=r) ) %>%
    arrange( Region, Year )
  # Return data
  return( out )
}  # End LoadIndices function

# Load spawn indices
dat <- LoadIndices( r=regions )