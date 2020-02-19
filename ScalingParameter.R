# Clear the workspace
rm( list=ls() )

# Packages
require( tidyverse )
require( scales )

# Regions
regions <- c( "HG", "PRD", "CC", "SoG", "WCVI" )

# New survey year
newSurvYr <- 1988

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
  # Bind tibbles and wrangle
  out <- bind_rows( res ) %>%
    replace_na( replace=list(Surf=0, Macro=0, Under=0) ) %>%
    group_by( Region, Year, Total ) %>%
    summarise( Surface=Surf/Total, Dive=(Macro+Under)/Total ) %>%
    ungroup( ) %>%
    pivot_longer( cols=c(Surface, Dive), names_to="Survey",
                  values_to="Proportion" ) %>%
    mutate( Index=Proportion*Total, Region=factor(Region, levels=r),
            Survey=factor(Survey, levels=c("Surface", "Dive")) ) %>%
    arrange( Region, Year ) %>%
    select( -Total )
  # Return data
  return( out )
}  # End LoadIndices function

# Load spawn indices
spawn <- LoadIndices( r=regions )

# Figure: bar plot
spawnProp <- ggplot( data=spawn, mapping=aes(x=Year, y=Proportion, group=Survey,
                                            fill=Survey) ) +
  geom_bar( stat="identity" ) + 
  geom_vline( xintercept=newSurvYr-0.5, linetype="dashed", size=0.5 ) +
  facet_wrap( Region ~ ., dir="v", ncol=2 ) +
  scale_fill_viridis_d( ) +
  theme_bw( ) +
  theme( legend.position="top" ) +
  ggsave( filename="SpawnProp.png", height=6, width=6 )

# Figure: points
spawnIndex <- ggplot( data=spawn, mapping=aes(x=Year, y=Index, group=Survey,
                                            fill=Survey) ) +
  geom_bar( stat="identity" ) +
  geom_vline( xintercept=newSurvYr-0.5, linetype="dashed", size=0.5 ) +
  facet_wrap( Region ~ ., dir="v", ncol=2, scales="free_y" ) +
  scale_y_continuous( labels=comma ) +
  scale_fill_viridis_d( ) +
  theme_bw( ) +
  theme( legend.position="top" ) +
  ggsave( filename="SpawnIndex.png", height=6, width=6 )
