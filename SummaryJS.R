###############################################################################
# 
# Author:       Matthew H. Grinnell
# Affiliation:  Pacific Biological Station, Fisheries and Oceans Canada (DFO) 
# Group:        Quantitative Assessment Methods Section, Science
# Address:      3190 Hammond Bay Road, Nanaimo, BC, Canada, V9T 6N7
# Contact:      e-mail: matt.grinnell@dfo-mpo.gc.ca | tel: 250.756.7055
# Project:      Herring
# Code name:    Summary.R
# Version:      2.0
# Date started: Jun 03, 2016
# Date edited:  Sep 07, 2017
# 
# Overview: 
# Generate tables and figures for annual Pacific herring preliminary data
# summaries by region(s), and generate the input file for the Pacific herring 
# stock assessment, which uses ADMB.
# 
# Requirements: 
# Access to the main herring databases on the shared network drive, or local 
# copies: catch, biosample, and spawn. In addition, look-up tables are used to 
# convert numeric codes in the databases into reader-friendly text.
# 
# Notes: 
# Usually only one region is considered at a time, but this script can analyse
# multiple regions if requested. Output (e.g., .RData, pdf figures, latex 
# tables) can be used by the Knitr file 'DataReport.Rnw' to generate a dynamic 
# and reproducible document (http://yihui.name/knitr/).
#
# References:
# This script is based on several R scripts writen by Jaclyn Cleary and Ashleen 
# Benson which wrangle herring catch, biosample, and spawn data.
#
# Versions: Version 1 used the spawn index calculated in the database. Version 
# 2 sources the script 'SpawnIndex.R', and does not rely on the database.
# 
###############################################################################

# TODO:
# 1. Get better section shapefile polygons (i.e., contiguous with no holes or 
#    overlapping borders).
# 2. Label extra sampling in Central Coast Area 8 as non-representative (i.e.,
#    for years when additional samples were collected)? This isn't really 
#    necessary since we have a work-around using the historic sample ratio.
# 3. Differentiate between representative, unrepresentative, and unknown 
#    samples. The default should be unknown (e.g., for old samples).
# 4. Update column and variable names in the look-up tables so that I don't have 
#    to make any manual changes in the R script (e.g., Period1=Gear1). Better
#    yet, omit the need for look-up tables by having the actual variable names
#    in the main tables! Why isn't this the case?
# 5. Calculate tide/datum corrected spawn depths.
# 6. Get better location information (i.e., X, and Y for locations).
# 11. Fix spawn metric figures (e.g., length, width, layers, index) so that the 
#     y-axis CIs don't go negative (i.e., log scale?). Or, omit the CIs?
# 12. Add SOK harvest (spawning biomass in tonnes) to the time series of landed
#     catch? That is to say, add the data from Table 2 to Figure 3 in the data
#     summary report.


########################
##### Housekeeping #####
########################

# General options
rm( list=ls( ) )      # Clear the workspace
sTime <- Sys.time( )  # Start the timer
graphics.off( )       # Turn graphics off

# Install missing packages and load required packages (if required)
UsePackages <- function( pkgs, locn="https://cran.rstudio.com/" ) {
  # Reverse the list 
  rPkgs <- rev( pkgs )
  # Identify missing (i.e., not yet installed) packages
  newPkgs <- rPkgs[!(rPkgs %in% installed.packages( )[, "Package"])]
  # Install missing packages if required
  if( length(newPkgs) )  install.packages( newPkgs, repos=locn )
  # Loop over all packages
  for( i in 1:length(rPkgs) ) {
    # Load required packages using 'library'
    eval( parse(text=paste("suppressPackageStartupMessages(library(", rPkgs[i], 
                "))", sep="")) )
  }  # End i loop over package names
}  # End UsePackages function

# Make packages available
UsePackages( pkgs=c("tidyverse", "RODBC", "zoo", "Hmisc", "scales", "sp", 
        "maptools", "rgdal", "rgeos", "raster", "xtable", "cowplot", "grid", 
        "colorRamps", "RColorBrewer", "stringr", "lubridate", "readxl", 
        "plyr", "lettercase") ) 


#################### 
##### Controls ##### 
#################### 

# Select region(s): major (HG, PRD, CC, SoG, WCVI); or minor (A27, A2W, JS)
region <- "JS"

# Select a subset of sections (or NA for all)
sectionSub <- NA  # c( 67, 70:79 )

# Include test fishery catch
inclTestCatch <- TRUE

# Include test seine biological data
inclTestSNBio <- TRUE

# Include test gillnet biological data
inclTestGNBio <- FALSE

# Include spawn on kelp biological data
inclSOKBio <- TRUE

# Location of the shared network drive
dirShare <- file.path( "\\\\dcbcpbsna01a", "hdata$" )

# Databases: remote or local
dbLoc <- "Remote"

# Database name
dbName <- "HSA_Program_v6.2.mdb"

# Input coordinate reference system
inCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Coordinate reference system (http://spatialreference.org/ref/sr-org/82/)
outCRS <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000
    +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Geographic projection
geoProj <- "Projection: BC Albers (NAD 1983)"


######################
##### Parameters #####
######################

# Year range: include data
yrRange <- 1951:2017

# Age range: omit below, plus group above
ageRange <- 2:10

# Age to highlight in figure
ageShow <- 3

# Number of years to calculate running mean
nRoll <- 5

# Conversion factors: short tons to tonnes, feet to metres
convFac <- list( st2t=0.90718474, ft2m=0.3048 )

# First year of data to include in summary tables
firstYrTab <- min( yrRange )

# First year of data to include in summary figures
firstYrFig <- min( yrRange )

# Age schedule and population parameters for model input
parsADMB <- list( 
    "Natural mortality (m)"=0.334, 
    "Growth (linf, k, to)"=c(27, 0.48, 0),
    "Length Weight (a, b)"=c(4.5e-6, 3.127),
    "Maturity at age"=c(2.055, 0.05),
    "Maturity vector"=c(0.24974, 0.9, 1, 1, 1, 1, 1, 1, 1) )

# Spawn survey method changed from surface (1951--1987) to dive (1988--present)
newSurvYr <- 1988

# Figure width
figWidth <- 6

# Type of smoothing line
smLine <- "loess"

# Level of confidence interval for smoothed conditional mean
ciLevel <- 0.9

# Get ylimits (e.g., weight in g) for the weight-at-age plot
wtRange <- c( 35, 130 )

# TODO: Omit spawn data prior, or draw a vertical line (minor stock areas) 
firstYrSpawnMinor <- 1978

# Year range for historic ratio of biosample effort (Central Coast)
yrsRatioHist <- 1994:2013

# Years to fix using historic ratio of biosample effort (Central Coast)
yrsRatioFix <- c( 2014, 2015 )

# Expand the transect length
doExpand <- FALSE

# Years to expand the transect length (i.e., footrope)
expYrs <- 2003:2014

# Proportion to expand the transect length (i.e., footrope)
expFac <- 0.075

# Years where intensity is used to determine egg layers
intenseYrs <- yrRange[yrRange < 1979]

# Years where intensity needs to be re-scaled
rescaleYrs <- intenseYrs[intenseYrs < 1951]

# Transect width (m; used for macrocystis spawn)
TransectW <- 2

# Buffer distance (m; to include locations that are outside the region polygon)
maxBuff <- 5000 


##################
#### Sources #####
##################

# File name for dive transect XY
diveLoc <- list(
    loc=file.path("..", "Data"),
    fn="dive_transects_with_lat_long_June2_2017.xlsx" )

# Location and names of lookup tables with catch codes
codesLoc <- list(
    loc=file.path("..", "Data"),
    fns=list(tDisposal="tDisposal.csv", tGear="tGear.csv", 
        tSource="tSource.csv") )

# Location and name of the location database and tables
areaLoc <- list(
    loc=file.path("..", "Data", dbLoc),
    db=dbName,
    fns=list(sections="Sections", locations="Location") )

# Location(s) and names of the Sections and land shapefiles
shapesLoc <- list(
    locSec=file.path(dirShare, "Kristen", "Herring_Shapefiles"),
    locLand=file.path("..", "Data", "Polygons"),
    fns=list(sections="SectionsIntegrated", land="GSHHS_h_L1_Alb") )

# Location and name of the catch database and tables
catchLoc <- list(
    loc=file.path("..", "Data", dbLoc),
    db=dbName,
    fns=list(tCatch="tCatchData", hCatch="HailCatch", sokCatch="SpawnOnKelp") )

# Location and name of the biological database and tables
bioLoc <- list(
    loc=file.path("..", "Data", dbLoc),
    db=dbName,
    fns=list(samples="sample", fish="fish", bmc="BMcCarter") )

# Location and name of the surface database and tables
surfLoc <- list(
    loc=file.path("..", "Data", dbLoc),
    db=dbName, 
    fns=list(regionStd="RegionStd", sectionStd="SectionStd", poolStd="PoolStd", 
        surface="tSSSurface", intensity="Intensity", allSpawn="tSSAllspawn") )

# Location and name of the macrocystis database and tables
macroLoc <- list(
    loc=file.path("..", "Data", dbLoc),
    db=dbName,
    fns=list(allSpawn="tSSAllspawn", plants="tSSMacPlant", 
        transects="tSSMacTrans") )

# Location and name of the macrocystis database and tables
underLoc <- list(
    loc=file.path("..", "Data", dbLoc),
    db=dbName,
    fns=list(allSpawn="tSSAllspawn", algTrans="tSSVegTrans", 
        stations="tSSStations", algae="tSSVegetation", 
        typeAlg="tSSTypeVegetation") )


#####################
##### Functions #####
#####################

# Load helper functions
source( file="Functions.R" )

# Load spawn index functions
source( file="SpawnIndex.R" )


################
##### Data #####
################

# Breaks for years
yrBreaks <- seq( from=round_any(x=min(yrRange), accuracy=10, f=floor), 
    to=round_any(x=max(yrRange), accuracy=10, f=ceiling), by=10 )

# Function to load transect spatial info
LoadTransectXY <- function( loc ) {
  # Load the data and wrangle
  dat <- read_excel( path=file.path(loc$loc, loc$fn), sheet=1 ) %>%
      rename( LocationCode=LOC_CODE ) %>%
      mutate( LocationCode=as.integer(LocationCode) ) %>%
      group_by( LocationCode ) %>%
      summarise( 
          Longitude=MeanNA(c(StartLong, MidLong, EndLong)),
          Latitude=MeanNA(c(StartLat, MidLat, EndLat)) ) %>%
      ungroup( ) %>%
      filter( !is.na(Longitude), !is.na(Latitude), LocationCode!=0 )
  # Grab the spatial info (X and Y)
  locSP <- dat %>%
      transmute( X=Longitude, Y=Latitude )
  # Put X and Y into a spatial points object
  locPts <- SpatialPointsDataFrame( coords=locSP, 
      data=data.frame(LocationCode=dat$LocationCode), proj4string=CRS(inCRS) )
  # Convert X and Y from WGS to Albers
  locPtsAlb <- spTransform( x=locPts, CRSobj=CRS(outCRS) )
  # Extract spatial info
  dfAlb <- as_tibble( locPtsAlb ) %>%
      rename( Eastings=X, Northings=Y )
  # Return the data
  return( dfAlb )
}  # End LoadTransectXY function

# Load 'auxiliary' dive transect spatial data
transectXY <- LoadTransectXY( loc=diveLoc )

# If region is a vector, collapse region names for output; otherwise region
regName <- paste( region, collapse="." )

# If old directory exists
if( regName %in% list.files() ) {
  # Remove the old directory
  unlink( regName, recursive=TRUE )
  # Warning: remove previous summary output
  warning( "Removed existing directory '", regName, "'", call.=FALSE )
  # Create the main directory for output
  dir.create( regName )
} else {  # End if directory exists, otherwise
  # Create the main directory for output
  dir.create( regName )
}  # End if directory doesn't exists

# Load disposal codes
tDisposal <- read_csv( file=file.path(codesLoc$loc, codesLoc$fns$tDisposal),
    col_types=cols("i", "c", "c", "i", "i", "c") )

# Load gear codes
tGear <- read_csv( file=file.path(codesLoc$loc, codesLoc$fns$tGear), 
    col_types=cols("i", "c", "i", "i", "i") )

# Load source codes
tSource <- read_csv( file=file.path(codesLoc$loc, codesLoc$fns$tSource), 
    col_types=cols("i", "c", "c") )

# Load herring areas
areas <- LoadAreaData( where=areaLoc )

# Get BC land data etc (for plots)
shapes <- LoadShapefiles( where=shapesLoc, a=areas )

# Load raw catch data, and some light wrangling
LoadCatchData <- function( where ) {
  # This function loads the tree types of herring catch data, drops unnecessary 
  # rows and columns, and combines the data frames for the region(s) in question.
  # Progress message
  cat( "Loading catch data... " )
  # Establish connection with access
  accessDB <- odbcConnectAccess( access.file=file.path(where$loc, where$db) )
  # Access the tCatch worksheet and wrangle
  tCatch <- sqlFetch( channel=accessDB, sqtable=where$fns$tCatch ) %>%
      mutate( Year=Season2Year(Season), Source=rep("Tab", times=n()) ) %>%
      left_join( y=areas, by="LocationCode" ) %>%
      filter( Section %in% areas$Section ) %>%
      group_by( Year, Source, Section, GearCode, DisposalCode ) %>%
      summarise( Catch=SumNA(Catch) ) %>%
      ungroup( )
  # Access the hail worksheet and wrangle
  hCatch <- sqlFetch( channel=accessDB, sqtable=where$fns$hCatch ) %>%
      filter( Active == 1, Section %in% areas$Section ) %>%
      mutate( Year=Season2Year(Season), Catch=CatchTons*convFac$st2t,
          Source=rep("Hail", times=n()) ) %>%
      group_by( Year, Source, Section, GearCode, DisposalCode ) %>%
      summarise( Catch=SumNA(Catch) ) %>%
      ungroup( )
  # Access the sok worksheet
  sokCatch <- sqlFetch( channel=accessDB, sqtable=where$fns$sokCatch ) %>%
      mutate( Year=Season2Year(Season), Source=rep("SOK", times=n()) ) %>%
      rename( Catch=ProductLanded ) %>%
      filter( Section %in% areas$Section ) %>%
      group_by( Year, Source, Section, GearCode, DisposalCode ) %>%
      summarise( Catch=SumNA(Catch) ) %>%
      ungroup( )
  # Combine the three tables
  allCatch <- bind_rows( tCatch, hCatch, sokCatch )
  # Smaller subset of area information
  areasSm <- areas %>%
      select( Region, RegionName, StatArea, Section, Group ) %>%
      distinct( )
  # Merge with area information
  res <- allCatch %>%
      left_join( y=areasSm, by="Section" ) %>%
      select( Year, Source, Region, StatArea, Section, GearCode, DisposalCode,
          Catch ) %>%
      arrange( Year, Source, Region, StatArea, Section, GearCode, DisposalCode )
  # Warning if more recent data is available
  if( max(res$Year, na.rm=TRUE) > max(yrRange) )
    warning( "Recent catch data exists; update 'yrRange' to include ",
        paste(unique(res$Year[which(res$Year > max(yrRange))]), collapse=", "),
        call.=FALSE )
  # Close the connection
  odbcClose( accessDB )
  # Update progress message
  cat( "done\n" )
  # Return the data
  return( res )
}  # End LoadCatchData function

# Load raw catch data
catchRaw <- LoadCatchData( where=catchLoc )

# Load biological data, and some light wrangling
LoadBioData <- function( where, XY ) {
  # This function loads the herring biosample data: one table with general 
  # sample information, and the other with fish measurements. Unnecessary 
  # rows and columns are dropped. The output is a data frame of the two merged
  # tables for the region(s) in question.
  # Progress message
  cat( "Loading biosample data... " )
  # Establish connection with access
  accessDB <- odbcConnectAccess( access.file=file.path(where$loc, where$db) )
  # Access the sample worksheet
  sampleDat <- sqlFetch( channel=accessDB, sqtable=where$fns$samples )
  # Grab the spatial info and process
  sampleSP <- sampleDat %>%
      transmute( X=ifelse(is.na(Set_Longitude), 0, Set_Longitude),
          Y=ifelse(is.na(Set_Latitude), 0, Set_Latitude))
  # Put X and Y into a spatial points object
  sPts <- SpatialPoints( coords=sampleSP, proj4string=CRS(inCRS) )
  # Convert X and Y from WGS to Albers
  sPtsAlb <- spTransform( x=sPts, CRSobj=CRS(outCRS) )
  # Extract spatial info
  dfAlb <- as_tibble( sPtsAlb )
  # Extract relevant sample data
  samples <- sampleDat %>%
      cbind( dfAlb ) %>%
      rename( LocationCode=loc_code, Sample=isamp, Month=month,
          Representative=Representative_Set, SourceCode=source_code, 
          GearCode=gear_code ) %>%
      mutate( Year=Season2Year(season),
          Eastings=ifelse(is.na(Set_Longitude), Set_Longitude, X),
          Northings=ifelse(is.na(Set_Latitude), Set_Latitude, Y) ) %>%
      select( Year, Month, Sample, Representative, LocationCode, Eastings, 
          Northings, SourceCode, GearCode ) %>%
      as_tibble( )
  # Access the fish worksheet and wrangle
  fish <- sqlFetch( channel=accessDB, sqtable=where$fns$fish ) %>%
      rename( Sample=isamp, Fish=fish, Length=len, Weight=wgt, Sex=sex_alpha, 
          MaturityCode=mat_code, DualAge=dual_age, 
          GonadLength=gonad_len, GonadWeight=gonad_wgt ) %>%
      mutate( Year=Season2Year(season), 
          Age=ifelse(age<=max(ageRange), age, max(ageRange)) ) %>%
      filter( Age >= min(ageRange) ) %>%
      select( Year, Sample, Fish, Length, Weight, Sex, MaturityCode, Age, 
          DualAge, GonadLength, GonadWeight ) %>%
      as_tibble( )
  # Combine the two tables (note that Sample re-starts at 1 each Year)
  fishSamples <- full_join( x=samples, y=fish, by=c("Year", "Sample") )
  # More wrangling: filter to region(s)
  raw <- fishSamples %>%
      filter( LocationCode %in% areas$LocationCode ) %>%
      left_join( y=areas, by="LocationCode" ) %>%
      mutate( Eastings=ifelse(is.na(Eastings.x), Eastings.y, Eastings.x),
          Northings=ifelse(is.na(Northings.x), Northings.y, Northings.x)) %>%
      select( Year, Month, Region, StatArea, Group, Section, LocationCode, 
          LocationName, Eastings, Northings, Sample, Representative, SourceCode, GearCode, 
          Fish, Length, Weight, Sex, MaturityCode, Age, DualAge, GonadLength, 
          GonadWeight ) %>%
      arrange( Year, Month, Region, StatArea, Group, Section, LocationCode, 
          Sample, Fish )
  # Clip the extent
  df <- ClipExtent( dat=raw, spObj=shapes$regSPDF, bufDist=maxBuff, 
      silent=TRUE )
  # Subset data with 'good' X and Y
  dfNotNA <- df %>%
      filter( !is.na(Eastings) & !is.na(Northings) )
  # Subset data with 'bad' X or Y, and try to fill in using transect X and Y
  dfNA <- df %>%
      filter( is.na(Eastings) | is.na(Northings) ) %>%
      select( -Eastings, -Northings ) %>%
      left_join( y=XY, by="LocationCode" )
  # Re-combine the two subsets
  df2 <- bind_rows( dfNotNA, dfNA )
  # Clip the extent (again)
  res <- ClipExtent( dat=df2, spObj=shapes$regSPDF, bufDist=maxBuff,
      silent=TRUE )
  # Get locations with missing X or Y
  noXY <- res %>%
      filter( is.na(Eastings) | is.na(Northings) ) %>%
      select( Region, StatArea, Group, Section, LocationCode, LocationName ) %>%
      distinct( )
  # Message re missing X and Y, if any
  if( nrow(noXY) >=1 )  warning( "There are ", nrow(noXY), 
        " biological sample location(s) with missing or incorrect spatial info",
        call.=FALSE )
  # Stop if we're missing rows
  if( nrow(raw) != nrow(res) )  stop( "Missing rows!", call.=FALSE )
  # Warning if more recent data is available
  if( max(res$Year, na.rm=TRUE) > max(yrRange) )
    warning( "Recent biological data exists; update 'yrRange' to include ",
        paste(unique(res$Year[which(res$Year > max(yrRange))]), collapse=", "),
        call.=FALSE )
  # Close the connection
  odbcClose( accessDB )
  # Update progress message
  cat( "done\n" )
  # Return the data
  return( res )
}  # End LoadBioData function

# Load raw biological data
bioRaw <- LoadBioData( where=bioLoc, XY=transectXY )

# Load spawn data, and some light wrangling
LoadSpawnData <- function( whereSurf, whereMacro, whereUnder, XY ) {
  # This function loads the herring spawn data, and drops nnnecessary rows and 
  # columns. The output is a data frame for the region(s) in question.
  # Progress message
  cat( "Calculating spawn index: " )
  # Fecundity conversion factor
  ECF <- CalcEggConversion( fecundity=200, pFemale=0.5 )
  # Access and calculate surface spawn
  surface <- CalcSurfSpawn( where=whereSurf, a=areas, f=ECF )
  # Access and calculate macrocystis spawn
  macrocystis <- CalcMacroSpawn( where=whereMacro, a=areas, f=ECF )
  # Access and calculate understory spawn
  understory <- CalcUnderSpawn( where=whereUnder, a=areas, f=ECF )
  # Load the all spawn data
  allSpawn <- GetAllSpawn( where=underLoc, a=areas )
  # Combine the spawn types (by spawn number)
  raw <- surface$biomassSpawn %>%
      full_join( y=macrocystis$biomassSpawn, by=c("Year", "Region", "StatArea", 
              "Section", "LocationCode", "SpawnNumber") ) %>%
      full_join( y=understory$biomassSpawn, by=c("Year", "Region", "StatArea", 
              "Section", "LocationCode", "SpawnNumber") ) %>%
      full_join( y=allSpawn, by=c("Year", "Region", "StatArea", "Section", 
              "LocationCode", "SpawnNumber") ) %>%
      select( Year, Region, StatArea, Group, Section, LocationCode, 
          LocationName, SpawnNumber, Eastings, Northings, Start, End, Length, 
          Width, Depth, Method, SurfLyrs, SurfSI, MacroLyrs, MacroSI, UnderLyrs, 
          UnderSI ) %>%
      mutate( Year=as.integer(Year) ) %>%
      arrange( Year, Region, StatArea, Section, LocationCode, SpawnNumber, 
          Start, End )
  # Clip the extent
  df <- ClipExtent( dat=raw, spObj=shapes$regSPDF, bufDist=maxBuff, 
      silent=TRUE )
  # Subset data with 'good' X and Y
  dfNotNA <- df %>%
      filter( !is.na(Eastings) & !is.na(Northings) )
  # Subset data with 'bad' X or Y, and replace using transect X and Y
  dfNA <- df %>%
      filter( is.na(Eastings) | is.na(Northings) ) %>%
      select( -Eastings, -Northings ) %>%
      left_join( y=XY, by="LocationCode" )
  # Re-combine the two subsets
  df2 <- bind_rows( dfNotNA, dfNA )
  # Clip the extent (again)
  res <- ClipExtent( dat=df2, spObj=shapes$regSPDF, bufDist=maxBuff,
      silent=TRUE )
  # Get locations with missing X or Y
  noXY <- res %>%
      filter( is.na(Eastings) | is.na(Northings) ) %>%
      select( Region, StatArea, Group, Section, LocationCode, LocationName ) %>%
      distinct( )
  # Message re missing X and Y, if any
  if( nrow(noXY) >=1 )  warning( "There are ", nrow(noXY), 
        " spawn location(s) with missing or incorrect spatial info", 
        call.=FALSE )
  # Stop if we're missing rows
  if( nrow(raw) != nrow(res) )  stop( "Missing rows!", call.=FALSE )
  # Warning if more recent data is available
  if( max(res$Year, na.rm=TRUE) > max(yrRange) )
    warning( "Recent spawn data exists; update 'yrRange' to include ",
        paste(unique(res$Year[which(res$Year > max(yrRange))]), collapse=", "),
        call.=FALSE )
  # Return the data
  return( res )
}  # End LoadSpawnData function

# Load spawn data
spawnRaw <- LoadSpawnData( whereSurf=surfLoc, whereMacro=macroLoc, 
    whereUnder=underLoc, XY=transectXY )


##################
##### Update #####
##################

# Update catch data (more wrangling)
UpdateCatchData <- function( dat, a ) {
  # Get a short list of area information
  areasSm <- a %>%
      select( Region, StatArea, Group, Section ) %>%
      distinct( )
  # Wrangle catch data
  WrangleCatch <- function( df ) {
    # This function combines the three types of herring catch information into 
    # the three 'gear' types, which are representative of the three main periods
    # of herring catch. The output is a data frame with total catch (scaled as 
    # specified) by year and gear type (i.e., period).
    # Update tCatch for Period 1
    tc1 <- df %>%
        filter( Source == "Tab", DisposalCode %in% c(1, 3, 4, 5, 6) ) %>%
        select( Year, Catch )
    # Update hCatch for Period 1
    hc1 <- df %>%
        filter( Source == "Hail", DisposalCode %in% c(3, 6) ) %>%
        select( Year, Catch )
    # Combine catches for period 1
    dat1 <- bind_rows( tc1, hc1 )
    # Period 1 catch
    pd1 <- dat1 %>%
        group_by( Year ) %>%
        summarise( Gear1=SumNA(Catch) ) %>% 
        ungroup( )
    # Updated tCatch for Period 2
    tc2 <- df %>%
        filter( Source == "Tab", GearCode == 29 )
    # If including test fishery catch
    if( inclTestCatch ){
      # Include
      tc2 <- filter( .data=tc2, DisposalCode %in% c(7, 8) )
    } else {  # End if including, otherwise
      # Exclude
      tc2 <- filter( .data=tc2, DisposalCode == 7 ) 
    }  # End if exclude
    # Period 2 catch
    pd2 <- tc2 %>%
        group_by( Year ) %>%
        summarise( Gear2=SumNA(Catch) ) %>% 
        ungroup( )
    # Updated tCatch for Period 3
    tc3 <- df %>%
        filter( Source == "Tab", GearCode == 19 )
    # If including test fishery catch
    if( inclTestCatch ) {
      # Include 
      tc3 <- filter(.data=tc3, DisposalCode %in% c(7, 8))
    } else {  # End if include, otherwise
      # Exclude
      tc3 <- filter(.data=tc3, DisposalCode == 7)    
    }  # End if exclude
    # Period 3 catch
    pd3 <- tc3 %>%
        group_by( Year ) %>%
        summarise( Gear3=SumNA(Catch) ) %>% 
        ungroup( )
    # Combine the data frames
    dfList <- list( pd1, pd2, pd3 )
    # Merge the tables and wrangle
    pd123 <- Reduce( function(...) merge(..., all=TRUE), dfList ) %>%
        complete( Year=yrRange, fill=list(Gear1=0, Gear2=0, Gear3=0) )
    # Select years of interest and arrange by year
    res <- pd123 %>%
        filter( Year %in% yrRange ) %>%
        gather( key=Period, value=Catch, Gear1, Gear2, Gear3 ) %>%
        arrange( Period, Year ) %>%
        select( Period, Year, Catch ) %>%
        as_tibble( )
    # Return catch by year and period
    return( res )
  }  # End WrangleCatch function
  # Start a list to hold dataframes
  rList <- list( )
  # Get the unique sections
  uSection <- unique( dat$Section )
  # Loop over sections
  for( i in 1:length(uSection) ) {
    # Get the ith section
    iSec <- uSection[i]
    # Get the catch
    rList[[i]] <- catchRaw %>% 
        filter( Section == iSec ) %>%
        WrangleCatch( ) %>%
        group_by( Period, Year ) %>%
        summarise( Catch=SumNA(Catch) ) %>%
        ungroup( ) %>%
        #mutate( Catch=ifelse(Catch == 0, NA, Catch) ) %>%
        mutate( Section=iSec ) %>%
        #filter( !is.na(Catch) )
        filter( Catch>0 ) %>%
        select( Period, Year, Section, Catch )
  }  # End i loop over sections
  # Combine the data frames
  res <- bind_rows( rList) %>%
      left_join( y=areasSm, by="Section" ) %>%
      select( Period, Year, Region, StatArea, Group, Section, Catch ) %>%
      arrange( Period, Year, Region, StatArea, Group, Section )
  # Return the results
  return( res )
}  # End UpdateCatchData function

# Update catch data
catch <- UpdateCatchData( dat=catchRaw, a=areas )

# Update biological data (more wrangling)
UpdateBioData <- function( dat, rYr ) {
  # This function determines the three 'gear' types, which are representative 
  # of the three main periods of herring biological data. The output is a data 
  # frame with a new column indicating the period (i.e., gear type), and a new
  # column indicating the weights (to be used in special cases only).
  # Get data for period 1
  pd1 <- dat %>%
      filter( GearCode == 29, SourceCode %in% c(1, 6, 7) ) %>%
      mutate( Period=rep(1, times=n()) )
  # Get data for period 2
  pd2 <- dat %>%
      filter( GearCode == 29 ) %>%
      mutate( Period=rep(2, times=n()) )
  # If including test seine biological data
  if( inclTestSNBio ) {
    # Include
    pd2 <- filter(.data=pd2, SourceCode %in% c(0, 5))
  } else {  # End if include, otherwise
    # Exclude
    pd2 <- filter(.data=pd2, SourceCode == 0)
  }  # End if exclude
  # If including sok biological data
  if( inclSOKBio ) {
    # Get SOK data
    SOK <- dat %>%
        filter( GearCode == 29, SourceCode == 4, Month %in% c(3, 4) ) %>%   
        mutate( Period=rep(2, times=n()) )
    # Combine with non-SOK dat
    pd2 <- bind_rows( pd2, SOK )
  }  # End if including SOK data
  # Get data for period 3
  pd3 <- dat %>%
      filter( GearCode == 19 ) %>%
      mutate( Period=rep(3, times=n()) )
  # If including test gillnet biological data
  if( inclTestGNBio ) {
    # Include
    pd3 <- filter(.data=pd3, SourceCode %in% c(0, 5))
  } else {  # End if include, otherwise
    # Exclude
    pd3 <- filter(.data=pd3, SourceCode == 0)
  }  # End if exclude
  # Combine the three tables
  p123 <- bind_rows( pd1, pd2, pd3 )
  # Warning re representative samples
  warning( "Biosamples: keep all samples from ", min(yrRange), " to ", rYr-1, 
      ", and 'representative' samples from ", rYr, " to ", max(yrRange), sep="", 
      call.=FALSE )
  # Include only representative samples (ish)
  res <- p123 %>%
      filter( Year < rYr | Representative == 1 ) %>%
      filter( Year %in% yrRange ) %>%
      select( -Representative ) %>%
      mutate( SampWt=1 )
  # Return the data
  return( res )
}  # End UpdateBioData function

# Update biological data
bio <- UpdateBioData( dat=bioRaw, rYr=2014 )


################ 
##### Main ##### 
################     

# Calculate commercial catch in recent years
CalcCommCatch <- function( dat ) {
  # This function calculates total catch by gear (fishery) and use (disposal)
  # for recent years, and returns a data frame.
  # Get catch in current year by gear and disposal
  cat <- dat %>%
      filter( Year >= firstYrFig ) %>% 
      group_by( Year, GearCode, DisposalCode ) %>% 
      summarise( Catch=SumNA(Catch) ) %>%
      ungroup( )
  # Combine with gear names
  catGear <- left_join( x=cat, y=select(.data=tGear, Gear, GearCode), 
      by="GearCode" )
  # Combine with disposal names
  catGearDisp <- left_join( x=catGear, 
      y=select(.data=tDisposal, DisposalCode, Disposal), by="DisposalCode" )
  # Wrangle
  res <- catGearDisp %>%
      select( Year, Gear, Disposal, Catch ) %>%
      rename( Fishery=Gear, Use=Disposal ) %>%
      filter( Use != "SoK" ) %>%
      arrange( Year, Fishery, Use )
  # If there are no rows
  if( nrow(res) == 0 ) {
    # Ensure there are 3 columns
    if( ncol(res) != 4 )  warning( "Result must have 4 columns", call.=FALSE )
    # Add a dummy row
    res[1, ] <- c( NA, NA, NA, as.integer(0) )
  }  # End if there are no rows
  # Return the catch
  return( res )
}  # End CalcCommCatch function

# Calculate commercial catch in recent years
catchCommUse <- CalcCommCatch( dat=catchRaw )

# Get commercial catch in current year
GetCatchYear <- function( dat ) {
  # Subset the commercial catch for the current year and ensure there is at
  # least 1 row for the table. Returns a table
  # Subset to get most recent year
  res <- dat %>%
      filter( Year == max(yrRange) ) %>%
      select( -Year ) %>%
      arrange( Fishery, Use )
  # If there are no rows
  if( nrow(res) == 0 ) {
    # Ensure there are 3 columns
    if( ncol(res) != 3 )  warning( "Result must have 3 columns", call.=FALSE )
    # Add a dummy row
    res[1, ] <- c( NA, NA, as.integer(0) )
  }  # End if there are no rows
  # Return the data
  return( res )
}  # End GetCatchYear function

# Calculate commercial catch in current year
catchCommUseYr <- GetCatchYear( dat=catchCommUse )

# Calculate commercial SoK harvest and biomass in recent year
harvestSOK <- catchRaw %>%
    filter( Year >= firstYrTab, DisposalCode == 2 ) %>%
    group_by( Year ) %>%
    summarise( Harvest=SumNA(Catch),
        SoG=SumNA(Catch[Section%in%c(132, 135)])*100/Harvest ) %>%
    ungroup( ) %>%
    # TODO: Reference? Note: covert harvest (lb) to spawning biomass (t).
    mutate( Biomass=Harvest * 831932.773 / 100000000 ) %>%
    complete( Year=firstYrTab:max(yrRange), 
        fill=list(Harvest=0, Biomass=0, SoG=0) ) %>%
    arrange( Year )

# Count the number of biological samples per year
CountBiosamplesYear <- function( dat ) {
  # This function counts the number of biosamples collected in the current year
  # and the previous few years from commercial vessels as well as test/
  # research vessels. It returns a data frame that includes the total number of
  # samples collected each year.
  # Count the number of samples by year and gear: commercial fisheries
  numComm <- dat %>%
      filter( Year >= firstYrTab, !SourceCode %in% c(2, 3, 5) ) %>%
      select( Year, Sample ) %>%
      group_by( Year ) %>%
      summarise( Commercial=n_distinct(Sample) ) %>%
      ungroup( )
  # Count the number of samples by year and gear: test and research
  numTest <- dat %>%
      filter( Year >= firstYrTab, SourceCode %in% c(2, 3, 5) ) %>%
      select( Year, Sample ) %>%
      group_by( Year ) %>%
      summarise( Test=n_distinct(Sample) ) %>%
      ungroup( )
  # Count the number of samples in SoG
  numSoG <- dat %>%
      filter( Year >= firstYrTab, Section %in% c(132, 135) ) %>%
      select( Year, Sample ) %>%
      group_by( Year ) %>%
      summarise( SoG=n_distinct(Sample) ) %>%
      ungroup( )
  # Merge the two tables and wrangle
  res <- full_join( x=numComm, y=numTest, by="Year" ) %>%
      full_join( y=numSoG, by="Year" ) %>%
      complete( Year=firstYrTab:max(yrRange), 
          fill=list(Commercial=0, Test=0, SoG=0) ) %>%
      mutate( Total=as.integer(Commercial+Test),
          SoG=SoG*100/Total) %>%
      replace_na( replace=list(SoG=0) ) %>%
      arrange( Year ) %>%
      select( Year, Commercial, Test, Total, SoG )
  # If there are no rows
  if( nrow(res) == 0 ) {
    # Ensure there are 4 columns
    if( ncol(res) != 4 )  warning( "Result must have 4 columns", call.=FALSE )
    # Add a dummy row
    res[1, ] <- c( NA, as.integer(0), as.integer(0), as.integer(0) )
  }  # End if there are no rows
  # Return the table
  return( res )
}  # End CountBiosamplesYear function

# Count the number of biological samples per year
# TODO: Update this to be only 'Representative' samples (i.e., dat=bio?)
bioNum <- CountBiosamplesYear( dat=bioRaw ) 

# Determine number of biosample by type in current year
GetSampleNumType <- function( dat ) {
  # This function determines biosample type by gear (fishery) and use (source)
  # for the current year, and returns a data frame.
  # Get catch in current year by gear and disposal
  samp <- dat %>%
      filter( Year == max(yrRange) ) %>% 
      group_by( SourceCode, GearCode ) %>% 
      summarise( Number=n_distinct(Sample) ) %>%
      ungroup( ) %>%
      mutate( Type=ifelse(SourceCode %in% c(2, 3, 5), "Test", "Commercial") )
  # Combine with gear names
  sampGear <- left_join( x=samp, y=select(.data=tGear, Gear, GearCode), 
      by="GearCode" )
  # Combine with disposal names
  sampGearSource <- left_join( x=sampGear, y=tSource, by="SourceCode" )
  # A bit more wrangling
  res <- sampGearSource %>%
      rename( Use=SampleSource ) %>%
      select( Type, Gear, Use, Number ) %>%
      arrange( Type, Gear, Use )
  # Replace NA with 0
  res[is.na(res)] <- 0
  # If there are no rows
  if( nrow(res) == 0 ) {
    # Ensure there are 3 columns
    if( ncol(res) != 4 )  warning( "Result must have 4 columns", call.=FALSE )
    # Add a dummy row
    res[1, ] <- c( NA, NA, NA, as.integer(0) )
  }  # End if there are no rows
  # Warning if number of biosamples don't match
  if( bioNum$Total[bioNum$Year==max(yrRange)] != sum(res$Number) )
    warning( "Number of biosamples differ in most recent year", call.=FALSE )
  # Return the catch
  return( res )
}  # End GetSampleNumType function

# Determine biosample types in current year
# TODO: Update this to be only 'Representative' samples (i.e., dat=bio?)
bioTypeNum <- GetSampleNumType( dat=bioRaw )

# If region is Central Coast
if( region == "CC" ) {
  # Ratio of number of biological samples between groups
  propNumBioHist <- bio %>%
      filter( GearCode == 29, Year %in% yrsRatioHist ) %>%
      group_by( Year, Group ) %>%
      summarise( Number=n_distinct(Sample) ) %>%
      mutate( Proportion=Number/SumNA(Number) ) %>%
      group_by( Group ) %>%
      summarise( SampWt=MeanNA(Proportion) ) %>%
      ungroup( )
  # Merge weights in the main bio table (i.e., to fix unbalanced sampling among
  # groups in identified years)
  bio <- bio %>%
      select( -SampWt ) %>%
      left_join( y=propNumBioHist, by="Group" ) %>%
      mutate( SampWt=ifelse(Year %in% yrsRatioFix & Period == 2, SampWt, 1) )
}  # End if region is Central Coast

# Count the number of fish aged by year and gear (and as a proportion): use the 
# 'SampWt' column to fix unrepresentative sampling if identified
numAgedYearGear <- bio %>%
    select( Period, Year, Age, SampWt ) %>%
    na.omit( ) %>%
    group_by( Period, Year, Age ) %>%
    summarise( Number=SumNA(SampWt) ) %>%
    mutate( Proportion=Number/SumNA(Number) ) %>%
    ungroup( ) %>%
    arrange( Period, Year, Age ) 

# Count the number of fish aged by year (and as a proportion) by seine gear:
# use the 'SampWt' column to fix unrepresentative sampling if identified
numAgedYear <- bio %>%
    filter( GearCode == 29 ) %>%  # %in% c(19, 29) (originally == 29)
    select( Year, Age, SampWt ) %>%
    na.omit( ) %>%
    group_by( Year, Age ) %>%
    summarise( Number=SumNA(SampWt) ) %>%
    mutate( Proportion=Number/SumNA(Number) ) %>%
    ungroup( ) %>%
    arrange( Year, Age )

# Proportion in SoG per year
SoGYr <- bio %>%
    filter( GearCode == 29 ) %>%
    select( Year, Age, SampWt, Section ) %>%
    na.omit( ) %>%
    group_by( Year ) %>%
    summarise( SoG=SumNA(SampWt[Section %in% c(132, 135)])*100/
            SumNA(SampWt) ) %>%
    ungroup( )

# Reshape and format proportion-at-age
FormatPropAtAge <- function( dat ) {
  # This function reshapes the proportion-at-age data from long to wide, 
  # replaces NAs with zeros, ensures that all years are present, and returns a
  # data frame
  # Make a table and reshape to wide: proportion-at-age
  wide <- dat %>%
      select( Year, Age, Proportion ) %>%
      spread( key=Age, value=Proportion, drop=FALSE )
  # Replace NAs with 0
  wide[is.na(wide)] <- 0
  # Fill in missing years
  res <- wide %>%
      filter( Year >= firstYrTab ) %>%
      complete( Year=yrRange ) %>%
      arrange( Year )
  # Return the output
  return( res )
}  # End FormatPropAtAge function

# Format proportion-at-age
propAgedYearTab <- FormatPropAtAge( dat=numAgedYear ) #%>%
   # full_join( y=SoGYr, by="Year" )

# Determine weighted mean and approximate CI age by year
qAgedYear <- numAgedYear %>%
    select( Year, Age, Proportion ) %>%
    group_by( Year ) %>%
    summarise( 
        MeanAge=weighted.mean(x=Age, w=Proportion),
        # CI is based on R code by Steve Martel
        sBar=qnorm(1 - (1 - ciLevel) / 2) * 
            sum(sqrt(Proportion * (1 - Proportion)) / sqrt(Age)),
        Lower=exp(log(MeanAge) - log(sBar)), 
        Upper=exp(log(MeanAge) + log(sBar)) ) %>%
    ungroup( ) %>%
    mutate( GroupID=ConsecutiveGroup(Year) ) %>%
    arrange( Year )

# Calculate mean weight-at-age by year
CalcWeightAtAge <- function( dat ) {
  # This function calculates mean weight-at-age by year, and fills in missing
  # data (i.e., NAs) using a suitable technique. Calculate the weighted mean
  # using the 'SampWt' column to fix unrepresentative sampling if identified
  # Calculate mean weight-at-age
  wtAge <- dat %>% 
      filter( GearCode == 29 ) %>%
      select( Year, Age, Weight, SampWt ) %>%
      na.omit( ) %>%
      group_by( Year, Age ) %>%
      summarise( MeanWeight=WtMeanNA(x=Weight, w=SampWt) ) %>%
      ungroup( ) %>%
      arrange( Year, Age )
  # Reshape from long to wide and merge with complete year sequence
  wtAgeW <- wtAge %>%
      spread( key=Age, value=MeanWeight ) %>%
      complete( Year=yrRange ) %>%
      arrange( Year )
  # Reshape from wide to long, and fill in NAs
  wtAgeL <- wtAgeW %>%
      gather_( key="Age", value="Weight", ageRange, convert=TRUE ) %>%
      group_by( Age ) %>%
      # Replace NAs: mean of (up to) previous n years 
      mutate( Weight=RollMeanNA(Weight, n=nRoll) ) %>%
      # Replace persistent NAs (i.e., at the beginning of the time series)
      mutate( Weight=na.fill(Weight, fill=c("extend", NA, NA)) ) %>%
      ungroup( ) %>%
      filter( Year %in% yrRange )
  # Return weight-at-age by year
  return( wtAgeL )
}  # End CalcWeightAtAge function

# Calculate mean weight-at-age by year 
weightAge <- CalcWeightAtAge( dat=bio )

# Calculate running mean weight-at-age by year
muWeightAge <- weightAge %>%
    group_by( Age ) %>%
    mutate( muWeight=rollmean(x=Weight, k=nRoll, align="right", 
            na.pad=TRUE) ) %>%
    ungroup( ) %>%
    mutate( Age=factor(Age) )

# Get biosample locations in the current year
GetBioLocations <- function( dat, spObj ) {
  # Wrangle data
  samp <- dat %>%
      filter( Year == max(yrRange), !is.na(Eastings), !is.na(Northings) ) %>%   
      mutate( Type=ifelse(SourceCode==2, "Nearshore", 
              ifelse(SourceCode %in% c(3, 5), "Seine test", "Commercial")) ) %>%
      group_by( Type, Eastings, Northings ) %>%
      summarise( Number=n_distinct(Sample) ) %>%
      ungroup( )
  # If there are rows
  if( nrow(samp) > 0 ) {
    # Clip to the region's extent
    res <- ClipExtent( dat=samp, spObj=shapes$regSPDF, bufDist=maxBuff )
  } else {  # End if there are rows, otherwise
    # Warning
    warning( "There are no geo-referenced biosamples", call.=FALSE )
    # Return the empty dataframe
    res <- samp
  }  # End if there are no rows
  # Arrange for plotting
  res <- res %>%
      arrange( desc(Number), Type )
  # Return the data
  return( res )
}  # End GetBioLocations function

# Get biosample locations
# TODO: Update this to be only 'Representative' samples (i.e., dat=bio?)
bioLocations <- GetBioLocations( dat=bioRaw, spObj=shapes$regSPDF )

# Calculate spawn summary by groups (e.g., year and section)
CalcSpawnSummary <- function( dat, g ) {
  # Calculate some basic yearly spawn statistics by section, and ensure that 
  # years are complete (i.e., missing years are populated with NA).
  # Some wrangling
  spawnByYear <- dat %>%
      select( Year, StatArea, Section, Group, Length, Width, SurfLyrs, 
          MacroLyrs, UnderLyrs, MacroSI, SurfSI, UnderSI ) %>%
      mutate( Group=as.character(Group) ) %>%
      group_by_( .dots=g ) %>%
      summarise( 
          TotalLength=SumNA(Length), 
          MeanWidth=MeanNA(Width), 
          MeanLayers=MeanNA(c(SurfLyrs, MacroLyrs, UnderLyrs)), 
          TotalSI=SumNA(c(MacroSI, SurfSI, UnderSI)),
          SoG=SumNA(c(MacroSI[Section %in% c(132, 135)], 
                  SurfSI[Section %in% c(132, 135)], 
                  UnderSI[Section %in% c(132, 135)]))*100/TotalSI) %>%
      ungroup( ) %>%
      replace_na( replace=list(SoG=0) )
  # Get the full year range
  if( all(g=="Year") )  yrsFull <- tibble( Year=yrRange )
  # Get the full year range and stat areas
  if( all(c("Year", "StatArea") %in% g) )  
    yrsFull <- expand.grid( Year=yrRange, StatArea=unique(dat$StatArea) )
  # Get the full year range and sections
  if( all(c("Year", "Section") %in% g) )  
    yrsFull <- expand.grid( Year=yrRange, Section=unique(dat$Section) )
  # Get the full year range and groups
  if( all(c("Year", "Group") %in% g) ) 
    yrsFull <- expand.grid( Year=yrRange, Group=unique(dat$Group) ) %>%
        mutate( Group=as.character(Group) )
  # Merge to ensure that each year has an entry
  res <- spawnByYear %>%
      full_join( y=yrsFull, by=g ) %>%
      filter( Year %in% yrRange ) %>%
      arrange_( g )
  # Return the data
  return( res )
}  # End CalcSpawnSummary function

# Calculate spawn summary by year
spawnYr <- CalcSpawnSummary( dat=spawnRaw, g=c("Year") )

# Smaller subset for figures: spawn by year
spawnYrFig <- spawnYr %>% 
    filter( Year >= firstYrFig )

# Smaller subset for table: spawn by year
spawnYrTab <- spawnYr %>%
    filter( Year >= firstYrTab )

# Calculate spawn summary by year and section
spawnYrSec <- CalcSpawnSummary( dat=spawnRaw, g=c("Year", "Section") ) %>%
    group_by( Year ) %>%
    mutate( PercSI=100*TotalSI/SumNA(TotalSI) ) %>%
    ungroup( ) %>%
    full_join( y=areas %>% select(StatArea, Section, Group) %>% distinct( ),
        by="Section" ) %>%
    mutate( Section=formatC(Section, width=3, flag="0"),
        StatArea=formatC(StatArea, width=2, flag="0") )

# Smaller subset for figures: spawn by year
spawnYrSecFig <- spawnYrSec %>%
    filter( Year >= firstYrFig )

# Calculate spawn summary by year and statistical area
spawnYrSA <- CalcSpawnSummary( dat=spawnRaw, g=c("Year", "StatArea") ) %>%
    group_by( Year ) %>%
    mutate( PercSI=100*TotalSI/SumNA(TotalSI) ) %>%
    ungroup( ) %>%
    mutate( StatArea=formatC(StatArea, width=2, flag="0") )

# Smaller subset for figures: spawn by year
spawnYrSAFig <- spawnYrSA %>%
    filter( Year >= firstYrFig )

# Calculate spawn summary in current year by location code
spawnByLocXY <- spawnRaw %>%
    filter( Year == max(yrRange) ) %>%
    group_by( StatArea, Section, LocationCode, LocationName ) %>%
    summarise( TotalSI=SumNA(c(MacroSI, SurfSI, UnderSI)),
        Eastings=unique(Eastings), Northings=unique(Northings) ) %>%
    ungroup( ) %>%
    arrange( TotalSI )

# Calculate spawn summary in current year by location code
spawnByLoc <- spawnByLocXY %>%
    select( -Eastings, -Northings )

# For plotting, remove rows with no spatial info
spawnByLocXY <- spawnByLocXY %>%
    filter( !is.na(Eastings), !is.na(Northings) )

# Calculate spawn summary for the last decade
spawnDecade <- spawnRaw %>%
    filter( Year %in% (max(yrRange)-1):(max(yrRange)-10) ) %>%
    group_by( Year, LocationCode ) %>%
    summarise( Eastings=unique(Eastings), Northings=unique(Northings),
        TotalSI=SumNA(c(SurfSI, MacroSI, UnderSI)) ) %>%
    ungroup( ) %>%
    mutate( Decade=paste(min(Year), max(Year), sep=" to ") ) %>%
    group_by( Decade, LocationCode ) %>%
    summarise( Eastings=unique(Eastings), Northings=unique(Northings),
         MeanSI=MeanNA(TotalSI), Frequency=n() ) %>%
    ungroup( ) %>%
#    filter( Frequency >= 2, MeanSI >=quantile(MeanSI, probs=0.1, na.rm=TRUE) ) %>%
    arrange( desc(Frequency), MeanSI )


##################
##### Region #####
##################

# If region is Prince Rupert District
if( region == "PRD" ) {
  # Catch by StatArea
  catchStatArea <- catch %>%
      group_by( Year, StatArea ) %>%
      summarise( Catch=SumNA(Catch) ) %>%
      ungroup( )
  # Smaller subset for figures: catch by year and stat area
  catchStatAreaFig <- catchStatArea %>%
      rename( SA=StatArea ) %>%
      filter( Year >= firstYrFig )
}  # ENd if region is Prince Rupert District

# If region is Cental Coast
if( region == "CC" ) {
  # Calculate spawn statistics by year and statistical area
  spawnStatsYrSA <- spawnRaw %>%
      mutate( StatArea=formatC(StatArea, width=2, flag="0") ) %>%
      rowwise( ) %>%
      mutate( Layers=MeanNA(c(SurfLyrs, MacroLyrs, UnderLyrs)) ) %>%
      ungroup( ) %>%
      select( Year, StatArea, Depth, Layers ) %>%
      filter( Year >= firstYrFig ) %>%
      arrange( Year, StatArea )
  # Calculate spawn depth by year and section for statistical area 7
  spawnStatsYrSecSA07 <- spawnRaw %>%
      filter( StatArea == 7 ) %>%
      mutate( Section=formatC(Section, width=3, flag="0") ) %>%
      rowwise( ) %>%
      mutate( Layers=MeanNA(c(SurfLyrs, MacroLyrs, UnderLyrs)) ) %>% 
      ungroup( ) %>%
      select( Year, Section, Depth, Layers ) %>%
      filter( Year >= firstYrFig ) %>%
      arrange( Year, Section )
  # Weight-at-age by year and group
  weightAgeGroup <- bio %>%
      filter( GearCode == 29, Year >= max(yrRange)-19 ) %>%
      left_join( y=areas, 
          by=c("Region", "StatArea", "Section", "LocationCode", "Group") ) %>%
      mutate( Decade=ifelse( Year>=max(yrRange)-9, "Recent", "Previous") ) %>%
      select( Year, Age, Weight, Group, Decade ) %>%
      na.omit( )
  # Determine sample sizes
  weightAgeGroupN <- weightAgeGroup %>%
      group_by( Decade, Group, Age ) %>%
      summarise( Sample=n() ) %>%
      spread( key=Decade, value=Sample ) %>%
      rename( SA=Group ) %>%
      ungroup( )
} # End if region is Central Coast

# If region is Strait of Georgia
if( region == "SoG" ) {
  # Calculate spawn summary by year and group
  spawnYrGrp <- CalcSpawnSummary( dat=spawnRaw, g=c("Year", "Group") ) %>%
      group_by( Year ) %>%
      mutate( PercSI=100*TotalSI/SumNA(TotalSI) ) %>%
      ungroup( )
  # Smaller subset for figures: spawn by year
  spawnYrGrpFig <- spawnYrGrp %>%
      filter( Year >= firstYrFig )
  # Weight by catch type
  weightCatch <- bio %>%
      filter( GearCode == 29, StatArea %in% c(14, 17) ) %>%
      left_join( y=tSource, by="SourceCode" ) 
  # Smaller subset for figures: biosamples big
  weightCatchFig <- weightCatch %>%
      filter( Year >= firstYrTab ) 
  # Average weight by age
  weightCatchFigMu <- weightCatchFig %>%
      select( Age, Weight ) %>%
      na.omit( ) %>%
      group_by( Age ) %>%
      summarise( MuWeight=MeanNA(Weight) ) %>%
      ungroup( )
}  # End if region is Strait of Georgia

# If region is West Coast of Vancouver Island
if( region == "WCVI" ) {
  # Compare differences by group: number, proportion, and weight-at-age
  npwAgeGrp <- bioRaw %>%
      filter( Year == max(yrRange), SourceCode %in% c(2, 5), 
          Representative == 1 ) %>%
      left_join( y=tSource, by="SourceCode" ) %>%
      group_by( Age, SampleSource2 ) %>%
      summarise( Number=n(), Weight=MeanNA(Weight) ) %>%
      group_by( SampleSource2 ) %>%
      mutate( Proportion=Number/SumNA(Number) ) %>%
      select( SampleSource2, Age, Number, Proportion, Weight )
  # Calculate total: number, proportion, and weight-at-age
  npwAgeTot <- bioRaw %>%
      filter( Year == max(yrRange), SourceCode %in% c(2, 5), 
          Representative == 1 ) %>%
      mutate( SampleSource2="Total" ) %>%
      group_by( Age, SampleSource2 ) %>%
      summarise( Number=n(), Weight=MeanNA(Weight) ) %>%
      ungroup( ) %>%
      mutate( Proportion=Number/SumNA(Number) ) %>%
      select( SampleSource2, Age, Number, Proportion, Weight )
  # Combine the grouped statistics with the total statistics
  npwAge <- bind_rows( npwAgeGrp, npwAgeTot ) %>%       
      complete( SampleSource2=unique(SampleSource2), Age=ageRange, 
          fill=list(Number=0, Proportion=0) ) %>%
      arrange( SampleSource2, Age )
  # Get differences in number-at-age
  deltaNumAgeYr <- npwAge %>%
      select( Age, SampleSource2, Number ) %>%
      spread( key=Age, value=Number ) %>%
      rename( 'Sample type'=SampleSource2 )
  # Get differences in proportion-at-age
  deltaPropAgeYr <- npwAge %>%
      select( Age, SampleSource2, Proportion ) %>%
      spread( key=Age, value=Proportion ) %>%
      rename( 'Sample type'=SampleSource2 )
  # Get differences in weight-at-age
  deltaWtAgeYr <- npwAge %>%
      select( Age, SampleSource2, Weight ) %>%
      spread( key=Age, value=Weight ) %>%
      rename( 'Sample type'=SampleSource2 )
}  # End if region is West Coast of Vancouver Island


################
##### ADMB #####
################

# Make ADMB input data: catch (t*10^3)
catchADMB <- catch %>%
    complete( Year=yrRange, Period=c("Gear1", "Gear2", "Gear3"), 
        fill=list(Catch=0) ) %>%
    group_by( Period, Year ) %>%
    summarise( Catch=SumNA(Catch) ) %>%
    ungroup( ) %>%
    spread( key=Period, value=Catch ) %>%
    mutate( Period1=round(Gear1/1000, digits=3), 
        Period2=round(Gear2/1000, digits=3), 
        Period3=round(Gear3/1000, digits=3), Survey1=0, Survey2=0 ) %>%
    select( Year, Period1, Period2, Period3, Survey1, Survey2 )

# Proportion of catch from SoG
catchSoG <- catch %>%
    group_by( Year ) %>%
    summarise( SoG=SumNA(Catch[Section %in% c(132, 135)])*100/SumNA(Catch) ) %>%
    ungroup( )

# Make ADMB input data: spawn (t*10^3)
spawnADMB <- spawnYr %>%
    mutate( Spawn=round(TotalSI/1000, digits=3), 
        Gear=ifelse(Year<newSurvYr, 4, 5), 
        Weight=ifelse(Year<newSurvYr, 1, 1.1666),
        Timing=1 ) %>%
    select( Year, Spawn, Gear, Weight, Timing )

# Make ADMB input data: number aged
numAgedADMB <- numAgedYearGear %>%
    select( Year, Period, Age, Number ) %>%
    mutate( Number=round(Number) ) %>%
    spread( key=Age, value=Number, fill=0 ) %>%
    arrange( Period, Year )

# Make ADMB input data: weight-at-age (kg)
weightAgeADMB <- weightAge %>%
    mutate( Weight=round(Weight/1000, digits=4) ) %>%
    spread( key=Age, value=Weight ) 

# Write ADMB input file
WriteInputFile <- function(  ) {
  # Create the file name
  fName <- file.path( regName, paste("Herring", regName, max(yrRange), ".dat", 
          sep="") )
  
  # Initialize the file and write the title
  write( x="##### Input file for herring stock assessment #####", 
      file=fName, append=FALSE )
  # Write the date
  write( x=paste("# Created:\t", Sys.Date( ), sep=""), file=fName, append=TRUE )
  # Write the region(s)
  write( x=paste("# Region(s):\t", paste(unique(areas$RegionName), 
              collapse=", "), sep=""), file=fName, append=TRUE )
  # Write the subset of sections, if it applies
  if( !all(is.na(sectionSub)) )
    write( x=paste("# Section(s):\t", paste(sectionSub, collapse=", "), sep=""),
        file=fName, append=TRUE )
  
  # Write header for model dimensions
  write( x="#\n##### Model dimensions #####", file=fName, append=TRUE )
  # Write model dimensions
  write( x=paste(min(yrRange), "\t# First year", sep=""), file=fName, 
      append=TRUE )
  write( x=paste(max(yrRange), "\t# Last year", sep=""), file=fName, 
      append=TRUE )
  write( x=paste(min(ageRange), "\t# Youngest age", sep=""), file=fName, 
      append=TRUE )
  write( x=paste(max(ageRange), "\t# Plus group", sep=""), file=fName, 
      append=TRUE )
  write( x=paste(ncol(catchADMB)-1, "\t# Number of gear types", sep=""), 
      file=fName, append=TRUE)
  
  # Write header for fishery flags
  write( x="#\n##### Fishery flags #####", file=fName, append=TRUE )
  # Determine ratio of last 20 years of catch
  c20 <- catchADMB %>%
      filter( Year >= max(yrRange)-19 ) %>%
      select( -Year ) %>%
      summarise_all( funs(sum) )
  c20 <- round( c20/sum(c20), digits=4 )
  # Write fishery flags
  write( x=paste(paste(c20, collapse="\t"), "\t# TAC allocations", sep=""), 
      file=fName, 
      append=TRUE )
  write( x=paste(paste(rep(1, times=ncol(catchADMB)-1), collapse="\t"), 
          "\t# Catch type (1=biomass, 2=catch in numbers, 3=roe removed)", 
          sep=""), file=fName, append=TRUE )
  
  # Write header for age and population parameters
  write( x="#\n##### Population parameters #####", file=fName, append=TRUE )
  # Write population parameters
  write( x=paste(paste(parsADMB[[1]], collapse=", "), "\t# ", 
          names(parsADMB)[1], sep=""), file=fName, append=TRUE )
  write( x=paste(paste(parsADMB[[2]], collapse=", "), "\t# ", 
          names(parsADMB)[2], sep=""), file=fName, append=TRUE )
  write( x=paste(paste(parsADMB[[3]], collapse=", "), "\t# ", 
          names(parsADMB)[3], sep=""), file=fName, append=TRUE )
  write( x=paste(paste(parsADMB[[4]], collapse=", "), "\t# ", 
          names(parsADMB)[4], sep=""), file=fName, append=TRUE )
  write( x=paste(paste(parsADMB[[5]], collapse=", "), "\t# ", 
          names(parsADMB)[5], sep=""), file=fName, append=TRUE )
  
  # Write header for catch data
  write( x="#\n##### Catch (t*10^3) #####", file=fName, append=TRUE )
  # Write catch column names
  write( x=paste("#", paste(colnames(catchADMB), collapse="\t")), file=fName, 
      append=TRUE )
  # Write catch data
  write.table( x=catchADMB, file=fName, append=TRUE, sep="\t", row.names=FALSE,
      col.names=FALSE )
  
  # Write header for spawn data
  write( x="#\n##### Spawn (t*10^3) #####", file=fName, append=TRUE )
  # Write spawn information
  write( x=paste(2, "\t# Number of survey types", sep=""), file=fName, 
      append=TRUE )
  write( x=paste(paste(c(newSurvYr-min(yrRange), max(yrRange)-newSurvYr+1), 
              collapse="\t"), "\t# Number of years per survey", sep=""), 
      file=fName, append=TRUE )
  write( x=paste(paste(c(3, 3), collapse="\t"), 
          "\t# Survey type (1=vulnerable numbers, 2=vulnerable biomass,", 
          " 3=spawning biomass)", sep=""), file=fName, append=TRUE )
  # Write spawn column names
  write( x=paste("#", paste(colnames(spawnADMB), collapse="\t")), file=fName, 
      append=TRUE )
  # Write spawn data
  write.table( x=spawnADMB, file=fName, append=TRUE, sep="\t", row.names=FALSE,
      col.names=FALSE )
  
  # Write header for number-at-age data
  write( x="#\n##### Number-at-age #####", file=fName, append=TRUE )
  # Determine model aged dimensions
  dimNumAged <- numAgedADMB %>% 
      group_by( Period ) %>% 
      summarise( Number=n() ) %>%
      ungroup( )
  # Write number aged dimensions
  write( x=paste(nrow(dimNumAged), "\t# Number of periods", sep=""), 
      file=fName, append=TRUE )
  write( x=paste(paste(dimNumAged$Number, collapse="\t"), 
          "\t# Number of years per period", sep=""), file=fName, append=TRUE )
  write( x=paste(paste(rep(min(ageRange), times=3), collapse="\t"), 
          "\t# Youngest age", sep=""), file=fName, append=TRUE )
  write( x=paste(paste(rep(max(ageRange), times=3), collapse="\t"), 
          "\t# Plus group", sep=""), file=fName, append=TRUE )
  # Write number aged column names
  write( x=paste("#", paste(colnames(numAgedADMB), collapse="\t")), file=fName, 
      append=TRUE )
  # Write number aged data
  write.table( x=numAgedADMB, file=fName, append=TRUE, sep="\t", 
      row.names=FALSE, col.names=FALSE )
  
  # Write header for weight-at-age data
  write( x="#\n##### Weight-at-age (kg) #####", file=fName, append=TRUE )
  # Write weight-at-age dimensions
  write( x=paste(nrow(weightAgeADMB), "\t# Number of years", sep=""), 
      file=fName, append=TRUE)
  # Write weight-at-age column names
  write( x=paste("#", paste(colnames(weightAgeADMB), collapse="\t")), 
      file=fName, append=TRUE )
  # Write weight-at-age data
  write.table( x=weightAgeADMB, file=fName, append=TRUE, sep="\t", 
      row.names=FALSE, col.names=FALSE )
  
  # End of file message
  write( x="#\n# eof\n999", file=fName, append=TRUE )
}  # End WriteInputFile function

# Write ADMB input file
WriteInputFile( )

# TODO: Remove this once the model has been run (this is for testing only!)
if( region %in% allRegions$major ) {
  # Get the file name
  fName <- file.path( regName, paste(regName, "Herring", max(yrRange), ".dat", 
          sep="") )
  # Copy the input file to the model output location
  file.copy( from=fName, to=file.path("..", "ModelOutput", fName), 
      overwrite=TRUE )
}  # End if major


###################
##### Figures #####
###################

# Progress message
cat( "Printing figures... " )

# Plot the BC coast and regions
BCMap <- ggplot( data=shapes$landAllCropDF, aes(x=Eastings, y=Northings) ) +
    geom_polygon( data=shapes$landAllCropDF, aes(group=group), 
        fill="lightgrey" ) +
    geom_point( data=shapes$extAllDF, colour="transparent" ) +
    geom_path( data=shapes$regAllDF, aes(group=Region), size=0.75, 
        colour="black" ) + 
    geom_label( data=shapes$regCentDF, alpha=0.5, aes(label=Region) ) +
    annotate( geom="text", x=1100000, y=800000, label="British\nColumbia",
        size=5 ) +
    annotate( geom="text", x=650000, y=550000, label="Pacific\nOcean", 
        size=5 ) +
    coord_equal( ) +
    labs( x="Eastings (km)", y="Northings (km)", caption=geoProj ) +
    scale_x_continuous( labels=function(x) comma(x/1000), expand=c(0, 0) ) + 
    scale_y_continuous( labels=function(x) comma(x/1000), expand=c(0, 0) ) +
    myTheme +
    ggsave( filename=file.path(regName, "BC.pdf"), width=figWidth, 
        height=min(8, 5.75/shapes$xyAllRatio) )

# Create a base map for the region
BaseMap <- ggplot( data=shapes$landCropDF, aes(x=Eastings, y=Northings) ) +
    geom_polygon( data=shapes$landCropDF, aes(group=group), fill="lightgrey" ) +
    geom_point( data=shapes$extDF, colour="transparent" ) +
    geom_polygon( data=shapes$secDF %>% filter(id %in% c(132, 135)), 
        aes(group=Section), fill="black", colour="transparent", alpha=0.35 ) +
    geom_path( data=shapes$regDF, aes(group=Region), size=0.75, 
        colour="black", linetype="dashed" ) +
    coord_equal( ) +
    labs( x="Eastings (km)", y="Northings (km)", caption=geoProj ) +
    scale_x_continuous( labels=function(x) comma(x/1000), expand=c(0, 0) ) + 
    scale_y_continuous( labels=function(x) comma(x/1000), expand=c(0, 0) ) +
    myTheme 

# Plot the region, and statistical areas
RegionMap <- BaseMap +
    geom_path( data=shapes$saDF, aes(group=StatArea), size=0.25, 
        colour="black" ) + 
    {if( nrow(shapes$saCentDF) >= 1 )  
        geom_label( data=shapes$saCentDF, alpha=0.25,
            aes(label=paste("SA", StatArea, sep=" ")) )} +
    ggsave( filename=file.path(regName, "Region.pdf"), width=figWidth, 
        height=min(8, 6.5/shapes$xyRatio) )

# Plot catch by year and gear type (i.e., period)
catchGearPlot <- ggplot( data=catch, aes(x=Year, y=Catch) ) + 
    geom_bar( stat="identity", position="stack", aes(fill=Period) ) +
    labs( y=expression(paste("Catch (t"%*%10^3, ")", sep="")) )  +
    scale_x_continuous( breaks=yrBreaks ) +
    scale_y_continuous( labels=function(x) comma(x/1000) ) +
    scale_fill_grey( start=0, end=0.8 ) + 
    myTheme +
    theme( legend.position=c(0.99, 0.98), legend.justification=c(1, 1) ) +
    ggsave( filename=file.path(regName, "CatchGear.pdf"), width=figWidth, 
        height=figWidth/2 )

# If weight by catch type
if( exists("weightCatchFig") ) {
  # Plot weight by year and catch type
  weightCatchPlot <- ggplot( data=weightCatchFig, 
          aes(x=Year, y=Weight, fill=SampleSource2, 
              group=interaction(Year, SampleSource2)) ) + 
      geom_boxplot( outlier.colour="black", size=0.25 ) + 
      labs( y="Weight (g)", fill="Sample type" )  +
      scale_x_continuous( breaks=pretty_breaks() ) +
      scale_fill_grey( start=0, end=0.8 ) + 
      geom_hline( data=weightCatchFigMu, aes(yintercept=MuWeight), size=0.25,
          linetype="dashed" ) + 
      expand_limits( y=0 ) +
      myTheme +
      theme( legend.position="top" ) +
      ggsave( filename=file.path(regName, "WeightCatch.pdf"), width=figWidth, 
          height=3.5 )
}  # End if weight by catch type

# If catch by stat area
if( exists("catchStatAreaFig") ) {
  # Plot catch by year and gear type (i.e., period)
  catchStatAreaPlot <- ggplot( data=catchStatAreaFig, aes(x=Year, y=Catch,
              fill=Year==max(yrRange) ) ) + 
      geom_bar( stat="identity", position="stack" ) +
      labs( y=expression(paste("Catch (t"%*%10^3, ")", sep="")) )  +
      scale_x_continuous( breaks=yrBreaks ) +
      scale_y_continuous( labels=function(x) comma(x/1000) ) +
      scale_fill_grey( start=0.5, end=0 ) +
      facet_wrap( ~ SA, labeller=label_both ) +
      myTheme +
      theme( legend.position="none" ) +
      ggsave( filename=file.path(regName, "CatchStatArea.pdf"), width=figWidth, 
          height=figWidth/2 )
}  # End if catch by stat area

# Plot biosample locations
if( nrow(bioLocations) > 0 ) 
  bioLocPlot <- BaseMap +
      geom_path( data=shapes$secDF, aes(group=Section), size=0.25, 
          colour="black" ) +
      geom_text( data=shapes$secCentDF, alpha=0.6, size=2,
          aes(label=paste("Sec", Section, sep=" ")) ) +
      geom_point( data=bioLocations, aes(shape=Type, size=Number), alpha=0.6 ) +
      scale_size( breaks=pretty_breaks(), guide=guide_legend(order=2) ) +
      labs( shape="Sample type" ) +
      guides( size=guide_legend(order=1) ) +
      theme( legend.justification=c(0, 0), legend.box.just="left", 
          legend.position=if(region == "PRD") "right" else c(0.01, 0.01),
          legend.box=if(region %in% c("WCVI", "JS")) "horizontal" else 
                "vertical" ) +
      ggsave( filename=file.path(regName, "BioLocations.pdf"), width=figWidth, 
          height=min(8, 6.5/shapes$xyRatio) )

# Plot proportion-at-age by year
propAgedPlot <- ggplot( data=numAgedYear, aes(x=Year)  ) +
    geom_point( aes(y=Age, size=Proportion) ) +
    geom_path( data=qAgedYear, aes(y=MeanAge, group=GroupID) ) +
    geom_ribbon( data=qAgedYear, aes(ymin=Lower, ymax=Upper, group=GroupID), 
        alpha=0.25 ) +
    scale_size( range=c(0, 3) ) +
    labs( x=NULL, y="Proportion-at-age" ) +
    scale_x_continuous( breaks=yrBreaks ) +
    scale_y_continuous( breaks=pretty_breaks() ) +
    coord_cartesian( xlim=range(yrRange) ) +
    myTheme +
    theme( legend.position="top", axis.text.x=element_blank() )

# Plot number aged by year
numAgedPlot <- ggplot( data=numAgedYear, aes(x=Year, y=Number) ) +
    geom_bar( stat="identity", width=0.9 ) +
    labs( y=expression(paste("Number aged"%*%10^3, sep="")) )  +
    scale_x_continuous( breaks=yrBreaks ) +
    scale_y_continuous( labels=function(x) comma(x/1000) ) +
    coord_cartesian( xlim=range(yrRange) ) +
    myTheme

# Arrange and save the proportion-at-age and number aged plots
pnPlots <- plot_grid( propAgedPlot, numAgedPlot, align="v", ncol=1, 
        rel_heights=c(3, 2), labels="auto", label_size=12, hjust=0, vjust=1 ) +
    ggsave( filename=file.path(regName, "ProportionAged.pdf"), width=figWidth, 
        height=figWidth )

# Plot weight-at-age by year
weightAgePlot <- ggplot( data=muWeightAge ) + 
    geom_line( aes(x=Year, y=muWeight, group=Age) ) +
    geom_point( data=filter(.data=muWeightAge, Age == ageShow), 
        aes(x=Year, y=Weight), shape=1, size=1 ) +
    geom_line( data=filter(.data=muWeightAge, Age == ageShow), 
        aes(x=Year, y=muWeight), size=1 ) +
    labs( y="Weight-at-age (g)" ) +
    scale_x_continuous( breaks=yrBreaks ) +
    coord_cartesian( ylim=wtRange ) +
    myTheme +
    ggsave( filename=file.path(regName, "WeightAge.pdf"), width=figWidth, 
        height=figWidth/2 )

# If weight by age and group
if( exists("weightAgeGroup") ) {
  # Function to count sample size (i.e., number of observations) 
  CountN <- function( x ) {
    # Specify the y location
    yLoc <- max( x )
    # Count the sample size
    nObs <- length( x )
    # Return the location and number
    return( c(y=yLoc, label=nObs) ) 
  }  # End CountN function
  # Plot weight by age and group 
  weightAgeGroupPlot <- ggplot( data=weightAgeGroup, 
          aes(x=Age, y=Weight, fill=Group, group=interaction(Age, Group)) ) + 
      geom_boxplot( outlier.colour="black", size=0.25 ) + 
      labs( y="Weight (g)", fill="SA" )  +
      scale_x_continuous( breaks=pretty_breaks() ) +
      scale_fill_grey( start=0, end=0.8 ) + 
      expand_limits( y=0 ) +
#      stat_summary( fun.data=CountN, geom="text" ) +
      facet_grid( . ~ Decade ) +
      myTheme +
      theme( legend.position="top" ) +
      ggsave( filename=file.path(regName, "WeightAgeGroup.pdf"), width=figWidth, 
          height=figWidth*0.67 ) +
      ggsave( filename=file.path(regName, "WeightAgeGroup.jpg"), width=figWidth, 
          height=figWidth*0.67 )
}  # End if weight by age and group

# Plot the spawn index locations
spawnByLocPlot <- BaseMap + 
    geom_path( data=shapes$secDF, aes(group=Section), size=0.25, 
        colour="black" ) +
    geom_text( data=shapes$secCentDF, alpha=0.6, size=2,
        aes(label=paste("Sec", Section, sep=" ")) ) +
    geom_point( data=spawnByLocXY, aes(colour=TotalSI), alpha=0.75, size=4 ) +
    scale_colour_distiller( type="seq", palette="Spectral", labels=comma ) +
    labs( colour="Spawn index (t)" ) +
    theme( legend.justification=c(0, 0), legend.box.just="left", 
        legend.position=if(region == "PRD") "right" else c(0.01, 0.01) ) +
    ggsave( filename=file.path(regName, "SpawnByLoc.pdf"), width=figWidth, 
        height=min(7.5, 6.5/shapes$xyRatio) )

# Plot the spawn index locations
spawnDecadePlot <- BaseMap + 
    geom_path( data=shapes$secDF, aes(group=Section), size=0.25, 
        colour="black" ) +
    geom_text( data=shapes$secCentDF, alpha=0.6, size=2,
        aes(label=paste("Sec", Section, sep=" ")) ) +
    geom_point( data=spawnDecade, aes(colour=MeanSI, size=Frequency), 
        alpha=0.75 ) +
    scale_colour_distiller( type="seq", palette="Spectral", labels=comma) +
    scale_size( breaks=pretty_breaks(), guide=guide_legend(order=2) ) +
    labs( colour="Mean spawn\nindex (t)" ) +
    theme( legend.position="top", legend.text.align=0.5 ) +
    ggsave( filename=file.path(regName, "SpawnDecade.pdf"), width=figWidth, 
        height=4.5 )

# Plot total spawn length by year
spawnLengthPlot <- ggplot( data=spawnYrFig, aes(x=Year, y=TotalLength) ) +
    geom_point( aes(shape=Year>=newSurvYr) ) + 
    geom_line( aes(group=Year>=newSurvYr) ) +
    stat_smooth( method=smLine, colour="black", level=ciLevel ) +
    labs( y=expression(paste("Total spawn length (m"%*%10^3, ")", sep="")) )  +
    scale_x_continuous( breaks=yrBreaks ) +
    scale_y_continuous( labels=function(x) comma(x/1000) ) +
    guides( shape=FALSE ) +
    expand_limits( y=0 ) +
    myTheme +
    ggsave( filename=file.path(regName, "SpawnLength.pdf"), width=figWidth, 
        height=figWidth/2 )

# Plot mean spawn width by year
spawnWidthPlot <- ggplot( data=spawnYrFig, aes(x=Year, y=MeanWidth) ) +
    geom_point( aes(shape=Year>=newSurvYr) ) + 
    geom_line( aes(group=Year>=newSurvYr) ) +
    stat_smooth( method=smLine, colour="black", level=ciLevel ) +
    labs( y="Mean spawn width (m)" ) + 
    scale_x_continuous( breaks=yrBreaks ) +
    scale_y_continuous( labels=comma ) +
#    scale_y_log10( breaks=pretty(spawnYrFig$MeanWidth), labels=comma ) +
    guides( shape=FALSE ) +
    expand_limits( y=0 ) +
    myTheme +
    ggsave( filename=file.path(regName, "SpawnWidth.pdf"), width=figWidth, 
        height=figWidth/2 )

# Plot mean layers of spawn by year
spawnLayersPlot <- ggplot( data=spawnYrFig, aes(x=Year, y=MeanLayers) ) +
    geom_point( aes(shape=Year>=newSurvYr) ) + 
    geom_line( aes(group=Year>=newSurvYr) ) +
    stat_smooth( method=smLine, colour="black", level=ciLevel ) +
    labs( y="Mean number of egg layers" ) + 
    scale_x_continuous( breaks=yrBreaks ) +
    scale_y_continuous( labels=comma ) +
    guides( shape=FALSE ) +
    expand_limits( y=0 ) +
    myTheme +
    ggsave( filename=file.path(regName, "SpawnLayers.pdf"), width=figWidth, 
        height=figWidth/2 )

# Plot total spawn index by year
spawnIndexPlot <- ggplot( data=spawnYrFig, aes(x=Year, y=TotalSI) ) +
    geom_point( aes(shape=Year>=newSurvYr) ) + 
    geom_line( aes(group=Year>=newSurvYr) ) +
    stat_smooth( method=smLine, colour="black", level=ciLevel ) +
    labs( x=NULL, y=expression(paste("Spawn index (t"%*%10^3, ")", sep="")) )  +
    scale_x_continuous( breaks=yrBreaks ) +
    scale_y_continuous( labels=function(x) comma(x/1000) ) +
    coord_cartesian( xlim=range(spawnYrFig$Year) ) +
    guides( shape=FALSE ) +
    expand_limits( y=0 ) +
    myTheme +
    theme( axis.text.x=element_blank() )

# If using groups
if( exists("spawnYrGrpFig") ) { 
  # Plot percent spawn index by group
  spawnPercentSAStackPlot <- ggplot( data=spawnYrGrpFig, 
          aes(x=Year, y=PercSI, fill=Group) ) +
      geom_bar( stat="identity", width=1, colour="black", size=0.1 ) +
      labs( x=NULL, y="Spawn index (%)", fill="Group" ) + 
      scale_x_continuous( breaks=yrBreaks ) +
      scale_y_continuous( labels=comma ) +
      coord_cartesian( xlim=range(spawnYrFig$Year) ) +
      myTheme +
      theme( legend.position="bottom", axis.text.x=element_blank() )
} else {  # End if using groups, otherwise stat areas
  # Plot percent spawn index by group (stat areas)
  spawnPercentSAStackPlot <- ggplot( data=spawnYrSAFig, 
          aes(x=Year, y=PercSI, fill=StatArea) ) +
      geom_bar( stat="identity", width=1, colour="black", size=0.1 ) +
      labs( x=NULL, y="Spawn index (%)", fill="SA" ) + 
      scale_x_continuous( breaks=yrBreaks ) +
      scale_y_continuous( labels=comma ) +
      coord_cartesian( xlim=range(spawnYrFig$Year) ) +
      myTheme +
      theme( legend.position="bottom", axis.text.x=element_blank() )
}  # End if using stat areas

# Determine the number of sections
nSecCol <- n_distinct( spawnYrSecFig$Section )

# Make nice brewer colours
myColSec <- colorRampPalette( colors=brewer.pal(n=min(nSecCol, 12), 
        name="Set3") )

# Plot proportion of spawn in each section and year
spawnPercentSecStackPlot <- ggplot( data=spawnYrSecFig, 
        aes(x=Year, y=PercSI, fill=Section) ) +
    geom_bar( stat="identity", width=1, colour="black", size=0.1 ) +
    labs( y="Spawn index (%)" ) + 
    scale_x_continuous( breaks=yrBreaks ) +
    scale_y_continuous( labels=comma ) +
    coord_cartesian( xlim=range(spawnYrFig$Year) ) +
    scale_fill_manual( values=myColSec(nSecCol), 
        guide=guide_legend(nrow=ceiling(nSecCol/8)) ) +
    myTheme +
    theme( legend.position="bottom" )

# Arrange and save the index and proportion plots
ipPlots <- plot_grid( spawnIndexPlot, spawnPercentSAStackPlot, 
        spawnPercentSecStackPlot, align="v", ncol=1, 
        rel_heights=c(2, 2.5, 2.5), labels="auto", label_size=12, hjust=0, 
        vjust=1 ) +
    ggsave( filename=file.path(regName, "SpawnIndexPercent.pdf"), 
        width=figWidth, height=6.9 ) 

# Plot percent contribution by Section faceted by Stat Area or Group
PlotPCSecSA <- function( dat ) {
  # Rename the Statistical Area column
  dat <- rename( .data=dat, SA=StatArea )
  # Start a list to hold plots
  pList <- list( )
  # Set group name
  gName <- ifelse( exists("spawnYrGrpFig"), "Group", "SA" )
  # Get unique group names
  uGroups <- unique( dat[[gName]] )[order(unique(dat[[gName]]))]
  # Get the number of plots
  nPlots <- length( uGroups )
  # Get the maximum y range
  yUpper <- dat %>% 
      group_by_( "Year", gName ) %>% 
      summarise( Total=SumNA(PercSI, omitNA=TRUE) ) %>% 
      ungroup( )
  # Get the y limits
  yLims <- c( 0, max(yUpper$Total, na.rm=TRUE) )
  # Loop over groups
  for( i in 1:nPlots ) {
    # Subset the data
    datSub <- dat[dat[[gName]] == uGroups[i], ]
    # Make the plot
    pList[[i]] <- ggplot( data=datSub, 
            aes_string(x="Year", y="PercSI", fill="Section") ) + 
        geom_bar( stat="identity", width=1, colour="black", size=0.1 ) +
        labs( x=NULL, y=NULL ) +
        scale_x_continuous( breaks=yrBreaks ) +
        scale_y_continuous( labels=comma ) +
        coord_cartesian( xlim=range(spawnYrFig$Year), ylim=yLims ) +
        scale_fill_discrete( guide=
                guide_legend(ncol=ceiling(length(unique(datSub$Section))/6)) ) +
        facet_grid( paste(gName, "~ ."), labeller=label_both ) + 
        myTheme 
    # Update the plot if it's not the bottom plot
    if( i != nPlots )  pList[[i]] <- pList[[i]] +
          theme( axis.text.x=element_blank() )  
  }  # End i loop over groups
  # Arrange into a grid
  pGrid <- plot_grid( plotlist=pList, align="v", ncol=1 )
  # Make a title for the x-axis
  titleX <- ggdraw( ) +
      draw_label("Year", size=12, x=0.4 )
  # Combine the x-axis title
  pGridX <- plot_grid( pGrid, titleX, ncol=1, rel_heights=c(1, 0.05) )
  # Make a title for the y-axes
  titleY <- ggdraw() + 
      draw_label("Spawn index (%)", angle=90, size=12 )
  # Combine the y-axis title, and save the plots
  pGridXY <- plot_grid( titleY, pGridX, ncol=2, rel_widths=c(0.04, 1) ) +
      ggsave( filename=file.path(regName, "SpawnPercentGrid.pdf"), 
          width=figWidth, height=min(7.1, length(pList)*3) )
  # Return the plot list
  return( pGridXY )
}  # End PlotPCSecSA function

# Get the list of plots showing percent contribution
plotGridPCs <- PlotPCSecSA( dat=spawnYrSecFig )

# Plot proportion of spawn in each section and year
spawnPercentPanelPlot <- ggplot( data=spawnYrSecFig, 
        aes(x=Year, y=PercSI, fill=Year==max(yrRange)) ) +
    geom_bar( stat="identity" ) +
    labs( y="Spawn index (%)" ) + 
    scale_x_continuous( breaks=yrBreaks ) +
    scale_y_continuous( labels=comma ) +
    scale_fill_grey( start=0.5, end=0 ) +
    facet_wrap( ~ Section, labeller=label_both, drop=TRUE, ncol=3 ) +
    myTheme +
    theme( legend.position="none" ) +
    ggsave( filename=file.path(regName, "SpawnPercentPanel.pdf"), 
        width=figWidth, 
        height=min(7.3, 1.5*(ceiling(n_distinct(spawnYrSecFig$Section)/3))) )

# Plot index of spawn in each section and year
spawnIndexPanelPlot <- ggplot( data=spawnYrSecFig, 
        aes(x=Year, y=TotalSI, fill=Year==max(yrRange)) ) +
    geom_bar( stat="identity" ) +
    labs( y=expression(paste("Spawn index (t"%*%10^3, ")", sep="")) ) + 
    scale_x_continuous( breaks=yrBreaks ) +
    scale_y_continuous( labels=function(x) comma(x/1000) ) +
    scale_fill_grey( start=0.5, end=0 ) +
    facet_wrap( ~ Section, labeller=label_both, drop=TRUE, ncol=3 ) +
    myTheme +
    theme( legend.position="none" ) +
    ggsave( filename=file.path(regName, "SpawnIndexPanel.pdf"), 
        width=figWidth, 
        height=min(7.9, 1.5*(ceiling(n_distinct(spawnYrSecFig$Section)/3))) )

# If spawn depth by year and statistical area
if( exists("spawnStatsYrSA") & exists("spawnStatsYrSecSA07") ) {
  # Determine the number of sections
  nSecCol <- n_distinct( spawnStatsYrSecSA07$Section )
  # Plot spawn depth (maximum by spawn number) in each statistical area and year
  spawnDepthSAPlot <- ggplot( data=spawnStatsYrSA, 
          aes(x=Year, y=Depth, fill=StatArea, 
              group=interaction(Year, StatArea)) ) +
      geom_boxplot( outlier.colour="black", outlier.size = 0.5, size=0.25 ) +
      scale_fill_grey( start=0, end=0.8 ) +
      expand_limits( y=0 ) +
      labs( x=NULL, y="Maximum spawn depth (m)", fill="SA" ) + 
      scale_x_continuous( breaks=yrBreaks ) +
      myTheme +
      theme( legend.position="top", axis.text.x=element_blank() )
  # Plot spawn depth (maximum by spawn number) in each section and year  
  spawnDepthSecPlot <- ggplot( data=spawnStatsYrSecSA07, 
          aes(x=Year, y=Depth, fill=Section, 
              group=interaction(Year, Section)) ) +
      geom_boxplot( outlier.colour="black", outlier.size = 0.5, size=0.25 ) +
      expand_limits( y=0 ) +
      labs( y="Maximum spawn depth (m)" ) + 
      scale_x_continuous( breaks=yrBreaks ) +
      guides( fill=guide_legend(nrow=ceiling(nSecCol/8)) ) +
      myTheme +
      theme( legend.position="bottom" )
  # Arrange and save the depth plots
  depthPlots <- plot_grid( spawnDepthSAPlot, spawnDepthSecPlot, align="v", 
          ncol=1, rel_heights=c(2, 2), labels="auto", label_size=12, hjust=0, 
          vjust=1 ) +
      ggsave( filename=file.path(regName, "SpawnDepthSASec.pdf"), 
          width=figWidth, height=figWidth )
  # Plot spawn layers in each statistical area and year
  spawnLayersSAPlot <- ggplot( data=spawnStatsYrSA, 
          aes(x=Year, y=Layers, fill=StatArea, 
              group=interaction(Year, StatArea)) ) +
      geom_boxplot( outlier.colour="black", outlier.size = 0.5, size=0.25 ) +
      scale_fill_grey( start=0, end=0.8 ) +
      expand_limits( y=0 ) +
      labs( x=NULL, y="Number of egg layers", fill="SA" ) + 
      scale_x_continuous( breaks=yrBreaks ) +
      myTheme +
      theme( legend.position="top", axis.text.x=element_blank() )
  # Plot spawn layers in each section and year  
  spawnLayersSecPlot <- ggplot( data=spawnStatsYrSecSA07, 
          aes(x=Year, y=Layers, fill=Section, 
              group=interaction(Year, Section)) ) +
      geom_boxplot( outlier.colour="black", outlier.size = 0.5, size=0.25 ) +
      expand_limits( y=0 ) +
      labs( y="Number of egg layers" ) + 
      scale_x_continuous( breaks=yrBreaks ) +
      guides( fill=guide_legend(nrow=ceiling(nSecCol/8)) ) +
      myTheme +
      theme( legend.position="bottom" )
  # Arrange and save the depth plots
  layerPlots <- plot_grid( spawnLayersSAPlot, spawnLayersSecPlot, align="v", 
          ncol=1, rel_heights=c(2, 2), labels="auto", label_size=12, hjust=0, 
          vjust=1 ) +
      ggsave( filename=file.path(regName, "SpawnLayersSASec.pdf"), 
          width=figWidth, height=figWidth )
}  # End if spawn depth by year and statistical area

# Update progress
cat( "done\n" )


###################
##### xTables #####
###################

# Format regions table
xRegions <- regions %>%
    mutate( Major=ifelse(Major, "Major", "Minor") ) %>%
    rename( Name=RegionName, Code=Region, Type=Major ) %>%
    select( Name, Code, Type ) %>%
    xtable( )

# Write regions to disc
print( x=xRegions, file=file.path(regName, "Regions.tex"),
    include.rownames=FALSE, booktabs=TRUE, only.contents=TRUE, NA.string=NA )

# Format commercial catch
xCatchCommUseYr <- catchCommUseYr %>%
    mutate( Catch=format(Catch, big.mark=",", digits=0, scientific=FALSE) ) %>%
    rename( 'Catch (t)'=Catch ) %>%
    xtable( )

# Write commercial catch to disc
print( x=xCatchCommUseYr, file=file.path(regName, "CatchCommUseYr.tex"),
    include.rownames=FALSE, booktabs=TRUE, only.contents=TRUE, NA.string=NA )

# Format commercial harvest
xHarvestSOK <- harvestSOK %>%
    select( Year, Harvest, SoG ) %>%
    mutate( Year=as.integer(Year), 
        Harvest=format(Harvest, big.mark=",", digits=0, scientific=FALSE),
        SoG=format(SoG, digits=0, scientific=FALSE) ) %>%
    rename( 'Harvest (lb)'=Harvest ) %>%
    xtable( )

# Write commercial harvest (longtable) to disc
WriteLongTable( dat=xHarvestSOK, fn=file.path(regName, "HarvestSOK.tex") )

# Format catch
xCatchADMB <- catchADMB %>%
    full_join( y=catchSoG, by="Year" ) %>%
    replace_na( replace=list(SoG=0) ) %>%
    mutate( SoG=format(SoG, digits=0, scientific=FALSE) ) %>%
    select( Year, Period1, Period2, Period3, SoG ) %>%
    rename( Gear1=Period1, Gear2=Period2, Gear3=Period3 ) %>%
    xtable( )

# Write catch (longtable) to disc
WriteLongTable( dat=xCatchADMB, fn=file.path(regName, "CatchADMB.tex") )

# Format number of biosamples
xBioNum <- bioNum %>%
    mutate( Year=as.integer(Year), Commercial=as.integer(Commercial),
        Test=as.integer(Test), SoG=format(SoG, digits=0, scientific=FALSE) ) %>%
    xtable()

# Write number of biosamples (longtable) to disc
WriteLongTable( dat=xBioNum, fn=file.path(regName, "BioNum.tex") )

# Format number of biosamples by type
xBioTypeNum <- bioTypeNum %>%
    mutate( Number=as.integer(Number) ) %>%
    rename( 'Number of samples'=Number ) %>%
    xtable( )

# Write number of biosamples by type to disc
print( x=xBioTypeNum, file=file.path(regName, "BioTypeNum.tex"),
    include.rownames=FALSE, booktabs=TRUE, only.contents=TRUE, NA.string=NA )

# Number-, proportion-, and weight-at-age
if( exists("deltaNumAgeYr") & exists("deltaPropAgeYr") & 
    exists("deltaWtAgeYr") ) {
  # Format number-at-age
  xDeltaNumAgeYr <- deltaNumAgeYr %>%
      xtable( digits=c(0, 0, rep(0, times=length(ageRange))) )
  # Write number-at-age to disc
  print( x=xDeltaNumAgeYr, file=file.path(regName, "DeltaNumAgeYr.tex"),
      include.rownames=FALSE, booktabs=TRUE, only.contents=TRUE, NA.string=NA )
  # Format proportion-at-age
  xDeltaPropAgeYr <- deltaPropAgeYr %>%
      xtable( digits=c(0, 0, rep(3, times=length(ageRange))) )
  # Write proportion-at-age to disc
  print( x=xDeltaPropAgeYr, file=file.path(regName, "DeltaPropAgeYr.tex"),
      include.rownames=FALSE, booktabs=TRUE, only.contents=TRUE, NA.string=NA )
  # Format weight-at-age
  xDeltaWtAgeYr <- deltaWtAgeYr %>%
      xtable( digits=c(0, 0, rep(1, times=length(ageRange))) )
  # Write weight-at-age to disc
  print( x=xDeltaWtAgeYr, file=file.path(regName, "DeltaWtAgeYr.tex"),
      include.rownames=FALSE, booktabs=TRUE, only.contents=TRUE, NA.string=NA )
}  # End if number-, proportion-, and weight-at-age

# Format proportion-at-age
xPropAgedYearTab <- propAgedYearTab %>%
    xtable( digits=c(0, 0, rep(3, times=length(ageRange))) )

# Write proportion-at-age (longtable) to disc
WriteLongTable( dat=xPropAgedYearTab, 
                fn=file.path(regName, "PropAgedYearTab.tex") )

# If weight-at-age by group exists
if( exists("weightAgeGroupN") ) {
  # Format weight-at-age
  xWeightAgeGroupN <- weightAgeGroupN %>%
      mutate( 
          Previous=format(Previous, big.mark=",", scientific=FALSE),
          Recent=format(Recent, big.mark=",", scientific=FALSE) ) %>%
      rename( 'Previous decade'=Previous, 'Recent decade'=Recent ) %>%
      xtable( digits=c(0, 0, 0, 0, 0) )
  # Write weight-at-age to disc
  print( x=xWeightAgeGroupN, file=file.path(regName, "WeightAgeGroupN.tex"),
      include.rownames=FALSE, booktabs=TRUE, only.contents=TRUE, NA.string=NA )
}  # End if weight-at-age exists

# Format spawn summary
xSpawnYrTab <- spawnYrTab %>%
    mutate( TotalLength=format(TotalLength, big.mark=",", digits=0, 
            scientific=FALSE),
        MeanWidth=format(MeanWidth, big.mark=",", digits=0),
        MeanLayers=format(MeanLayers, big.mark=",", digits=3),
        TotalSI=format(TotalSI, big.mark=",", digits=0, 
            scientific=FALSE),
        SoG=format(SoG, digits=0, scientific=FALSE)) %>%
    rename( 'Total length (m)'=TotalLength, 'Mean width (m)'=MeanWidth,
        'Mean number of egg layers'=MeanLayers, 'Spawn index (t)'=TotalSI ) %>%
    xtable( )

# Write spawn summary (longtable) to disc
WriteLongTable( dat=xSpawnYrTab, fn=file.path(regName, "SpawnYrTab.tex") )

# If there is spawn reported
if( nrow(spawnByLoc) >= 1 ) {
  # Format spawn summary
  xSpawnByLoc <- spawnByLoc %>%
      arrange( StatArea, Section, LocationCode ) %>%
      mutate( StatArea=formatC(StatArea, width=2, format="d", flag="0"),
          Section=formatC(Section, width=3, format="d", flag="0"),
          TotalSI=format(TotalSI, big.mark=",", digits=0, 
              scientific=FALSE) ) %>%
      rename( 'Statistical Area'=StatArea, 'Location code'=LocationCode, 
          'Location name'=LocationName, 'Spawn index (t)'=TotalSI ) %>%
      xtable( )
  # Write spawn summary (longtable) to disc
  WriteLongTable( dat=xSpawnByLoc, fn=file.path(regName, "SpawnByLoc.tex") )
} else { # End if there was spawn, otherwise
  # Make an empty file (required for latex)
  write_csv( x=spawnByLoc, path=file.path(regName, "SpawnByLoc.tex") )
}  # End if there was no spawn


#################
##### LaTeX #####
#################

# Number of years in the time series
nYrs <- length( yrRange )

# Determine if region is major or minor
if( region %in% allRegions$major )  regionType <- "major"
if( region %in% allRegions$minor )  regionType <- "minor"

# Current season code
thisSeason <- paste( yrRange[nYrs-1], yrRange[nYrs], sep="/" )

# Turn the toggle to true: biosamples
tfBiosamples <- "\\toggletrue{biosamples}"
# Turn the toggle to false: spawn depth
tfSpawnDepth <- "\\togglefalse{spawnDepth}"
# Turn the toggle to false: weight by group
tfWeightGroup <- "\\togglefalse{weightGroup}"
# Turn the toggle to false: weight by catch
tfWeightCatch <- "\\togglefalse{weightCatch}"
# Turn the toggle to false: catch by stat area
tfCatchStatArea <- "\\togglefalse{catchStatArea}"
# Turn the toggle to false: spawn index by group
tfNumPropWtAge <- "\\togglefalse{numPropWtAge}"
# Turn the toggle to true: spawn this year
tfSpawnByLoc <- "\\toggletrue{spawnByLoc}"
# Turn the toggle to true: spawn this year with X and Y
tfSpawnByLocXY <- "\\toggletrue{spawnByLocXY}"

# Group name
spawnIndexGroupName <- list( a="Statistical Area", b="Statistical Area (SA)" )
# Create an empty legend
spawnIndexGroupLegend <- ""
# Get year ranges (none)
wtAgeYrRanges <- list( old="", new="" )

# If there are no biosamples to show, set the switch to false
if( nrow(bioLocations) == 0 )  tfBiosamples <- "\\togglefalse{biosamples}"

# If there was no spawn, set the switch to false
if( nrow(spawnByLoc) == 0 )  tfSpawnByLoc <- "\\togglefalse{spawnByLoc}"

# If there was no spawn to show on the map, set the switch to false
if( nrow(spawnByLocXY) == 0 )  tfSpawnByLocXY <- "\\togglefalse{spawnByLocXY}"

# If spawn index is summarized by group: set the group name
if( exists("spawnYrGrpFig") )  
  spawnIndexGroupName <- list( a="Group", b="Group" )

# If spawn depth by year and statistical area: set toggle to true
if( exists("spawnStatsYrSA") & exists("spawnStatsYrSecSA07") )  
  tfSpawnDepth <- "\\toggletrue{spawnDepth}"

# If weight by age and group
if( exists("weightAgeGroup") ) {
  # Set the toggle to true
  tfWeightGroup <- "\\toggletrue{weightGroup}"
  # Get year ranges
  wtAgeYrRanges <- list(
      recent=weightAgeGroup %>% 
          filter( Decade=="Recent" ) %>% 
          select( Year ) %>% 
          range( ) %>% 
          paste( collapse=" to "),
      previous=weightAgeGroup %>% 
          filter( Decade=="Previous" ) %>% 
          select( Year ) %>% 
          range( ) %>% 
          paste( collapse=" to ") )
}  # End if weight by age and group

# If weight by catch type: set the toggle to true
if( exists("weightCatchFig") )  tfWeightCatch <- "\\toggletrue{weightCatch}"

# If catch by stat area: set toggle to true
if( exists("catchStatAreaFig") )  
  tfCatchStatArea <- "\\toggletrue{catchStatArea}"

# If the region is Strait of Georgia
if( region == "SoG" ) {
  # Create a legend for Groups
  spawnIndexGroupLegend <- "Legend: `14\\&17' is Statistical Areas 14 and 17
      (excluding Section 173); `Dodd' is below Dodd Narrows; `ESoG' is eastern 
      Strait of Georgia; and `Lazo' is above Cape Lazo."
}  # End if Strait of Georgia

# If the region is Central Coast
if( region == "CC" ) {
  # Get print friendly values: historic years
  histYrs <- paste( range(yrsRatioHist), collapse=" and " )
  # Get print friendly values: historic ratio
  histRat <- propNumBioHist %>% 
      filter( Group == 8 ) %>% 
      select( SampWt ) %>% 
      mutate( SampWt=SampWt*100 ) %>% 
      round( digits=1 )
  # Get print friendly values: years to fix
  fixYrs <- PasteNicely( yrsRatioFix )
} else { # End if Central Coast, otherwise
  # Dummy values
  histYrs <- ""
  histRat <- ""
  fixYrs <- ""
}  # End if not Central Coast

# If number-, proportion, and weight-at-age: set the toggle to true
if( exists("deltaNumAgeYr") & exists("deltaPropAgeYr") & 
    exists("deltaWtAgeYr") )  tfNumPropWtAge <- "\\toggletrue{numPropWtAge}"

# Total commercial catch: this year 
tCatchThisYr <- catch %>% 
    complete( Year=yrRange, fill=list(Catch=0) ) %>%
    filter( Year == max(yrRange) ) %>% 
    select( Catch ) %>%
    sum( )

# Total commercial catch: last year
tCatchLastYr <- catch %>% 
    complete( Year=yrRange, fill=list(Catch=0) ) %>%
    filter(Year == max(yrRange)-1) %>% 
    select( Catch ) %>%
    sum( )

# Percent change in catch
tCatchPC <- 100 * (tCatchThisYr - tCatchLastYr) / tCatchLastYr

# Update percent change to zero if it's NA
if( is.na(tCatchPC) )  tCatchPC <- 0

# Change in catch
tCatchDir <- ifelse( tCatchPC == 0, "the same as", ifelse(tCatchPC > 0, 
        paste(round(abs(tCatchPC), digits=1), "\\% more than", sep=""), 
        paste(round(abs(tCatchPC), digits=1), "\\% less than", sep="")) )

# Total SOK: this year 
tSoKThisYr <- harvestSOK %>% 
    filter( Year == max(yrRange) )

# Number of samples: this year
nSampThisYr <- bioNum %>% 
    filter( Year == max(yrRange) ) %>%
    select( Total )

# If there were samples
if( nSampThisYr > 0 ) {
  # Number aged: this year
  numAgedThisYr <- numAgedYear %>%
      filter( Year == max(yrRange) ) %>%
      select( Number ) %>%
      sum( ) %>%
      formatC( big.mark="," )
} else {  # End if there were samples
  # Number aged: this year
  numAgedThisYr <- 0
}  # End if there were no samples

# Largest spawn contributor this year
pSpawnThisYr <- spawnYrSecFig %>%
    filter( Year == max(yrRange) ) %>%
    slice( which.max(PercSI) ) %>%
    mutate( Percent=round(PercSI) ) %>%
    select( Section, Percent )

# If there was spawn this year
if( nrow(spawnByLoc) >= 1 ) {
  # Number of locations surveyed for herring spawn
  nSpawnLoc <- nrow( xSpawnByLoc )
  # Column names for spawn location table
  namesSpawnLoc <- paste( names(xSpawnByLoc), collapse=" & " )
} else {  # End if spawn, otherwise
  # Number of locations surveyed for herring spawn
  nSpawnLoc <- 0
  # No names required
  namesSpawnLoc <- ""
}  # End if no spawn

# Formatted year ranges for q1 (surface) and q2 (dive)
qYrs <- list(
    q1=paste(range(yrRange[yrRange<newSurvYr]), collapse="--"),
    q2=paste(range(yrRange[yrRange>=newSurvYr]), collapse="--") )

# Justification for age tables
ageTableAlign <- paste( c("{l", rep("r", times=length(ageRange)), "}"), 
    collapse="" )


##################
##### Output #####
##################

# Save the workspace image 
save.image( file=file.path(regName, "Image.RData") ) 


############### 
##### End ##### 
############### 

# Print end of file message and elapsed time
cat( "End of file Summary.R: ", sep="" ) ;  print( Sys.time( ) - sTime )
