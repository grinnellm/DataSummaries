##### Header #####
# Author:       Matthew H. Grinnell
# Affiliation:  Pacific Biological Station, Fisheries and Oceans Canada (DFO) 
# Group:        Quantitative Assessment Methods Section, Science
# Address:      3190 Hammond Bay Road, Nanaimo, BC, Canada, V9T 6N7
# Contact:      e-mail: matt.grinnell@dfo-mpo.gc.ca | tel: 250.756.7055
# Project:      Herring
# Code name:    Summary.R
# Version:      2.0
# Date started: Jun 03, 2016
# Date edited:  Oct 22, 2018
# 
# Overview: 
# Generate tables and figures for annual Pacific Herring preliminary data
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

# TODO:
# 1. Get better section shapefile polygons (i.e., contiguous with no holes or 
#    overlapping borders).
# 2. Label extra sampling in Central Coast Area 8 as non-representative (i.e.,
#    for years when additional samples were collected)? This isn't really 
#    necessary since we have a work-around using the historic sample ratio.
# 3. Differentiate between representative, unrepresentative, and unknown 
#    samples in the database. The default should be unknown (e.g., for old 
#    samples).
# 4. Update column and variable names in the look-up tables so that I don't have 
#    to make any manual changes in the R script (e.g., Period1=Gear1). Better
#    yet, omit the need for look-up tables by having the actual variable names
#    and values in the main tables! Why isn't this the case? Especially for the
#    Gear1, Gear2, and Gear3 variables.
# 5. Calculate tide/datum corrected spawn depths.
# 6. Get better location information (i.e., X, and Y for locations).
# 12. Add SOK harvest (spawning biomass in tonnes) to the time series of landed
#     catch? That is to say, add the data from Table 2 to Figure 3 in the data
#     summary report.
# 14. Look at age composition of reduction fishery catch (i.e., did they catch
#     a fair number of age-1/immature fish?).
# 15. Show the full time-series of data for figures (1951 to present).
# 17. Catch figure: zoom into last few years (maybe last 10, or all of the post-
#     reduction era).
# 18. Consider another method to impute missing values for length- and weight-
#     at-age: for the values that are not at the start of the time series, use 
#     the mean of the surrounding values, not just the preceding values. Maybe
#     go with six (three on either side).

##### Housekeeping #####

# General options
# Tesing automatic solution to commenting out rm( list=ls() )
# if( basename(sys.frame(1)$ofile)=="Summary.R" )
# rm( list=ls( ) )      # Clear the workspace
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
  "colorRamps", "RColorBrewer", "stringr", "lubridate", "readxl", "plyr", 
  "lettercase", "ggforce", "viridis", "ggthemes") ) 

##### Controls #####

# Select region(s): major (HG, PRD, CC, SoG, WCVI); or minor (A27, A2W, JS)
if( !exists('region') )  region <- "HG"

# Sections to include for sub-stock analyses
SoGS <- c( 173, 181, 182, 191:193 )
SoGN <- c( 132, 135, 140:143, 171, 172, 151, 152, 161:165, 280, 291, 292 )
Area23 <- c( 230:233, 239 )
Area25 <- c( 250:253, 259 )
Tlaamin <- c( 135, 141, 151, 152, 161:163 )
Area15 <- c( 150:159 )
AllHG <- c( 0:6, 11, 12, 21:25 )

# Select a subset of sections (or NA for all)
sectionSub <- NA

# Generate the annual spawn survey animation (this takes a few mins!)
makeAnimation <- FALSE

# Include test fishery catch
inclTestCatch <- TRUE

# Include test seine biological data
inclTestSNBio <- TRUE

# Include test gillnet biological data
inclTestGNBio <- FALSE

# Include spawn on kelp biological data
inclSOKBio <- TRUE

# Adjust understory spawn width due to lead line shrinkage
doAdjWidth <- TRUE

# Location of herring databases (catch, biosamples, spawn, etc)
dirDBs <- file.path( "..", "Data" )

# Location of the shapefiles
#dirShape <- file.path( "\\\\dcbcpbsna01a", "hdata$", "Kristen", 
#    "Herring_Shapefiles" )
dirShape <- file.path( dirDBs, "Polygons" )

# Databases: remote (i.e., H:\ for hdata$) or local (e.g., C:\)
dbLoc <- "Local"

# Database name
dbName <- "HSA_Program_v6.2.mdb"

# Input coordinate reference system
inCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Coordinate reference system (http://spatialreference.org/ref/sr-org/82/)
outCRS <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 
    +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Geographic projection
geoProj <- "Projection: BC Albers (NAD 1983)"

# Make French figures
makeFrench <- FALSE

##### Parameters #####

# Year range: include data
yrRange <- 1951:2018

# Age range: omit below, plus group above
ageRange <- 2:10

# Age to highlight in figure
ageShow <- 3

# Number of years to calculate running mean
nRoll <- 5

# Conversion factors: short tons to tonnes, feet to metres, pound to kilograms
convFac <- list( st2t=0.90718474, ft2m=0.3048, lb2kg=0.453592 )

# First year of data to include in summary tables
firstYrTab <- max( yrRange ) - 10

# First year of data to include in summary figures
firstYrFig <- max( yrRange ) - 35

# Age schedule and population parameters for model input
parsADMB <- list(
  AgePopulation=list(
    "Asymptotic length (linf)"=27,
    "Brody growth coefficient (k)"=0.48,
    "Theoretical age at zero length (t0)"=0,
    "Scalar in length-weight allometry (alpha)"=4.5e-6,
    "Power parameter in length-weight allometry (beta)"=3.127,
    "Age at 50% maturity"=2.055,
    "Standard deviation at 50% maturity"=0.05,
    "Natural mortality indicator"=1,
    "Maturity vector"=c(0.24974, 0.9, 1, 1, 1, 1, 1, 1, 1)),
  DelayDifference=list(
    "(kage)"=0,
    "(alpha_g)"=0,
    "(rho_g)"=0,
    "(wk)"=0 ),
  General=list(
    "HistoricCatch"=20,
    "SurveyType"=3,
    "SampleSize"=100,
    "Composition"=1) )

# Spawn survey method changed from surface (1951--1987) to dive (1988--present)
newSurvYr <- 1988

# Figure width
figWidth <- 6

# Type of smoothing line
smLine <- "loess"

# Level of confidence interval
ciLevel <- 0.9

# Get ylimits (e.g., weight in g) for the weight-at-age plot
wtRange <- c( 50, 250 )

# Get ylimits (e.g., length in mm) for the length-at-age plot
lenRange <- c( 150, 275 )

# Year range for historic ratio of biosample effort (Central Coast)
yrsRatioHist <- 1994:2013

# Years to fix using historic ratio of biosample effort (Central Coast)
yrsRatioFix <- c( 2014, 2015 )

# Years where intensity is used to determine egg layers
intenseYrs <- yrRange[yrRange < 1979]

# Years where intensity needs to be re-scaled
rescaleYrs <- intenseYrs[intenseYrs < 1951]

# Transect swath (i.e., width of the transect in m; used for macrocystis spawn)
transectSwath <- 2

# Buffer distance (m; to include locations that are outside the region polygon)
maxBuff <- 5000 

# Productivity parameters
parsProd <- list(
  fecundity=200000,     # Number of eggs per kilogram of female spawners
  pFemale=0.5,          # Proportion of spawners that are female
  eggKelpProp=0.88,     # Proportion of the SOK product that is eggs, not kelp
  eggBrineProp=1/1.13,  # Proportion of SOK product that is eggs after brining
  eggWt=2.38*10^-6 )    # Weight in kg of a fertilized egg

# Start year for catch validation program (not currently used)
validCatch <- list( RoeGN=1998, RoeSN=1999 )

# Correction factors for understory spawn width (by year and region)
adjWidthFacs <- read_csv(file=
    "Year, HG, PRD, CC, SoG, WCVI, A27, A2W
  2003, 1, 1.075, 1.075, 1.075, 1.075, 1.075, 1
  2004, 1, 1.075, 1.075, 1.075, 1.075, 1.075, 1
  2005, 1, 1.075, 1.075, 1.075, 1.075, 1.075, 1
  2006, 1, 1.075, 1.075, 1.075, 1.075, 1.075, 1
  2007, 1, 1.075, 1.075, 1.075, 1.075, 1.075, 1
  2008, 1, 1.075, 1.075, 1.075, 1.075, 1.075, 1
  2009, 1.15, 1.075, 1.075, 1.075, 1.075, 1.075, 1.15
  2010, 1.15, 1.075, 1.075, 1.075, 1.075, 1.075, 1.15
  2011, 1.15, 1.075, 1.075, 1.075, 1.075, 1.075, 1.15
  2012, 1.15, 1.075, 1.075, 1.075, 1.075, 1.075, 1.15
  2013, 1.15, 1.15, 1.075, 1.075, 1.075, 1, 1.15
  2014, 1.15, 1.15, 1, 1, 1, 1, 1.15", 
  col_types=cols("i", "d", "d", "d", "d", "d", "d", "d") )

#### Sources #####

# File name for dive transect XY
diveLoc <- list(
  loc=dirDBs,
  fn="dive_transects_with_lat_long_June2_2017.xlsx" )

# Location and names of lookup tables with catch codes
codesLoc <- list(
  loc=dirDBs,
  fns=list(tDisposal="tDisposal.csv", tGear="tGear.csv", 
    tSource="tSource.csv", tPeriod="tPeriod.csv") )

# Location and name of the location database and tables
areaLoc <- list(
  loc=file.path(dirDBs, dbLoc),
  db=dbName,
  fns=list(sections="Sections", locations="Location") )

# Location(s) and names of the Sections and land shapefiles
shapesLoc <- list(
  locSec=file.path( dirShape ),
  locLand=file.path( dirShape ),
  fns=list(sections="SectionsIntegrated", land="GSHHS_h_L1_Alb") )

# Location and name of the catch database and tables
catchLoc <- list(
  loc=file.path(dirDBs, dbLoc),
  db=dbName,
  fns=list(tCatch="tCatchData", hCatch="HailCatch", sokCatch="SpawnOnKelp") )

# Location and name of the biological database and tables
bioLoc <- list(
  loc=file.path(dirDBs, dbLoc),
  db=dbName,
  fns=list(samples="sample", fish="fish", bmc="BMcCarter") )

# Location and name of the surface database and tables
surfLoc <- list(
  loc=file.path(dirDBs, dbLoc),
  db=dbName, 
  fns=list(regionStd="RegionStd", sectionStd="SectionStd", poolStd="PoolStd", 
    surface="tSSSurface", intensity="Intensity", allSpawn="tSSAllspawn") )

# Location and name of the macrocystis database and tables
macroLoc <- list(
  loc=file.path(dirDBs, dbLoc),
  db=dbName,
  fns=list(allSpawn="tSSAllspawn", plants="tSSMacPlant", 
    transects="tSSMacTrans") )

# Location and name of the macrocystis database and tables
underLoc <- list(
  loc=file.path(dirDBs, dbLoc),
  db=dbName,
  fns=list(allSpawn="tSSAllspawn", algTrans="tSSVegTrans", 
    stations="tSSStations", algae="tSSVegetation", 
    typeAlg="tSSTypeVegetation") )

##### Functions #####

# Load helper functions
source( file=file.path( "..", "HerringFunctions", "Functions.R") )
# source_url( url="https://github.com/grinnellm/HerringFunctions/blob/master/Functions.R" )

# Load spawn index functions
source( file=file.path("..", "HerringSpawnIndex", "SpawnIndex.R") )

##### Data #####

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

# Load period codes
tPeriod <- read_csv( file=file.path(codesLoc$loc, codesLoc$fns$tPeriod),
  col_types=cols("c", "c") )

# Load herring areas
areas <- LoadAreaData( where=areaLoc )

# Get BC land data etc (for plots)
shapes <- LoadShapefiles( where=shapesLoc, a=areas )

# Load raw catch data, and some light wrangling
LoadCatchData <- function( where ) {
  # This function loads the tree types of herring catch data, drops unnecessary 
  # rows and columns, and combines the data frames for the region(s) in 
  # question.
  # Progress message
  cat( "Loading catch data... " )
  # Establish connection with access
  accessDB <- odbcConnectAccess( access.file=file.path(where$loc, where$db) )
  # Access the tCatch worksheet
  tCatch <- sqlFetch( channel=accessDB, sqtable=where$fns$tCatch )
  # Error if data was not fetched
  if( class(tCatch) != "data.frame" )
    stop( "No data available in MS Access connection" )
  # Wrangle catch
  tCatch <- tCatch %>%
    mutate( Year=Season2Year(Season), Source=rep("Tab", times=n()) ) %>%
    left_join( y=areas, by="LocationCode" ) %>%
    filter( Section %in% areas$Section ) %>%
    group_by( Year, Source, Section, GearCode, DisposalCode ) %>%
    summarise( Catch=SumNA(Catch) ) %>%
    ungroup( )
  # Access the hail worksheet
  hCatch <- sqlFetch( channel=accessDB, sqtable=where$fns$hCatch ) 
  # Error if data was not fetched
  if( class(hCatch) != "data.frame" )
    stop( "No data available in MS Access connection" )
  # Wrangle catch
  hCatch <- hCatch %>%
    filter( Active == 1, Section %in% areas$Section ) %>%
    mutate( Year=Season2Year(Season), Catch=CatchTons*convFac$st2t,
      Source=rep("Hail", times=n()) ) %>%
    group_by( Year, Source, Section, GearCode, DisposalCode ) %>%
    summarise( Catch=SumNA(Catch) ) %>%
    ungroup( )
  # Access the sok worksheet
  sokCatch <- sqlFetch( channel=accessDB, sqtable=where$fns$sokCatch ) 
  # Error if data was not fetched
  if( class(sokCatch) != "data.frame" )
    stop( "No data available in MS Access connection" )
  # Wrangle sok
  sokCatch <- sokCatch %>%
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
  # Trim years outside the desired year range
  res <- res %>%
    filter( Year %in% yrRange )
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
  # Error if data was not fetched
  if( class(sampleDat) != "data.frame" )
    stop( "No data available in MS Access connection" )
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
  # Access the fish worksheet
  fish <- sqlFetch( channel=accessDB, sqtable=where$fns$fish ) 
  # Error if data was not fetched
  if( class(fish) != "data.frame" )
    stop( "No data available in MS Access connection" )
  # Wrangle biosamples
  fish <- fish %>%
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
      LocationName, Eastings, Northings, Sample, Representative, SourceCode, 
      GearCode, Fish, Length, Weight, Sex, MaturityCode, Age, DualAge, 
      GonadLength, GonadWeight ) %>%
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
  # Trim years outside the desired year range
  res <- res %>%
    filter( Year %in% yrRange )
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
  cat( "Calculating spawn index:\n" )
  # Fecundity conversion factor
  ECF <<- CalcEggConversion( fecundity=parsProd$fecundity, 
    pFemale=parsProd$pFemale )
  # Progress message
  cat( "\tsurface...\n" )
  # Access and calculate surface spawn
  surface <- CalcSurfSpawn( where=whereSurf, a=areas, f=ECF )
  # Progress message
  cat( "\tmacrocystis...\n" )
  # Access and calculate macrocystis spawn
  macrocystis <- CalcMacroSpawn( where=whereMacro, a=areas, f=ECF )
  # Progress message
  cat( "\tunderstory...\n" )
  # Access and calculate understory spawn
  understory <- CalcUnderSpawn( where=whereUnder, a=areas, f=ECF )
  # Update progress message
  cat( "\ttotal... " )
  # Load the all spawn data
  allSpawn <- GetAllSpawn( where=underLoc, a=areas )
  # Combine the spawn types (by spawn number)
  raw <- surface$biomassSpawn %>%
    full_join( y=macrocystis$biomassSpawn, by=c("Year", "Region", "StatArea", 
      "Section", "LocationCode", "SpawnNumber") ) %>%
    # TODO: Look into why this Width is different from the allSpawn$Width
    select( -Width ) %>%
    full_join( y=understory$biomassSpawn, by=c("Year", "Region", "StatArea", 
      "Section", "LocationCode", "SpawnNumber") ) %>%
    full_join( y=allSpawn, by=c("Year", "Region", "StatArea", "Section", 
      "LocationCode", "SpawnNumber") ) %>%
    select( Year, Region, StatArea, Group, Section, LocationCode, 
      LocationName, SpawnNumber, Eastings, Northings, Longitude, Latitude, 
      Start, End, Length, Width, Depth, Method, SurfLyrs, SurfSI, MacroLyrs, 
      MacroSI, UnderLyrs, UnderSI ) %>%
    mutate( Year=as.integer(Year), StartDOY=yday(Start), 
      EndDOY=yday(End), Decade=paste(Year%/%10*10, "s", sep="") ) %>%
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
  # Add a column to indicate the survey period
  res <- res %>%
    mutate( Survey=ifelse(Year < newSurvYr, "Surface", "Dive"),
      Survey=factor(Survey, levels=c("Surface", "Dive")) ) %>%
    filter( Year %in% yrRange )
  # Update the progress message
  cat( "done\n" )
  # Return the data
  return( res )
}  # End LoadSpawnData function

# Load spawn data
spawnRaw <- LoadSpawnData( whereSurf=surfLoc, whereMacro=macroLoc, 
  whereUnder=underLoc, XY=transectXY )

# # For Kristen, CC spawn
# spawnRaw %>% 
#   select( Year, Region, StatArea, Section, LocationCode, LocationName, 
#     SpawnNumber, Eastings, Northings, Longitude, Latitude, SurfSI, MacroSI,
#     UnderSI ) %>% 
#   filter( Year>=2014 ) %>% 
#   arrange(Year, Region, StatArea, Section, LocationCode ) %>% 
#   write_csv( path="SpawnCC.csv" )

# For comparing tweaks to the SpawnIndex script
spawnSummary <- spawnRaw %>%
  group_by( Year ) %>%
  summarise( Surf=SumNA(SurfSI), Macro=SumNA(MacroSI), Under=SumNA(UnderSI),
    Total=SumNA(c(Surf, Macro, Under)) ) %>%
  ungroup( ) %>%
  mutate( Surf=formatC(as.numeric(Surf), digits=3, format="f"), 
    Macro=formatC(as.numeric(Macro), digits=3, format="f"),
    Under=formatC(as.numeric(Under), digits=3, format="f"), 
    Total=formatC(as.numeric(Total), digits=3, format="f") )

# Write to disc
write_csv( x=spawnSummary, path=paste("Spawn", region, ".csv", sep="") )

##### Update #####

# Update catch data (more wrangling)
UpdateCatchData <- function( dat, a ) {
  # TODO: This is some pretty ugly wrangling, probably carried over from the 
  # way it was done in Access -- should be cleaned up
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
      tc2 <- tc2 %>% filter( DisposalCode %in% c(7, 8) )
    } else {  # End if including, otherwise
      # Exclude
      tc2 <- tc2 %>% filter( DisposalCode == 7 ) 
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
      tc3 <- tc3 %>% filter( DisposalCode %in% c(7, 8) )
    } else {  # End if include, otherwise
      # Exclude
      tc3 <- tc3 %>% filter( DisposalCode == 7 )    
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
  # Combine the data frames and add gear types
  res <- bind_rows( rList) %>%
    left_join( y=areasSm, by="Section" ) %>%
    select( Period, Year, Region, StatArea, Group, Section, Catch ) %>%
    arrange( Period, Year, Region, StatArea, Group, Section ) %>%
    left_join( y=tPeriod, by="Period" ) %>%
    mutate( Gear=factor(Gear, levels=tPeriod$Gear) )
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

##### Main ##### 

# Calculate commercial catch in current year
catchCommUseYr <- catch %>%
  complete( Year=yrRange, Gear, fill=list(Catch=0) ) %>%
  filter( Year == max(yrRange) ) %>%
  group_by( Gear ) %>%
  summarise( Catch=SumNA(Catch) ) %>%
  ungroup( )    

# Calculate commercial SoK harvest and biomass
harvestSOK <- catchRaw %>%
  filter( DisposalCode == 2, Source=="SOK" ) %>%
  group_by( Year ) %>%
  summarise( Harvest=SumNA(Catch) ) %>%
  ungroup( ) %>%
  # Covert harvest (lb) to spawning biomass (t)
  mutate( Biomass=CalcBiomassSOK(SOK=Harvest*convFac$lb2kg, 
    eggKelpProp=parsProd$eggKelpProp, 
    eggBrineProp=parsProd$eggBrineProp, 
    eggWt=parsProd$eggWt, ECF=ECF) ) %>%
  complete( Year=yrRange, fill=list(Harvest=0, Biomass=0) ) %>%
  arrange( Year )

# Table that doesn't get pruned for privacy, recent years, etc (for the res doc)
allHarvSOK <- harvestSOK %>% 
  mutate( Region=regName, Harvest=Harvest*convFac$lb2kg )

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
  # Merge the two tables and wrangle
  res <- full_join( x=numComm, y=numTest, by="Year" ) %>%
    complete( Year=firstYrTab:max(yrRange), 
      fill=list(Commercial=0, Test=0) ) %>%
    mutate( Total=as.integer(Commercial+Test) ) %>%
    arrange( Year )
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

# Get the total number of biosamples (all years)
numBiosamples <- bioRaw %>% 
  group_by( Year ) %>% 
  summarise( Total=n_distinct(Sample) ) %>% 
  ungroup( ) %>%
  mutate( Region=region )

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
    full_join( y=tibble(Year=firstYrTab:max(yrRange)), by=c("Year") ) %>%
    arrange( Year )
  # Return the output
  return( res )
}  # End FormatPropAtAge function

# Format proportion-at-age
propAgedYearTab <- FormatPropAtAge( dat=numAgedYear )

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
    gather( key=Age, value="Weight", ageRange, convert=TRUE ) %>%
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

# Calculate mean length-at-age by year
CalcLengthAtAge <- function( dat ) {
  # This function calculates mean length-at-age by year, and fills in missing
  # data (i.e., NAs) using a suitable technique. Calculate the weighted mean
  # using the 'SampWt' column to fix unrepresentative sampling if identified
  # Calculate mean length-at-age
  lenAge <- dat %>% 
    filter( GearCode == 29 ) %>%
    select( Year, Age, Length, SampWt ) %>%
    na.omit( ) %>%
    group_by( Year, Age ) %>%
    summarise( MeanLength=WtMeanNA(x=Length, w=SampWt) ) %>%
    ungroup( ) %>%
    arrange( Year, Age )
  # Reshape from long to wide and merge with complete year sequence
  lenAgeW <- lenAge %>%
    spread( key=Age, value=MeanLength ) %>%
    complete( Year=yrRange ) %>%
    arrange( Year )
  # Reshape from wide to long, and fill in NAs
  lenAgeL <- lenAgeW %>%
    gather( key=Age, value="Length", ageRange, convert=TRUE ) %>%
    group_by( Age ) %>%
    # Replace NAs: mean of (up to) previous n years 
    mutate( Length=RollMeanNA(Length, n=nRoll) ) %>%
    # Replace persistent NAs (i.e., at the beginning of the time series)
    mutate( Length=na.fill(Length, fill=c("extend", NA, NA)) ) %>%
    ungroup( ) %>%
    filter( Year %in% yrRange )
  # Return length-at-age by year
  return( lenAgeL )
}  # End CalcLengthAtAge function

# Calculate mean length-at-age by year 
lengthAge <- CalcLengthAtAge( dat=bio )

# Calculate running mean length-at-age by year
muLengthAge <- lengthAge %>%
  group_by( Age ) %>%
  mutate( muLength=rollmean(x=Length, k=nRoll, align="right", 
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
      MacroSI=SumNA(MacroSI),
      SurfSI=SumNA(SurfSI),
      UnderSI=SumNA(UnderSI),
      TotalSI=SumNA(c(MacroSI, SurfSI, UnderSI)) ) %>%
    ungroup( )
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
    arrange_( g ) %>%
    mutate( Survey=ifelse(Year < newSurvYr, "Surface", "Dive"),
      Survey=factor(Survey, levels=c("Surface", "Dive")) )
  # Return the data
  return( res )
}  # End CalcSpawnSummary function

# Calculate spawn summary by year
spawnYr <- CalcSpawnSummary( dat=spawnRaw, g=c("Year") )

# Smaller subset for figures: spawn by year
spawnYrFig <- spawnYr %>% 
  filter( Year >= firstYrFig )

# Wrangle spawn by type to long
spawnYrFigType <- spawnYrFig %>%
  select( Year, MacroSI, SurfSI, UnderSI, Survey ) %>%
  rename( Macrocystis=MacroSI, Surface=SurfSI, Understory=UnderSI ) %>%
  gather( "Macrocystis", "Surface", "Understory", key="Type", value="SI" ) %>%
  mutate( Type=factor(Type, 
    levels=c("Surface", "Macrocystis", "Understory")), 
    Survey=factor(Survey, levels=c("Surface", "Dive"))) %>%
  arrange( Year, Survey, Type )

# Smaller subset for table: spawn by year
spawnYrTab <- spawnYr %>%
  filter( Year >= firstYrTab ) %>%
  select( Year, TotalLength, MeanWidth, MeanLayers, TotalSI )

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

# Calculate the proportion of spawn by group or statistical area
CalcPropSpawn <- function( dat, g, yrs1=max(yrRange):(max(yrRange)-4),
  yrs2=(max(yrRange)-5):(max(yrRange)-9) ) {
  # Total years
  yrs <- c( yrs1, yrs2 )
  # Error if grouping variable not specified
  if( !g %in%c("StatArea", "Group") )  stop( "Grouping variable not specified" )
  # Get the full year range and stat areas
  if( g=="StatArea" )
    yrsFull <- expand.grid( Year=yrs, StatArea=unique(dat$StatArea) )
  # Get the full year range and groups
  if( g=="Group" )
    yrsFull <- expand.grid( Year=yrs, Group=unique(dat$Group) ) %>%
      mutate( Group=as.character(Group) )
  # Determin spawn proportions
  pSpawn <- dat %>%
    replace_na( replace=list( SurfSI=0, MacroSI=0, UnderSI=0) ) %>%
    mutate( TotalSI=SurfSI + MacroSI + UnderSI ) %>%
    group_by_( .dots=c("Year", g) ) %>%
    summarise( TotalSI=SumNA(TotalSI) ) %>%
    group_by( Year ) %>%
    mutate( Proportion=TotalSI/SumNA(TotalSI) ) %>%
    ungroup( ) %>%
    full_join( y=yrsFull, by=c("Year", g) ) %>%
    replace_na( replace=list(Proportion=0) )
  # Extract total spawn by year
  tSpawn <- pSpawn %>%
    group_by( Year ) %>%
    summarise( TotalSI=SumNA(TotalSI) ) %>%
    ungroup( ) %>%
    filter( Year%in%yrs )
  # Loop over years (1)
  for( i in 1:length(yrs1) ) {
    # Get the ith year
    iYr <- yrs1[i]
    # Calculate the proportion by year
    temp1 <- pSpawn %>%
      filter( Year == iYr) %>%
      group_by_( .dots=g ) %>%
      summarise( Mean=MeanNA(Proportion) ) %>%
      ungroup( ) %>%
      mutate( Year=iYr, Name="SI" )
    # Compile results (1)
    ifelse( i==1, out1 <- temp1, out1 <- bind_rows(temp1, out1) )
  }  # End i loop over years 1
  # Fill missing data
  out1 <- out1 %>%
    complete( Year=yrs1, fill=list(Mean=0, Name="SI") )
  # Loop over years (2)
  for( j in 1:length(yrs2) ) {
    # Get the jth year
    jYr <- yrs2[j]
    # Calculate means of last n years
    temp2 <- pSpawn %>%
      filter( Year>=jYr ) %>%
      group_by_( .dots=g ) %>%
      summarise( Mean=MeanNA(Proportion) ) %>%
      ungroup( ) %>%
      mutate( Year=jYr, Name="SI_bar" )
    # Compile results
    ifelse( j==1, out2 <- temp2, out2 <- bind_rows(temp2, out2) )
  }  # End j loop over years 2
  # If stat areas
  if( g=="StatArea" ) {
    # Update stat area names (1)
    out1 <- out1 %>%
      mutate( StatArea=formatC(StatArea, width=2, format="d", flag="0") )
    # Update stat area names (2)
    out2 <- out2 %>%
      mutate( StatArea=formatC(StatArea, width=2, format="d", flag="0") )
  }  # End if stat areas
  # Full join and wrangle
  res <- bind_rows( out1, out2 ) %>%
    mutate( Mean=formatC(Mean, digits=3, format="f") ) %>%
    select( -Name ) %>%
    spread_( key=g, value="Mean" ) %>%
    full_join( y=tSpawn, by="Year" ) %>%
    arrange( desc(Year) ) %>%
    mutate( Year=ifelse(Year%in%out2$Year, 
      paste(Year, max(yrs), sep=" to "), Year) ) %>%
    mutate( TotalSI=formatC(TotalSI, digits=0, format="f", big.mark=",") ) %>%
    select( Year, TotalSI, everything() ) %>%
    rename( `Year(s)`=Year, `Spawn index (t)`=TotalSI )
  # Replace NAs
  res[is.na(res)] <- "0.000"
  # Return the results
  return( res )
}  # End CalcPropSpawn function

# Calculate spawn summary in current year by location code
spawnByLocXY <- spawnRaw %>%
  filter( Year == max(yrRange) ) %>%
  group_by( StatArea, Section, LocationCode, LocationName ) %>%
  summarise( Start=min(Start), TotalSI=SumNA(c(MacroSI, SurfSI, UnderSI)),
    Eastings=unique(Eastings), Northings=unique(Northings) ) %>%
  ungroup( ) %>%
  arrange( TotalSI )

# Calculate spawn summary in current year by location code
spawnByLoc <- spawnByLocXY %>%
  select( -Eastings, -Northings, -LocationCode )

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
  #    filter( Frequency >= 2, MeanSI >=quantile(MeanSI, probs=0.1, 
  #            na.rm=TRUE) ) %>%
  arrange( desc(Frequency), MeanSI )

# Get table of stat area, section, and group
spatialGroup <- areas %>% 
  select( RegionName, StatArea, Section, Group ) %>% 
  distinct( ) %>%
  mutate( StatArea=formatC(StatArea, width=2, format="d", flag="0"),
    Section=formatC(Section, width=3, format="d", flag="0") ) %>%
  arrange( RegionName, StatArea, Section, Group )

# Calculate spawn index by location and year
siYearLoc <- spawnRaw %>%
  group_by( Year, LocationCode ) %>%
  summarise( Eastings=unique(Eastings), Northings=unique(Northings),
    SITotal=SumNA(c(SurfSI, MacroSI, UnderSI)) ) %>%
  ungroup( ) %>%
  complete( Year=yrRange )

##### Region #####

# If region is Haida Gwaii
if( region == "HG" ) {
  # Remove Group info
  spatialGroup <- spatialGroup %>%
    select( -Group )
  # Plot spawn timing by Stat Area
  spawnTimingGroup <- FALSE
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn( dat=spawnRaw, g="Group" )
}  # End if region is Haida Gwaii

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
  # Use with catch data that are not releasable ("None" if all OK)
  privGearCatch <- "None"
  # Remove catch data for certain uses due to privacy concerns
  catchCommUseYr <- catchCommUseYr %>%
    mutate( Catch=format(Catch, big.mark=",", digits=0, scientific=FALSE),
      Catch=ifelse(Gear == privGearCatch, "WP", Catch) )
  # Years with SOK data that are not releasable
  privYrsSOK <- 2016
  # Remove SoK data for certain years due to privacy concerns
  harvestSOK <- harvestSOK %>%
    mutate( 
      Harvest=format(Harvest, big.mark=",", digits=0, scientific=FALSE),
      Biomass=format(Biomass, big.mark=",", digits=0, scientific=FALSE),
      Harvest=ifelse(Year %in% privYrsSOK, "WP", Harvest), 
      Biomass=ifelse(Year %in% privYrsSOK, "WP", Biomass) )
  # Remove Group info
  spatialGroup <- spatialGroup %>%
    select( -Group )
  # Plot spawn timing by Stat Area
  spawnTimingGroup <- FALSE
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn( dat=spawnRaw, g="StatArea" )
}  # End if region is Prince Rupert District

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
    ungroup( )
  #  # Average length-at-age by year and statistical area
  #  lengthAgeSA <- bio %>%
  #      filter( GearCode == 29, Year >= 1988 ) %>%
  #      select( Year, StatArea, Age, Length ) %>%
  #      na.omit( ) %>%
  #      group_by( Year, StatArea, Age ) %>%
  #      summarise( MeanLength=mean(Length) ) %>%
  #      ungroup( ) %>%
  #      spread( key=Age, value=MeanLength )
  # Plot spawn timing by Stat Area
  spawnTimingGroup <- FALSE
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn( dat=spawnRaw, g="StatArea" )
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
  # Plot spawn timing by Group
  spawnTimingGroup <- TRUE
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn( dat=spawnRaw, g="Group" )
} # End if region is Strait of Georgia

# If region is West Coast of Vancouver Island
if( region == "WCVI" ) {
  # Compare differences by group: number, proportion, weight, and length-at-age
  npwAgeGrp <- bioRaw %>%
    filter( Year == max(yrRange), SourceCode %in% c(2, 5), 
      Representative == 1 ) %>%
    left_join( y=tSource, by="SourceCode" ) %>%
    group_by( Age, SampleSource2 ) %>%
    summarise( Number=n(), Weight=MeanNA(Weight), Length=MeanNA(Length) ) %>%
    group_by( SampleSource2 ) %>%
    mutate( Proportion=Number/SumNA(Number) ) %>%
    select( SampleSource2, Age, Number, Proportion, Weight, Length )
  # Calculate total: number, proportion, and weight-at-age
  npwAgeTot <- bioRaw %>%
    filter( Year == max(yrRange), SourceCode %in% c(2, 5), 
      Representative == 1 ) %>%
    mutate( SampleSource2="Total" ) %>%
    group_by( Age, SampleSource2 ) %>%
    summarise( Number=n(), Weight=MeanNA(Weight), Length=MeanNA(Length) ) %>%
    ungroup( ) %>%
    mutate( Proportion=Number/SumNA(Number) ) %>%
    select( SampleSource2, Age, Number, Proportion, Weight, Length )
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
  # Get differences in length-at-age
  deltaLenAgeYr <- npwAge %>%
    select( Age, SampleSource2, Length ) %>%
    spread( key=Age, value=Length ) %>%
    rename( 'Sample type'=SampleSource2 )
  # Remove Group info
  spatialGroup <- spatialGroup %>%
    select( -Group )
  # Plot spawn timing by Stat Area
  spawnTimingGroup <- FALSE
  # Determine years for the FN nearshore pilot study
  yrsNearshore <- bioRaw %>% 
    filter( SourceCode==2, GearCode==1 ) %>% 
    select( Year ) %>% 
    distinct( ) 
  # Get length at age for the two sampling protocols
  lenAgeSample <- bioRaw %>%
    filter( Year %in% yrsNearshore$Year, SourceCode %in% c(2, 5), 
      Representative == 1 ) %>%
    left_join( y=tSource, by="SourceCode" ) %>%
    select( SampleSource2, Age, Length )
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn( dat=spawnRaw, g="StatArea" )
}  # End if region is West Coast of Vancouver Island

# If region is Area 27
if( region == "A27" ) {
  # Years with SOK data that are not releasable
  privYrsSOK <- 2010:2014
  # Remove SoK data for certain years due to privacy concerns
  harvestSOK <- harvestSOK %>%
    mutate( 
      Harvest=format(Harvest, big.mark=",", digits=0, scientific=FALSE),
      Biomass=format(Biomass, big.mark=",", digits=0, scientific=FALSE),
      Harvest=ifelse(Year %in% privYrsSOK, "WP", Harvest), 
      Biomass=ifelse(Year %in% privYrsSOK, "WP", Biomass) )
  # Remove Group info
  spatialGroup <- spatialGroup %>%
    select( -Group )
  # Plot spawn timing by Stat Area
  spawnTimingGroup <- FALSE
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn( dat=spawnRaw, g="StatArea" )
}  # End if region is Area 27

# If region is Area 2 West
if( region == "A2W" ) {
  # Years with SOK data that are not releasable
  privYrsSOK <- c( 2009, 2012:2014 )
  # Remove SoK data for certain years due to privacy concerns
  harvestSOK <- harvestSOK %>%
    mutate( 
      Harvest=format(Harvest, big.mark=",", digits=0, scientific=FALSE),
      Biomass=format(Biomass, big.mark=",", digits=0, scientific=FALSE),
      Harvest=ifelse(Year %in% privYrsSOK, "WP", Harvest), 
      Biomass=ifelse(Year %in% privYrsSOK, "WP", Biomass) )
  # Remove Group info
  spatialGroup <- spatialGroup %>%
    select( -Group )
  # Plot spawn timing by Stat Area
  spawnTimingGroup <- FALSE
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn( dat=spawnRaw, g="StatArea" )
}  # End if region is Area 2 West

##### ADMB #####

# Make ADMB input data: catch (t*10^3)
catchADMB <- catch %>%
  group_by( Period, Year ) %>%
  summarise( Catch=SumNA(Catch) ) %>%
  ungroup( ) %>%
  rename( Gear=Period ) %>%
  mutate( Gear=as.integer(parse_number(Gear)),
    Value=round(Catch/1000, digits=3),
    Area=as.integer(1), Group=as.integer(1), Sex=as.integer(0), 
    Type=as.integer(1) ) %>%
  select( Year, Gear, Area, Group, Sex, Type, Value ) %>%
  arrange( Gear, Year )

# Make ADMB input data: spawn (t*10^3)
spawnADMB <- spawnYr %>%
  select( Year, TotalSI ) %>%
  rename( Spawn=TotalSI ) %>%
  mutate( Spawn=round(Spawn/1000, digits=3), 
    Gear=ifelse(Year<newSurvYr, as.integer(4), as.integer(5)), 
    Area=as.integer(1), Group=as.integer(1), Sex=as.integer(0),
    Weight=ifelse(Year<newSurvYr, 1, 1.1666),
    Timing=as.integer(1) ) %>%
  arrange( Gear, Year )

# Make ADMB input data: number aged
numAgedADMB <- numAgedYearGear %>%
  select( Year, Period, Age, Number ) %>%
  mutate( Period=as.integer(Period), Area=as.integer(1), Group=as.integer(1), 
    Sex=as.integer(0), Number=round(Number) ) %>%
  spread( key=Age, value=Number, fill=0 ) %>%
  rename( Gear=Period ) %>%
  mutate_all( as.integer ) %>%
  arrange( Gear, Year )

# Make ADMB input data: weight-at-age (kg)
weightAgeADMB <- weightAge %>%
  mutate( Weight=round(Weight/1000, digits=4),
    Gear=as.integer(1), Area=as.integer(1), Group=as.integer(1), 
    Sex=as.integer(0) ) %>%
  spread( key=Age, value=Weight ) %>%
  arrange( Gear, Year )

# Write ADMB input file
WriteInputFile <- function( pADMB, cADMB, sADMB, nADMB, wADMB ) {
  # Note that some values are printed with extra decimals (i.e., CC value 0.1078
  # becomes 0.10780000000000001) because of issues representing floating point 
  # numbers, but the values are read back correctly in R (see emails)
  # Create the file name
  fName <- file.path( regName, 
    paste("Herring", regName, max(yrRange), ".dat", sep="") )
  # Start a connection (binary)
  out <- file( description=fName, open="wb" )
  
  # Initialize the file and write the start of the main header
  write( x=paste(rep("#", times=80), collapse=""), file=out, append=FALSE )
  # Write the main title
  write( x="# Data file for Pacific Herring stock assessment using iSCAM", 
    file=out, append=TRUE )
  # Write the region(s)
  write( x=paste("# Region(s):\t", PasteNicely(unique(areas$RegionName)), 
    sep=""), file=out, append=TRUE )
  # Write the subset of sections, if it applies
  if( !all(is.na(sectionSub)) )
    write( x=paste("# Section(s):\t", PasteNicely(sectionSub), sep=""), 
      file=out, append=TRUE )
  # Write the date
  write( x=paste("# Created:\t", Sys.Date( ), sep=""), file=out, append=TRUE )
  # Write space for editing
  write( x="# Edited:\t", file=out, append=TRUE )
  # Write the end of the main header
  write( x=paste(rep("#", times=80), collapse=""), file=out, append=TRUE )
  
  # Write header for model dimensions
  write( x="#\n##### Model dimensions #####", file=out, append=TRUE )
  # Get number of distinct gears
  ngear <- n_distinct( c(cADMB$Gear, sADMB$Gear, nADMB$Gear, wADMB$Gear) )
  # Get number of distinct areas
  narea <- n_distinct( c(cADMB$Area, sADMB$Area, nADMB$Area, wADMB$Area) )
  # Get number of distinct groups
  ngroup <- n_distinct( c(cADMB$Group, sADMB$Group, nADMB$Group, wADMB$Group) )
  # Get number of distinct sexes
  nsex <- n_distinct( c(cADMB$Sex, sADMB$Sex, nADMB$Sex, wADMB$Sex) )
  # Write model dimensions
  write( x=paste(narea, "\t# Number of areas (narea)", sep=""), file=out, 
    append=TRUE )
  write( x=paste(ngroup, "\t# Number of groups (ngroup)", sep=""), file=out, 
    append=TRUE )
  write( x=paste(nsex, "\t# Number of sexes (nsex)", sep=""), file=out, 
    append=TRUE )
  write( x=paste(min(yrRange), "\t# First year (syr)", sep=""), file=out, 
    append=TRUE )
  write( x=paste(max(yrRange), "\t# Last year (nyr)", sep=""), file=out, 
    append=TRUE )
  write( x=paste(min(ageRange), "\t# Youngest age (sage)", sep=""), file=out, 
    append=TRUE )
  write( x=paste(max(ageRange), "\t# Plus group (nage)", sep=""), file=out, 
    append=TRUE )
  write( x=paste(ngear, "\t# Number of gears (ngear)", sep=""), file=out, 
    append=TRUE)
  
  # Write header for fishery flags
  write( x="#\n##### Fishery flags #####", file=out, append=TRUE )
  # Determine ratio of last x years of catch
  cHist <- cADMB %>%
    filter( Year >= max(yrRange)-(pADMB$General$HistoricCatch-1) ) %>%
    group_by( Gear ) %>%
    summarise( Total=SumNA(Value) ) %>%
    ungroup( ) %>%
    mutate( Proportion=round(Total/SumNA(Total), digits=4) ) %>%
    complete( Gear=1:ngear, fill=list(Proportion=0) ) %>%
    arrange( Gear )
  # Write fishery flags
  write( x=paste(paste(cHist$Proportion, collapse="\t"),
    "\t# TAC allocations (mean of last ", pADMB$General$HistoricCatch,
    " years)", sep=""), file=out, append=TRUE )
  # These are for testing only:
  # write( x=paste(paste(c(0.5, 0.3, 0.2, 0, 0), collapse="\t"), 
  #         "\t# TAC allocations", sep=""), file=out, append=TRUE )
  
  # Write header for age and population parameters
  write( x="#\n##### Age and population parameters #####", file=out, 
    append=TRUE )
  # Loop over age and population parameters
  for( p in 1:length(pADMB$AgePopulation) ) 
    # Write age and population parameters, and names
    write( x=paste(paste(pADMB$AgePopulation[[p]], collapse=", "), "\t# ",  
      names(pADMB$AgePopulation)[p], sep=""), file=out, append=TRUE )
  
  # Write header for delay-difference data
  write( x="#\n##### Delay-difference parameters (not used) #####", file=out, 
    append=TRUE )
  # Loop over delay-difference parameters
  for( d in 1:length(pADMB$DelayDifference) ) 
    # Write delay-difference parameters, and names
    write( x=paste(paste(pADMB$DelayDifference[[d]], collapse=", "), "\t# ",  
      names(pADMB$DelayDifference)[d], sep=""), file=out, append=TRUE )
  
  # Write header for catch data
  write( x="#\n##### Catch (t*10^3) #####", file=out, append=TRUE )
  # Write catch information
  write( x=paste(nrow(cADMB), "\t# Number of observations", sep=""), file=out,
    append=TRUE )
  # Write catch column names and data
  write( x=paste("#", paste(colnames(cADMB), collapse="\t")), file=out, 
    append=TRUE )
  write_delim( x=cADMB, path=out, delim="\t", append=TRUE )
  
  # Write header for spawn data
  write( x="#\n##### Spawn (t*10^3) #####", file=out, append=TRUE )
  # Determine spawn dimensions
  dimSpawn <- sADMB %>% 
    group_by( Gear ) %>% 
    summarise( Number=n() ) %>%
    ungroup( )
  # Write spawn information
  write( x=paste(nrow(dimSpawn), "\t\t# Number of survey types", sep=""), 
    file=out, append=TRUE )
  write( x=paste(paste(dimSpawn$Number, collapse="\t"), 
    "\t# Number of years per survey", sep=""), file=out, append=TRUE )
  write( x=paste(paste(rep(pADMB$General$SurveyType, times=nrow(dimSpawn)), 
    collapse="\t"), "\t# Survey type (1=vuln. number, ", 
    "2=vuln. biomass, 3=spawn biomass)", sep=""), file=out, 
    append=TRUE )
  # Write spawn column names and data
  write( x=paste("#", paste(colnames(sADMB), collapse="\t")), file=out, 
    append=TRUE )
  write_delim( x=sADMB, path=out, delim="\t", append=TRUE )
  
  # Write header for number-at-age data
  write( x="#\n##### Number-at-age #####", file=out, append=TRUE )
  # Determine model aged dimensions
  dimNumAged <- nADMB %>% 
    group_by( Gear ) %>% 
    summarise( Number=n() ) %>%
    ungroup( )
  # Write number aged dimensions
  write( x=paste(nrow(dimNumAged), "\t\t\t# Number of gears", sep=""), 
    file=out, append=TRUE )
  write( x=paste(paste(dimNumAged$Number, collapse="\t"), 
    "\t# Number of years per gear", sep=""), file=out, append=TRUE )
  write( x=paste(paste(rep(min(ageRange), times=nrow(dimNumAged)), 
    collapse="\t"), "\t# Youngest age", sep=""), file=out, 
    append=TRUE )
  write( x=paste(paste(rep(max(ageRange), times=nrow(dimNumAged)), 
    collapse="\t"), "\t# Plus group", sep=""), file=out, 
    append=TRUE )
  write( x=paste(paste(rep(pADMB$General$SampleSize, times=nrow(dimNumAged)), 
    collapse="\t"), "\t# Effective sample size", sep=""), file=out, 
    append=TRUE )
  write( x=paste(paste(rep(pADMB$General$Composition, times=nrow(dimNumAged)), 
    collapse="\t"), "\t# Composition (1=age, 2=length)", sep=""), 
    file=out, append=TRUE )
  # Write number aged column names and data
  write( x=paste("#", paste(colnames(nADMB), collapse="\t")), file=out, 
    append=TRUE )
  write_delim( x=nADMB, path=out, delim="\t", append=TRUE )
  
  # Write header for weight-at-age data
  write( x="#\n##### Weight-at-age (kg) #####", file=out, append=TRUE )
  # Write weight-at-age dimensions
  # TODO: This should come from the data, or an input parameter 
  write( x=paste(1, "\t# Number of weight-at-age tables", sep=""), file=out, 
    append=TRUE)
  write( x=paste(nrow(wADMB), "\t# Number of years", sep=""), file=out, 
    append=TRUE )
  # Write weight-at-age column names and data
  write( x=paste("#", paste(colnames(wADMB), collapse="\t")), file=out, 
    append=TRUE )
  write_delim( x=wADMB, path=out, delim="\t", append=TRUE )
  
  # Write header for annual mean weight data
  write( x="#\n##### Annual mean weight (not used) #####", file=out, 
    append=TRUE )
  # Write annual mean weight dimensions
  # TODO: This should come from the data, or an input parameter  
  write( x=paste(1, "\t# Number of annual mean weight tables"), file=out, 
    append=TRUE )
  # TODO: This should come from the data, or an input parameter
  write( x=paste(0, "\t# Number of observations per table"), file=out, 
    append=TRUE )
  
  # Write header for end of data file
  write( x="#\n##### Marker for end of data file #####", file=out, 
    append=TRUE )
  # End of file message
  write( x=paste(999, "\t# End of file"), file=out, append=TRUE )
  # Close the connection
  close( con=out )
}  # End WriteInputFile function

# Write ADMB input file
WriteInputFile( pADMB=parsADMB, cADMB=catchADMB, sADMB=spawnADMB,
  nADMB=numAgedADMB, wADMB=weightAgeADMB )

##### Figures #####

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
    height=min(7, 5.75/shapes$xyAllRatio) ) 

# Make a french version if requested
if( makeFrench ) {
  # Make a png map in the main folder (english)
  BCMap +  ggsave( filename=file.path("BC.png"), width=figWidth, 
    height=min(7, 5.75/shapes$xyAllRatio) )
  # French SAR names (short)
  frenchSARs <- data.frame( 
    Region=c("HG", "PRD", "CC", "SoG", "WCVI", "A27", "A2W"),
    RegionFR=c("HG", "DPR", "CC", "DG", "COIV", "Z27", "Z2O") )
  # Attach french names
  shapes$regCentDF <- shapes$regCentDF %>%
    left_join( y=frenchSARs, by="Region" )
  # Plot the BC coast and regions (french)
  BCMapFR <- ggplot( data=shapes$landAllCropDF, aes(x=Eastings, y=Northings) ) +
    geom_polygon( data=shapes$landAllCropDF, aes(group=group), 
      fill="lightgrey" ) +
    geom_point( data=shapes$extAllDF, colour="transparent" ) +
    geom_path( data=shapes$regAllDF, aes(group=Region), size=0.75, 
      colour="black" ) + 
    geom_label( data=shapes$regCentDF, alpha=0.5, aes(label=RegionFR) ) +
    annotate( geom="text", x=1100000, y=800000, label="Colombie-\nBritannique",
      size=5 ) +
    annotate( geom="text", x=650000, y=550000, label="Océan\nPacifique", 
      size=5 ) +
    coord_equal( ) +
    labs( x="Abcsisses (km)", y="Ordonnées (km)", caption=geoProj ) +
    scale_x_continuous( labels=function(x) comma(x/1000), expand=c(0, 0) ) + 
    scale_y_continuous( labels=function(x) comma(x/1000), expand=c(0, 0) ) +
    myTheme +
    ggsave( filename=file.path("BC-FR.png"), width=figWidth, 
      height=min(7, 5.75/shapes$xyAllRatio) )
}  # End if making french

# Create a base map for the region
BaseMap <- ggplot( data=shapes$landCropDF, aes(x=Eastings, y=Northings) ) +
  geom_polygon( data=shapes$landCropDF, aes(group=group), fill="lightgrey" ) +
  geom_point( data=shapes$extDF, colour="transparent" ) +
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
  geom_path( data=shapes$secDF, aes(group=Section), size=0.25, 
    colour="black", linetype="dotted" ) +
    {if( !is.null(shapes$grpDF) & region %in% c("CC", "SoG") )
      geom_polygon( data=shapes$grpDF, aes(group=id, fill=id), alpha=0.25 )} +
      {if( nrow(shapes$saCentDF) >= 1 )  
        geom_label( data=shapes$saCentDF, alpha=0.25,
          aes(label=paste("SA", StatArea, sep=" ")) )} +
  scale_fill_viridis( discrete=TRUE ) +
  labs( fill="Group" ) +
  theme( legend.position=c(0.01, 0.01), legend.justification=c(0, 0) ) +
  ggsave( filename=file.path(regName, "Region.pdf"), width=figWidth, 
    height=min(7.5, 6.5/shapes$xyRatio) )

# Plot catch by year and gear type (i.e., period)
catchGearPlot <- ggplot( data=catch, aes(x=Year, y=Catch) ) + 
  geom_bar( stat="identity", position="stack", aes(fill=Gear) ) +
  labs( y=expression(paste("Catch (t"%*%10^3, ")", sep="")) )  +
  scale_x_continuous( breaks=yrBreaks ) +
  scale_y_continuous( labels=function(x) comma(x/1000) ) +
  scale_fill_viridis( discrete=TRUE ) +
  expand_limits( x=yrRange, y=0 ) +
  facet_zoom( xy=Year>=firstYrFig, zoom.size=1, horizontal=FALSE,
    show.area=FALSE ) +
  myTheme +
  theme( legend.position="top" ) +
  ggsave( filename=file.path(regName, "CatchGear.pdf"), width=figWidth, 
    height=figWidth )

# Plot it again, in French
# Instead of a different name, maybe put the plot in a /French subfolder; this
# might be easier to bring into the PDF..
#catchGearPlotFr <- catchGearPlot + 
#    labs( x="Ann\\`{e}e", y=expression(paste("Capture (t"%*%10^3, ")", sep="")),
#        fill="Equipement" )  +  # ? ?
#    ggsave( filename=file.path(regName, "CatchGearFr.pdf"), width=figWidth, 
#        height=figWidth*0.75 )

# If weight by catch type
if( exists("weightCatchFig") ) {
  # Plot weight by year and catch type
  weightCatchPlot <- ggplot( data=weightCatchFig, 
    aes(x=Year, y=Weight, fill=SampleSource2, 
      group=interaction(Year, SampleSource2)) ) + 
    geom_boxplot( outlier.colour="black", size=0.25 ) + 
    labs( y="Weight (g)", fill="Sample type" )  +
    scale_x_continuous( breaks=pretty_breaks() ) +
    scale_fill_viridis( discrete=TRUE ) + 
    geom_hline( data=weightCatchFigMu, aes(yintercept=MuWeight), size=0.25,
      linetype="dashed" ) + 
    expand_limits( x=c(firstYrTab, max(yrRange)), y=0 ) +
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
    expand_limits( x=c(firstYrFig, max(yrRange)), y=0 ) +
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
  scale_size( guide=guide_legend(order=2), range=c(3, 6) ) +
  labs( shape="Sample type" ) +
  guides( size=guide_legend(order=1),
    shape=guide_legend(override.aes=list(size=3)) ) +
  theme( legend.justification=c(0, 0), 
    legend.box.just=if(region %in% c("WCVI", "JS")) "top" else "left", 
    legend.position=if(region == "PRD") "right" else c(0.01, 0.01),
    legend.box=if(region %in% c("WCVI", "JS")) "horizontal" else 
      "vertical" ) +
  ggsave( filename=file.path(regName, "BioLocations.pdf"), width=figWidth, 
    height=min(7.5, 6.5/shapes$xyRatio) )

# Plot proportion-at-age by year
propAgedPlot <- ggplot( data=numAgedYear, aes(x=Year)  ) +
  geom_point( aes(y=Age, size=Proportion) ) +
  geom_path( data=qAgedYear, aes(y=MeanAge, group=GroupID) ) +
  geom_ribbon( data=qAgedYear, aes(ymin=Lower, ymax=Upper, group=GroupID), 
    alpha=0.25 ) +
  scale_size( range=c(0, 3) ) +
  labs( x=NULL, y="Age" ) +
  scale_x_continuous( breaks=yrBreaks ) +
  scale_y_continuous( breaks=pretty_breaks() ) +
  expand_limits( x=c(min(yrRange)-0.5, max(yrRange)+0.5) ) +
  annotate( geom="text", x=-Inf, y=Inf, label="(a)", vjust=1.3, hjust=-0.1 ) +
  myTheme +
  theme( legend.position="top", axis.text.x=element_blank() )

# Plot number aged by year
numAgedPlot <- ggplot( data=numAgedYear, aes(x=Year, y=Number) ) +
  geom_bar( stat="identity", width=0.9 ) +
  labs( y="Number aged (thousands)" )  +
  scale_x_continuous( breaks=yrBreaks ) +
  scale_y_continuous( labels=function(x) comma(x/1000) ) +
  expand_limits( x=yrRange, y=0 ) +
  annotate( geom="text", x=-Inf, y=Inf, label="(b)", vjust=1.3, hjust=-0.1 ) +
  myTheme

# Arrange and save the proportion-at-age and number aged plots
pnPlots <- plot_grid( propAgedPlot, numAgedPlot, align="v", 
  ncol=1, rel_heights=c(1, 0.7) ) +
  ggsave( filename=file.path(regName, "ProportionAged.pdf"), width=figWidth, 
    height=figWidth )

# Plot weight-at-age by year
weightAgePlot <- ggplot( data=muWeightAge ) + 
  geom_point( data=filter(.data=muWeightAge, Age == ageShow), 
    aes(x=Year, y=Weight), shape=1, size=1 ) +
  geom_line( aes(x=Year, y=muWeight, group=Age, colour=Age), size=1 ) +
  #    geom_line( data=filter(.data=muWeightAge, Age == ageShow), 
  #        aes(x=Year, y=muWeight), size=1 ) +
  scale_colour_viridis( guide=guide_legend(nrow=1), discrete=TRUE ) +
  labs( x=NULL, y="Weight-at-age (g)" ) +
  scale_x_continuous( breaks=yrBreaks ) +
  #    coord_cartesian( ylim=wtRange ) +
  expand_limits( x=yrRange ) +
  annotate( geom="text", x=-Inf, y=Inf, label="(a)", vjust=1.3, hjust=-0.1 ) +
  myTheme +
  theme( axis.text.x=element_blank(), legend.position="top" )

# Plot length-at-age by year
lengthAgePlot <- ggplot( data=muLengthAge ) + 
  geom_point( data=filter(.data=muLengthAge, Age == ageShow), 
    aes(x=Year, y=Length), shape=1, size=1 ) +
  geom_line( aes(x=Year, y=muLength, group=Age, colour=Age), size=1 ) +
  #    geom_line( data=filter(.data=muLengthAge, Age == ageShow), 
  #        aes(x=Year, y=muLength), size=1 ) +
  scale_colour_viridis( guide=guide_legend(nrow=1), discrete=TRUE ) +
  labs( y="Length-at-age (mm)" ) +
  guides( colour=FALSE ) +
  scale_x_continuous( breaks=yrBreaks ) +
  #    coord_cartesian( ylim=lenRange ) +
  expand_limits( x=yrRange ) +
  annotate( geom="text", x=-Inf, y=Inf, label="(b)", vjust=1.3, hjust=-0.1 ) +
  myTheme
#    facet_zoom( y=Age==ageShow, zoom.size=1 )

# Arrange and save the weight- and length-at-age plots
lwPlots <- plot_grid( weightAgePlot, lengthAgePlot, align="v", ncol=1, 
  rel_heights=c(1.2, 1.1) ) +
  ggsave( filename=file.path(regName, "WtLenAge.pdf"), width=figWidth, 
    height=figWidth )

# If weight by age and group
if( exists("weightAgeGroup") ) {
  # Plot weight by age and group 
  weightAgeGroupPlot <- ggplot( data=weightAgeGroup, 
    aes(x=Age, y=Weight, fill=Group, group=interaction(Age, Group)) ) + 
    geom_boxplot( outlier.colour="black", size=0.25 ) + 
    labs( y="Weight (g)" )  +
    scale_x_continuous( breaks=pretty_breaks() ) +
    scale_fill_viridis( discrete=TRUE, alpha=0.5 ) +
    expand_limits( y=0 ) +
    facet_grid( . ~ Decade, labeller=label_both ) +
    myTheme +
    theme( legend.position="top" ) +
    ggsave( filename=file.path(regName, "WeightAgeGroup.pdf"), width=figWidth, 
      height=figWidth*0.67 )
}  # End if weight by age and group

# Plot the spawn index locations
spawnByLocPlot <- BaseMap + 
  geom_path( data=shapes$secDF, aes(group=Section), size=0.25, 
    colour="black" ) +
  geom_text( data=shapes$secCentDF, alpha=0.6, size=2,
    aes(label=paste("Sec", Section, sep=" ")) ) +
  geom_point( data=spawnByLocXY, aes(colour=TotalSI), alpha=0.75, size=4 ) +
  scale_colour_viridis( labels=comma ) +
  labs( colour="Spawn/nindex (t)" ) +
  theme( legend.justification=c(0, 0), 
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
  scale_colour_viridis( labels=comma ) +
  scale_size( breaks=pretty_breaks(), guide=guide_legend(order=2) ) +
  labs( colour="Mean spawn\nindex (t)" ) +
  theme( legend.justification=c(0, 0), 
    legend.box.just=if(region %in% c("WCVI", "JS")) "top" else "left", 
    legend.position=if(region == "PRD") "right" else c(0.01, 0.01),
    legend.box=if(region %in% c("WCVI", "JS")) "horizontal" else 
      "vertical" ) +
  ggsave( filename=file.path(regName, "SpawnDecade.pdf"), width=figWidth, 
    height=min(7.5, 6.5/shapes$xyRatio) )

# Basic spawn timing plot
spawnTimingPlot <- ggplot( data=spawnRaw, aes(x=StartDOY) ) +
  annotate( geom="rect", xmin=60, xmax=90, ymin=0, ymax=Inf, fill="grey", 
    alpha=0.5 ) +
  geom_freqpoly( bins=30, size=0.5 ) +
  scale_x_continuous( breaks=c(1, 32, 60, 91, 121, 152, 182), 
    labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul") ) +
  labs( x="Start of spawn", y="Number of spawns" ) +
  { if( spawnTimingGroup ) facet_grid( Decade ~ Group, labeller=label_both )
    else facet_grid( Decade ~ StatArea, labeller=label_both ) }  +
  myTheme +
  theme( legend.position="top", axis.text.x=element_text(angle=45, hjust=1) ) +
  ggsave( filename=file.path(regName, "SpawnTiming.pdf"), width=figWidth,
    height=figWidth*1.25 )

# Plot total spawn length by year
spawnLengthPlot <- ggplot( data=spawnYrFig, aes(x=Year, y=TotalLength) ) +
  geom_point( aes(shape=Survey) ) + 
  geom_line( aes(group=Survey) ) +
  #    geom_smooth( method=smLine, colour="black", level=ciLevel ) +
  labs( x=NULL, 
    y=expression(paste("Total spawn length (m"%*%10^3, ")", sep="")) )  +
  scale_x_continuous( breaks=yrBreaks ) +
  scale_y_continuous( labels=function(x) comma(x/1000) ) +
  annotate( geom="text", x=-Inf, y=Inf, label="(a)", vjust=1.3, hjust=-0.1 ) +
  #    guides( shape=FALSE ) +
  expand_limits( x=c(firstYrFig, max(yrRange)), y=0 ) +
  myTheme +
  theme( axis.text.x=element_blank(), legend.position="top" )

# Plot mean spawn width by year
spawnWidthPlot <- ggplot( data=spawnYrFig, aes(x=Year, y=MeanWidth) ) +
  geom_point( aes(shape=Survey) ) + 
  geom_line( aes(group=Survey) ) +
  #    geom_smooth( method=smLine, colour="black", level=ciLevel ) +
  labs( x=NULL, y="Mean spawn width (m)" ) + 
  scale_x_continuous( breaks=yrBreaks ) +
  scale_y_continuous( labels=comma ) +
  annotate( geom="text", x=-Inf, y=Inf, label="(b)", vjust=1.3, hjust=-0.1 ) +
  guides( shape=FALSE ) +
  expand_limits( x=c(firstYrFig, max(yrRange)), y=0 ) +
  myTheme +
  theme( axis.text.x=element_blank() )

# Plot mean layers of spawn by year
spawnLayersPlot <- ggplot( data=spawnYrFig, aes(x=Year, y=MeanLayers) ) +
  geom_point( aes(shape=Survey) ) + 
  geom_line( aes(group=Survey) ) +
  #    geom_smooth( method=smLine, colour="black", level=ciLevel ) +
  labs( y="Mean number of egg layers" ) + 
  scale_x_continuous( breaks=yrBreaks ) +
  scale_y_continuous( labels=comma ) +
  annotate( geom="text", x=-Inf, y=Inf, label="(c)", vjust=1.3, hjust=-0.1 ) +
  guides( shape=FALSE ) +
  expand_limits( x=c(firstYrFig, max(yrRange)), y=0 ) +
  myTheme 

# Arrange and save length, width, and layer plots
plot_grid( spawnLengthPlot, spawnWidthPlot, spawnLayersPlot, align="v", ncol=1, 
  rel_heights=c(1.2, 1, 1.1) ) +
  ggsave( filename=file.path(regName, "SpawnDimensions.pdf"), width=figWidth, 
    height=figWidth*1.25 )

# Plot spawn index by survey type: surface, macro, under
spawnIndexTypePlot <- ggplot( data=spawnYrFigType, aes(x=Year, y=SI) ) +
  geom_point( aes(shape=Survey) ) +
  geom_line( aes(group=Survey) ) +
  labs( y=expression(paste("Spawn index (t"%*%10^3, ")", sep="")) )  +
  scale_x_continuous( breaks=yrBreaks ) +
  scale_y_continuous( labels=function(x) comma(x/1000) ) +
  expand_limits( x=c(firstYrFig-0.5, max(yrRange)+0.5), y=0 ) +
  facet_grid( Type ~ . ) +
  myTheme +
  theme( legend.position="top" ) +
  ggsave( filename=file.path(regName, "SpawnIndexType.pdf"), width=figWidth, 
    height=figWidth*1.2 )

# Plot total spawn index by year
spawnIndexPlot <- ggplot( data=spawnYrFig, aes(x=Year, y=TotalSI) ) +
  geom_point( aes(shape=Survey) ) + 
  geom_line( aes(group=Survey) ) +
  #    geom_smooth( method=smLine, colour="black", level=ciLevel ) +
  labs( x=NULL, y=expression(paste("Spawn index (t"%*%10^3, ")", sep="")) )  +
  scale_x_continuous( breaks=yrBreaks ) +
  scale_y_continuous( labels=function(x) comma(x/1000) ) +
  #    guides( shape=FALSE ) +
  expand_limits( x=c(firstYrFig-0.5, max(yrRange)+0.5), y=0 ) +
  annotate( geom="text", x=-Inf, y=Inf, label="(a)", vjust=1.3, hjust=-0.1 ) +
  myTheme +
  theme( axis.text.x=element_blank(), legend.position="top" )

# If using groups
if( exists("spawnYrGrpFig") ) { 
  # Plot percent spawn index by group
  spawnPercentSAStackPlot <- ggplot( data=spawnYrGrpFig, 
    aes(x=Year, y=PercSI, fill=Group) ) +
    geom_bar( stat="identity", width=1, colour="black", size=0.1 ) +
    labs( x=NULL, y="Spawn index (%)", fill="Group" ) + 
    scale_x_continuous( breaks=yrBreaks ) +
    scale_y_continuous( labels=comma ) +
    expand_limits( x=c(firstYrFig, max(yrRange)) ) +
    scale_fill_viridis( discrete=TRUE ) +
    annotate( geom="text", x=-Inf, y=Inf, label="(b)", vjust=1.3, 
      hjust=-0.1 ) +
    myTheme +
    theme( legend.position="top", axis.text.x=element_blank() )
} else {  # End if using groups, otherwise stat areas
  # Plot percent spawn index by group (stat areas)
  spawnPercentSAStackPlot <- ggplot( data=spawnYrSAFig, 
    aes(x=Year, y=PercSI, fill=StatArea) ) +
    geom_bar( stat="identity", width=1, colour="black", size=0.1 ) +
    labs( x=NULL, y="Spawn index (%)", fill="SA" ) + 
    scale_x_continuous( breaks=yrBreaks ) +
    scale_y_continuous( labels=comma ) +
    expand_limits( x=c(firstYrFig, max(yrRange)) ) +
    scale_fill_viridis( discrete=TRUE ) +
    annotate( geom="text", x=-Inf, y=Inf, label="(b)", vjust=1.3, 
      hjust=-0.1 ) +
    myTheme +
    theme( legend.position="top", axis.text.x=element_blank() )
}  # End if using stat areas

# Determine the number of sections
nSecCol <- n_distinct( spawnYrSecFig$Section )

# Plot proportion of spawn in each section and year
spawnPercentSecStackPlot <- ggplot( data=spawnYrSecFig, 
  aes(x=Year, y=PercSI, fill=Section) ) +
  geom_bar( stat="identity", width=1, colour="black", size=0.1 ) +
  labs( y="Spawn index (%)" ) + 
  scale_x_continuous( breaks=yrBreaks ) +
  scale_y_continuous( labels=comma ) +
  expand_limits( x=c(firstYrFig, max(yrRange)) ) +
  scale_fill_viridis( guide=guide_legend(nrow=ceiling(nSecCol/8)), 
    discrete=TRUE ) +
  annotate( geom="text", x=-Inf, y=Inf, label="(c)", vjust=1.3, hjust=-0.1 ) +
  myTheme +
  theme( legend.position="top" )

# Arrange and save the index and proportion plots
ipPlots <- plot_grid( spawnIndexPlot, spawnPercentSAStackPlot, 
  spawnPercentSecStackPlot, align="v", ncol=1, 
  rel_heights=c(2.1, 2.5, 2.5) ) +
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
      expand_limits( x=c(firstYrFig, max(yrRange)), y=yLims ) +
      scale_fill_viridis( guide=
          guide_legend(ncol=ceiling(length(unique(datSub$Section))/6)),
        discrete=TRUE ) +
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
  expand_limits( x=c(firstYrFig, max(yrRange)) ) +
  facet_wrap( ~ Section, labeller=label_both, drop=TRUE, ncol=3 ) +
  myTheme +
  theme( legend.position="none" ) +
  ggsave( filename=file.path(regName, "SpawnPercentPanel.pdf"), 
    width=figWidth, 
    height=min(7.5, 1.5*(ceiling(n_distinct(spawnYrSecFig$Section)/3))) )

# Plot index of spawn in each section and year
spawnIndexPanelPlot <- ggplot( data=spawnYrSecFig, 
  aes(x=Year, y=TotalSI, fill=Year==max(yrRange)) ) +
  geom_bar( stat="identity" ) +
  labs( y=expression(paste("Spawn index (t"%*%10^3, ")", sep="")) ) + 
  scale_x_continuous( breaks=yrBreaks ) +
  scale_y_continuous( labels=function(x) comma(x/1000) ) +
  scale_fill_grey( start=0.5, end=0 ) +
  expand_limits( x=c(firstYrFig, max(yrRange)) ) +
  facet_wrap( ~ Section, labeller=label_both, drop=TRUE, ncol=3 ) +
  myTheme +
  theme( legend.position="none" ) +
  ggsave( filename=file.path(regName, "SpawnIndexPanel.pdf"), 
    width=figWidth, 
    height=min(7.9, 1.5*(ceiling(n_distinct(spawnYrSecFig$Section)/3))) )

# If length by sampling protocol
if( exists("lenAgeSample") ) {
  # Compare the two sampling protocols (length)
  lenAgeSamplePlot <- ggplot( data=lenAgeSample, 
    aes(x=Age, y=Length, fill=SampleSource2, 
      group=interaction(Age, SampleSource2)) ) + 
    geom_boxplot( outlier.colour="black", size=0.25 ) + 
    labs( y="Length (mm)", fill="Sample" )  +
    scale_x_continuous( breaks=pretty_breaks() ) +
    scale_fill_viridis( discrete=TRUE, alpha=0.5 ) +
    myTheme +
    theme( legend.position="top" ) +
    ggsave( filename=file.path(regName, "LengthAgeSample.pdf"), 
      width=figWidth, height=figWidth*0.67 )
}  # End if length by sampling protocol

# If spawn depth by year and statistical area
if( exists("spawnStatsYrSA") & exists("spawnStatsYrSecSA07") ) {
  # Determine the number of sections
  nSecCol <- n_distinct( spawnStatsYrSecSA07$Section )
  # Plot spawn depth (maximum by spawn number) in each statistical area and year
  spawnDepthSAPlot <- ggplot( data=spawnStatsYrSA, 
    aes(x=Year, y=Depth, fill=StatArea, 
      group=interaction(Year, StatArea)) ) +
    geom_boxplot( outlier.colour="black", outlier.size = 0.5, size=0.25 ) +
    expand_limits( x=c(firstYrFig, max(yrRange)), y=0 ) +
    scale_fill_viridis( discrete=TRUE ) +
    labs( x=NULL, y="Maximum spawn depth (m)", fill="SA" ) + 
    scale_x_continuous( breaks=yrBreaks ) +
    scale_y_reverse( ) +
    annotate( geom="text", x=-Inf, y=-Inf, label="(a)", vjust=1.3, 
      hjust=-0.1 ) +
    myTheme +
    theme( legend.position="top", axis.text.x=element_blank() )
  # Plot spawn depth (maximum by spawn number) in each section and year  
  spawnDepthSecPlot <- ggplot( data=spawnStatsYrSecSA07, 
    aes(x=Year, y=Depth, fill=Section, 
      group=interaction(Year, Section)) ) +
    geom_boxplot( outlier.colour="black", outlier.size = 0.5, size=0.25 ) +
    expand_limits( x=c(firstYrFig, max(yrRange)), y=0 ) +
    scale_fill_viridis( discrete=TRUE ) +
    labs( y="Maximum spawn depth (m)" ) + 
    scale_x_continuous( breaks=yrBreaks ) +
    scale_y_reverse( ) +
    annotate( geom="text", x=-Inf, y=-Inf, label="(b)", vjust=1.3, 
      hjust=-0.1 ) +
    guides( fill=guide_legend(nrow=ceiling(nSecCol/8)) ) +
    myTheme +
    theme( legend.position="bottom" )
  # Arrange and save the depth plots
  depthPlots <- plot_grid( spawnDepthSAPlot, spawnDepthSecPlot, align="v", 
    ncol=1, rel_heights=c(2, 2) ) +
    ggsave( filename=file.path(regName, "SpawnDepthSASec.pdf"), 
      width=figWidth, height=figWidth )
  # Plot spawn layers in each statistical area and year
  spawnLayersSAPlot <- ggplot( data=spawnStatsYrSA, 
    aes(x=Year, y=Layers, fill=StatArea, 
      group=interaction(Year, StatArea)) ) +
    geom_boxplot( outlier.colour="black", outlier.size = 0.5, size=0.25 ) +
    scale_fill_viridis( discrete=TRUE ) +
    expand_limits( x=c(firstYrFig, max(yrRange)), y=0 ) +
    labs( x=NULL, y="Number of egg layers", fill="SA" ) + 
    scale_x_continuous( breaks=yrBreaks ) +
    annotate( geom="text", x=-Inf, y=Inf, label="(a)", vjust=1.3, 
      hjust=-0.1 ) +
    myTheme +
    theme( legend.position="top", axis.text.x=element_blank() )
  # Plot spawn layers in each section and year  
  spawnLayersSecPlot <- ggplot( data=spawnStatsYrSecSA07, 
    aes(x=Year, y=Layers, fill=Section, 
      group=interaction(Year, Section)) ) +
    geom_boxplot( outlier.colour="black", outlier.size = 0.5, size=0.25 ) +
    expand_limits( x=c(firstYrFig, max(yrRange)), y=0 ) +
    labs( y="Number of egg layers" ) + 
    scale_x_continuous( breaks=yrBreaks ) +
    annotate( geom="text", x=-Inf, y=Inf, label="(b)", vjust=1.3, 
      hjust=-0.1 ) +
    guides( fill=guide_legend(nrow=ceiling(nSecCol/8)) ) +
    myTheme +
    theme( legend.position="bottom" )
  # Arrange and save the depth plots
  layerPlots <- plot_grid( spawnLayersSAPlot, spawnLayersSecPlot, align="v", 
    ncol=1, rel_heights=c(2, 2) ) +
    ggsave( filename=file.path(regName, "SpawnLayersSASec.pdf"), 
      width=figWidth, height=figWidth )
}  # End if spawn depth by year and statistical area

# Show spawn index by locations by year
PlotLocationsYear <- function( dat ) {
  # Wrangle data for spawn index by year
  datYr <- dat %>%
    group_by( Year ) %>%
    summarise( SITotal=SumNA(SITotal) ) %>%
    ungroup( ) %>%
    mutate( Survey=ifelse(Year < newSurvYr, "Surface", "Dive"),
      Survey=factor(Survey, levels=c("Surface", "Dive")) )
  # Get the number of plots
  uPages <- unique( dat$Year )
  # Set up the map
  MapGIF <- BaseMap + 
    geom_path( data=shapes$secDF, aes(group=Section), size=0.25, 
      colour="black" ) +
    geom_text( data=shapes$secCentDF, alpha=0.6, size=2,
      aes(label=paste("Sec", Section, sep=" ")) )
  # Start the PDF
  pdf( file=file.path(regName, "SpawnIndexAnimation.pdf"), width=figWidth, 
    height=min(6.75, 6.5/shapes$xyRatio) )
  # Loop over pages/years
  for( i in 1:length(uPages) ) { 
    # Get the index (up to 9999)
    iLong <- formatC( uPages[i], width=4, flag="0" )
    # The plot
    layersPlot <- MapGIF +
      facet_wrap_paginate( ~ Year, ncol=1, nrow=1, page=i, 
        labeller=label_both ) +
      geom_point( data=dat, aes(colour=SITotal), size=4, alpha=0.75 ) +
      scale_colour_viridis( labels=comma ) +
      labs( colour="Spawn\nindex (t)" ) +
      theme( legend.justification=c(0, 0), 
        legend.position=if(region == "PRD") "right" else c(0.01, 0.01) )
    # Inset: spawn index vs year
    subPlot <- ggplot( data=datYr, aes(x=Year, y=SITotal) ) +
      geom_point( size=0.25, aes(shape=Survey) ) +
      geom_point( data=datYr[i, ], aes(shape=Survey), size=1.5 ) +
      geom_path( size=0.2, aes(group=Survey) ) +
      scale_y_continuous( labels=comma ) +
      labs( x=NULL, y=NULL ) +
      guides( shape=FALSE ) +
      theme_tufte( ) +
      theme( plot.background=element_rect(fill=alpha("white", 0.5), 
        size=0.1),
        plot.margin=unit(c(0.3, 0.6, 0.1, 0.1), "lines") )
    # Convert to a grob
    subGrob <- ggplotGrob( x=subPlot )
    # Determine the x location for the grob
    grobLeft <- max(shapes$landCropDF$Eastings) - 
      diff(range(shapes$landCropDF$Northings)) / 2.5
    # Determine the y location for the grob
    grobBottom <- max(shapes$landCropDF$Northings) - 
      diff(range(shapes$landCropDF$Northings)) / 5
    # Add the inset to the map
    finalPlot <- layersPlot + 
      annotation_custom( grob=subGrob, xmin=grobLeft, xmax=Inf, 
        ymin=grobBottom, ymax=Inf )
    # Print the main plot
    print( finalPlot )
  }  # End i loop over years
  # Turn the device off
  dev.off( )
  # Save a copy (for later, if desired)
  file.copy( from=file.path(regName, "SpawnIndexAnimation.pdf"),
    to=file.path("Animations", 
      paste("SpawnIndexAnimation", regName, "pdf", sep=".")),
    overwrite=TRUE )
}  # End PlotLocationsYear

# Make the animation
if( makeAnimation ) {
  # Show spawn locations (this takes a few minutes!)
  PlotLocationsYear( dat=siYearLoc )
} else {  # End if making the animation, otherwise  
  # Copy the saved version
  file.copy( from=file.path("Animations", 
    paste("SpawnIndexAnimation", regName, "pdf", sep=".")),  
    to=file.path(regName, "SpawnIndexAnimation.pdf") )
}  # End if not making the animation

# Update progress
cat( "done\n" )

##### xTables #####

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

# Format commercial harvest (recent years)
xHarvestSOK <- harvestSOK %>%
  filter( Year >= firstYrTab ) %>%
  mutate( Year=as.integer(Year), 
    Harvest=format(Harvest, big.mark=",", digits=0, scientific=FALSE),
    Biomass=format(Biomass, big.mark=",", digits=0, scientific=FALSE) ) %>%
  rename( 'Harvest (lb)'=Harvest, 'Spawning biomass (t)'=Biomass ) %>%
  xtable( )

# Write commercial harvest to disc
print( x=xHarvestSOK, file=file.path(regName, "HarvestSOK.tex"),
  include.rownames=FALSE, booktabs=TRUE, only.contents=TRUE, NA.string=NA )

###### This stuff is for the stock assessment research document #####
# write_csv( x=allHarvSOK, path=paste("allHarvSOK", regName, ".csv", sep="") )
#pSOK1 <- ggplot( data=allHarvSOK, aes(x=Year, y=Harvest) ) +
#    geom_bar( stat="identity", aes(fill=Year==max(yrRange)) ) +
#    scale_fill_grey( start=0.5, end=0 ) +
#    labs( y=expression(paste("Harvest (kg"%*%10^3, ")", sep="")) )  +
#    scale_y_continuous( labels=function(x) comma(x/1000) ) +
#    guides( fill=FALSE ) +
#    myTheme + 
#    ggsave( filename=paste("HarvestSOK", regName, ".png", sep=""), 
#        width=figWidth, height=figWidth*0.67 )
#pSOK2 <- ggplot( data=allHarvSOK, aes(x=Year, y=Biomass) ) +
#    geom_bar( stat="identity", aes(fill=Year==max(yrRange)) ) +
#    scale_fill_grey( start=0.5, end=0 ) +
#    labs( y=expression(paste("Spawning biomass (t"%*%10^3, ")", sep="")) )  +
#    scale_y_continuous( labels=function(x) comma(x/1000) ) +
#    guides( fill=FALSE ) +
#    myTheme + 
#    ggsave( filename=paste("BiomassSOK", regName, ".png", sep=""), 
#        width=figWidth, height=figWidth*0.67 )

# Format number of biosamples
xBioNum <- bioNum %>%
  mutate( Year=as.integer(Year), Commercial=as.integer(Commercial),
    Test=as.integer(Test) ) %>%
  xtable()

# Write number of biosamples to disc
print( x=xBioNum, file=file.path(regName, "BioNum.tex"),
  include.rownames=FALSE, booktabs=TRUE, only.contents=TRUE, NA.string=NA )

# Format number of biosamples by type
xBioTypeNum <- bioTypeNum %>%
  mutate( Number=as.integer(Number) ) %>%
  rename( 'Number of samples'=Number ) %>%
  xtable( )

# Write number of biosamples by type to disc
print( x=xBioTypeNum, file=file.path(regName, "BioTypeNum.tex"),
  include.rownames=FALSE, booktabs=TRUE, only.contents=TRUE, NA.string=NA )

# Number-, proportion-, weight- and length-at-age
if( exists("deltaNumAgeYr") & exists("deltaPropAgeYr") & 
    exists("deltaWtAgeYr") & exists("deltaLenAgeYr") ) {
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
    xtable( digits=c(0, 0, rep(0, times=length(ageRange))) )
  # Write weight-at-age to disc
  print( x=xDeltaWtAgeYr, file=file.path(regName, "DeltaWtAgeYr.tex"),
    include.rownames=FALSE, booktabs=TRUE, only.contents=TRUE, NA.string=NA )
  # Format length-at-age
  xDeltaLenAgeYr <- deltaLenAgeYr %>%
    xtable( digits=c(0, 0, rep(0, times=length(ageRange))) )
  # Write length-at-age to disc
  print( x=xDeltaLenAgeYr, file=file.path(regName, "DeltaLenAgeYr.tex"),
    include.rownames=FALSE, booktabs=TRUE, only.contents=TRUE, NA.string=NA )
}  # End if number-, proportion-, weight-, and length-at-age

# Format proportion-at-age
xPropAgedYearTab <- propAgedYearTab %>%
  xtable( digits=c(0, 0, rep(3, times=length(ageRange))) )

# Write proportion-at-age to disc
print( x=xPropAgedYearTab, file=file.path(regName, "PropAgedYearTab.tex"),
  include.rownames=FALSE, booktabs=TRUE, only.contents=TRUE, NA.string=NA )

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
    MeanLayers=format(MeanLayers, big.mark=",", digits=1),
    TotalSI=format(TotalSI, big.mark=",", digits=0, 
      scientific=FALSE) ) %>%
  rename( 'Total length (m)'=TotalLength, 'Mean width (m)'=MeanWidth,
    'Mean number of egg layers'=MeanLayers, 'Spawn index (t)'=TotalSI ) %>%
  xtable( )

# Write spawn summary to disc
print( x=xSpawnYrTab, file=file.path(regName, "SpawnYrTab.tex"),
  include.rownames=FALSE, booktabs=TRUE, only.contents=TRUE, NA.string=NA )

# If there is spawn reported
if( nrow(spawnByLoc) >= 1 ) {
  # Format spawn summary
  xSpawnByLoc <- spawnByLoc %>%
    arrange( StatArea, Section, LocationName, Start ) %>%
    mutate( StatArea=formatC(StatArea, width=2, format="d", flag="0"),
      Section=formatC(Section, width=3, format="d", flag="0"),
      Start=format(Start, format="%B %d"),
      TotalSI=format(TotalSI, big.mark=",", digits=0, 
        scientific=FALSE) ) %>%
    rename( 'Statistical Area'=StatArea, 'Location name'=LocationName,
      'Start date'=Start, 'Spawn index (t)'=TotalSI ) %>%
    xtable( )
  # Write spawn summary (longtable) to disc
  WriteLongTable( dat=xSpawnByLoc, fn=file.path(regName, "SpawnByLoc.tex") )
} else { # End if there was spawn, otherwise
  # Make an empty file (required for latex)
  write_csv( x=spawnByLoc, path=file.path(regName, "SpawnByLoc.tex") )
}  # End if there was no spawn

# Format the spatial table
xSpatialGroup <- spatialGroup %>%
  rename( Region=RegionName, 'Statistical Area'=StatArea ) %>%
  xtable( )
# Write the spatial table (longtable) to disc
WriteLongTable( dat=xSpatialGroup, fn=file.path(regName, "SpatialGroup.tex") )

##### LaTeX #####

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
# Turn the toggle to false: show spatial table of stat area, section, and group
tfSpatialGroup <- "\\togglefalse{spatialGroup}"

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
  # Turn the toggle to true: show spatial table with group info
  tfSpatialGroup <- "\\toggletrue{spatialGroup}"
  # Create a legend for Groups
  spawnIndexGroupLegend <- "Legend: `14\\&17' is Statistical Areas 14 and 17
      (excluding Section 173); `ESoG' is eastern Strait of Georgia; 
      `Lazo' is above Cape Lazo; and `SDodd' is South of Dodd Narrows"
}  # End if Strait of Georgia

# If the region is Central Coast
if( region == "CC" ) {
  # Turn the toggle to true: show spatial table with group info
  tfSpatialGroup <- "\\toggletrue{spatialGroup}"
  # Create a legend for Groups
  spawnIndexGroupLegend <- "Legend: `6\\&7' is Statistical Areas 6 and 7; and
      `8' is Statistical Area 8."
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

# If number-, proportion-, weight-, and length-at-age: set the toggle to true
if( exists("deltaNumAgeYr") & exists("deltaPropAgeYr") & 
    exists("deltaWtAgeYr") & exists("deltaLenAgeYr") )  
  tfNumPropWtAge <- "\\toggletrue{numPropWtAge}"

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

# Get names for spatial groups
namesSpatialGroup <- paste( names(xSpatialGroup), collapse=" & " )

# Formatted year ranges for q1 (surface) and q2 (dive)
qYrs <- list(
  q1=paste(range(yrRange[yrRange<newSurvYr]), collapse=" to "),
  q2=paste(range(yrRange[yrRange>=newSurvYr]), collapse=" to ") )

# Justification for age tables
ageTableAlign <- paste( c("{l", rep("r", times=length(ageRange)), "}"), 
  collapse="" )

##### Tables #####

# Progress message
cat( "Writing tables... " )

## Write areas to a csv
#write_csv( x=areas, path=file.path(regName, "Areas.csv") )
#
## Write raw catch data to a csv
#write_csv( x=catchRaw, path=file.path(regName, "CatchRaw.csv") )
#
## Write raw biological data to a csv
#write_csv( x=bioRaw, path=file.path(regName, "BioRaw.csv") )
#
## Write raw spawn data to a csv (choose one)
# write_csv( x=spawnRaw, path=file.path(regName, "SpawnRaw.csv") )
# write_csv( x=spawnRaw, path="SpawnRaw.csv", append=ifelse(r==1, FALSE, TRUE) )
#
## Write catch data to a csv
#write_csv( x=catch, path=file.path(regName, "Catch.csv") )
#
## Write biological data to a csv
#write_csv( x=bio, path=file.path(regName, "Bio.csv") )
#
## Write ADMB catch to a csv
#write_csv( x=catchADMB, path=file.path(regName, "CatchADMB.csv") )
#
## Write ADMB spawn to a csv
#write_csv( x=spawnADMB, path=paste(regName, "SpawnADMB.csv", sep="") )
#
## Write ADMB number aged to a csv
#write_csv( x=numAgedADMB, path=file.path(regName, "NumAgedADMB.csv") )
#
## Write ADMB weight-at-age to a csv
#write_csv( x=weightAgeADMB, path=file.path(regName, "WeightAgeADMB.csv") )
#
## If exists, write weight-at-age by group to a csv
#if( exists("weightAgeGroupN") )
#  write_csv( x=weightAgeGroupN, path=file.path(regName, 
#          "WeightAgeGroupN.csv") )

# Write the number of biosamples to a csv
write_csv( x=numBiosamples, 
  path=file.path(regName, paste("NumBiosamples", regName, ".csv", sep="")) )

# Write the spawn distribution to a csv
write_csv( x=propSpawn, 
  path=file.path(regName, paste("PropSpawn", regName, ".csv", sep="")) )

# Write the SOK harvest to a csv
write_csv( x=allHarvSOK, 
  path=file.path(regName, paste("HarvSOK", regName, ".csv", sep="")) )

## Format the spawn summary
#spawnYrF <- spawnYr %>%
#    mutate( TotalLength=formatC(TotalLength, digits=0, format="f"),
#        MeanWidth=formatC(MeanWidth, digits=0, format="f"),
#        MeanLayers=formatC(MeanLayers, digits=3, format="f"),
#        TotalSI=formatC(TotalSI, digits=0, format="f"),
#        Region=regName ) %>%
#    select( Region, Year, TotalLength, MeanWidth, MeanLayers, TotalSI )
#
## Write the spawn summary
#write_csv( x=spawnYrF, path=paste("RawIndex", regName, ".csv", sep="") )

# Update progress
cat( "done\n" )

##### Output #####

# Save the workspace image 
save.image( file=file.path(regName, 
  paste("Image.", regName, ".RData", sep="")) ) 

##### QAQC #####

## Missing spatial groups: sections and statistical areas
#areaNoGroup <- areas %>%
#    filter( is.na(Group) ) %>%
#    select( Region, StatArea, Section ) %>%
#    distinct( ) %>%
#    write_csv( path="AreasNoGroup.csv", append=ifelse(r==1, FALSE, TRUE) )

## Missing spatial groups: spawn
#spawnNoGroup <- spawnRaw %>%
#    filter( is.na(Group) ) %>%
#    group_by( Region, Year, StatArea, Section, LocationCode ) %>%
#    summarise( Spawn=SumNA(c(SurfSI, MacroSI, UnderSI)) ) %>%
#    ungroup( ) %>%
#    mutate( Spawn=round(Spawn, digits=1) ) %>%
#    write_csv( path="SpawnNoGroup.csv", append=ifelse(r==1, FALSE, TRUE) )

## Missing spatial groups: bio
#bioNoGroup <- bio %>%
#    filter( is.na(Group) ) %>%
#    group_by( Region, Year, StatArea, Section, LocationCode, Sample ) %>%
#    summarise( NFish=n() ) %>%
#    ungroup( ) %>%
#    write_csv( path="BioNoGroup.csv", append=ifelse(r==1, FALSE, TRUE) )

## Missing spatial groups: catch
#catchNoGroup <- catch %>%
#    filter( is.na(Group) ) %>%
#    group_by( Region, Year, StatArea, Section, Gear ) %>%
#    summarise( Catch=SumNA(Catch) ) %>%
#    ungroup( ) %>%
#    mutate( Catch=round(Catch, digits=1) ) %>%
#    write_csv( path="CatchNoGroup.csv", append=ifelse(r==1, FALSE, TRUE) )

##### End ##### 

# Print end of file message and elapsed time
cat( "End of file Summary.R: ", sep="" ) ;  print( Sys.time( ) - sTime )
