##### Header #####
# Author:       Matthew H. Grinnell
# Affiliation:  Pacific Biological Station, Fisheries and Oceans Canada (DFO)
# Group:        Quantitative Assessment Methods Section, Science
# Address:      3190 Hammond Bay Road, Nanaimo, BC, Canada, V9T 6N7
# Contact:      e-mail: matthew.grinnell@dfo-mpo.gc.ca | tel: 778.268.1026
# Project:      Herring
# Code name:    Summary.R
# Version:      2.0
# Date started: Jun 03, 2016
# Date edited:  Feb 27, 2024
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
# multiple regions if requested. Output (e.g., .RData, png figures, latex
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
# 19. Add option to analyses more than one SAR at a time (in addition to 'All').
# 20. Go through and fix all the warnings (i.e., so it runs without warnings).

##### Housekeeping #####

# General options
# Tesing automatic solution to commenting out rm( list=ls() )
# if( basename(sys.frame(1)$ofile)=="Summary.R" )
rm(list = ls()) # Clear the workspace
sTime <- Sys.time() # Start the timer
graphics.off() # Turn graphics off

# Install missing packages and load required packages (if required)
UsePackages <- function(pkgs, locn = "https://cran.rstudio.com/") {
  # Reverse the list
  rPkgs <- rev(pkgs)
  # Identify missing (i.e., not yet installed) packages
  newPkgs <- rPkgs[!(rPkgs %in% installed.packages()[, "Package"])]
  # Install missing packages if required
  if (length(newPkgs)) install.packages(newPkgs, repos = locn)
  # Loop over all packages
  for (i in 1:length(rPkgs)) {
    # Load required packages using 'library'
    eval(parse(text = paste("suppressPackageStartupMessages(library(", rPkgs[i],
                            "))",
                            sep = ""
    )))
  } # End i loop over package names
} # End UsePackages function

# Make packages available
UsePackages(pkgs = c(
  "tidyverse", "RODBC", "zoo", "Hmisc", "scales", "sp", "maptools", "rgdal",
  "rgeos", "raster", "xtable", "cowplot", "grid", "colorRamps", "RColorBrewer",
  "stringr", "lubridate", "readxl", "plyr", "ggforce", "viridis", "ggthemes",
  "SpawnIndex", "tidyselect", "ggrepel", "here", "rnaturalearth",
  "rnaturalearthhires"
))

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

##### Controls #####

# Select region(s): major (HG, PRD, CC, SoG, WCVI); minor (A27, A2W); special
# (JS, A10); or all (All)
if (!exists("region")) region <- "SoG"

# Sections to include for sub-stock analyses
Sec002 <- c(2)
Sec003 <- c(3)
Sec006 <- c(6)
Sec021025 <- c(21, 25)
Sec023024 <- c(23, 24)
Sec002003 <- c(2, 3)
Sec172 <- c(172)
Sec173 <- c(173)
Area06 <- c(67)
Area07 <- c(70:79)
Area08 <- c(85, 86)
Area10 <- c(101:103)
Area13 <- c(131:136)
Area15 <- c(150:159)
Area23 <- c(230:233, 239)
Area24 <- c(240:245, 249)
Area25 <- c(250:253, 259)
AllHG <- c(0:6, 11, 12, 21:25)
Atlegay <- c(
  132, 135, 140:143, 150:152, 160:165, 170:173, 180:182, 190:193, 280, 291, 292,
  230:233, 239, 240:245, 249, 250:253, 259,
  111, 112, 121:127, 131, 133, 134, 136,
  270:274
)
Broughton <- c(111, 112, 121:127)
ESoG <- c(150:152, 160:165, 280, 291, 292)
JS <- c(111, 112, 121:127, 131:136)
Lazo <- c(132, 135, 141)
OutHG <- c(11, 12, 22)
SA1417 <- c(140, 142, 143, 170:172)
SDodd <- c(173, 180:182, 190:193 )
SoGN <- c(132, 135, 140:143, 171, 172, 151, 152, 161:165, 280, 291, 292)
SoGS <- c(173, 181, 182, 191:193)
Swift <- c(201, 202, 211, 220, 230:233, 239)
Tlaamin <- c(135, 141, 151, 152, 161:163)

# Select a subset of sections (or NULL for all)
sectionSub <- NULL
secSubNum <- 1
secSubName <- "Swift"

# if(is.null(sectionSub)){
#   secSubNum <- 1
#   secSubName <- toupper(region)
# }

# Send to SISCA folder otherwise SISCA data only outputs to Summaries folder
send2sisca <- FALSE

# Make the spawn animation (takes 5--8 mins per SAR); see issue #3
makeAnimation <- FALSE

# Open 64-bit R in a separate window (to make the animation)
system64 <- TRUE

# Include test fishery catch
inclTestCatch <- TRUE

# Include test seine biological data
inclTestSNBio <- TRUE

# Include test gillnet biological data
inclTestGNBio <- FALSE

# Include spawn on kelp biological data
inclSOKBio <- TRUE

# Location of herring databases (catch, biosamples, spawn, etc)
dirDBs <- file.path("..", "Data")

# Location of the shapefiles
# dirShape <- file.path( "\\\\dcbcpbsna01a", "hdata$", "Kristen",
#    "Herring_Shapefiles" )
dirShape <- file.path(dirDBs, "Polygons")

# Location of privacy data
dirPriv <- file.path(dirDBs, "Privacy")

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

# Location of the "data" folder in the herringsr repository
srLoc <- file.path("..", "herringsr", "data")

##### Parameters #####

# Load parameter values (for spawn index)
data(pars)

# Year range to include data (data starts at 1928; 1951 for stock assessment)
yrRange <- pars$year$assess:2023

# Age range: omit below, plus group above
ageRange <- 2:10

# Age to highlight in figure
ageShow <- 3

# Second age to hightlight in figure
ageShow2 <- 6

# Number of years to calculate running mean
nRoll <- 5

# Conversion factors: short tons to tonnes, feet to metres, pounds to kilograms
convFac <- list(st2t = 0.90718474, ft2m = 0.3048, lb2kg = 0.453592)

# First year of data to include in summary tables
firstYrTab <- max(yrRange) - 10

# First year of data to include in summary figures
firstYrFig <- max(yrRange) - 35

# Age schedule and population parameters for model input
parsADMB <- list(
  AgePopulation = list(
    "Asymptotic length (linf)" = 27,
    "Brody growth coefficient (k)" = 0.48,
    "Theoretical age at zero length (t0)" = 0,
    "Scalar in length-weight allometry (alpha)" = 4.5e-6,
    "Power parameter in length-weight allometry (beta)" = 3.127,
    "Age at 50% maturity" = 2.055,
    "Standard deviation at 50% maturity" = 0.05,
    "Natural mortality indicator" = 1,
    "Maturity vector" = c(0.24974, 0.9, 1, 1, 1, 1, 1, 1, 1)
  ),
  DelayDifference = list(
    "(kage)" = 0,
    "(alpha_g)" = 0,
    "(rho_g)" = 0,
    "(wk)" = 0
  ),
  General = list(
    "HistoricCatch" = 20,
    "SurveyType" = 3,
    "SampleSize" = 100,
    "Composition" = 1
  )
)

# Last year of the reduction fishery #To do JC SP is this year
lastRedYr <- 1970

# Figure width
figWidth <- 6

# Figure resolution (DPI; use 90 for herringsr maps)
figRes <- 320

# Type of smoothing line
smLine <- "loess"

# Level of confidence interval
ciLevel <- 0.9

# Get ylimits (e.g., weight in g) for the weight-at-age plot
wtRange <- c(50, 250)

# Get ylimits (e.g., length in mm) for the length-at-age plot
lenRange <- c(150, 275)

# Year range for historic ratio of biosample effort (Central Coast)
yrsRatioHist <- 1994:2013

# Years to fix using historic ratio of biosample effort (Central Coast)
yrsRatioFix <- c(2014, 2015)

# Transect swath (i.e., width of the transect in m; used for macrocystis spawn)
transectSwath <- 2

# Buffer distance (m; to include locations that are outside the region polygon)
maxBuff <- 5000

# Start year for catch validation program (not currently used)
validCatch <- list(RoeGN = 1998, RoeSN = 1999)

# Spawn index scaling parameter
q <- tribble(
  ~Region, ~Survey, ~Median,
  "HG",       "Surface", 0.428,
  "HG",       "Dive",    0.999,
  "PRD",      "Surface", 0.537,
  "PRD",      "Dive",    1.000,
  "CC",       "Surface", 0.321,
  "CC",       "Dive",    0.999,
  "SoG",      "Surface", 1.037,
  "SoG",      "Dive",    0.999,
  "WCVI",     "Surface", 0.844,
  "WCVI",     "Dive",    0.999
  ) %>%
  filter(Region == region)

#### Sources #####

# File name for dive transect XY
diveLoc <- list(
  loc = dirDBs,
  fn = "dive_transects_with_lat_long_June2_2017.xlsx"
)

# Location and names of lookup tables with catch codes
codesLoc <- list(
  loc = dirDBs,
  fns = list(
    tDisposal = "tDisposal.csv", tGear = "tGear.csv", tSource = "tSource.csv",
    tPeriod = "tPeriod.csv", tGroup = "tGroup.csv"
  )
)

# Location and name of the location database and tables
areaLoc <- list(
  loc = file.path(dirDBs, dbLoc),
  db = dbName,
  fns = list(sections = "Sections", locations = "Location")
)

# Location and name of tables for widths.
widthLoc <- list(
  loc = file.path(dirDBs, dbLoc),
  db = dbName,
  fns = list(
    region_std = "RegionStd", section_std = "SectionStd", pool_std = "PoolStd"
  )
)

# Location(s) and names of the Sections and land shapefiles
shapesLoc <- list(
  locSec = file.path(dirShape),
  locLand = file.path(dirShape),
  fns = list(sections = "SectionsIntegrated", land = "GSHHS_h_L1_Alb")
)

# Location and name of the catch database and tables
catchLoc <- list(
  loc = file.path(dirDBs, dbLoc),
  db = dbName,
  fns = list(
    tCatch = "tCatchData", hCatch = "HailCatch", sokCatch = "SpawnOnKelp"
  )
)

# Location and name of the biological database and tables
bioLoc <- list(
  loc = file.path(dirDBs, dbLoc),
  db = dbName,
  fns = list(samples = "sample", fish = "fish", bmc = "BMcCarter")
)

# Location and name of the surface database and tables
surfLoc <- list(
  loc = file.path(dirDBs, dbLoc),
  db = dbName,
  fns = list(surface = "tSSSurface", all_spawn = "tSSAllspawn")
)

# Location and name of the macrocystis database and tables
macroLoc <- list(
  loc = file.path(dirDBs, dbLoc),
  db = dbName,
  fns = list(
    all_spawn = "tSSAllspawn", plants = "tSSMacPlant", transects = "tSSMacTrans"
  )
)

# Location and name of the macrocystis database and tables
underLoc <- list(
  loc = file.path(dirDBs, dbLoc),
  db = dbName,
  fns = list(
    all_spawn = "tSSAllspawn", alg_trans = "tSSVegTrans",
    stations = "tSSStations", algae = "tSSVegetation"
  )
)

# Location and name of the all spawn tables
allLoc <- list(
  loc = file.path(dirDBs, dbLoc),
  db = dbName,
  fns = list(all_spawn = "tSSAllspawn", stations = "tSSStations")
)

# Location and name of catch and harvest privacy data
privLoc <- list(
  loc = file.path(dirPriv),
  fn = list(Region = "PrivacyRegion.csv", StatArea = "PrivacyStatArea.csv")
)

# Location and name of incidental catch data
icLoc <- list(
  loc = dirDBs,
  fn = "Herring Data Request.xlsx",
  sheets = list(ic = "IC", wm = "WM")
)

# TODO: Load and save data from SQL, then re-load and re-save only if desired,
# to save time (instead of re-loading each time). Save as *.RData objects.

##### Functions #####

# Load helper functions
# TODO: Move these to `herringutils`
source(file = file.path("..", "HerringFunctions", "Functions.R"))
# source_url( url="https://github.com/grinnellm/HerringFunctions/blob/master/Functions.R" )

##### Data #####

# Load regions table
data(regions)

# Determine if region is major or minor (or special)
regionType <- tolower(regions$Type[which(regions$Region == region)])

# Fix for all regions
if(region == "All") regionType <- "All"

# If region is special
if(regionType == "special") {
  # Include major, minor, and this region
  regions <- regions %>%
    filter(Type != "Special" | Region == region)
} else{ # End if special, otherwise
  # Omit special regions
  regions <- regions %>% 
    filter(Type != "Special")  
} # End if not special

# Possible regions by type (major and minor)
allRegions <- list(
  major = regions %>%
    filter(Type == "Major") %>%
    pull(Region),
  minor = regions %>%
    filter(Type == "Minor") %>%
    pull(Region)
)

# Possible regions by type (long; major and minor)
allRegionsLong <- list(
  major = regions %>%
    mutate(Name = paste0(RegionName, " (", Region, ")")) %>%
    filter(Type == "Major") %>%
    pull(Name),
  minor = regions %>%
    mutate(Name = paste0(RegionName, " (", Region, ")")) %>%
    filter(Type == "Minor") %>%
    pull(Name)
)

# Load intensity categories
data(intensity)

# Load algae coefficients
data(algae_coefs)

# Load understory width adjustments
data(under_width_facs)

# Message re region
cat("Region(s): ", paste(region, collapse = ", "), " from ",
    paste(range(yrRange), collapse = " to "), "\n", sep = ""
)

# Breaks for years
yrBreaks <- seq(
  from = round_any(x = min(yrRange), accuracy = 10, f = floor),
  to = round_any(x = max(yrRange), accuracy = 10, f = ceiling), by = 10
)

# Function to load transect spatial info
LoadTransectXY <- function(loc) {
  # Load the data and wrangle
  dat <- read_excel(path = file.path(loc$loc, loc$fn), sheet = 1) %>%
    rename(LocationCode = LOC_CODE) %>%
    mutate(LocationCode = as.integer(LocationCode)) %>%
    group_by(LocationCode) %>%
    summarise(
      Longitude = MeanNA(c(StartLong, MidLong, EndLong)),
      Latitude = MeanNA(c(StartLat, MidLat, EndLat))
    ) %>%
    ungroup() %>%
    filter(!is.na(Longitude), !is.na(Latitude), LocationCode != 0)
  res <- dat #%>%
    # st_as_sf(coords = c("Longitude", "Latitude"))
  # # Grab the spatial info (X and Y)
  # locSP <- dat %>%
  #   transmute(X = Longitude, Y = Latitude)
  # # Put X and Y into a spatial points object
  # locPts <- SpatialPointsDataFrame(
  #   coords = locSP,
  #   data = data.frame(LocationCode = dat$LocationCode), proj4string = CRS(inCRS)
  # )
  # # Convert X and Y from WGS to Albers
  # locPtsAlb <- spTransform(x = locPts, CRSobj = CRS(outCRS))
  # # Extract spatial info
  # dfAlb <- as_tibble(locPtsAlb) %>%
  #   rename(Eastings = X, Northings = Y)
  # Return the data
  return(res)
} # End LoadTransectXY function

# Load 'auxiliary' dive transect spatial data
transectXY <- LoadTransectXY(loc = diveLoc)
# TODO: Make a function to replace missing Lat/Long values with these

# If region is a vector, collapse region names for output; otherwise region
regName <- paste(region, collapse = ".")

# Make required folders
if (!"Summaries" %in% list.files()) dir.create(path = "Summaries")
if (!"Animations" %in% list.files()) dir.create(path = "Animations")

# If old directory exists
if (regName %in% list.files()) {
  # Remove the old directory
  unlink(x = regName, recursive = TRUE)
  # Warning: remove previous summary output
  warning("Removed existing directory '", regName, "'", call. = FALSE)
  # Create the main directory for output
  dir.create(path = regName)
} else { # End if directory exists, otherwise
  # Create the main directory for output
  dir.create(path = regName)
} # End if directory doesn't exists

# Load disposal codes
tDisposal <- read_csv(
  file = file.path(codesLoc$loc, codesLoc$fns$tDisposal),
  col_types = cols("i", "c", "c", "i", "i", "c")
)

# Load gear codes
tGear <- read_csv(
  file = file.path(codesLoc$loc, codesLoc$fns$tGear),
  col_types = cols("i", "c", "i", "i", "i")
)

# Load source codes
tSource <- read_csv(
  file = file.path(codesLoc$loc, codesLoc$fns$tSource),
  col_types = cols("i", "c", "c")
)

# Load period codes
tPeriod <- read_csv(
  file = file.path(codesLoc$loc, codesLoc$fns$tPeriod),
  col_types = cols("c", "c")
)

# Load groups
tGroup <- read_csv(
  file = file.path(codesLoc$loc, codesLoc$fns$tGroup), col_types = cols()
)

# Load herring areas
areas <- load_area_data(where = areaLoc, reg = region, sec_sub = sectionSub,
                      groups = tGroup)
all_areas <- load_area_data(where = areaLoc, reg = "All")

# Use herring sections from SpawnIndex package
data(sections)

# Get herring shapefiles (for plots)
shapes <- load_sections(sections = sections, areas = areas)
# shapes <- LoadShapefiles(where = shapesLoc, a = areas)
all_shapes <- load_sections(sections = sections, areas = all_areas)
all_regions <- all_shapes$regions %>%
  filter(SAR != -1)

# Bounding box (extent)
reg_bbox <- shapes$regions %>%
  st_buffer(dist = 10000) %>%
  st_bbox() %>%
  st_as_sfc()

# Smaller bounding box for plots
reg_bbox_small <- shapes$regions %>%
  st_buffer(dist = 5000) %>%
  st_bbox()
  
# X:Y ratio for plots
reg_ratio_small <- (reg_bbox_small$ymax - reg_bbox_small$ymin) /
  (reg_bbox_small$xmax - reg_bbox_small$xmin)

# Bounding box (extent)
bc_bbox <- all_regions %>%
  st_buffer(dist = 50000) %>%
  st_bbox() %>%
  st_as_sfc()

# Smaller bounding box for plots
bc_bbox_small <- all_regions %>%
  st_buffer(dist = 25000) %>%
  st_bbox()

# X:Y ratio for plots
bc_ratio_small <- (bc_bbox_small$ymax - bc_bbox_small$ymin) /
  (bc_bbox_small$xmax - bc_bbox_small$xmin)

bc_coast <- ne_countries(
  scale = "large", returnclass = "sf", 
  country = c("Canada", "United States of America")
) %>%
  select(geometry) %>%
  st_transform(crs = st_crs(bc_bbox)) %>%
  st_crop(bc_bbox)

# Get land polygons
reg_coast <- st_read(dsn = file.path("..", "Data", "Polygons"),
                    layer = "GSHHS_h_L1_Alb", quiet = TRUE) %>%
  st_transform(crs = st_crs(all_shapes$sections)) %>%
  st_crop(y = reg_bbox)

# reg_coast <- bc_coast %>%
#   st_intersection(y = reg_bbox)

# Load median widths to correct surface spawns
barWidth <- load_width(where = widthLoc, a = areas)

# Load raw catch data, and some light wrangling
LoadCatchData <- function(where, area_table) {
  # This function loads the tree types of herring catch data, drops unnecessary
  # rows and columns, and combines the data frames for the region(s) in
  # question.
  # Progress message
  cat("Loading catch data... ")
  # Establish connection with access
  accessDB <- odbcConnectAccess(access.file = file.path(where$loc, where$db))
  # Access the tCatch worksheet
  tCatch <- sqlFetch(channel = accessDB, sqtable = where$fns$tCatch)
  # Error if data was not fetched
  if (class(tCatch) != "data.frame") {
    stop("No data available in MS Access connection")
  }
  # Wrangle areas
  areas <- area_table %>%
    tibble() %>%
    select(
      SAR, Region, RegionName, StatArea, Group, Section, LocationCode,
      LocationName
    ) %>%
    distinct()
  # Wrangle catch
  tCatch <- tCatch %>%
    mutate(
      Year = Season2Year(Season), Source = rep("Tab", times = n()), 
      Date=date(Date)
    ) %>%
    left_join(y = areas, by = "LocationCode") %>%
    filter(Section %in% areas$Section) %>%
    group_by(Year, Source, Section, GearCode, DisposalCode, Date) %>%
    summarise(Catch = SumNA(Catch)) %>%
    ungroup()
  # Access the hail worksheet
  hCatch <- sqlFetch(channel = accessDB, sqtable = where$fns$hCatch)
  # Error if data was not fetched
  if (class(hCatch) != "data.frame") {
    stop("No data available in MS Access connection")
  }
  # Wrangle catch
  hCatch <- hCatch %>%
    mutate(Section = formatC(Section, width=3, format="d", flag="0")) %>%
    filter(Active == 1, Section %in% areas$Section) %>%
    mutate(
      Year = Season2Year(Season), Catch = CatchTons * convFac$st2t,
      Source = rep("Hail", times = n()), Date = as.Date(NA)
    ) %>%
    group_by(Year, Source, Section, GearCode, DisposalCode, Date) %>%
    summarise(Catch = SumNA(Catch)) %>%
    ungroup()
  # Access the sok worksheet
  sokCatch <- sqlFetch(channel = accessDB, sqtable = where$fns$sokCatch)
  # Error if data was not fetched
  if (class(sokCatch) != "data.frame") {
    stop("No data available in MS Access connection")
  }
  # Wrangle sok
  sokCatch <- sokCatch %>%
    mutate(
      Year = Season2Year(Season), Source = rep("SOK", times = n()), 
      Date = as.Date(NA),
      Section = formatC(Section, width=3, format="d", flag="0")
    ) %>%
    rename(Catch = ProductLanded) %>%
    filter(Section %in% areas$Section) %>%
    group_by(Year, Source, Section, GearCode, DisposalCode, Date) %>%
    summarise(Catch = SumNA(Catch)) %>%
    ungroup()
  # Combine the three tables
  allCatch <- bind_rows(tCatch, hCatch, sokCatch)
  # Smaller subset of area information
  areasSm <- areas %>%
    select(SAR, Region, RegionName, StatArea, Group, Section) %>%
    distinct()
  # Merge with area information
  res <- allCatch %>%
    left_join(y = areasSm, by = "Section") %>%
    select(
      Year, Source, Region, StatArea, Section, GearCode, DisposalCode, Date,
      Catch
    ) %>%
    arrange(
      Year, Source, Region, StatArea, Section, GearCode, DisposalCode, Date
    )
  # Warning if more recent data is available
  if (max(res$Year, na.rm = TRUE) > max(yrRange)) {
    warning("Recent catch data exists; update 'yrRange' to include ",
            paste(unique(res$Year[which(res$Year > max(yrRange))]), collapse = ", "),
            call. = FALSE
    )
  }
  # Trim years outside the desired year range
  res <- res %>%
    filter(Year %in% yrRange)
  # Close the connection
  odbcClose(accessDB)
  # Update progress message
  cat("done\n")
  # Return the data
  return(res)
} # End LoadCatchData function

# Load raw catch data
catchRaw <- LoadCatchData(where = catchLoc, area_table = areas)

# Load biological data, and some light wrangling
LoadBioData <- function(where, XY) {
  # This function loads the herring biosample data: one table with general
  # sample information, and the other with fish measurements. Unnecessary
  # rows and columns are dropped. The output is a data frame of the two merged
  # tables for the region(s) in question.
  # Progress message
  cat("Loading biosample data... ")
  # Establish connection with access
  accessDB <- odbcConnectAccess(access.file = file.path(where$loc, where$db))
  # Access the sample worksheet
  sampleDat <- sqlFetch(channel = accessDB, sqtable = where$fns$samples)
  # Error if data was not fetched
  if (class(sampleDat) != "data.frame") {
    stop("No data available in MS Access connection")
  }
  # # Grab the spatial info and process
  # sampleSP <- sampleDat %>%
  #   transmute(
  #     X = ifelse(is.na(Set_Longitude), 0, Set_Longitude),
  #     Y = ifelse(is.na(Set_Latitude), 0, Set_Latitude)
  #   )
  # # Put X and Y into a spatial points object
  # sPts <- SpatialPoints(coords = sampleSP, proj4string = CRS(inCRS))
  # # Save the original points
  # sPtsOrig <- as_tibble(sPts) %>%
  #   rename(Longitude = X, Latitude = Y)
  # # Convert X and Y from WGS to Albers
  # sPtsAlb <- spTransform(x = sPts, CRSobj = CRS(outCRS))
  # # Extract spatial info
  # dfAlb <- as_tibble(sPtsAlb)
  # Extract relevant sample data
  samples <- sampleDat %>%
    # cbind(dfAlb) %>%
    # cbind(sPtsOrig) %>%
    rename(
      LocationCode = loc_code, Longitude = Set_Longitude,
      Latitude = Set_Latitude, Sample = isamp, Month = month,
      Representative = Representative_Set, SourceCode = source_code,
      GearCode = gear_code
    ) %>%
    mutate(
      Year = Season2Year(season)
      # Eastings = ifelse(is.na(Set_Longitude), Set_Longitude, X),
      # Northings = ifelse(is.na(Set_Latitude), Set_Latitude, Y)
    ) %>%
    select(
      Year, Month, Sample, Representative, LocationCode, Longitude, Latitude,
      SourceCode, GearCode
    ) %>%
    as_tibble()
  # Access the fish worksheet
  fish <- sqlFetch(channel = accessDB, sqtable = where$fns$fish)
  # Error if data was not fetched
  if (class(fish) != "data.frame") {
    stop("No data available in MS Access connection")
  }
  # Wrangle biosamples
  fish <- fish %>%
    rename(
      Sample = isamp, Fish = fish, Length = len, Weight = wgt, Sex = sex_alpha,
      MaturityCode = mat_code, DualAge = dual_age, GonadLength = gonad_len,
      GonadWeight = gonad_wgt
    ) %>%
    mutate(
      Year = Season2Year(season),
      Age = ifelse(age <= max(ageRange), age, max(ageRange))
    ) %>%
    filter(Age >= min(ageRange)) %>%
    select(
      Year, Sample, Fish, Length, Weight, Sex, MaturityCode, Age, DualAge,
      GonadLength, GonadWeight
    ) %>%
    as_tibble()
  # Combine the two tables (note that Sample re-starts at 1 each Year)
  fishSamples <- full_join(x = samples, y = fish, by = c("Year", "Sample"))
  # More wrangling: filter to region(s)
  raw <- fishSamples %>%
    filter(LocationCode %in% areas$LocationCode) %>%
    left_join(y = areas, by = "LocationCode") %>%
    # mutate(
      # Eastings = ifelse(is.na(Eastings.x), Eastings.y, Eastings.x),
      # Northings = ifelse(is.na(Northings.x), Northings.y, Northings.x),
      # Longitude = ifelse(Longitude.x == 0, Longitude.y, Longitude.x),
      # Latitude = ifelse(Latitude.x == 0, Latitude.y, Latitude.x)
    # ) %>%
    select(
      Year, Month, Region, StatArea, Group, Section, LocationCode,
      LocationName, Longitude, Latitude, Sample, Representative, SourceCode,
      GearCode, Fish, Length, Weight, Sex, MaturityCode, Age, DualAge,
      GonadLength, GonadWeight
    ) %>%
    arrange(
      Year, Month, Region, StatArea, Group, Section, LocationCode, Sample, Fish
    )
  # # Clip the extent
  # df <- ClipExtent(
  #   dat = raw, spObj = shapes$regSPDF, bufDist = maxBuff, silent = TRUE
  # )
  # # Subset data with 'good' X and Y
  # dfNotNA <- df %>%
  #   filter(!is.na(Eastings) & !is.na(Northings))
  # # Subset data with 'bad' X or Y, and try to fill in using transect X and Y
  # dfNA <- df %>%
  #   filter(is.na(Eastings) | is.na(Northings)) %>%
  #   select(-Eastings, -Northings) %>%
  #   left_join(y = XY, by = "LocationCode")
  # # Re-combine the two subsets
  # df2 <- bind_rows(dfNotNA, dfNA)
  # # Clip the extent (again)
  # res <- ClipExtent(
  #   dat = df2, spObj = shapes$regSPDF, bufDist = maxBuff, silent = TRUE
  # )
  res <- raw %>%
    left_join(y = XY, by = "LocationCode") %>%
    mutate(
      Longitude = ifelse(is.na(Longitude.x), Longitude.y, Longitude.x),
      Latitude = ifelse(is.na(Latitude.x), Latitude.y, Latitude.x) ) %>%
    select(-Longitude.x, -Longitude.y, -Latitude.x, -Latitude.y) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), na.fail = FALSE)
  # Get locations with missing X or Y
  noXY <- res %>%
    filter(is.na(geometry)) %>%
    select(Region, StatArea, Group, Section, LocationCode, LocationName) %>%
    distinct()
  # Message re missing X and Y, if any
  if (nrow(noXY) >= 1) {
    warning("There are ", nrow(noXY),
            " biological sample location(s) with missing or incorrect spatial info",
            call. = FALSE
    )
  }
  # Stop if we're missing rows
  if (nrow(raw) != nrow(res)) stop("Missing rows!", call. = FALSE)
  # Warning if more recent data is available
  if (max(res$Year, na.rm = TRUE) > max(yrRange)) {
    warning("Recent biological data exists; update 'yrRange' to include ",
            paste(unique(res$Year[which(res$Year > max(yrRange))]), collapse = ", "),
            call. = FALSE
    )
  }
  # Trim years outside the desired year range
  res <- res %>%
    filter(Year %in% yrRange)
  # Close the connection
  odbcClose(accessDB)
  # Update progress message
  cat("done\n")
  # Return the data
  return(res)
} # End LoadBioData function

# Load raw biological data
bioRaw <- LoadBioData(where = bioLoc, XY = transectXY)

# Load spawn data, and some light wrangling
LoadSpawnData <- function(whereSurf, whereMacro, whereUnder, XY) {
  # This function loads the herring spawn data, and drops unnecessary rows and
  # columns. The output is a data frame for the region(s) in question.
  # Progress message
  cat("Calculating spawn index:\n")
  # Fecundity conversion factor
  ECF <<- eggs_to_sb()
  # Progress message
  cat("\tsurface...\n")
  # Access and calculate surface spawn
  surface <- calc_surf_index(
    where = whereSurf, areas = areas, widths = barWidth, years = yrRange,
    quiet = TRUE
  )
  # Progress message
  cat("\tmacrocystis...\n")
  # Access and calculate macrocystis spawn
  macrocystis <- calc_macro_index(
    where = whereMacro, areas = areas, years = yrRange, quiet = TRUE
  )
  # Progress message
  cat("\tunderstory...\n")
  # Access and calculate understory spawn
  understory <- calc_under_index(
    where = whereUnder, areas = areas, years = yrRange, quiet = TRUE
  )
  # Update progress message
  cat("\ttotal... ")
  # Load the all spawn data
  allSpawn <- load_all_spawn(
    where = allLoc, areas = areas, years = yrRange, ft2m = convFac$ft2m
  )
  # Combine the spawn types (by spawn number)
  raw <- surface$biomass_spawn %>%
    full_join(y = macrocystis$biomass_spawn, by = c(
      "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber"
    )) %>%
    # TODO: Look into why this Width is different from the allSpawn$Width
    select(-Width) %>%
    full_join(y = understory$biomass_spawn, by = c(
      "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber"
    )) %>%
    full_join(y = allSpawn, by = c(
      "Year", "Region", "StatArea", "Section", "LocationCode", "SpawnNumber"
    )) %>%
    select(
      Year, Region, StatArea, Group, Section, LocationCode, LocationName,
      SpawnNumber, geometry, Start, End, Length, Width, Depth, Method,
      SurfLyrs, SurfSI, MacroLyrs, MacroSI, UnderLyrs, UnderSI
    ) %>%
    mutate(
      Year = as.integer(Year), StartDOY = yday(Start),
      EndDOY = yday(End), Decade = paste(Year %/% 10 * 10, "s", sep = "")
    ) %>%
    arrange(
      Year, Region, StatArea, Section, LocationCode, SpawnNumber, Start, End
    ) %>%
    st_as_sf()
  
  coords_raw <- st_coordinates(raw) %>%
    as_tibble() %>%
    rename(Longitude = X, Latitude = Y) %>%
    mutate(LocationCode = raw$LocationCode)
  res <- coords_raw %>%
    left_join(y = XY, by = "LocationCode") %>%
    mutate(
      Longitude = ifelse(is.na(Longitude.x), Longitude.y, Longitude.x),
      Latitude = ifelse(is.na(Latitude.x), Latitude.y, Latitude.x) ) %>%
    select(-Longitude.x, -Longitude.y, -Latitude.x, -Latitude.y) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), na.fail = FALSE)
  
  raw$geometry <- res$geometry
  
  # # Clip the extent
  # df <- ClipExtent(
  #   dat = raw, spObj = shapes$regSPDF, bufDist = maxBuff, silent = TRUE
  # )
  # # Subset data with 'good' X and Y
  # dfNotNA <- df %>%
  #   filter(!is.na(Eastings) & !is.na(Northings))
  # # Subset data with 'bad' X or Y, and replace using transect X and Y
  # dfNA <- df %>%
  #   filter(is.na(Eastings) | is.na(Northings)) %>%
  #   select(-Eastings, -Northings) %>%
  #   left_join(y = XY, by = "LocationCode")
  # # Re-combine the two subsets
  # df2 <- bind_rows(dfNotNA, dfNA)
  # # Clip the extent (again)
  # res <- ClipExtent(
  #   dat = df2, spObj = shapes$regSPDF, bufDist = maxBuff, silent = TRUE
  # )
  # Get locations with missing X or Y
  noXY <- raw %>%
    filter(is.na(geometry)) %>%
    select(Region, StatArea, Group, Section, LocationCode, LocationName) %>%
    distinct()
  # Message re missing X and Y, if any
  if (nrow(noXY) >= 1) {
    warning("There are ", nrow(noXY),
            " spawn location(s) with missing or incorrect spatial info",
            call. = FALSE
    )
  }
  # Stop if we're missing rows
  if (nrow(raw) != nrow(res)) stop("Missing rows!", call. = FALSE)
  # Warning if more recent data is available
  if (max(res$Year, na.rm = TRUE) > max(yrRange)) {
    warning("Recent spawn data exists; update 'yrRange' to include ",
            paste(unique(res$Year[which(res$Year > max(yrRange))]), collapse = ", "),
            call. = FALSE
    )
  }
  # Add a column to indicate the survey period
  raw <- raw %>%
    mutate(
      Survey = ifelse(Year < pars$years$dive, "Surface", "Dive"),
      Survey = factor(Survey, levels = c("Surface", "Dive"))
    ) %>%
    filter(Year %in% yrRange)
  # Update the progress message
  cat("done\n")
  # Return the data
  return(raw)
} # End LoadSpawnData function

# Load spawn data
spawnRaw <- LoadSpawnData(
  whereSurf = surfLoc, whereMacro = macroLoc, whereUnder = underLoc,
  XY = transectXY
)

# Load incidental catch
LoadIncidentalCatch <- function(file, a = areas) {
  # Months that get included in the next year
  next_yr <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  # Subset area data
  a_sm <- a %>%
    select(Region, StatArea) %>%
    unique()
  # Grab incidental catch and wrangle
  ic <- read_excel(file.path(file$loc, file$fn), sheet = file$sheets$ic) %>%
    mutate(Source = "IC") %>%
    select(
      Source, PFMA, `Calendar Year`, `Calendar Month`, `Released Count`,
      `Mortalities Count`
    ) %>%
    rename(
      Year = `Calendar Year`, Month = `Calendar Month`,
      Released = `Released Count`, Dead = `Mortalities Count`
    ) %>%
    mutate(Month = str_to_sentence(Month))
  # Grab wild mortalities and wrangle
  wm <- read_excel(file.path(file$loc, file$fn), sheet = file$sheet$wm) %>%
    mutate(Source = "WM") %>%
    select(
      Source, PFMA, `Catch Year`, `Catch Month`, `Released Count`,
      `Mortalities Count`
    ) %>%
    rename(
      Year = `Catch Year`, Month = `Catch Month`,
      Released = `Released Count`, Dead = `Mortalities Count`
    ) %>%
    mutate(Month = str_trunc(Month, width = 3, ellipsis = ""))
  # Combine the two data sets and wrangle
  res <- bind_rows(ic, wm) %>%
    mutate(
      Year = ifelse(Month %in% next_yr, Year + 1, Year),
      StatArea = formatC(PFMA, width = 2, flag = "0")
    ) %>%
    filter(StatArea %in% a_sm$StatArea) %>%
    left_join(y = a_sm, by="StatArea") %>%
    replace_na(replace = list(Released = 0, Dead = 0)) %>%
    group_by(Region, Year) %>%
    summarise(Released = sum(Released), Dead = sum(Dead)) %>%
    ungroup() %>%
    pivot_longer(
      cols = c(Released, Dead), names_to = "Type", values_to = "Number"
    )
  # Return the data
  res
} # End LoadIncidentalCatch function

# Load incidental catch
incidental <- LoadIncidentalCatch(file = icLoc)

# # For Lynn Lee (HG and A2W)
# spawnRaw %>%
#   # filter( Year > 2015 ) %>%
#   select( Year, Region, StatArea, Section, LocationCode, LocationName,
#           SpawnNumber, Longitude, Latitude, Length, Width, Start, End, Method,
#           Survey, SurfSI, MacroSI, UnderSI ) %>%
#   arrange( Region, Year, StatArea, Section, LocationCode, SpawnNumber ) %>%
#   write_csv( file = paste(regName, "csv", sep=".") )

# # For Kristen, CC spawn
# spawnRaw %>%
#   select( Year, Region, StatArea, Section, LocationCode, LocationName,
#     SpawnNumber, Eastings, Northings, Longitude, Latitude, SurfSI, MacroSI,
#     UnderSI ) %>%
#   filter( Year>=2014 ) %>%
#   arrange(Year, Region, StatArea, Section, LocationCode ) %>%
#   write_csv( file="SpawnCC.csv" )

# HG 1972-2006 (D,G): roe seine (7,29), roe gillnet (7,19), test seine (8,29),
# and test gillnet (8,19)
# dispGear <- catchRaw %>%
#   filter(
#     Year %in% 1972:2006, DisposalCode %in% c(7, 8), GearCode %in% c(19, 29)
#   ) %>%
#   mutate(
#     Disposal = ifelse(DisposalCode == 7, "Roe", "Test"),
#     Gear = ifelse(GearCode == 19, "Gillnet", "Seine")
#   ) %>%
#   group_by(Year, Disposal, Gear) %>%
#   summarise(Catch = SumNA(Catch)) %>%
#   ungroup() %>%
#   expand( Year=1972:2006 ) %>%
#   pivot_wider(names_from = c(Disposal, Gear), values_from = Catch,
#               values_fill=list(Catch=0)) %>%
#   write_csv(file="DisposalGear.csv")

##### Update #####

# Update catch data (more wrangling)
UpdateCatchData <- function(dat, a) {
  # TODO: This is some pretty ugly wrangling, probably carried over from the
  # way it was done in Access -- should be cleaned up
  # Get a short list of area information
  areasSm <- a %>%
    tibble() %>%
    select(Region, StatArea, Group, Section) %>%
    distinct()
  # Wrangle catch data
  WrangleCatch <- function(df) {
    # This function combines the three types of herring catch information into
    # the three 'gear' types, which are representative of the three main periods
    # of herring catch. The output is a data frame with total catch (scaled as
    # specified) by year and gear type (i.e., period).
    # Update tCatch for Period 1
    tc1 <- df %>%
      filter(Source == "Tab", DisposalCode %in% c(1, 3, 4, 5, 6)) %>%
      select(Year, Catch, Date)
    # Update hCatch for Period 1
    hc1 <- df %>%
      filter(Source == "Hail", DisposalCode %in% c(3, 6)) %>%
      select(Year, Catch, Date)
    # Combine catches for period 1
    dat1 <- bind_rows(tc1, hc1)
    # Period 1 catch
    pd1 <- dat1 %>%
      group_by(Year, Date) %>%
      summarise(Gear1 = SumNA(Catch)) %>%
      ungroup()
    # Updated tCatch for Period 2
    tc2 <- df %>%
      filter(Source == "Tab", GearCode == 29)
    # If including test fishery catch
    if (inclTestCatch) {
      # Include
      tc2 <- tc2 %>% filter(DisposalCode %in% c(7, 8))
    } else { # End if including, otherwise
      # Exclude
      tc2 <- tc2 %>% filter(DisposalCode == 7)
    } # End if exclude
    # Period 2 catch
    pd2 <- tc2 %>%
      group_by(Year, Date) %>%
      summarise(Gear2 = SumNA(Catch)) %>%
      ungroup()
    # Updated tCatch for Period 3
    tc3 <- df %>%
      filter(Source == "Tab", GearCode == 19)
    # If including test fishery catch
    if (inclTestCatch) {
      # Include
      tc3 <- tc3 %>% filter(DisposalCode %in% c(7, 8))
    } else { # End if include, otherwise
      # Exclude
      tc3 <- tc3 %>% filter(DisposalCode == 7)
    } # End if exclude
    # Period 3 catch
    pd3 <- tc3 %>%
      group_by(Year, Date) %>%
      summarise(Gear3 = SumNA(Catch)) %>%
      ungroup()
    # Combine the data frames
    dfList <- list(pd1, pd2, pd3)
    # Merge the tables and wrangle
    pd123 <- Reduce(function(...) merge(..., all = TRUE), dfList) %>%
      complete(Year = yrRange, fill = list(Gear1 = 0, Gear2 = 0, Gear3 = 0))
    # Select years of interest and arrange by year
    res <- pd123 %>%
      filter(Year %in% yrRange) %>%
      gather(key = Period, value = Catch, Gear1, Gear2, Gear3) %>%
      arrange(Period, Year, Date) %>%
      select(Period, Year, Date, Catch) %>%
      as_tibble()
    # Return catch by year and period
    return(res)
  } # End WrangleCatch function
  # Start a list to hold dataframes
  rList <- list()
  # Get the unique sections
  uSection <- unique(dat$Section)
  # Loop over sections
  for (i in 1:length(uSection)) {
    # Get the ith section
    iSec <- uSection[i]
    # Get the catch
    rList[[i]] <- catchRaw %>%
      filter(Section == iSec) %>%
      WrangleCatch() %>%
      group_by(Period, Year, Date) %>%
      summarise(Catch = SumNA(Catch)) %>%
      ungroup() %>%
      # mutate( Catch=ifelse(Catch == 0, NA, Catch) ) %>%
      mutate(Section = iSec) %>%
      # filter( !is.na(Catch) )
      filter(Catch > 0) %>%
      select(Period, Year, Date, Section, Catch)
  } # End i loop over sections
  # Combine the data frames and add gear types
  res <- bind_rows(rList) %>%
    left_join(y = areasSm, by = "Section") %>%
    select(Period, Year, Region, StatArea, Group, Section, Date, Catch) %>%
    arrange(Period, Year, Region, StatArea, Group, Section, Date) %>%
    left_join(y = tPeriod, by = "Period") %>%
    mutate(Gear = factor(Gear, levels = tPeriod$Gear))
  # Return the results
  return(res)
} # End UpdateCatchData function

# Update catch data
catch <- UpdateCatchData(dat = catchRaw, a = areas)

# Summarise food and bait
catchFB <- catchRaw %>%
  # Disposal code 3 is food and bait; 6 is special use
  filter(DisposalCode %in% c(3, 6)) %>%
  group_by(Year, Section) %>%
  summarise(Catch = sum(Catch, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(Year = yrRange, fill = list(Section = NA, Catch = NA)) %>%
  arrange(Year, Section)

# Calculate commercial SoK harvest and biomass
harvestSOK <- catchRaw %>%
  filter(DisposalCode == 2, Source == "SOK") %>%
  group_by(Year) %>%
  summarise(Harvest = SumNA(Catch)) %>%
  ungroup() %>%
  # Covert harvest (lb) to spawning biomass (t)
  mutate(Biomass = calc_sok_index(sok = Harvest * convFac$lb2kg)) %>%
  complete(Year = yrRange, fill = list(Harvest = 0, Biomass = 0)) %>%
  arrange(Year)

# Table that doesn't get pruned for privacy, recent years, etc (for the res doc)
allHarvSOK <- harvestSOK %>%
  mutate(Region = regName, Harvest = Harvest * convFac$lb2kg)

# # For Landmark MSE
# catchDate <- catch %>%
#   mutate(YDay = yday(Date), Date = as.Date(YDay, origin="2020-01-01"))
# noDate <- catch %>%
#   filter(is.na(Date)) %>%
#   group_by(Gear)%>%
#   summarise(Catch=SumNA(Catch)) %>%
#   ungroup() %>%
#   mutate(
#     Catch = format(Catch, big.mark = ",", digits = 0, scientific = FALSE)
#   ) %>%
#   filter(!is.na(Gear))
# catchDatePlot <- ggplot(data = catchDate, mapping = aes(x=Date, y=Catch)) +
#   geom_bar(stat = "identity", position = "stack", width = 1) +
#   scale_x_date(date_labels = "%b", date_breaks = "1 month") +
#   scale_y_continuous(labels = comma) + 
#   facet_grid(Gear ~ ., scales = "free_y") +
#   labs(y = "Catch (t)", title = regName)
# if (nrow(noDate) >= 1)
#   catchDatePlot <- catchDatePlot + 
#   geom_text(
#     data = noDate %>%filter(!is.na(Gear)),
#     mapping = aes(x = as.Date(180, origin="2020-01-01"), y = Inf, 
#                   label = paste("Catch with no date info:", Catch, "t"),
#                   vjust = 1)
#   )
# catchDatePlot <- catchDatePlot +
#   ggsave(filename = paste("CatchDate", regName, "png", sep="."), height = 6,
#          width = 6)

# Update biological data (more wrangling)
UpdateBioData <- function(dat, rYr) {
  # This function determines the three 'gear' types, which are representative
  # of the three main periods of herring biological data. The output is a data
  # frame with a new column indicating the period (i.e., gear type), and a new
  # column indicating the weights (to be used in special cases only).
  # Get data for period 1
  pd1 <- dat %>%
    filter(GearCode == 29, SourceCode %in% c(1, 6, 7)) %>%
    mutate(Period = rep(1, times = n()))
  # Get data for period 2
  pd2 <- dat %>%
    filter(GearCode == 29) %>%
    mutate(Period = rep(2, times = n()))
  # If including test seine biological data
  if (inclTestSNBio) {
    # Include
    pd2 <- filter(.data = pd2, SourceCode %in% c(0, 5))
  } else { # End if include, otherwise
    # Exclude
    pd2 <- filter(.data = pd2, SourceCode == 0)
  } # End if exclude
  # If including sok biological data
  if (inclSOKBio) {
    # Get SOK data
    SOK <- dat %>%
      filter(GearCode == 29, SourceCode == 4, Month %in% c(3, 4)) %>%
      mutate(Period = rep(2, times = n()))
    # Combine with non-SOK dat
    pd2 <- bind_rows(pd2, SOK)
  } # End if including SOK data
  # Get data for period 3
  pd3 <- dat %>%
    filter(GearCode == 19) %>%
    mutate(Period = rep(3, times = n()))
  # If including test gillnet biological data
  if (inclTestGNBio) {
    # Include
    pd3 <- filter(.data = pd3, SourceCode %in% c(0, 5))
  } else { # End if include, otherwise
    # Exclude
    pd3 <- filter(.data = pd3, SourceCode == 0)
  } # End if exclude
  # Combine the three tables
  p123 <- bind_rows(pd1, pd2, pd3)
  # Warning re representative samples
  warning("Biosamples: keep all samples from ", min(yrRange), " to ", rYr - 1,
          ", and 'representative' samples from ", rYr, " to ", max(yrRange),
          sep = "",
          call. = FALSE
  )
  # Include only representative samples (ish)
  res <- p123 %>%
    filter(Year < rYr | Representative == 1) %>%
    filter(Year %in% yrRange) %>%
    select(-Representative) %>%
    mutate(SampWt = 1)
  # Return the data
  return(res)
} # End UpdateBioData function

# Update biological data
bio <- UpdateBioData(dat = bioRaw, rYr = 2014)

##### Summaries #####

# Spawn summary
spawnSummary <- spawnRaw %>%
  group_by(Year) %>%
  summarise(
    Surf = SumNA(SurfSI), Macro = SumNA(MacroSI), Under = SumNA(UnderSI)
  ) %>%
  ungroup() %>%
  mutate(
    Surf = formatC(as.numeric(Surf), digits = 3, format = "f"),
    Macro = formatC(as.numeric(Macro), digits = 3, format = "f"),
    Under = formatC(as.numeric(Under), digits = 3, format = "f")
  ) %>%
  complete(Year = yrRange) %>%
  arrange(Year) %>%
  select(Year, Surf, Macro, Under) %>%
  write_csv(
    file = file.path("Summaries", paste("Spawn", region, ".csv", sep = ""))
  )

# Catch summary
catchSummary <- catch %>%
  select(Year, Catch, Gear) %>%
  group_by(Year, Gear) %>%
  summarise(Catch = SumNA(Catch)) %>%
  ungroup() %>%
  complete(
    Year = yrRange, Gear = c("RoeSN", "RoeGN", "Other"),
    fill = list(Catch = 0)
  ) %>%
  pivot_wider(names_from = Gear, values_from = Catch, values_fill = 0) %>%
  full_join(y = select(allHarvSOK, Year, Harvest), by = "Year") %>%
  mutate(
    RoeSN = formatC(RoeSN, digits = 3, format = "f"),
    RoeGN = formatC(RoeGN, digits = 3, format = "f"),
    Other = formatC(Other, digits = 3, format = "f"),
    SOK = formatC(Harvest / 1000, digits = 3, format = "f")
    ) %>%
  arrange(Year) %>%
  select(Year, Other, RoeSN, RoeGN, SOK) %>%
  mutate(SOK = as.numeric(SOK)) %>%
  replace_na(replace = list(SOK = 0)) %>%
  write_csv(
    file = file.path("Summaries", paste("Catch", region, ".csv", sep = ""))
  )

# Biosample summary
bioSummary <- bio %>%
  group_by(Year) %>%
  summarise(
    Sample = n_distinct(na.omit(Sample)),
    Length = length(na.omit(Length)),
    Weight = length(na.omit(Weight)),
    Age = length(na.omit(Age))
  ) %>%
  ungroup() %>%
  complete(
    Year = yrRange, fill = list(Sample = 0, Length = 0, Weight = 0, Age = 0)
  ) %>%
  arrange(Year) %>%
  select(Year, Sample, Length, Weight, Age) %>%
  write_csv(
    file = file.path("Summaries", paste("Bio", region, ".csv", sep = ""))
  )

##### Overlay #####

# # Check area data for inconsistent spatial overlays
# overAreas <- CheckSpatialOverlay(
#   pts = areas, shape = shapes$secAllSPDF, type = "Location"
# )
# 
# # Check spawn data for inconsistent spatial overlays
# overSpawn <- CheckSpatialOverlay(
#   pts = spawnRaw, shape = shapes$secAllSPDF, type = "Spawn"
# )
# 
# # Check biosample data for inconsistent spatial overlays
# overBio <- CheckSpatialOverlay(
#   pts = bioRaw, shape = shapes$secAllSPDF, type = "Biosample"
# )

##### Main #####

# WCVI data
# catch %>% 
#   group_by(Year, Gear) %>% 
#   summarise(Catch = sum(Catch)) %>% 
#   ungroup() %>% 
#   write_csv(file = paste0("Catch", regName, ".csv"))

# Calculate commercial catch in current year
catchCommUseYr <- catch %>%
  complete(Year = yrRange, Gear, fill = list(Catch = 0)) %>%
  filter(Year == max(yrRange)) %>%
  group_by(Gear) %>%
  summarise(Catch = SumNA(Catch)) %>%
  ungroup()

# WCVI data
# allHarvSOK %>%
#   select(Year, Harvest) %>% 
#   write_csv(file = paste0("SOK", regName, ".csv"))

# Count the number of biological samples per year
CountBiosamplesYear <- function(dat) {
  # This function counts the number of biosamples collected in the current year
  # and the previous few years from commercial vessels as well as test/
  # research vessels. It returns a data frame that includes the total number of
  # samples collected each year.
  # Count the number of samples by year and gear: commercial fisheries
  numComm <- dat %>%
    filter(Year >= firstYrTab, !SourceCode %in% c(2, 3, 5)) %>%
    select(Year, Sample) %>%
    as_tibble() %>%
    group_by(Year) %>%
    summarise(Commercial = n_distinct(Sample)) %>%
    ungroup()
  # Count the number of samples by year and gear: test and research
  numTest <- dat %>%
    filter(Year >= firstYrTab, SourceCode %in% c(3, 5)) %>%
    select(Year, Sample) %>%
    as_tibble() %>%
    group_by(Year) %>%
    summarise(Test = n_distinct(Sample)) %>%
    ungroup()
  # Count the number of samples by year and gear: nearshore
  numNear <- dat %>%
    filter(Year >= firstYrTab, SourceCode %in% c(2)) %>%
    select(Year, Sample) %>%
    as_tibble() %>%
    group_by(Year) %>%
    summarise(Nearshore = n_distinct(Sample)) %>%
    ungroup()
  # Merge the two tables and wrangle
  res <- full_join(x = numComm, y = numTest, by = "Year") %>%
    full_join(y = numNear, by = "Year") %>%
    complete(
      Year = firstYrTab:max(yrRange),
      fill = list(Commercial = 0, Test = 0, Nearshore = 0)
    ) %>%
    mutate(Total = as.integer(Commercial + Test + Nearshore)) %>%
    arrange(Year)
  # If there are no rows
  if (nrow(res) == 0) {
    # Ensure there are 4 columns
    if (ncol(res) != 4) warning("Result must have 4 columns", call. = FALSE)
    # Add a dummy row
    res[1, ] <- c(NA, as.integer(0), as.integer(0), as.integer(0))
  } # End if there are no rows
  # Return the table
  return(res)
} # End CountBiosamplesYear function

# Count the number of biological samples per year
# TODO: Update this to be only 'Representative' samples (i.e., dat=bio?)
bioNum <- CountBiosamplesYear(dat = bioRaw)

# Get the total number of biosamples (all years)
numBiosamples <- bioRaw %>%
  group_by(Year) %>%
  summarise(Total = n_distinct(Sample)) %>%
  ungroup() %>%
  mutate(Region = region)

# Determine number of biosample by type in current year
GetSampleNumType <- function(dat) {
  # This function determines biosample type by gear (fishery) and use (source)
  # for the current year, and returns a data frame.
  # Get catch in current year by gear and disposal
  samp <- dat %>%
    filter(Year == max(yrRange)) %>%
    group_by(SourceCode, GearCode) %>%
    summarise(Number = n_distinct(Sample)) %>%
    ungroup() %>%
    mutate(Type = ifelse(SourceCode %in% c(2, 3, 5), "Test", "Commercial"))
  # Combine with gear names
  sampGear <- left_join(
    x = samp, y = select(.data = tGear, Gear, GearCode), by = "GearCode"
  )
  # Combine with disposal names
  sampGearSource <- left_join(x = sampGear, y = tSource, by = "SourceCode")
  # A bit more wrangling
  res <- sampGearSource %>%
    rename(Use = SampleSource) %>%
    select(Type, Gear, Use, Number) %>%
    arrange(Type, Gear, Use)
  # Update for SOK samples: Gear==Seine and Use==Other
  # TODO: Make sure this is always true (i.e., are the instances when Use should
  # stay 'Other' when Gear is Seine?)
  res <- res %>%
    mutate(Use = ifelse(Gear == "Seine" & Use == "Other", "SOK", Use))
  # Replace NA with 0
  res[is.na(res)] <- 0
  # If there are no rows
  if (nrow(res) == 0) {
    # Ensure there are 3 columns
    if (ncol(res) != 4) warning("Result must have 4 columns", call. = FALSE)
    # Add a dummy row
    res[1, ] <- list(NA, NA, NA, as.integer(0))
  } # End if there are no rows
  # Warning if number of biosamples don't match
  if (bioNum$Total[bioNum$Year == max(yrRange)] != sum(res$Number)) {
    warning("Number of biosamples differ in most recent year", call. = FALSE)
  }
  # Return the catch
  return(res)
} # End GetSampleNumType function

# Determine biosample types in current year
# TODO: Update this to be only 'Representative' samples (i.e., dat=bio?)
bioTypeNum <- GetSampleNumType(dat = bioRaw)

# If region is Central Coast
if (region == "CC") {
  # Ratio of number of biological samples between groups
  propNumBioHist <- bio %>%
    tibble() %>%
    filter(GearCode == 29, Year %in% yrsRatioHist) %>%
    group_by(Year, Group) %>%
    summarise(Number = n_distinct(Sample)) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    group_by(Group) %>%
    summarise(SampWt = MeanNA(Proportion)) %>%
    ungroup()
  # Merge weights in the main bio table (i.e., to fix unbalanced sampling among
  # groups in identified years)
  bio <- bio %>%
    tibble() %>%
    select(-SampWt) %>%
    left_join(y = propNumBioHist, by = "Group") %>%
    mutate(SampWt = ifelse(Year %in% yrsRatioFix & Period == 2, SampWt, 1))
} # End if region is Central Coast

# Count the number of fish aged by year and gear (and as a proportion): use the
# 'SampWt' column to fix unrepresentative sampling if identified
numAgedYearGear <- bio %>%
  select(Period, Year, Age, SampWt) %>%
  na.omit() %>%
  group_by(Period, Year, Age) %>%
  summarise(Number = SumNA(SampWt)) %>%
  mutate(Proportion = Number / SumNA(Number)) %>%
  ungroup() %>%
  arrange(Period, Year, Age)

# Count the number of fish aged by year (and as a proportion) by seine gear:
# use the 'SampWt' column to fix unrepresentative sampling if identified
numAgedYear <- bio %>%
  filter(GearCode == 29) %>% # %in% c(19, 29) (originally == 29)
  select(Year, Age, SampWt) %>%
  na.omit() %>%
  group_by(Year, Age) %>%
  summarise(Number = SumNA(SampWt)) %>%
  mutate(Proportion = Number / SumNA(Number)) %>%
  ungroup() %>%
  arrange(Year, Age)

# # Cleary Jan 11
# yrs <- (max(yrRange)-4) : max(yrRange)
# 
# npaJC <- bio %>%
#   filter(GearCode %in% c(19, 29), SourceCode == 0, Year %in% yrs) %>%
#   left_join(y = tGear, by="GearCode") %>%
#   select(Gear, Age, SampWt) %>%
#   na.omit() %>%
#   group_by(Gear, Age) %>%
#   summarise(Number = SumNA(SampWt)) %>%
#   mutate(Proportion = Number / SumNA(Number)) %>%
#   ungroup() %>%
#   arrange(Age) %>%
#   write_csv(file = paste0("PropAgeSoGRoe", min(yrs), ".csv"))
# 
# p <- ggplot(data=npaJC, mapping = aes(x = Age, y = Proportion, fill = Gear)) +
#   geom_col(position = position_dodge()) +
#   scale_fill_viridis_d() +
#   scale_x_continuous(breaks = ageRange, labels = ageRange) +
#   labs(title = paste0("Roe catch (", paste(unique(range(yrs)), collapse = " to "),
#                       ")")) +
#   theme(legend.position = "top") +
#   ggsave(filename = paste0("PropAgeSoGRoe", min(yrs), ".png"),
#          width = 6, height = 4)

# Reshape and format proportion-at-age
FormatPropAtAge <- function(dat) {
  # This function reshapes the proportion-at-age data from long to wide,
  # replaces NAs with zeros, ensures that all years are present, and returns a
  # data frame
  # Make a table and reshape to wide: proportion-at-age
  wide <- dat %>%
    select(Year, Age, Proportion) %>%
    spread(key = Age, value = Proportion, drop = FALSE)
  # Replace NAs with 0
  wide[is.na(wide)] <- 0
  # Fill in missing years
  res <- wide %>%
    filter(Year >= firstYrTab) %>%
    full_join(y = tibble(Year = firstYrTab:max(yrRange)), by = c("Year")) %>%
    arrange(Year)
  # Return the output
  return(res)
} # End FormatPropAtAge function

# Format proportion-at-aget dc
propAgedYearTab <- FormatPropAtAge(dat = numAgedYear)

# Determine weighted mean and approximate CI age by year
qAgedYear <- numAgedYear %>%
  select(Year, Age, Proportion) %>%
  group_by(Year) %>%
  summarise(
    MeanAge = weighted.mean(x = Age, w = Proportion),
    # CI is based on R code by Steve Martel
    sBar = qnorm(1 - (1 - ciLevel) / 2) *
      sum(sqrt(Proportion * (1 - Proportion)) / sqrt(Age)),
    Lower = exp(log(MeanAge) - log(sBar)),
    Upper = exp(log(MeanAge) + log(sBar))
  ) %>%
  ungroup() %>%
  mutate(GroupID = ConsecutiveGroup(Year)) %>%
  arrange(Year)

# Calculate mean weight-at-age by year
CalcWeightAtAge <- function(dat) {
  # This function calculates mean weight-at-age by year, and fills in missing
  # data (i.e., NAs) using a suitable technique. Calculate the weighted mean
  # using the 'SampWt' column to fix unrepresentative sampling if identified
  # Calculate mean weight-at-age
  wtAge <- dat %>%
    tibble() %>%
    filter(GearCode == 29) %>%
    select(Year, Age, Weight, SampWt) %>%
    na.omit() %>%
    group_by(Year, Age) %>%
    summarise(MeanWeight = WtMeanNA(x = Weight, w = SampWt)) %>%
    ungroup() %>%
    complete(Year = yrRange, Age = ageRange) %>%
    arrange(Year, Age)
  # Reshape from long to wide and merge with complete year sequence
  wtAgeW <- wtAge %>%
    pivot_wider(names_from = Age, values_from = MeanWeight) %>%
    arrange(Year)
  # Reshape from wide to long, and fill in NAs
  wtAgeL <- wtAgeW %>%
    pivot_longer(cols = !Year, names_to = "Age", values_to = "Weight",
                 names_transform = as.integer) %>%
    group_by(Age) %>%
    # Replace NAs: mean of (up to) previous n years
    mutate(Weight = RollMeanNA(Weight, n = nRoll)) %>%
    # Replace persistent NAs (i.e., at the beginning of the time series)
    mutate(Weight = na.fill(Weight, fill = c("extend", NA, NA))) %>%
    ungroup() %>%
    filter(Year %in% yrRange)
  # Return weight-at-age by year
  return(wtAgeL)
} # End CalcWeightAtAge function

CalcWeightAtAgeBySiscaGear <- function(dat, Gear = 2, yearRange = yrRange) {
  # For SISCA
  # This function calculates mean weight-at-age by year, and fills in missing
  # data (i.e., NAs) using a suitable technique. Calculate the weighted mean
  # using the 'SampWt' column to fix unrepresentative sampling if identified
  # Calculate mean weight-at-age
  # Uses Gear_Code: (19 = Gillnet, 29 = Seine)
  # and Source_Code: (0 = Roe, 1 = Bait, 5 = Test, 6 = Food)
  # For food and bait use Gear =1 then (Gear_Code = 29, Source_Code = c(1,6))
  # For Seine test& roe use Gear =2, then Gear_Code = 29, Source_Code = c(0,5)
  # For Gillnet use Gear =3 then Gear_Code = 19, Source_Code = c(0,5)
  # For Gear = 19 -This is all gears that use gear 19 in the Database, 
  # which is equivalent to Gear =3
  # For Gear = 29 -This is all gears that use gear 29 in the Database, 
  # which is equivalent to Gear = 1 & 2
  
  #for Testing
  #dat <- bio
  
  Gear_Code <- case_when(Gear == 2 | Gear == 1 ~ 29,
    Gear ==  3 ~ 19,
    Gear == 19 ~ 19,
    Gear == 29 ~ 29)
  Source_Code <- case_when(Gear ==  1 ~ c(1,6,6,6,7) , #Note repeat numbers are because case_when
                           Gear ==  2 ~ c(0,5,5,5,5),  #requires vectors be of same length
                           Gear ==  3 ~ c(0,5,5,5,5),
                           Gear == 19 ~ c(0,5,5,5,5),
                           Gear == 29 ~ c(0,1,5,6,7))
  wtAge <- dat %>% 
    filter(GearCode == Gear_Code,
           SourceCode %in% Source_Code) %>% 
    select(Year, Age, Weight, SampWt) %>%
    na.omit() %>%
    group_by(Year, Age) %>%
    summarise(MeanWeight = WtMeanNA(x = Weight, w = SampWt)) %>%
    ungroup() %>%
    complete(Year = yearRange, Age = ageRange) %>%
    arrange(Year, Age)
  # Reshape from long to wide and merge with complete year sequence
  wtAgeW <- wtAge %>%
    pivot_wider(names_from = Age, values_from = MeanWeight) %>%
    arrange(Year)
  # Reshape from wide to long, and fill in NAs
  wtAgeL <- wtAgeW %>%
    pivot_longer(cols = !Year, names_to = "Age", values_to = "Weight",
                 names_transform = as.integer) %>%
    group_by(Age) %>%
    # Replace NAs: mean of (up to) previous n years
    mutate(Weight = RollMeanNA(Weight, n = nRoll)) %>%
    # Replace persistent NAs (i.e., at the beginning of the time series)
    mutate(Weight = na.fill(Weight, fill = c("extend", NA, NA))) %>%
    ungroup() %>%
    filter(Year %in% yearRange) %>%
    mutate(Weight = round(Weight/1000, digits = 4)) # do we want to round the digits?
           #Do we only have 4 significant digits when we multiply it by numbers to calculate biomass?
  wtAgeW <- wtAgeL %>%
    pivot_wider(names_from = Age, values_from = Weight, values_fill = 0) %>%
    rename("Year" = 1, "a1" = 2, "a2" = 3, "a3" = 4, "a4" = 5, "a5" = 6, 
           "a6" = 7, "a7" = 8, "a8" = 9, "a9" = 10, "a10" = 11) %>%
    mutate(Gear = Gear) %>%
    select(Year, Gear, a1:a10)
  # Return weight-at-age by year
  return(wtAgeW)
} # End CalcWeightAtAgeBySiscaGear function

#For sisca
# weightAgeByGear1 <- CalcWeightAtAgeBySiscaGear(bio, Gear = 1) 
# weightAgeByGear2 <- CalcWeightAtAgeBySiscaGear(bio, Gear = 2)
# weightAgeByGear3 <- CalcWeightAtAgeBySiscaGear(bio, Gear = 3)
# weightAgeByGear <- rbind(weightAgeByGear1, weightAgeByGear2)
# weightAgeByGear <- rbind(weightAgeByGear, weightAgeByGear3)
# weightAgeByGear <- weightAgeByGear %>% 
#   mutate(Area = secSubNum, 
#          Stock = secSubName) %>%
#   select(Year, Area, Gear, a2:a10, Stock)

### Output for SISCA ###
# write_csv(weightAgeByGear,
#           file = file.path("Summaries", paste("fleetWtAge", secSubName, ".csv", sep = "")),
#           append = FALSE)
# if(send2sisca == TRUE){
# write_csv(weightAgeByGear,
#           file = file.path(paste0("../SISCAH/Data/", region), paste0("fleetWtAge.csv")),
#           append = FALSE)
# }
# Do we need this for SISCA?
# Different for major vs others
if(regionType == "major") {
  # Calculate mean weight-at-age by year
  weightAge <- CalcWeightAtAge(dat = bio)
  # Calculate running mean weight-at-age by year
  muWeightAge <- weightAge %>%
    mutate(Measure = "Weight") %>%
    rename(Value = Weight) %>%
    arrange(Age, Year) %>%
    group_by(Age) %>%
    mutate(
      RollMean = rollmean(x = Value, k = nRoll, align = "right", na.pad = TRUE),
      PctChange = DeltaPercent(x = Value, type = "PctChange")
    ) %>%
    ungroup() %>%
    mutate(Age = factor(Age))
} else {
  weightAge <- bio %>%
    filter(GearCode == 29) %>%
    select(Year, Age, Weight, SampWt) %>%
    na.omit() %>%
    group_by(Year, Age) %>%
    summarise(Weight = WtMeanNA(x = Weight, w = SampWt)) %>%
    ungroup() %>%
    complete(Year = yrRange, Age = ageRange) %>%
    arrange(Year, Age)
}

# Calculate mean length-at-age by year
CalcLengthAtAge <- function(dat, yearRange = yrRange) {
  # This function calculates mean length-at-age by year, and fills in missing
  # data (i.e., NAs) using a suitable technique. Calculate the weighted mean
  # using the 'SampWt' column to fix unrepresentative sampling if identified
  # Calculate mean length-at-age
  lenAge <- dat %>%
    tibble() %>%
    filter(GearCode == 29) %>%
    select(Year, Age, Length, SampWt) %>%
    na.omit() %>%
    group_by(Year, Age) %>%
    summarise(MeanLength = WtMeanNA(x = Length, w = SampWt)) %>%
    ungroup() %>%
    complete(Year = yearRange, Age = ageRange) %>%
    arrange(Year, Age)
  # Reshape from long to wide and merge with complete year sequence
  lenAgeW <- lenAge %>%
    pivot_wider(names_from = Age, values_from = MeanLength) %>%
    arrange(Year)
  # Reshape from wide to long, and fill in NAs
  lenAgeL <- lenAgeW %>%
    pivot_longer(cols = !Year, names_to = "Age", values_to = "Length",
                 names_transform = as.integer) %>%
    #gather(key = Age, value = "Length", all_of(ageRange), convert = TRUE) %>%
    group_by(Age) %>%
    # Replace NAs: mean of (up to) previous n years
    mutate(Length = RollMeanNA(Length, n = nRoll)) %>%
    # Replace persistent NAs (i.e., at the beginning of the time series)
    mutate(Length = na.fill(Length, fill = c("extend", NA, NA))) %>%
    ungroup() %>%
    filter(Year %in% yearRange)
  # Return length-at-age by year
  return(lenAgeL)
} # End CalcLengthAtAge function

# Different for major vs others
if(regionType == "major") {
  # Calculate mean length-at-age by year
  lengthAge <- CalcLengthAtAge(dat = bio)
  # Calculate running mean length-at-age by year
  muLengthAge <- lengthAge %>%
    mutate(Measure = "Length") %>%
    rename(Value = Length) %>%
    arrange(Age, Year) %>%
    group_by(Age) %>%
    mutate(
      RollMean = rollmean(x = Value, k = nRoll, align = "right", na.pad = TRUE),
      PctChange = DeltaPercent(x = Value, type = "PctChange")
    ) %>%
    ungroup() %>%
    mutate(Age = factor(Age))
} else {
  lengthAge <- bio %>%
    filter(GearCode == 29) %>%
    select(Year, Age, Length, SampWt) %>%
    na.omit() %>%
    group_by(Year, Age) %>%
    summarise(Length = WtMeanNA(x = Length, w = SampWt)) %>%
    ungroup() %>%
    complete(Year = yrRange, Age = ageRange) %>%
    arrange(Year, Age)
}

# Different for major vs others
if(regionType == "major") {
  # Combine length- and weight-at-age by year
  muWtLenAge <- bind_rows(muWeightAge, muLengthAge) %>%
    mutate(Measure = factor(Measure, levels = unique(Measure)))
} else {
  # Temporary table
  wtAgeCombine <- weightAge %>%
    mutate(Measure = "Weight", Age = factor(Age)) %>%
    rename(Value = Weight)
  lenAgeCombine <- lengthAge %>%
    mutate(Measure = "Length", Age = factor(Age)) %>%
    rename(Value = Length) 
  # Combine length- and weight-at-age by year
  wtLenAge <- bind_rows(wtAgeCombine, lenAgeCombine) %>%
    mutate(Measure = factor(Measure, levels = unique(Measure)))
}

# Proportion female
propFemale <- bio %>%
  filter(GearCode == 29) %>%
  select(Year, Sex, SampWt) %>%
  na.omit() %>%
  group_by(Year) %>%
  summarise(Proportion = SumNA(SampWt[Sex == "F"]) / 
              (SumNA(SampWt[Sex == "F"]) + SumNA(SampWt[Sex == "M"]))) %>%
  ungroup() %>%
  complete(Year = yrRange) %>%
  arrange(Year)

# Get biosample locations in the current year
GetBioLocations <- function(dat, spObj) {
  # Wrangle data
  samp <- dat %>%
    filter(Year == max(yrRange), !is.na(geometry)) %>%
    mutate(
      Type = ifelse(SourceCode == 2, "Nearshore",
                    ifelse(SourceCode %in% c(3, 5), "Seine test", 
                           ifelse(SourceCode %in% c(1, 6), "Food and bait",
                                  "Commercial")))
    ) %>%
    group_by(Type, geometry) %>%
    summarise(Number = n_distinct(Sample)) %>%
    ungroup()
  # If there are rows
  if (nrow(samp) > 0) {
    # Clip to the region's extent
    # res <- ClipExtent(dat = samp, spObj = shapes$regSPDF, bufDist = maxBuff)
    st_crs(samp) <- st_crs(spObj)
    res <- st_intersection(x = samp, y = spObj)
  } else { # End if there are rows, otherwise
    # Warning
    warning("There are no geo-referenced biosamples", call. = FALSE)
    # Return the empty dataframe
    res <- samp
  } # End if there are no rows
  # Arrange for plotting
  res <- res %>%
    arrange(desc(Number), Type)
  # Return the data
  return(res)
} # End GetBioLocations function

# Get biosample locations
# TODO: Update this to be only 'Representative' samples (i.e., dat=bio?)
bioLocations <- GetBioLocations(dat = bioRaw, spObj = shapes$regions)

# Calculate spawn summary by groups (e.g., year and section)
CalcSpawnSummary <- function(dat, g) {
  # Calculate some basic yearly spawn statistics by section, and ensure that
  # years are complete (i.e., missing years are populated with NA)
  # Some wrangling
  spawnByYear <- dat %>%
    tibble() %>%
    select(
      Year, StatArea, Section, Group, Length, Width, SurfLyrs, MacroLyrs,
      UnderLyrs, MacroSI, SurfSI, UnderSI
    ) %>%
    mutate(Group = as.character(Group)) %>%
    group_by(.dots = g) %>%
    summarise(
      TotalLength = SumNA(Length), MeanWidth = MeanNA(Width),
      MeanLayers = MeanNA(c(SurfLyrs, MacroLyrs, UnderLyrs)),
      MacroSI = SumNA(MacroSI), SurfSI = SumNA(SurfSI),
      UnderSI = SumNA(UnderSI), TotalSI = SumNA(c(MacroSI, SurfSI, UnderSI))
    ) %>%
    ungroup()
  # Get the full year range
  if (all(g == "Year")) yrsFull <- tibble(Year = yrRange)
  # Get the full year range and stat areas
  if (all(c("Year", "StatArea") %in% g)) {
    yrsFull <- expand.grid(Year = yrRange, StatArea = unique(dat$StatArea)) %>%
      tibble() %>%
      mutate(StatArea = formatC(StatArea, width=2, format="d", flag="0"))
  }
  # Get the full year range and sections
  if (all(c("Year", "Section") %in% g)) {
    yrsFull <- expand.grid(Year = yrRange, Section = unique(dat$Section)) %>%
      tibble() %>%
      mutate(Section = formatC(Section, width=3, format="d", flag="0"))
  }
  # Get the full year range and groups
  if (all(c("Year", "Group") %in% g)) {
    yrsFull <- expand.grid(Year = yrRange, Group = unique(dat$Group)) %>%
      mutate(Group = as.character(Group))
  }
  # Merge to ensure that each year has an entry
  res <- spawnByYear %>%
    full_join(y = yrsFull, by = g) %>%
    filter(Year %in% yrRange) %>%
    # arrange(g) %>%
    mutate(
      Survey = ifelse(Year < pars$years$dive, "Surface", "Dive"),
      Survey = factor(Survey, levels = c("Surface", "Dive"))
    )
  # Return the data
  return(res)
} # End CalcSpawnSummary function

# Calculate spawn summary by year
spawnYr <- CalcSpawnSummary(dat = spawnRaw, g = c("Year")) %>%
  mutate(
    PctChange = DeltaPercent(TotalSI, type = "PctChange"),
    PctDiff = DeltaPercent(TotalSI, type = "PctDiff"),
    PctChange = ifelse(Year == pars$years$dive, NA, PctChange),
    PctDiff = ifelse(Year == pars$years$dive, NA, PctDiff)
  )

# # Calculate relative abundance
# relAbund <- spawnYr %>%
#   mutate(Survey = as.character(Survey)) %>%
#   left_join(y = q, by = "Survey") %>%
#   rename(q = Median) %>%
#   mutate(
#     Survey = factor(Survey, levels = c("Surface", "Dive")),
#     Abund = TotalSI / q) %>%
#   select(Year, TotalSI, Survey, Region, q, Abund)

# sYr <- spawnYr %>%
#   filter( !is.na(TotalSI) ) %>%
#   complete( Year=full_seq(Year, 1) ) %>%
#   arrange( Year )
# p <- ggplot( data=sYr, aes(x=Year, y=TotalSI) ) +
#   geom_line( )  +
#   labs(title=paste(range(sYr$Year), collapse=" - "), y="Spawn index (t)" ) +
#   ggsave( filename=paste("SpawnIndex", regName, ".png", sep=""), dpi=figRes )

# Wrangle spawn by type to long
spawnYrType <- spawnYr %>%
  select(Year, MacroSI, SurfSI, UnderSI, Survey) %>%
  rename(Macrocystis = MacroSI, Surface = SurfSI, Understory = UnderSI) %>%
  gather("Macrocystis", "Surface", "Understory", key = "Type", value = "SI") %>%
  mutate(
    Type = factor(Type, levels = c("Surface", "Macrocystis", "Understory")),
    Survey = factor(Survey, levels = c("Surface", "Dive"))
  ) %>%
  arrange(Year, Survey, Type)

# Wrangle spawn showing proportion by type
spawnYrTypeProp <- spawnYrType %>%
  replace_na(replace = list(SI = 0)) %>%
  mutate(Type2 = ifelse(Type == "Surface", "Surface", "Dive")) %>%
  group_by(Year, Survey, Type2) %>%
  summarise(SI = SumNA(SI)) %>%
  mutate(Prop = SI / SumNA(SI)) %>%
  ungroup() %>%
  rename(Type = Type2) %>%
  mutate(
    Type = factor(Type, levels = c("Surface", "Dive")),
    Survey = factor(Survey, levels = c("Surface", "Dive"))
  )

# Smaller subset for table: spawn by year
spawnYrTab <- spawnYr %>%
  filter(Year >= firstYrTab) %>%
  select(Year, TotalLength, MeanWidth, MeanLayers, TotalSI)

# Calculate spawn summary by year and section
spawnYrSec <- CalcSpawnSummary(dat = spawnRaw, g = c("Year", "Section")) %>%
  group_by(Year) %>%
  mutate(PercSI = 100 * TotalSI / SumNA(TotalSI)) %>%
  ungroup() %>%
  full_join(
    y = areas %>% tibble() %>% select(StatArea, Section, Group) %>% distinct(),
    by = "Section"
  ) %>%
  mutate(
    Section = formatC(Section, width = 3, flag = "0"),
    StatArea = formatC(StatArea, width = 2, flag = "0")
  )

# Calculate spawn summary by year and statistical area
spawnYrSA <- CalcSpawnSummary(dat = spawnRaw, g = c("Year", "StatArea")) %>%
  group_by(Year) %>%
  mutate(PercSI = 100 * TotalSI / SumNA(TotalSI)) %>%
  ungroup() %>%
  mutate(StatArea = formatC(StatArea, width = 2, flag = "0"))

# Calculate the proportion of spawn by group or statistical area
CalcPropSpawn <- function(dat, g, yrs = yrRange) {
  # Error if grouping variable not specified
  if (!g %in% c("Section", "StatArea", "Group"))
    stop("Grouping variable not specified")
  # Get the full year range and stat areas
  if (g == "Section") {
    yrsFull <- expand.grid(Year = yrs, Section = unique(dat$Section))
  }
  # Get the full year range and stat areas
  if (g == "StatArea") {
    yrsFull <- expand.grid(Year = yrs, StatArea = unique(dat$StatArea))
  }
  # Get the full year range and groups
  if (g == "Group") {
    yrsFull <- expand.grid(Year = yrs, Group = unique(dat$Group)) %>%
      mutate(Group = as.character(Group))
  }
  # Determin spawn proportions
  pSpawn <- dat %>%
    tibble() %>%
    filter(Year %in% yrs) %>%
    replace_na(replace = list(SurfSI = 0, MacroSI = 0, UnderSI = 0)) %>%
    mutate(TotalSI = SurfSI + MacroSI + UnderSI) %>%
    group_by_(.dots = c("Year", g)) %>%
    summarise(TotalSI = SumNA(TotalSI)) %>%
    group_by(Year) %>%
    mutate(Proportion = TotalSI / SumNA(TotalSI)) %>%
    ungroup() %>%
    full_join(y = yrsFull, by = c("Year", g)) %>%
    replace_na(replace = list(Proportion = 0)) %>%
    mutate(
      Proportion = formatC(Proportion, digits = 3, format = "f", big.mark = ",")
    )
  # Calculate total spawn
  tSpawn <- pSpawn %>%
    group_by(Year) %>%
    summarise(TotalSI = SumNA(TotalSI)) %>%
    ungroup()
  # If sections
  if (g == "Section") {
    # Update stat area names (1)
    pSpawn <- pSpawn %>%
      mutate(Section = formatC(Section, width = 3, format = "d", flag = "0"))
  } # End if sections
  # If stat areas
  if (g == "StatArea") {
    # Update stat area names (1)
    pSpawn <- pSpawn %>%
      mutate(StatArea = formatC(StatArea, width = 2, format = "d", flag = "0"))
  } # End if stat areas
  # Full join and wrangle
  res <- pSpawn %>%
    select(-TotalSI) %>%
    spread_(key = g, value = "Proportion") %>%
    full_join(y = tSpawn, by = "Year") %>%
    arrange(Year) %>%
    mutate(
      TotalSI = formatC(TotalSI, digits = 0, format = "f", big.mark = ","),
      TotalSI = ifelse(TotalSI == 0, NA, TotalSI)
    ) %>%
    select(Year, TotalSI, everything()) %>%
    rename(`Spawn index` = TotalSI)
  # Return the results
  return(res)
} # End CalcPropSpawn function

# Calculate spawn summary in current year by location code
spawnByLocXY <- spawnRaw %>%
  filter(Year == max(yrRange)) %>%
  group_by(StatArea, Section, LocationCode, LocationName) %>%
  summarise(
    Start = MinNA(Start), TotalSI = SumNA(c(MacroSI, SurfSI, UnderSI)),
    geometry = unique(geometry)
  ) %>%
  ungroup() %>%
  arrange(TotalSI)

# Calculate spawn summary in current year by location code
spawnByLoc <- spawnByLocXY %>%
  select(-geometry, -LocationCode)

# For plotting, remove rows with no spatial info
spawnByLocXY <- spawnByLocXY %>%
  filter(!is.na(geometry))

# Calculate spawn summary for the last decade
spawnDecade <- spawnRaw %>%
  filter(Year %in% (max(yrRange) - 1):(max(yrRange) - 10)) %>%
  group_by(Year, LocationCode) %>%
  summarise(
    geometry = unique(geometry),
    TotalSI = SumNA(c(SurfSI, MacroSI, UnderSI))
  ) %>%
  ungroup() %>%
  mutate(Decade = paste(min(Year), max(Year), sep = " to ")) %>%
  group_by(Decade, LocationCode) %>%
  summarise(
    geometry = unique(geometry),
    MeanSI = MeanNA(TotalSI),
    Frequency = n()
  ) %>%
  ungroup() %>%
  #    filter( Frequency >= 2, MeanSI >=quantile(MeanSI, probs=0.1,
  #            na.rm=TRUE) ) %>%
  arrange(desc(Frequency), MeanSI)

# Get table of stat area, section, and group
spatialGroup <- areas %>%
  select(RegionName, StatArea, Section, Group) %>%
  distinct() %>%
  mutate(
    StatArea = formatC(StatArea, width = 2, format = "d", flag = "0"),
    Section = formatC(Section, width = 3, format = "d", flag = "0")
  ) %>%
  arrange(RegionName, StatArea, Section, Group)

# Calculate spawn index by location and year
siYearLoc <- spawnRaw %>%
  group_by(Year, LocationCode) %>%
  summarise(
    geometry = unique(geometry),
    SITotal = SumNA(c(SurfSI, MacroSI, UnderSI))
  ) %>%
  ungroup() %>%
  complete(Year = yrRange)

# Length at age data
lenAge <- bio %>%
  filter(Year %in% c((max(yrRange) - 4):max(yrRange))) %>%
  select(Year, Length, Age)

##### Privacy #####

# Load catch and harvest (i.e., SOK) privacy info
LoadPrivacy <- function(where, a) {
  # Load the privacy data: region
  privRegion <- read_csv(
    file = file.path(where$loc, where$fn$Region),
    col_types = cols()
  ) %>%
    filter(Region %in% a$Region) %>%
    mutate(Private = TRUE)
  # Load the privacy data: stat area
  privStatArea <- read_csv(
    file = file.path(where$loc, where$fn$StatArea), col_types = cols()
  ) %>%
    mutate(StatArea = formatC(StatArea, width = 2, flag = "0")) %>%
    filter(StatArea %in% a$StatArea) %>%
    mutate(Private = TRUE)
  # Return the data
  return(privDat = list(region = privRegion, statArea = privStatArea))
} # End LoadPrivacy function

# Load catch privacy data (if any)
privDat <- LoadPrivacy(where = privLoc, a = areas)

# Apply privacy to catch data
catchPriv <- privDat$region %>%
  filter(Gear != "SOK") %>%
  right_join(y = catch, by = c("Region", "Year", "Gear")) %>%
  replace_na(replace = list(Private = FALSE)) %>%
  mutate(
    Gear = factor(Gear, levels = tPeriod$Gear),
    CatchPriv = ifelse(Private, 0, Catch)
  )

# Remove SOK data for certain years due to privacy concerns
harvestSOK <- privDat$region %>%
  filter(Gear == "SOK") %>%
  select(Year, Private) %>%
  right_join(y = harvestSOK, by = "Year") %>%
  replace_na(replace = list(Private = FALSE)) %>%
  mutate(
    Harvest = format(Harvest, big.mark = ",", digits = 0, scientific = FALSE),
    Biomass = format(Biomass, big.mark = ",", digits = 0, scientific = FALSE),
    Harvest = ifelse(Private, "WP", Harvest),
    Biomass = ifelse(Private, "WP", Biomass)
  ) %>%
  select(-Private)

# Remove catch due to privacy concerns
catchCommUseYr <- catchPriv %>%
  complete(Year = yrRange, Gear, fill = list(Catch = 0)) %>%
  filter(Year == max(yrRange)) %>%
  group_by(Gear) %>%
  summarise(Catch = SumNA(Catch), Private = all(isTRUE(Private))) %>%
  ungroup() %>%
  mutate(Catch = ifelse(Private, "WP",
                        format(Catch, big.mark = ",", digits = 0, scientific = FALSE)
  )) %>%
  select(Gear, Catch)

##### Region #####

# If region is Haida Gwaii
if (region == "HG") {
  # Count the number of fish aged by year (and as a proportion) by seine gear:
  # use the 'SampWt' column to fix unrepresentative sampling if identified
  numAgedYearGrp <- bio %>%
    filter(GearCode == 29) %>% # %in% c(19, 29) (originally == 29)
    select(Year, Age, Group, SampWt) %>%
    na.omit() %>%
    group_by(Year, Group, Age) %>%
    summarise(Number = SumNA(SampWt)) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    ungroup() %>%
    arrange(Year, Group, Age)
  # Determine weighted mean and approximate CI age by year
  qAgedYearGrp <- numAgedYearGrp %>%
    select(Year, Age, Group, Proportion) %>%
    group_by(Year, Group) %>%
    summarise(
      MeanAge = weighted.mean(x = Age, w = Proportion),
      # CI is based on R code by Steve Martel
      sBar = qnorm(1 - (1 - ciLevel) / 2) *
        sum(sqrt(Proportion * (1 - Proportion)) / sqrt(Age)),
      Lower = exp(log(MeanAge) - log(sBar)),
      Upper = exp(log(MeanAge) + log(sBar))
    ) %>%
    ungroup() %>%
    arrange(Year, Group) %>%
    group_by(Group) %>%
    mutate(GroupID = ConsecutiveGroup(Year)) %>%
    ungroup() %>%
    arrange(Year, Group)
  # Remove Group info
  spatialGroup <- spatialGroup %>%
    select(-Group)
  # Plot spawn timing by Stat Area
  spawnTimingGroup <- FALSE
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn(dat = spawnRaw, g = "Group")
  # Dummy variable
  yrsNearshore <- 0
} # End if region is Haida Gwaii

# If region is Prince Rupert District
if (region == "PRD") {
  # Catch by StatArea
  catchStatArea <- catch %>%
    group_by(Year, StatArea) %>%
    summarise(Catch = SumNA(Catch)) %>%
    ungroup() %>%
    mutate(SA = formatC(StatArea, flag = "0", width = 2))
  # Apply privacy to catch data
  catchStatAreaPriv <- catchStatArea %>%
    left_join(y = privDat$statArea, by = c("StatArea", "Year")) %>%
    replace_na(replace = list(Private = FALSE)) %>%
    mutate(CatchPriv = ifelse(Private, 0, Catch))
  # Remove Group info
  spatialGroup <- spatialGroup %>%
    select(-Group)
  # Plot spawn timing by Stat Area
  spawnTimingGroup <- FALSE
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn(dat = spawnRaw, g = "StatArea")
  # Dummy variable
  yrsNearshore <- 0
} # End if region is Prince Rupert District

# If region is Central Coast
if (region == "CC") {
  # Calculate spawn statistics by year and statistical area
  spawnStatsYrSA <- spawnRaw %>%
    mutate(StatArea = formatC(StatArea, width = 2, flag = "0")) %>%
    rowwise() %>%
    mutate(Layers = MeanNA(c(SurfLyrs, MacroLyrs, UnderLyrs))) %>%
    ungroup() %>%
    select(Year, StatArea, Depth, Layers) %>%
    filter(Year >= firstYrFig) %>%
    arrange(Year, StatArea)
  # Calculate spawn depth by year and section
  spawnStatsYrSec <- spawnRaw %>%
    mutate(Section = formatC(Section, width = 3, flag = "0")) %>%
    rowwise() %>%
    mutate(Layers = MeanNA(c(SurfLyrs, MacroLyrs, UnderLyrs))) %>%
    ungroup() %>%
    select(Year, Section, Depth, Layers) %>%
    filter(Year >= firstYrFig) %>%
    arrange(Year, Section)
  # Weight-at-age by year and group
  weightAgeGroup <- bio %>%
    filter(GearCode == 29, Year >= max(yrRange) - 19) %>%
    left_join(
      y = areas,
      by = c("Region", "StatArea", "Section", "LocationCode", "Group")
    ) %>%
    mutate(Decade = ifelse(Year >= max(yrRange) - 9, "Recent", "Previous")) %>%
    select(Year, Age, Weight, Group, Decade) %>%
    na.omit()
  # Determine sample sizes
  weightAgeGroupN <- weightAgeGroup %>%
    group_by(Decade, Group, Age) %>%
    summarise(Sample = n()) %>%
    spread(key = Decade, value = Sample) %>%
    ungroup()
  #  # Average length-at-age by year and statistical area
  #  lengthAgeSA <- bio %>%
  #      filter( GearCode == 29, Year >= pars$years$dive ) %>%
  #      select( Year, StatArea, Age, Length ) %>%
  #      na.omit( ) %>%
  #      group_by( Year, StatArea, Age ) %>%
  #      summarise( MeanLength=mean(Length) ) %>%
  #      ungroup( ) %>%
  #      spread( key=Age, value=MeanLength )
  # Plot spawn timing by Stat Area
  spawnTimingGroup <- FALSE
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn(dat = spawnRaw, g = "StatArea")
  # Compare differences by group: number, proportion, weight, and length-at-age
  npwAgeGrp <- bioRaw %>%
    filter(
      Year == max(yrRange), SourceCode %in% c(2, 5), Representative == 1
    ) %>%
    left_join(y = tSource, by = "SourceCode") %>%
    group_by(Age, SampleSource2) %>%
    summarise(Number = n(), Weight = MeanNA(Weight), Length = MeanNA(Length)) %>%
    group_by(SampleSource2) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    select(SampleSource2, Age, Number, Proportion, Weight, Length)
  # Calculate total: number, proportion, and weight-at-age
  npwAgeTot <- bioRaw %>%
    filter(
      Year == max(yrRange), SourceCode %in% c(2, 5),
      Representative == 1
    ) %>%
    mutate(SampleSource2 = "Total") %>%
    group_by(Age, SampleSource2) %>%
    summarise(Number = n(), Weight = MeanNA(Weight), Length = MeanNA(Length)) %>%
    ungroup() %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    select(SampleSource2, Age, Number, Proportion, Weight, Length)
  # Combine the grouped statistics with the total statistics
  npwAge <- bind_rows(npwAgeGrp, npwAgeTot) %>%
    complete(
      Age = all_of(ageRange), # SampleSource2 = unique(SampleSource2), 
      fill = list(Number = 0, Proportion = 0)
    ) %>%
    arrange(SampleSource2, Age)
  # Get differences in number-at-age
  deltaNumAgeYr <- npwAge %>%
    select( Age, SampleSource2, Number ) %>%
    spread( key=Age, value=Number ) %>%
    rename( 'Sample type'=SampleSource2 )
  # Get differences in proportion-at-age
  deltaPropAgeYr <- npwAge %>%
    select(Age, SampleSource2, Proportion) %>%
    spread(key = Age, value = Proportion) %>%
    rename("Sample type" = SampleSource2)
  # Get differences in weight-at-age
  deltaWtAgeYr <- npwAge %>%
    select(Age, SampleSource2, Weight) %>%
    spread(key = Age, value = Weight) %>%
    rename("Sample type" = SampleSource2)
  # Get differences in length-at-age
  deltaLenAgeYr <- npwAge %>%
    select(Age, SampleSource2, Length) %>%
    spread(key = Age, value = Length) %>%
    rename("Sample type" = SampleSource2)
  # Determine years for the FN nearshore pilot study
  yrsNearshore <- bioRaw %>%
    filter(SourceCode == 2, GearCode == 1) %>%
    select(Year) %>%
    distinct() %>%
    pull(Year)
  # Get nearshore data by year and age
  nearYearAge <- bioRaw %>%
    filter(SourceCode == 2, GearCode == 1, Representative == 1) %>%
    group_by(Year, Age) %>%
    summarise(
      Number = n(), Weight = MeanNA(Weight), Length = MeanNA(Length)
    ) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    ungroup() %>%
    mutate(Year = as.character(Year)) %>%
    select(Year, Age, Number, Proportion, Weight, Length)
  # Get nearshore data: total
  nearAge <- bioRaw %>%
    filter(SourceCode == 2, GearCode == 1, Representative == 1) %>%
    group_by(Age) %>%
    summarise(
      Number = n(), Weight = MeanNA(Weight), Length = MeanNA(Length)
    ) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    ungroup() %>%
    mutate(Year = "Total") %>%
    select(Year, Age, Number, Proportion, Weight, Length)
  # Combine totals with annual stats
  nearAll <- bind_rows(nearYearAge, nearAge) %>%
    complete(Age = all_of(ageRange)) %>%
    # filter( !is.na(Year) ) %>%
    arrange(Year, Age)
  # Nearshore number-at-age
  nearNum <- nearAll %>%
    select(Year, Age, Number) %>%
    spread(key = Age, value = Number, drop = FALSE, fill = 0) %>%
    filter(!is.na(Year), Year != "Total")
  # Nearshore proportion-at-age
  nearProp <- nearAll %>%
    select(Year, Age, Proportion) %>%
    spread(key = Age, value = Proportion, drop = FALSE, fill = 0) %>%
    filter(!is.na(Year), Year != "Total")
  # Nearshore weight-at-age
  nearWt <- nearAll %>%
    select(Year, Age, Weight) %>%
    spread(key = Age, value = Weight) %>%
    filter(!is.na(Year), Year != "Total")
  # Nearshore length-at-age
  nearLen <- nearAll %>%
    select(Year, Age, Length) %>%
    spread(key = Age, value = Length) %>%
    filter(!is.na(Year), Year != "Total")
  # Get length at age for the two sampling protocols
  lenAgeSample <- bioRaw %>%
    filter(
      Year %in% yrsNearshore, SourceCode %in% c(2, 5),
      Representative == 1
    ) %>%
    left_join(y = tSource, by = "SourceCode") %>%
    select(Year, SampleSource2, StatArea, Age, Length) %>%
    rename(SA = StatArea)
  # Get nearshore samples
  nearYearAge2 <- bioRaw %>%
    filter(SourceCode == 2, GearCode == 1, Representative == 1) %>%
    group_by(Year, Age) %>%
    summarise(
      Number = n(), Weight = MeanNA(Weight), Length = MeanNA(Length)
    ) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    ungroup() %>%
    select(Year, Age, Number, Proportion) %>%
    mutate(Sample = "Nearshore")
  # Get seine test samples
  seineYearAge2 <- bio %>%
    filter(GearCode == 29, Year %in% yrsNearshore) %>%
    group_by(Year, Age) %>%
    summarise(Number = SumNA(SampWt)) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    ungroup() %>%
    select(Year, Age, Number, Proportion) %>%
    mutate(Sample = "Seine test")
  # Combine nearshore and seine test samples: parts A and B
  compNear <- bind_rows(nearYearAge2, seineYearAge2)
  # Get nearshore samples by stat area
  nearYearAgeSA <- bioRaw %>%
    filter(SourceCode == 2, GearCode == 1, Representative == 1) %>%
    group_by(Year, StatArea, Age) %>%
    summarise(
      Number = n(), Weight = MeanNA(Weight), Length = MeanNA(Length)
    ) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    ungroup() %>%
    select(Year, Age, StatArea, Number, Proportion) %>%
    mutate(Sample = "Nearshore")
  # Get seine test samples by stat area
  seineYearAgeSA <- bio %>%
    filter(GearCode == 29, Year %in% yrsNearshore) %>%
    group_by(Year, StatArea, Age) %>%
    summarise(Number = SumNA(SampWt)) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    ungroup() %>%
    select(Year, Age, StatArea, Number, Proportion) %>%
    mutate(Sample = "Seine test")
  # Combine nearshore and seine test samples by stat area
  compNearSA <- bind_rows(nearYearAgeSA, seineYearAgeSA)
  # Determine number by year and sample type
  nSampleSA <- compNearSA %>%
    group_by(Year, StatArea, Sample) %>%
    summarise(Number = SumNA(Number)) %>%
    ungroup()
} # End if region is Central Coast

# If region is Strait of Georgia
if (region == "SoG") {
  # # Spawn by decade and group for Andy/RDG
  # spawnDecGrp <- spawnRaw %>%
  #   group_by(Decade, Group) %>%
  #   summarise(Surface = SumNA(SurfSI),
  #             Macro = SumNA(MacroSI), 
  #             Under = SumNA(UnderSI),
  #             Total = SumNA(c(Surface, Under, Macro))) %>%
  #   group_by(Decade) %>%
  #   mutate(Percent = 100 * Total / SumNA(Total)) %>%
  #   ungroup() %>%
  #   select(Decade, Group, Total, Percent)
  # Calculate spawn summary by year and group
  spawnYrGrp <- CalcSpawnSummary(dat = spawnRaw, g = c("Year", "Group")) %>%
    group_by(Year) %>%
    mutate(PercSI = 100 * TotalSI / SumNA(TotalSI)) %>%
    ungroup()
  # Weight by catch type
  weightCatch <- bio %>%
    filter(GearCode == 29, StatArea %in% c(14, 17)) %>%
    # filter(GearCode %in% c(19, 29), StatArea %in% c(14, 17)) %>%
    left_join(y = tSource, by = "SourceCode") #%>%
    # mutate(SampleSource2 = ifelse(GearCode == 19, "Gillnet", SampleSource2))
  # Smaller subset for figures: biosamples big
  weightCatchFig <- weightCatch %>%
    filter(Year >= firstYrTab)
  # Average weight by age
  weightCatchFigMu <- weightCatchFig %>%
    select(Age, Weight, Length) %>%
    na.omit() %>%
    group_by(Age) %>%
    summarise(MuWeight = MeanNA(Weight), MuLength = MeanNA(Length)) %>%
    ungroup()
  # Plot spawn timing by Group
  spawnTimingGroup <- TRUE
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn(dat = spawnRaw, g = "Group")
  # Dummy variable
  yrsNearshore <- 0
} # End if region is Strait of Georgia

# If region is West Coast of Vancouver Island
if (region == "WCVI") {
  # Compare differences by group: number, proportion, weight, and length-at-age
  npwAgeGrp <- bioRaw %>%
    filter(
      Year == max(yrRange), SourceCode %in% c(2, 5), Representative == 1
    ) %>%
    left_join(y = tSource, by = "SourceCode") %>%
    group_by(Age, SampleSource2) %>%
    summarise(Number = n(), Weight = MeanNA(Weight), Length = MeanNA(Length)) %>%
    group_by(SampleSource2) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    select(SampleSource2, Age, Number, Proportion, Weight, Length)
  # Calculate total: number, proportion, and weight-at-age
  npwAgeTot <- bioRaw %>%
    filter(
      Year == max(yrRange), SourceCode %in% c(2, 5),
      Representative == 1
    ) %>%
    mutate(SampleSource2 = "Total") %>%
    group_by(Age, SampleSource2) %>%
    summarise(Number = n(), Weight = MeanNA(Weight), Length = MeanNA(Length)) %>%
    ungroup() %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    select(SampleSource2, Age, Number, Proportion, Weight, Length)
  # Combine the grouped statistics with the total statistics
  npwAge <- bind_rows(npwAgeGrp, npwAgeTot) %>%
    complete(
      Age = all_of(ageRange), # SampleSource2 = unique(SampleSource2), 
      fill = list(Number = 0, Proportion = 0)
    ) %>%
    arrange(SampleSource2, Age)
  # Get differences in number-at-age
  deltaNumAgeYr <- npwAge %>%
    select( Age, SampleSource2, Number ) %>%
    spread( key=Age, value=Number ) %>%
    rename( 'Sample type'=SampleSource2 )
  # Get differences in proportion-at-age
  deltaPropAgeYr <- npwAge %>%
    select(Age, SampleSource2, Proportion) %>%
    spread(key = Age, value = Proportion) %>%
    rename("Sample type" = SampleSource2)
  # Get differences in weight-at-age
  deltaWtAgeYr <- npwAge %>%
    select(Age, SampleSource2, Weight) %>%
    spread(key = Age, value = Weight) %>%
    rename("Sample type" = SampleSource2)
  # Get differences in length-at-age
  deltaLenAgeYr <- npwAge %>%
    select(Age, SampleSource2, Length) %>%
    spread(key = Age, value = Length) %>%
    rename("Sample type" = SampleSource2)
  # Remove Group info
  spatialGroup <- spatialGroup %>%
    select(-Group)
  # Plot spawn timing by Stat Area
  spawnTimingGroup <- FALSE
  # Determine years for the FN nearshore pilot study
  yrsNearshore <- bioRaw %>%
    filter(SourceCode == 2, GearCode == 1) %>%
    select(Year) %>%
    distinct() %>%
    pull(Year)
  # Get nearshore data by year and age
  nearYearAge <- bioRaw %>%
    filter(SourceCode == 2, GearCode == 1, Representative == 1) %>%
    group_by(Year, Age) %>%
    summarise(
      Number = n(), Weight = MeanNA(Weight), Length = MeanNA(Length)
    ) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    ungroup() %>%
    mutate(Year = as.character(Year)) %>%
    select(Year, Age, Number, Proportion, Weight, Length)
  # Get nearshore data: total
  nearAge <- bioRaw %>%
    filter(SourceCode == 2, GearCode == 1, Representative == 1) %>%
    group_by(Age) %>%
    summarise(
      Number = n(), Weight = MeanNA(Weight), Length = MeanNA(Length)
    ) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    ungroup() %>%
    mutate(Year = "Total") %>%
    select(Year, Age, Number, Proportion, Weight, Length)
  # Combine totals with annual stats
  nearAll <- bind_rows(nearYearAge, nearAge) %>%
    complete(Age = all_of(ageRange)) %>%
    # filter( !is.na(Year) ) %>%
    arrange(Year, Age)
  # Nearshore number-at-age
  nearNum <- nearAll %>%
    select(Year, Age, Number) %>%
    spread(key = Age, value = Number, drop = FALSE, fill = 0) %>%
    filter(!is.na(Year), Year != "Total")
  # Nearshore proportion-at-age
  nearProp <- nearAll %>%
    select(Year, Age, Proportion) %>%
    spread(key = Age, value = Proportion, drop = FALSE, fill = 0) %>%
    filter(!is.na(Year), Year != "Total")
  # Nearshore weight-at-age
  nearWt <- nearAll %>%
    select(Year, Age, Weight) %>%
    spread(key = Age, value = Weight) %>%
    filter(!is.na(Year), Year != "Total")
  # Nearshore length-at-age
  nearLen <- nearAll %>%
    select(Year, Age, Length) %>%
    spread(key = Age, value = Length) %>%
    filter(!is.na(Year), Year != "Total")
  # Get length at age for the two sampling protocols
  lenAgeSample <- bioRaw %>%
    filter(
      Year %in% yrsNearshore, SourceCode %in% c(2, 5),
      Representative == 1
    ) %>%
    left_join(y = tSource, by = "SourceCode") %>%
    select(Year, SampleSource2, StatArea, Age, Length) %>%
    rename(SA = StatArea)
  # Get nearshore samples
  nearYearAge2 <- bioRaw %>%
    filter(SourceCode == 2, GearCode == 1, Representative == 1) %>%
    group_by(Year, Age) %>%
    summarise(
      Number = n(), Weight = MeanNA(Weight), Length = MeanNA(Length)
    ) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    ungroup() %>%
    select(Year, Age, Number, Proportion) %>%
    mutate(Sample = "Nearshore")
  # Get seine test samples
  seineYearAge2 <- bio %>%
    filter(GearCode == 29, Year %in% yrsNearshore) %>%
    group_by(Year, Age) %>%
    summarise(Number = SumNA(SampWt)) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    ungroup() %>%
    select(Year, Age, Number, Proportion) %>%
    mutate(Sample = "Seine test")
  # Combine nearshore and seine test samples: parts A and B
  compNear <- bind_rows(nearYearAge2, seineYearAge2)
  # Get nearshore samples by stat area
  nearYearAgeSA <- bioRaw %>%
    filter(SourceCode == 2, GearCode == 1, Representative == 1) %>%
    group_by(Year, StatArea, Age) %>%
    summarise(
      Number = n(), Weight = MeanNA(Weight), Length = MeanNA(Length)
    ) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    ungroup() %>%
    select(Year, Age, StatArea, Number, Proportion) %>%
    mutate(Sample = "Nearshore")
  # Get seine test samples by stat area
  seineYearAgeSA <- bio %>%
    filter(GearCode == 29, Year %in% yrsNearshore) %>%
    group_by(Year, StatArea, Age) %>%
    summarise(Number = SumNA(SampWt)) %>%
    mutate(Proportion = Number / SumNA(Number)) %>%
    ungroup() %>%
    select(Year, Age, StatArea, Number, Proportion) %>%
    mutate(Sample = "Seine test")
  # Combine nearshore and seine test samples by stat area
  compNearSA <- bind_rows(nearYearAgeSA, seineYearAgeSA)
  # Determine number by year and sample type
  nSampleSA <- compNearSA %>%
    group_by(Year, StatArea, Sample) %>%
    summarise(Number = SumNA(Number)) %>%
    ungroup()
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn(dat = spawnRaw, g = "StatArea")
} # End if region is West Coast of Vancouver Island

# If region is Area 27
if (region == "A27") {
  # Remove Group info
  spatialGroup <- spatialGroup %>%
    select(-Group)
  # Plot spawn timing by Stat Area
  spawnTimingGroup <- FALSE
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn(dat = spawnRaw, g = "Section")
  # Dummy variable
  yrsNearshore <- 0
} # End if region is Area 27

# If region is Area 2 West
if (region == "A2W") {
  # Remove Group info
  spatialGroup <- spatialGroup %>%
    select(-Group)
  # Plot spawn timing by Stat Area
  spawnTimingGroup <- FALSE
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn(dat = spawnRaw, g = "Section")
  # Dummy variable
  yrsNearshore <- 0
} # End if region is Area 2 West

# If region is JS
if (region == "JS") {
  # Remove Group info
  spatialGroup <- spatialGroup %>%
    select(-Group)
  # Plot spawn timing by Stat Area
  spawnTimingGroup <- FALSE
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn(dat = spawnRaw, g = "StatArea")
  # Dummy variable
  yrsNearshore <- 0
} # End if region is JS

# If region is A10
if (region == "A10") {
  # Remove Group info
  spatialGroup <- spatialGroup %>%
    select(-Group)
  # Plot spawn timing by Stat Area
  spawnTimingGroup <- FALSE
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn(dat = spawnRaw, g = "Section")
  # If 101 is missing, add it
  if(!"101" %in% names(propSpawn)) {
    propSpawn <- propSpawn %>%
      mutate(`101` = "0.000") %>%
      select(Year, `Spawn index`, `101`, `102`, `103`)
  } # End if no 101
  # Dummy variable
  yrsNearshore <- 0
} # End if region is A10

# If region is all
if (region == "All") {
  # Plot spawn timing by Stat Area
  spawnTimingGroup <- FALSE
  # Determine the spatial distribution of spawn
  propSpawn <- CalcPropSpawn(dat = spawnRaw, g = "StatArea")
  # Dummy variable
  yrsNearshore <- 0
} # End if region is all

##### ADMB #####

# Make ADMB input data: catch (t*10^3)
catchADMB <- catch %>%
  group_by(Period, Year) %>%
  summarise(Catch = SumNA(Catch)) %>%
  ungroup() %>%
  rename(Gear = Period) %>%
  mutate(
    Gear = as.integer(parse_number(Gear)),
    Value = round(Catch / 1000, digits = 3),
    Area = as.integer(1), Group = as.integer(1), Sex = as.integer(0),
    Type = as.integer(1)
  ) %>%
  select(Year, Gear, Area, Group, Sex, Type, Value) %>%
  arrange(Gear, Year)

# Make ADMB input data: spawn (t*10^3)
spawnADMB <- spawnYr %>%
  tibble() %>%
  select(Year, TotalSI) %>%
  rename(Spawn = TotalSI) %>%
  na.omit() %>%
  mutate(
    Spawn = round(Spawn / 1000, digits = 3),
    Gear = ifelse(Year < pars$years$dive, as.integer(4), as.integer(5)),
    Area = as.integer(1), Group = as.integer(1), Sex = as.integer(0),
    Weight = ifelse(Year < pars$years$dive, 1, 1.1666), Timing = as.integer(1)
  ) %>%
  arrange(Gear, Year)

# Make ADMB input data: number aged
numAgedADMB <- numAgedYearGear %>%
  tibble() %>%
  select(Year, Period, Age, Number) %>%
  mutate(
    Period = as.integer(Period), Area = as.integer(1), Group = as.integer(1),
    Sex = as.integer(0), Number = round(Number)
  ) %>%
  spread(key = Age, value = Number, fill = 0) %>%
  rename(Gear = Period) %>%
  mutate_all(as.integer) %>%
  arrange(Gear, Year)

# Make ADMB input data: weight-at-age (kg)
weightAgeADMB <- weightAge %>%
  mutate(
    Weight = round(Weight / 1000, digits = 4), Gear = as.integer(1),
    Area = as.integer(1), Group = as.integer(1), Sex = as.integer(0)
  ) %>%
  spread(key = Age, value = Weight) %>%
  arrange(Gear, Year)
write_csv(x = weightAgeADMB, file = paste0(region, "WtAge.csv"))

lengthAgeADMB <- lengthAge %>%
  mutate(
    Length = round(Length, digits = 4), Gear = as.integer(1),
    Area = as.integer(1), Group = as.integer(1), Sex = as.integer(0)
  ) %>%
  spread(key = Age, value = Length) %>%
  arrange(Gear, Year)
write_csv(x = lengthAgeADMB, file = paste0(region, "LenAge.csv"))

##### For SISCA (TMB) #####

# # # Data for sub-stock analyses (Landmark;SISCA/ISCAM data with some tweaks)
# catchTMB <- catchADMB %>%
#    mutate(Area = secSubNum, Stock = secSubName) %>%
#    select(Year, Gear, Area, Type, Value, Stock) 

# #No SOK for SOG (any SOK data was experimental and not included in analysis)
# if(toupper(region) != "SOG"){
#   catchTMBSOK <- catchSummary %>% 
#     filter(Year > 1974) %>% 
#     mutate(Gear = 6, Area = secSubNum, Type = 2, Value = SOK/1000, Stock = secSubName) %>%
#     select(Year, Gear, Area, Type, Value, Stock)
#   catchTMB <- bind_rows(catchTMB, catchTMBSOK)
# }
# 
# write_csv(catchTMB,
#    file = file.path("Summaries", paste("catchData", secSubName, ".csv", sep = "")), 
#    append = ifelse(secSubNum == 1, FALSE, TRUE))
# 
# if(send2sisca == TRUE){
#   write_csv(catchTMB,
#             file = file.path(paste0("../SISCAH/Data/", region), 
#                              paste("catchData.csv")), #", secSubName, ".csv", sep = "")),
#             append = ifelse(secSubNum == 1, FALSE, TRUE))
# }
# 
# spawnTMB <- spawnADMB %>%
#   mutate(Area = secSubNum, Stock = secSubName) %>%
#   select(Year, Spawn, Gear, Area, Group, Sex, Weight, Timing, Stock)
# 
# write_csv(spawnTMB,
#  file = file.path("Summaries", paste0("IdxData", secSubName, ".csv")), 
#  append = ifelse(secSubNum == 1, FALSE, TRUE))
# 
# if(send2sisca == TRUE){   
# write_csv(spawnTMB,
#   file   = file.path(paste0("../SISCAH/Data/", region), 
#            paste0("IdxData.csv")), #2", secSubName, ".csv")),
#   append = ifelse(secSubNum == 1, FALSE, TRUE))
# }
# 
# numAgedTMB <- numAgedADMB %>%
#    mutate(Area = secSubNum, Group = 1) %>%
#    rename(
#      a1 = `1`, a2 = `2`, a3 = `3`, a4 = `4`, a5 = `5`, a6 = `6`, a7 = `7`, a8 = `8`,
#      a9 = `9`, a10 = `10`) %>%
#    select(Year, Gear, Area, Group, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) 
# 
# write_csv(numAgedTMB,
#           file = file.path("Summaries", paste0("ageData", secSubName, ".csv")), 
#           append = ifelse(secSubNum == 1, FALSE, TRUE))
# 
# if(send2sisca == TRUE){   
#   write_csv(numAgedTMB,
#             file   = file.path(paste0("../SISCAH/Data/", region), 
#                                paste0("ageData.csv")), #2", secSubName, ".csv")),
#             append = ifelse(secSubNum == 1, FALSE, TRUE))
# }
# 
# weightAgeTMB <- weightAgeADMB %>%
#    mutate(Area = secSubNum, Stock = secSubName) %>%
#    rename(
#      a2 = `2`, a3 = `3`, a4 = `4`, a5 = `5`, a6 = `6`, a7 = `7`, a8 = `8`,
#      a9 = `9`, a10 = `10`) %>%
#    select(Year, Area, a2, a3, a4, a5, a6, a7, a8, a9, a10, Stock)
# 
# write_csv(weightAgeTMB,
#    file = file.path("Summaries", paste0("wtAgeMixed.csv")), #", secSubName, ".csv")), 
#    append = ifelse(secSubNum == 1, FALSE, TRUE))
# 
# if(send2sisca == TRUE){   
#   write_csv(weightAgeTMB,
#             file   = file.path(paste0("../SISCAH/Data/", region), 
#                                paste0("wtAgeMixed.csv")), #2", secSubName, ".csv",)),
#             append = ifelse(secSubNum == 1, FALSE, TRUE))
# }
# 
# BlendedIdx <- spawnSummary %>%
#   mutate(Surf = as.numeric(Surf), Macro = as.numeric(Macro), Under = as.numeric(Under)) %>%
#   replace(is.na(.), 0) %>%
#   mutate(Dive    = (Macro + Under)/1000,
#          Surface = Surf/1000) %>%
#   select(Year, Dive, Surface) 
#   
# write_csv(BlendedIdx, file = paste0("Summaries/", secSubName, "BlendedIdx.csv"))
# 
# if(send2sisca == TRUE){   
#   write_csv(BlendedIdx,
#             file   = file.path(paste0("../SISCAH/Data/", region, "/SplitIdx"), 
#                                paste0(region, "_BlendedIdx.csv")),
#             append = FALSE)
# }

# spawnYrTypeProp %>%
#   select(Year, Type, SI) %>%
#   mutate(SI = SI / 1000) %>%
#   pivot_wider(names_from = Type, values_from = SI) %>%
#   write_csv(file = paste(secSubName, "csv", sep="."))

# Write ADMB input file
WriteInputFile <- function(pADMB, cADMB, sADMB, nADMB, wADMB) {
  # Note that some values are printed with extra decimals (i.e., CC value 0.1078
  # becomes 0.10780000000000001) because of issues representing floating point
  # numbers, but the values are read back correctly in R (see emails)
  # Create the file name
  fName <- file.path(
    regName, paste("Herring", regName, max(yrRange), ".dat", sep = "")
  )
  # Start a connection (binary)
  out <- file(description = fName, open = "wb")
  
  # Initialize the file and write the start of the main header
  write(x = paste(rep("#", times = 80), collapse = ""), file = out, append = FALSE)
  # Write the main title
  write(
    x = "# Data file for Pacific Herring stock assessment using iSCAM",
    file = out, append = TRUE
  )
  # Write the region(s)
  write(x = paste("# Region(s):\t", PasteNicely(unique(areas$RegionName)),
                  sep = ""
  ), file = out, append = TRUE)
  # Write the subset of sections, if it applies
  if (!all(is.na(sectionSub))) {
    write(
      x = paste("# Section(s):\t", PasteNicely(sectionSub), sep = ""),
      file = out, append = TRUE
    )
  }
  # Write the date
  write(
    x = paste("# Created:\t", Sys.Date(), sep = ""), file = out, append = TRUE
  )
  # Write space for editing
  write(x = "# Edited:\t", file = out, append = TRUE)
  # Write the end of the main header
  write(
    x = paste(rep("#", times = 80), collapse = ""), file = out, append = TRUE
  )
  
  # Write header for model dimensions
  write(x = "#\n##### Model dimensions #####", file = out, append = TRUE)
  # Get number of distinct gears
  ngear <- n_distinct(c(cADMB$Gear, sADMB$Gear, nADMB$Gear, wADMB$Gear))
  # Get number of distinct areas
  narea <- n_distinct(c(cADMB$Area, sADMB$Area, nADMB$Area, wADMB$Area))
  # Get number of distinct groups
  ngroup <- n_distinct(c(cADMB$Group, sADMB$Group, nADMB$Group, wADMB$Group))
  # Get number of distinct sexes
  nsex <- n_distinct(c(cADMB$Sex, sADMB$Sex, nADMB$Sex, wADMB$Sex))
  # Write model dimensions
  write(
    x = paste(narea, "\t# Number of areas (narea)", sep = ""), file = out,
    append = TRUE
  )
  write(
    x = paste(ngroup, "\t# Number of groups (ngroup)", sep = ""), file = out,
    append = TRUE
  )
  write(
    x = paste(nsex, "\t# Number of sexes (nsex)", sep = ""), file = out,
    append = TRUE
  )
  write(
    x = paste(min(yrRange), "\t# First year (syr)", sep = ""), file = out,
    append = TRUE
  )
  write(
    x = paste(max(yrRange), "\t# Last year (nyr)", sep = ""), file = out,
    append = TRUE
  )
  write(
    x = paste(min(ageRange), "\t# Youngest age (sage)", sep = ""), file = out,
    append = TRUE
  )
  write(
    x = paste(max(ageRange), "\t# Plus group (nage)", sep = ""), file = out,
    append = TRUE
  )
  write(
    x = paste(ngear, "\t# Number of gears (ngear)", sep = ""), file = out,
    append = TRUE
  )
  
  # Write header for fishery flags
  write(x = "#\n##### Fishery flags #####", file = out, append = TRUE)
  # Determine first year of catch data to include
  yrFirstCatch <- cADMB %>%
    select(Year, Gear, Value) %>%
    pivot_wider(names_from = Gear, values_from = Value) %>%
    select(Year) %>%
    top_n(n = 20) %>%
    pull(Year) %>%
    min()
  # Determine ratio of catch since first year
  cHist <- cADMB %>%
    filter(Year >= yrFirstCatch) %>%
    group_by(Gear) %>%
    summarise(Total = SumNA(Value)) %>%
    ungroup() %>%
    mutate(Proportion = round(Total / SumNA(Total), digits = 4)) %>%
    complete(Gear = 1:ngear, fill = list(Proportion = 0)) %>%
    arrange(Gear)
  # Write fishery flags
  write(x = paste(paste(cHist$Proportion, collapse = "\t"),
                  "\t# TAC allocations (mean of last ", pADMB$General$HistoricCatch,
                  " years)",
                  sep = ""
  ), file = out, append = TRUE)
  # These are for testing only:
  # write( x=paste(paste(c(0.5, 0.3, 0.2, 0, 0), collapse="\t"),
  #         "\t# TAC allocations", sep=""), file=out, append=TRUE )
  
  # Write header for age and population parameters
  write(
    x = "#\n##### Age and population parameters #####", file = out,
    append = TRUE
  )
  # Loop over age and population parameters
  for (p in 1:length(pADMB$AgePopulation)) {
    # Write age and population parameters, and names
    write(x = paste(paste(pADMB$AgePopulation[[p]], collapse = ", "), "\t# ",
                    names(pADMB$AgePopulation)[p],
                    sep = ""
    ), file = out, append = TRUE)
  }
  
  # Write header for delay-difference data
  write(
    x = "#\n##### Delay-difference parameters (not used) #####", file = out,
    append = TRUE
  )
  # Loop over delay-difference parameters
  for (d in 1:length(pADMB$DelayDifference)) {
    # Write delay-difference parameters, and names
    write(x = paste(paste(pADMB$DelayDifference[[d]], collapse = ", "), "\t# ",
                    names(pADMB$DelayDifference)[d],
                    sep = ""
    ), file = out, append = TRUE)
  }
  
  # Write header for catch data
  write(x = "#\n##### Catch (t*10^3) #####", file = out, append = TRUE)
  # Write catch information
  write(
    x = paste(nrow(cADMB), "\t# Number of observations", sep = ""), file = out,
    append = TRUE
  )
  # Write catch column names and data
  write(
    x = paste("#", paste(colnames(cADMB), collapse = "\t")), file = out,
    append = TRUE
  )
  write_delim(x = cADMB, path = out, delim = "\t", append = TRUE)
  
  # Write header for spawn data
  write(x = "#\n##### Spawn (t*10^3) #####", file = out, append = TRUE)
  # Determine spawn dimensions
  dimSpawn <- sADMB %>%
    group_by(Gear) %>%
    summarise(Number = n()) %>%
    ungroup()
  # Write spawn information
  write(
    x = paste(nrow(dimSpawn), "\t\t# Number of survey types", sep = ""),
    file = out, append = TRUE
  )
  write(x = paste(paste(dimSpawn$Number, collapse = "\t"),
                  "\t# Number of years per survey",
                  sep = ""
  ), file = out, append = TRUE)
  write(
    x = paste(paste(rep(pADMB$General$SurveyType, times = nrow(dimSpawn)),
                    collapse = "\t"
    ), "\t# Survey type (1=vuln. number, ",
    "2=vuln. biomass, 3=spawn biomass)",
    sep = ""
    ), file = out, append = TRUE
  )
  # Write spawn column names and data
  write(
    x = paste("#", paste(colnames(sADMB), collapse = "\t")), file = out,
    append = TRUE
  )
  write_delim(x = sADMB, path = out, delim = "\t", append = TRUE)
  
  # Write header for number-at-age data
  write(x = "#\n##### Number-at-age #####", file = out, append = TRUE)
  # Determine model aged dimensions
  dimNumAged <- nADMB %>%
    group_by(Gear) %>%
    summarise(Number = n()) %>%
    ungroup()
  # Write number aged dimensions
  write(
    x = paste(nrow(dimNumAged), "\t\t\t# Number of gears", sep = ""),
    file = out, append = TRUE
  )
  write(x = paste(paste(dimNumAged$Number, collapse = "\t"),
                  "\t# Number of years per gear",
                  sep = ""
  ), file = out, append = TRUE)
  write(
    x = paste(paste(rep(min(ageRange), times = nrow(dimNumAged)),
                    collapse = "\t"
    ), "\t# Youngest age", sep = ""), file = out,
    append = TRUE
  )
  write(
    x = paste(paste(rep(max(ageRange), times = nrow(dimNumAged)),
                    collapse = "\t"
    ), "\t# Plus group", sep = ""), file = out,
    append = TRUE
  )
  write(
    x = paste(paste(rep(pADMB$General$SampleSize, times = nrow(dimNumAged)),
                    collapse = "\t"
    ), "\t# Effective sample size", sep = ""), file = out,
    append = TRUE
  )
  write(
    x = paste(paste(rep(pADMB$General$Composition, times = nrow(dimNumAged)),
                    collapse = "\t"
    ), "\t# Composition (1=age, 2=length)", sep = ""),
    file = out, append = TRUE
  )
  # Write number aged column names and data
  write(
    x = paste("#", paste(colnames(nADMB), collapse = "\t")), file = out,
    append = TRUE
  )
  write_delim(x = nADMB, path = out, delim = "\t", append = TRUE)
  
  # Write header for weight-at-age data
  write(x = "#\n##### Weight-at-age (kg) #####", file = out, append = TRUE)
  # Write weight-at-age dimensions
  # TODO: This should come from the data, or an input parameter
  write(
    x = paste(1, "\t# Number of weight-at-age tables", sep = ""), file = out,
    append = TRUE
  )
  write(
    x = paste(nrow(wADMB), "\t# Number of years", sep = ""), file = out,
    append = TRUE
  )
  # Write weight-at-age column names and data
  write(
    x = paste("#", paste(colnames(wADMB), collapse = "\t")), file = out,
    append = TRUE
  )
  write_delim(x = wADMB, path = out, delim = "\t", append = TRUE)
  
  # Write header for annual mean weight data
  write(
    x = "#\n##### Annual mean weight (not used) #####", file = out,
    append = TRUE
  )
  # Write annual mean weight dimensions
  # TODO: This should come from the data, or an input parameter
  write(
    x = paste(1, "\t# Number of annual mean weight tables"), file = out,
    append = TRUE
  )
  # TODO: This should come from the data, or an input parameter
  write(
    x = paste(0, "\t# Number of observations per table"), file = out,
    append = TRUE
  )
  
  # Write header for end of data file
  write(
    x = "#\n##### Marker for end of data file #####", file = out,
    append = TRUE
  )
  # End of file message
  write(x = paste(999, "\t# End of file"), file = out, append = TRUE)
  # Close the connection
  close(con = out)
} # End WriteInputFile function

# Write ADMB input file
WriteInputFile(
  pADMB = parsADMB, cADMB = catchADMB, sADMB = spawnADMB, nADMB = numAgedADMB,
  wADMB = weightAgeADMB
)

##### Figures #####

# Progress message
cat("Printing figures... ")

# Plot the BC coast and regions
BCMap <- ggplot(data = bc_coast) +
  geom_sf(fill = "lightgrey") +
  geom_sf(
    data = all_regions, linewidth = 0.5, fill = "transparent", colour = "black"
  ) +
  geom_sf_label(data = all_regions, alpha = 0.5, aes(label = Region)) +
  annotate(
    geom = "text", x = -125, y = 52, label = "British\nColumbia", size = 5
  ) +
  annotate(
    geom = "text", x = -131, y = 49.5, label = "Pacific\nOcean", size = 5
  ) +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(
    xlim = c(bc_bbox_small$xmin, bc_bbox_small$xmax),
    ylim = c(bc_bbox_small$ymin, bc_bbox_small$ymax), expand = FALSE) +
  myTheme 
ggsave(
  BCMap, filename = file.path(regName, "BC.png"), width = figWidth,
  height = min(6.9, 5.75 / bc_ratio_small), dpi = figRes
)

# Make a french version if requested
if (makeFrench) {
  # Make a png map in the main folder (english)
  ggsave( BCMap,
    filename = file.path("BC.png"), width = figWidth,
    height = min(7, 5.75 / shapes$xyAllRatio), dpi = figRes
  )
  # French SAR names (short)
  frenchSARs <- data.frame(
    Region = c("HG", "PRD", "CC", "SoG", "WCVI", "A27", "A2W"),
    RegionFR = c("HG", "DPR", "CC", "DG", "COIV", "Z27", "Z2O")
  )
  # Attach french names
  shapes$regCentDF <- shapes$regCentDF %>%
    left_join(y = frenchSARs, by = "Region")
  # Plot the BC coast and regions (french)
  BCMapFR <- ggplot(
    data = shapes$landAllCropDF, mapping = aes(x = Eastings, y = Northings)
  ) +
    geom_polygon(
      data = shapes$landAllCropDF, mapping = aes(group = group),
      fill = "lightgrey"
    ) +
    geom_point(data = shapes$extAllDF, colour = "transparent") +
    geom_path(
      data = shapes$regAllDF, aes(group = Region), size = 0.75, colour = "black"
    ) +
    geom_label(
      data = shapes$regCentDF, alpha = 0.5, mapping = aes(label = RegionFR)
    ) +
    annotate(
      geom = "text", x = 1100000, y = 800000, label = "Colombie-\nBritannique",
      size = 5
    ) +
    annotate(
      geom = "text", x = 650000, y = 550000, label = "Océan\nPacifique",
      size = 5
    ) +
    coord_equal() +
    labs(x = "Abcsisses (km)", y = "Ordonnées (km)", caption = geoProj) +
    scale_x_continuous(
      labels = function(x) comma(x / 1000, big.mark = " "), expand = c(0, 0)
    ) +
    scale_y_continuous(
      labels = function(x) comma(x / 1000, big.mark = " "), expand = c(0, 0)
    ) +
    myTheme
  ggsave(
    BCMapFR, filename = file.path("BC-FR.png"), width = figWidth,
    height = min(7, 5.75 / shapes$xyAllRatio), dpi = figRes
    )
} # End if making french

# Create a base map for the region
BaseMap <- ggplot(
  data = shapes$landCropDF, mapping = aes(x = Eastings, y = Northings)
) +
  geom_polygon(
    data = shapes$landCropDF, mapping = aes(group = group), fill = "lightgrey"
  ) +
  geom_point(data = shapes$extDF, colour = "transparent") +
  geom_path(
    data = shapes$regDF, mapping = aes(group = Region), size = 0.75,
    colour = "black", linetype = "dashed"
  ) +
  coord_equal() +
  labs(x = "Eastings (km)", y = "Northings (km)", caption = geoProj) +
  scale_x_continuous(labels = function(x) comma(x / 1000), expand = c(0, 0)) +
  scale_y_continuous(labels = function(x) comma(x / 1000), expand = c(0, 0)) +
  myTheme

# Plot the region, and statistical areas
RegionMap <- BaseMap +
  geom_path(
    data = shapes$saDF, mapping = aes(group = StatArea), size = 0.25,
    colour = "black"
  ) +
  geom_path(
    data = shapes$secDF, mapping = aes(group = Section), size = 0.25,
    colour = "black", linetype = "dotted"
  ) +
  {
    if (!is.null(shapes$grpDF) & region %in% c("CC", "SoG", "All")) {
      geom_polygon(
        data = shapes$grpDF, mapping = aes(group = id, fill = id), alpha = 0.25
      )
    }
  } +
  {
    if (nrow(shapes$saCentDF) >= 1) {
      geom_label(
        data = shapes$saCentDF, alpha = 0.25,
        mapping = aes(label = paste("SA", StatArea, sep = " "))
      )
    }
  } +
      # geom_polygon(
        # data = filter(shapes$grpDF, 
        #               id %in% c("Prt Louis/Chanal", "Seal/Rennel/Kano",
        #                         "Englefield", "Juan Perez/Skincuttle",
        #                         "Cumshewa/Selwyn")),
        # mapping = aes(group = id, fill = id), alpha = 0.25
      # ) +
  # guides(fill = FALSE)+
  #     geom_path(
  #       data = shapes$grpDF, mapping = aes(group = id), size = 0.25,
  #       colour = "black"
  #     ) +
  #     geom_label_repel(
  #       data = shapes$grpCentDF,
  #       mapping = aes(label = Group), alpha = 0.75, min.segment.length = 0
  #     ) +
  # }
  # } +
  scale_fill_viridis(discrete = TRUE) +
  labs(fill = "Group") +
  theme(legend.position = c(0.01, 0.01), legend.justification = c(0, 0)) 
  
ggsave(
  RegionMap, filename = file.path(regName, "Region.png"), width = figWidth,
  height = min(7.5, 6.5 / shapes$xyRatio), dpi = figRes
)

# Determine whether or not to show the catch zoom
showCatchZoom <- ifelse(max(catchPriv$Year) >= firstYrFig, TRUE, FALSE)

# Plot catch by year and gear type (i.e., period)
catchGearPlot <- ggplot(
  data = catchPriv, mapping = aes(x = Year, y = CatchPriv)
) +
  geom_bar(stat = "identity", position = "stack", aes(fill = Gear)) +
  geom_point(
    data = filter(catchPriv, Private),
    mapping = aes(x = Year, shape = Gear), y = 0
  ) +
  labs(y = expression(paste("Catch (t" %*% 10^3, ")", sep = ""))) +
  scale_x_continuous(breaks = yrBreaks) +
  scale_y_continuous(labels = function(x) comma(x / 1000)) +
  scale_fill_viridis(discrete = TRUE) +
  scale_shape_manual(values = c(1, 3, 4)) +
  guides(
    fill = guide_legend(order = 1),
    shape = guide_legend(order = 2, title = NULL)
  ) +
  expand_limits( x=yrRange, y=0 ) +
  myTheme +
  theme(legend.position = "top")
if(showCatchZoom){
  # Zoom into recent time
  catchGearPlotZoom <- catchGearPlot + 
    scale_x_continuous(
      breaks = yrBreaks, limits = c(firstYrFig, max(yrRange) + 1)
    ) +
    guides(fill = "none", shape = "none")
  # Add zoom to plot
  catchGearPlot <- plot_grid(
    catchGearPlot, catchGearPlotZoom, align = "v", ncol = 1, 
    rel_heights = c(1, 0.7)
  )
}
ggsave(
  catchGearPlot, filename = file.path(regName, "CatchGear.png"), 
  width = figWidth, height = ifelse(showCatchZoom, figWidth, figWidth * 0.6),
  dpi = figRes
)

# Plot it again, in French
# Instead of a different name, maybe put the plot in a /French subfolder; this
# might be easier to bring into the PDF..
# catchGearPlotFr <- catchGearPlot +
#    labs( x="Ann\\`{e}e", y=expression(paste("Capture (t"%*%10^3, ")", sep="")),
#        fill="Equipement" )  +  # ? ?
#    ggsave( filename=file.path(regName, "CatchGearFr.png"), width=figWidth,
#        height=figWidth*0.75, dpi=figRes )

# If weight by catch type
if (exists("weightCatchFig")) {
  # Plot weight by year and catch type
  weightCatchPlot <- ggplot(
    data = weightCatchFig,
    mapping = aes(
      x = Year, y = Weight, fill = SampleSource2,
      group = interaction(Year, SampleSource2)
    )
  ) +
    geom_boxplot(
      outlier.colour = "black", size = 0.25,
      position = position_dodge(preserve = "single")
      ) +
    labs(y = "Weight (g)", fill = "Sample type") +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_fill_viridis(discrete = TRUE) +
    geom_hline(
      data = weightCatchFigMu, mapping = aes(yintercept = MuWeight),
      size = 0.25, linetype = "dashed"
    ) +
    expand_limits(x = c(firstYrTab, max(yrRange))) +
    myTheme +
    theme(legend.position = "top") 
  ggsave(
    weightCatchPlot, filename = file.path(regName, "WeightCatch.png"),
    width = figWidth, height = 3.5, dpi = figRes
  )
  # # Plot length by year and catch type
  # lengthCatchPlot <- ggplot(
  #   data = weightCatchFig,
  #   mapping = aes(
  #     x = Year, y = Length, fill = SampleSource2,
  #     group = interaction(Year, SampleSource2)
  #   )
  # ) +
  #   geom_boxplot(
  #     outlier.colour = "black", size = 0.25,
  #     position = position_dodge(preserve = "single")
  #   ) +
  #   labs(y = "Length (mm)", fill = "Sample type") +
  #   scale_x_continuous(breaks = pretty_breaks()) +
  #   scale_fill_viridis(discrete = TRUE) +
  #   geom_hline(
  #     data = weightCatchFigMu, mapping = aes(yintercept = MuLength),
  #     size = 0.25, linetype = "dashed"
  #   ) +
  #   expand_limits(x = c(firstYrTab, max(yrRange))) +
  #   myTheme +
  #   theme(legend.position = "top") 
  # ggsave(
  #   lengthCatchPlot, filename = file.path(regName, "LengthCatch.png"),
  #   width = figWidth, height = 3.5, dpi = figRes
  # )
  # # Plot distribution of ages by gear and source, by year
  # ageDistGearPlot <- ggplot(
  #   data = weightCatchFig %>% filter(SampleSource2 != "Seine test"), 
  #   mapping = aes(x=Age, fill = SampleSource2)
  # ) +
  #   scale_fill_viridis(discrete = TRUE) +
  #   geom_bar(position = position_dodge(preserve = "single")) + 
  #   labs(y = "Density", fill = "Sample type") +
  #   scale_x_continuous(breaks = pretty_breaks()) +
  #   scale_y_continuous(labels = comma) +
  #   facet_wrap(~ Year, nrow = 4, scales = "free") +
  #   myTheme + 
  #   theme(legend.position = "top")
  # ggsave(
  #   ageDistGearPlot, filename = file.path(regName, "AgeDistGear.png"),
  #   width = figWidth, height = figWidth, dpi = figRes
  # )
  # weightCatchFig %>%
  #   filter(SampleSource2 != "Seine test") %>%
  #   group_by(Year, SampleSource2) %>%
  #   summarise(MeanAge = mean(Age), N=n()) %>%
  #   ungroup() %>%
  #   rename("SampleType" = SampleSource2) %>%
  #   write_csv(file = "MeanAge.csv")
} # End if weight by catch type

# If catch by stat area
if (exists("catchStatArea")) {
  # Plot catch by year and gear type (i.e., period)
  catchStatAreaPlot <- ggplot(data = catchStatAreaPriv, mapping = aes(
    x = Year, y = CatchPriv, fill = Year == max(yrRange)
  )) +
    geom_bar(stat = "identity", position = "stack") +
    geom_point(data = filter(catchStatAreaPriv, Private), y = 0, size = 0.5) +
    labs(y = expression(paste("Catch (t" %*% 10^3, ")", sep = ""))) +
    scale_x_continuous(breaks = yrBreaks) +
    scale_y_continuous(labels = function(x) comma(x / 1000)) +
    scale_fill_grey(start = 0.5, end = 0) +
    expand_limits(x = c(firstYrFig, max(yrRange)), y = 0) +
    facet_wrap(~SA, labeller = label_both) +
    myTheme +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  ggsave(
    catchStatAreaPlot, filename = file.path(regName, "CatchStatArea.png"),
    width = figWidth, height = figWidth / 2, dpi = figRes
  )
} # End if catch by stat area

# Plot biosample locations
if (nrow(bioLocations) > 0) {
  bioLocPlot <- BaseMap +
    geom_path(
      data = shapes$secDF, mapping = aes(group = Section), size = 0.25,
      colour = "black"
    ) +
    geom_text(
      data = shapes$secCentDF, alpha = 0.6, size = 2,
      mapping = aes(label = paste("Sec", Section, sep = " "))
    ) +
    geom_point(
      data = bioLocations, mapping = aes(shape = Type, size = Number),
      alpha = 0.6
    ) +
    scale_size(guide = guide_legend(order = 2), range = c(3, 6)) +
    scale_shape_manual(values = 15:18) +
    labs(shape = "Sample type") +
    guides(
      size = guide_legend(order = 1),
      shape = guide_legend(override.aes = list(size = 3))
    ) +
    theme(
      legend.justification = c(0, 0),
      legend.box.just = if (region %in% c("WCVI", "JS")) "top" else "left",
      legend.position = if (region == "PRD") "right" else c(0.01, 0.01),
      legend.box = if (region %in% c("WCVI", "JS")) {
        "horizontal"
      } else {
        "vertical"
      }
    )
  ggsave(
    bioLocPlot, filename = file.path(regName, "BioLocations.png"),
    width = figWidth, height = min(7.5, 6.5 / shapes$xyRatio), dpi = figRes
  )
}

# Plot proportion-at-age by year
propAgedPlot <- ggplot(data = numAgedYear, mapping = aes(x = Year)) +
  geom_point(mapping = aes(y = Age, size = Proportion)) +
  geom_path(data = qAgedYear, mapping = aes(y = MeanAge, group = GroupID)) +
  geom_ribbon(
    data = qAgedYear,
    mapping = aes(ymin = Lower, ymax = Upper, group = GroupID),
    alpha = 0.25
  ) +
  scale_size(range = c(0, 3)) +
  labs(x = NULL, y = "Age") +
  scale_x_continuous(breaks = yrBreaks) +
  scale_y_continuous(breaks = pretty_breaks()) +
  expand_limits(x = c(min(yrRange) - 0.5, max(yrRange) + 0.5)) +
  annotate(
    geom = "text", x = -Inf, y = Inf, label = "(a)", vjust = 1.3, hjust = -0.1
  ) +
  myTheme +
  theme(legend.position = "top", axis.text.x = element_blank())

# Plot number aged by year
numAgedPlot <- ggplot(data = numAgedYear, mapping = aes(x = Year, y = Number)) +
  geom_bar(stat = "identity", width = 0.9) +
  labs(y = "Number aged (thousands)") +
  scale_x_continuous(breaks = yrBreaks) +
  scale_y_continuous(labels = function(x) comma(x / 1000)) +
  expand_limits(x = yrRange, y = 0) +
  annotate(
    geom = "text", x = -Inf, y = Inf, label = "(b)", vjust = 1.3, hjust = -0.1
  ) +
  myTheme

# Arrange and save the proportion-at-age and number aged plots
pnPlots <- plot_grid(propAgedPlot, numAgedPlot,
                     align = "v",
                     ncol = 1, rel_heights = c(1, 0.7)
) 
ggsave(
  pnPlots, filename = file.path(regName, "ProportionAged.png"), width = figWidth,
  height = figWidth, dpi = figRes
)

# If proportion-at-age by group
if (exists("numAgedYearGrp")) {
  # Plot proportion-at-age by year and group
  propAgedGrpPlot <- ggplot(data = numAgedYearGrp, mapping = aes(x = Year)) +
    geom_point(mapping = aes(y = Age, size = Proportion)) +
    geom_path(
      data = qAgedYearGrp, mapping = aes(y = MeanAge, group = GroupID)
    ) +
    geom_ribbon(
      data = qAgedYearGrp,
      mapping = aes(ymin = Lower, ymax = Upper, group = GroupID), alpha = 0.25
    ) +
    scale_size(range = c(0, 3)) +
    labs(y = "Age") +
    scale_x_continuous(breaks = yrBreaks) +
    scale_y_continuous(breaks = pretty_breaks()) +
    expand_limits(x = c(min(yrRange) - 0.5, max(yrRange) + 0.5)) +
    myTheme +
    facet_grid(Group ~ .) +
    theme(legend.position = "top") 
  ggsave(
    propAgedGrpPlot, filename = file.path(regName, "PropAgedGroup.png"),
    width = figWidth, height = figWidth, dpi = figRes
  )
  # Plot number aged by year and group
  numAgedGrpPlot <- ggplot(
    data = numAgedYearGrp, mapping = aes(x = Year, y = Number)
  ) +
    geom_bar(stat = "identity", width = 0.9) +
    labs(y = "Number aged (thousands)") +
    scale_x_continuous(breaks = yrBreaks) +
    scale_y_continuous(labels = function(x) comma(x / 1000)) +
    expand_limits(x = yrRange, y = 0) +
    facet_grid(Group ~ .) +
    myTheme 
  ggsave(
    numAgedGrpPlot, filename = file.path(regName, "NumAgedGroup.png"),
    width = figWidth,height = figWidth, dpi = figRes
  )
} # End if proportion-at-age by group

# Proportion female plot
propFemalePlot <- ggplot(
  data=propFemale, mapping = aes(x = Year, y=Proportion)
) +
  geom_point() +
  geom_line() + 
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  labs(y = "Proportion female") +
  scale_x_continuous(breaks = yrBreaks) +
  myTheme 
ggsave(
  propFemalePlot, filename = file.path(regName, "PropFemale.png"),
  width = figWidth, height = figWidth, dpi = figRes
)

# If nearshore comparison
if (exists("compNear") & exists("compNearSA") & exists("nSampleSA")) {
  # Plot proportion-at-age
  compNearPlot <- ggplot(
    data = compNear, mapping = aes(x = Age, y = Proportion, fill = Sample)
  ) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(Year ~ .) +
    scale_x_continuous(
      breaks = seq(from = min(ageRange), to = max(ageRange), by = 2)
    ) +
    # scale_fill_viridis_d( ) +
    myTheme +
    theme(legend.position = "top")
  ggsave(
    compNearPlot, filename = file.path(regName, "CompNear.png"),
    width = figWidth, height = figWidth, dpi = figRes
  )
  # Plot proportion-at-age by stat area
  compNearPlotSA <- ggplot(
    data = compNearSA, mapping = aes(x = Age, y = Proportion, fill = Sample)
  ) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(Year ~ StatArea) +
    scale_x_continuous(
      breaks = seq(from = min(ageRange), to = max(ageRange), by = 2)
    ) +
    # scale_fill_viridis_d( ) +
    myTheme +
    theme(legend.position = "top")
  ggsave(
    compNearPlotSA, filename = file.path(regName, "CompNearSA.png"),
    width = figWidth, height = figWidth, dpi = figRes
  )
  # Plot number aged by stat area
  numNearPlotSA <- ggplot(
    data = nSampleSA, mapping = aes(x = Sample, y = Number, fill = Sample)
  ) +
    geom_bar(stat = "identity", position = "dodge") +
    guides(fill = FALSE) +
    facet_grid(Year ~ StatArea) +
    myTheme
  ggsave(
    numNearPlotSA, filename = file.path(regName, "NumNearSA.png"),
    width = figWidth, height = figWidth, dpi = figRes
  )
} # End if nearshore comparison

# Different plot for major vs others
if(regionType == "major"){
  # Plot weight- and length-at-age by year
  wtLenAgePlot <- ggplot(
    data = muWtLenAge, mapping = aes(x = Year, y = RollMean)
  ) +
    geom_point(
      data = filter(.data = muWtLenAge, Age == ageShow), mapping = aes(y = Value),
      shape = 1, size = 1
    ) +
    geom_line(mapping = aes(group = Age, colour = Age), size = 1) +
    scale_colour_viridis(guide = guide_legend(nrow = 1), discrete = TRUE) +
    scale_x_continuous(breaks = yrBreaks) +
    expand_limits(x = yrRange) +
    labs(y = NULL) +
    facet_wrap(Measure ~ .,
               scales = "free_y", strip.position = "left",
               labeller = as_labeller(c(
                 Weight = "Weight-at-age (g)",
                 Length = "Length-at-age (mm)"
               )), nrow = 2
    ) +
    myTheme +
    theme(
      legend.position = "top", strip.background = element_blank(),
      strip.placement = "outside"
    )
  ggsave(
    wtLenAgePlot, filename = file.path(regName, "WtLenAge.png"),
    width = figWidth, height = figWidth, dpi = figRes
  )
  # Plot percent change in weight- and length-at-age by year
  wtLenAgeChangePlot <- ggplot(
    data = filter(muWtLenAge, Age == ageShow),
    mapping = aes(x = Year, y = PctChange)
  ) +
    geom_bar(mapping = aes(fill = PctChange >= 0), stat = "identity") +
    labs(y = paste("Percent change for age-", ageShow, " fish (%)", sep = "")) +
    scale_x_continuous(breaks = yrBreaks) +
    scale_fill_viridis(discrete = TRUE) +
    expand_limits(x = yrRange) +
    guides(fill = FALSE) +
    facet_grid(Measure ~ ., scales = "free_y") +
    myTheme +
    theme(legend.position = "top") 
  ggsave(
    wtLenAgeChangePlot, filename = file.path(regName, "WtLenAgeChange.png"),
    width = figWidth, height = figWidth, dpi = figRes
  )
  # Plot percent change in weight- and length-at-age by year
  wtLenAgeChangePlot2 <- ggplot(
    data = filter(muWtLenAge, Age == ageShow2),
    mapping = aes(x = Year, y = PctChange)
  ) +
    geom_bar(aes(fill = PctChange >= 0), stat = "identity") +
    labs(y = paste("Percent change for age-", ageShow2, " fish (%)", sep = "")) +
    scale_x_continuous(breaks = yrBreaks) +
    scale_fill_viridis(discrete = TRUE) +
    expand_limits(x = yrRange) +
    guides(fill = FALSE) +
    facet_grid(Measure ~ ., scales = "free_y") +
    myTheme +
    theme(legend.position = "top") 
  ggsave(
    wtLenAgeChangePlot2, filename = file.path(regName, "WtLenAgeChange2.png"),
    width = figWidth, height = figWidth, dpi = figRes
  )
} else {
  # Plot weight- and length-at-age by year
  wtLenAgePlot <- ggplot(
    data = wtLenAge, mapping = aes(x = Year, y = Value)
  ) +
    geom_point(
      data = filter(.data = wtLenAge, Age == ageShow), mapping = aes(y = Value),
      shape = 1, size = 1
    ) +
    geom_line(mapping = aes(group = Age, colour = Age), size = 1) +
    scale_colour_viridis(guide = guide_legend(nrow = 1), discrete = TRUE) +
    scale_x_continuous(breaks = yrBreaks) +
    expand_limits(x = yrRange) +
    labs(y = NULL) +
    facet_wrap(Measure ~ .,
               scales = "free_y", strip.position = "left",
               labeller = as_labeller(c(
                 Weight = "Weight-at-age (g)",
                 Length = "Length-at-age (mm)"
               )), nrow = 2
    ) +
    myTheme +
    theme(
      legend.position = "top", strip.background = element_blank(),
      strip.placement = "outside"
    )
  ggsave(
    wtLenAgePlot, filename = file.path(regName, "WtLenAge.png"),
    width = figWidth, height = figWidth, dpi = figRes
  )
}

# If weight by age and group
if (exists("weightAgeGroup")) {
  # Plot weight by age and group
  weightAgeGroupPlot <- ggplot(
    data = weightAgeGroup,
    mapping = aes(
      x = Age, y = Weight, fill = Group, group = interaction(Age, Group)
    )
  ) +
    geom_boxplot(outlier.colour = "black", size = 0.25) +
    labs(y = "Weight (g)") +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
    facet_grid(. ~ Decade, labeller = label_both) +
    myTheme +
    theme(legend.position = "top") 
  ggsave(
    weightAgeGroupPlot, filename = file.path(regName, "WeightAgeGroup.png"),
    width = figWidth, height = figWidth * 0.67, dpi = figRes
  )
} # End if weight by age and group

# Wrangle data for spawn index by year
datYr <- siYearLoc %>%
  group_by(Year) %>%
  summarise(SITotal = SumNA(SITotal)) %>%
  ungroup() %>%
  mutate(
    Survey = ifelse(Year < pars$years$dive, "Surface", "Dive"),
    Survey = factor(Survey, levels = c("Surface", "Dive"))
  )

# Inset for spawnByLocPlot: spawn index vs year
subPlot <- ggplot(data = datYr, mapping = aes(x = Year, y = SITotal)) +
  geom_point(size = 0.25, aes(shape = Survey)) +
  geom_path(size = 0.2, aes(group = Survey)) +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = NULL) +
  guides(shape = FALSE) +
  theme_tufte() +
  theme(
    plot.background = element_rect(fill = alpha("white", 0.5),
                                   size = 0.1),
    plot.margin = unit(c(0.3, 0.6, 0.1, 0.1), "lines")
  ) 
# Convert to a grob
subGrob <- ggplotGrob(x = subPlot)

# Plot the spawn index locations
spawnByLocPlot <- BaseMap +
  geom_path(
    data = shapes$secDF, mapping = aes(group = Section), size = 0.25,
    colour = "black"
  ) +
  geom_text(
    data = shapes$secCentDF, alpha = 0.6, size = 2,
    mapping = aes(label = paste("Sec", Section, sep = " "))
  ) +
  geom_point(
    data = spawnByLocXY, mapping = aes(colour = TotalSI), alpha = 0.75, size = 4
  ) +
  labs(colour = "Spawn\nindex (t)") +
  theme(
    legend.justification = c(0, 0),
    legend.position = if (region == "PRD") "right" else c(0.01, 0.01)
  ) +
  annotation_custom(
    grob = subGrob,
    xmin = max(shapes$landCropDF$Eastings) -
      diff(range(shapes$landCropDF$Northings)) / 2.5,
    xmax = Inf,
    ymin = max(shapes$landCropDF$Northings) -
      diff(range(shapes$landCropDF$Northings)) / 5,
    ymax = Inf
  )

if(!all(is.na(spawnByLocXY$TotalSI))){ 
  spawnByLocPlot <- spawnByLocPlot + 
    scale_colour_viridis_c(labels = comma, na.value = "darkgrey")
} else {
  spawnByLocPlot <- spawnByLocPlot + 
    scale_colour_viridis_d(labels = comma, na.value = "darkgrey")
}
ggsave(
  spawnByLocPlot, filename = file.path(regName, "SpawnByLoc.png"),
  width = figWidth, height = min(7.5, 6.5 / shapes$xyRatio),
  dpi = figRes
)

# Plot the spawn index locations
spawnDecadePlot <- BaseMap +
  geom_path(
    data = shapes$secDF, mapping = aes(group = Section), size = 0.25,
    colour = "black"
  ) +
  geom_text(
    data = shapes$secCentDF, alpha = 0.6, size = 2,
    mapping = aes(label = paste("Sec", Section, sep = " "))
  ) +
  geom_point(
    data = spawnDecade, mapping = aes(colour = MeanSI, size = Frequency),
    alpha = 0.75
  ) +
  scale_colour_viridis(labels = comma) +
  scale_size(breaks = pretty_breaks(), guide = guide_legend(order = 2)) +
  labs(colour = "Mean spawn\nindex (t)") +
  theme(
    legend.justification = c(0, 0),
    legend.box.just = if (region %in% c("WCVI", "JS")) "top" else "left",
    legend.position = if (region == "PRD") "right" else c(0.01, 0.01),
    legend.box = if (region %in% c("WCVI", "JS")) {
      "horizontal"
    } else {
      "vertical"
    }
  ) 
ggsave(
  spawnDecadePlot, filename = file.path(regName, "SpawnDecade.png"),
  width = figWidth, height = min(7.5, 6.5 / shapes$xyRatio), dpi = figRes
)

# Basic spawn timing plot
spawnTimingPlot <- ggplot(data = spawnRaw, mapping = aes(x = StartDOY)) +
  annotate(
    geom = "rect", xmin = 60, xmax = 90, ymin = 0, ymax = Inf, fill = "grey",
    alpha = 0.5
  ) +
  geom_freqpoly(bins = 30, size = 0.5) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul"),
    limits = c(1, 182) 
  ) +
  labs(x = "Start of spawn", y = "Number of spawns") +
  {
    if (spawnTimingGroup) {
      facet_grid(Decade ~ Group, scales = "free_y")
    } else {
      facet_grid(Decade ~ StatArea, scales = "free_y")
    }
  } +
  myTheme +
  theme(
    legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1)
    ) 
ggsave(
  spawnTimingPlot, filename = file.path(regName, "SpawnTiming.png"),
  width = figWidth,height = figWidth * 1.25, dpi = figRes
)

# Plot total spawn length by year
spawnLengthPlot <- ggplot(
  data = spawnYr, mapping = aes(x = Year, y = TotalLength)
) +
  geom_point(mapping = aes(shape = Survey)) +
  geom_line(mapping = aes(group = Survey)) +
  #    geom_smooth( method=smLine, colour="black", level=ciLevel ) +
  labs(
    x = NULL,
    y = expression(paste("Total spawn length (m" %*% 10^3, ")", sep = ""))
  ) +
  scale_x_continuous(breaks = yrBreaks) +
  scale_y_continuous(labels = function(x) comma(x / 1000)) +
  annotate(
    geom = "text", x = -Inf, y = Inf, label = "(a)", vjust = 1.3, hjust = -0.1
  ) +
  #    guides( shape=FALSE ) +
  expand_limits(x = c(firstYrFig, max(yrRange)), y = 0) +
  myTheme +
  theme(axis.text.x = element_blank(), legend.position = "top")

# Plot mean spawn width by year
spawnWidthPlot <- ggplot(
  data = spawnYr, mapping = aes(x = Year, y = MeanWidth)
) +
  geom_point(mapping = aes(shape = Survey)) +
  geom_line(mapping = aes(group = Survey)) +
  #    geom_smooth( method=smLine, colour="black", level=ciLevel ) +
  labs(x = NULL, y = "Mean spawn width (m)") +
  scale_x_continuous(breaks = yrBreaks) +
  scale_y_continuous(labels = comma) +
  annotate(
    geom = "text", x = -Inf, y = Inf, label = "(b)", vjust = 1.3, hjust = -0.1
  ) +
  guides(shape = FALSE) +
  expand_limits(x = c(firstYrFig, max(yrRange)), y = 0) +
  myTheme +
  theme(axis.text.x = element_blank())

# Plot mean layers of spawn by year
spawnLayersPlot <- ggplot(
  data = spawnYr, mapping = aes(x = Year, y = MeanLayers)
) +
  geom_point(mapping = aes(shape = Survey)) +
  geom_line(mapping = aes(group = Survey)) +
  #    geom_smooth( method=smLine, colour="black", level=ciLevel ) +
  labs(y = "Mean number of egg layers") +
  scale_x_continuous(breaks = yrBreaks) +
  scale_y_continuous(labels = comma) +
  annotate(
    geom = "text", x = -Inf, y = Inf, label = "(c)", vjust = 1.3, hjust = -0.1
  ) +
  guides(shape = FALSE) +
  expand_limits(x = c(firstYrFig, max(yrRange)), y = 0) +
  myTheme

# Arrange and save length, width, and layer plots
lwlPlots <- plot_grid(spawnLengthPlot, spawnWidthPlot, spawnLayersPlot,
          align = "v", ncol = 1, rel_heights = c(1.2, 1, 1.1)
) 
ggsave(
  lwlPlots, filename = file.path(regName, "SpawnDimensions.png"), width = figWidth,
  height = figWidth * 1.25, dpi = figRes
)

# Plot spawn index by survey type: surface, macro, under
spawnIndexTypePlot <- ggplot(
  data = spawnYrType, mapping = aes(x = Year, y = SI)
) +
  geom_point(mapping = aes(shape = Survey)) +
  geom_line(mapping = aes(group = Survey)) +
  labs(y = expression(paste("Spawn index (t" %*% 10^3, ")", sep = ""))) +
  scale_x_continuous(breaks = yrBreaks) +
  scale_y_continuous(labels = function(x) comma(x / 1000)) +
  expand_limits(x = c(firstYrFig - 0.5, max(yrRange) + 0.5), y = 0) +
  facet_grid(Type ~ ., scales = "free_y", labeller = label_both) +
  myTheme +
  theme(legend.position = "top") 
ggsave(
  spawnIndexTypePlot, filename = file.path(regName, "SpawnIndexType.png"),
  width = figWidth, height = figWidth * 1.15, dpi = figRes
)

# HG rebuilding
# mu8292 <- spawnYr %>%
#   filter( Year %in% c(1982:1992)) %>%
#   pull(TotalSI) %>%
#   mean()

# Plot total spawn index by year
spawnIndexPlot <- ggplot(data = spawnYr, mapping = aes(x = Year, y = TotalSI)) +
  geom_point(mapping = aes(shape = Survey)) +
  geom_line(mapping = aes(group = Survey)) +
  # geom_hline(yintercept = mu8292) +
  # geom_hline(yintercept = 0.5 * mu8292, linetype = "dashed") +
  #    geom_smooth( method=smLine, colour="black", level=ciLevel ) +
  labs(
    x = NULL, y = expression(paste("Spawn index (t" %*% 10^3, ")", sep = ""))
  ) +
  scale_x_continuous(breaks = yrBreaks) +
  scale_y_continuous(labels = function(x) comma(x / 1000)) +
  #    guides( shape=FALSE ) +
  expand_limits(x = c(min(yrRange) - 0.5, max(yrRange) + 0.5), y = 0) +
  annotate(
    geom = "text", x = -Inf, y = Inf, label = "(a)", vjust = 1.3, hjust = -0.1
  ) +
  myTheme +
  theme(axis.text.x = element_blank(), legend.position = "top")

# # Plot relative abundance by year
# ScaledAbundPlot <- ggplot(data = relAbund, mapping = aes(x = Year, y = Abund)) +
#   geom_point(mapping = aes(shape = Survey)) +
#   geom_line(mapping = aes(group = Survey)) +
#   labs(
#     x = "Year",
#     y = expression(paste("Scaled abundance (t" %*% 10^3, ")", sep = ""))
#   ) +
#   scale_x_continuous(breaks = yrBreaks) +
#   scale_y_continuous(labels = function(x) comma(x / 1000)) +
#   expand_limits(x = c(min(yrRange) - 0.5, max(yrRange) + 0.5), y = 0) +
#   myTheme +
#   theme(legend.position = "top")
# 
# ggsave(
#   plot = ScaledAbundPlot, filename = file.path(regName, "ScaledAbundance.png"),
#   width = figWidth, height = 4, dpi = figRes
# )

# If using groups
if (exists("spawnYrGrp")) {
  # Plot percent spawn index by group
  spawnPercentSAStackPlot <- ggplot(
    data = spawnYrGrp, mapping = aes(x = Year, y = PercSI, fill = Group)
  ) +
    geom_bar(stat = "identity", width = 1, colour = "black", size = 0.1) +
    labs(x = NULL, y = "Spawn index (%)", fill = "Group") +
    scale_x_continuous(breaks = yrBreaks) +
    scale_y_continuous(labels = comma) +
    expand_limits(x = c(min(yrRange) - 0.5, max(yrRange) + 0.5), y = 0) +
    scale_fill_viridis(discrete = TRUE) +
    annotate(
      geom = "text", x = -Inf, y = Inf, label = "(b)", vjust = 1.3, hjust = -0.1
    ) +
    myTheme +
    theme(legend.position = "top", axis.text.x = element_blank())
} else { # End if using groups, otherwise stat areas
  # Plot percent spawn index by group (stat areas)
  spawnPercentSAStackPlot <- ggplot(
    data = spawnYrSA, mapping = aes(x = Year, y = PercSI, fill = StatArea)
  ) +
    geom_bar(stat = "identity", width = 1, colour = "black", size = 0.1) +
    labs(x = NULL, y = "Spawn index (%)", fill = "SA") +
    scale_x_continuous(breaks = yrBreaks) +
    scale_y_continuous(labels = comma) +
    expand_limits(x = c(min(yrRange) - 0.5, max(yrRange) + 0.5), y = 0) +
    scale_fill_viridis(discrete = TRUE) +
    annotate(
      geom = "text", x = -Inf, y = Inf, label = "(b)", vjust = 1.3, hjust = -0.1
    ) +
    myTheme +
    theme(legend.position = "top", axis.text.x = element_blank())
} # End if using stat areas

# Determine the number of sections
nSecCol <- n_distinct(spawnYrSec$Section)

# Plot proportion of spawn in each section and year
spawnPercentSecStackPlot <- ggplot(
  data = spawnYrSec, mapping = aes(x = Year, y = PercSI, fill = Section)
) +
  geom_bar(stat = "identity", width = 1, colour = "black", size = 0.1) +
  labs(y = "Spawn index (%)") +
  scale_x_continuous(breaks = yrBreaks) +
  scale_y_continuous(labels = comma) +
  expand_limits(x = c(min(yrRange) - 0.5, max(yrRange) + 0.5), y = 0) +
  scale_fill_viridis(
    guide = guide_legend(nrow = ceiling(nSecCol / 8)), discrete = TRUE
  ) +
  annotate(
    geom = "text", x = -Inf, y = Inf, label = "(c)", vjust = 1.3, hjust = -0.1
  ) +
  myTheme +
  theme(legend.position = "top")

# # For Brigitte
# secCC <- formatC(x = c(67, 72:78, 86), width = 3, format = "d", flag = "0")
# qVals <- tibble(Survey = c("Surface", "Dive"), q = c(0.326, 0.999))
# spawnYrSecCC <- spawnYrSec %>%
#   filter(Section %in% secCC, !is.na(Survey)) %>%
#   left_join( y=qVals, by="Survey") %>%
#   mutate(
#     Biomass = TotalSI/q,
#     Survey = factor(Survey, levels = c("Surface", "Dive"))
#   ) %>%
#   select(Year, Section, Survey, TotalSI, q, Biomass)
# sectionsCCPlot <- ggplot(
#   data = spawnYrSecCC, mapping = aes(x=Year, y=Biomass)
# ) +
#   geom_point(mapping = aes(shape = Survey), size = 1) +
#   geom_line(mapping = aes(group = Survey), size = 0.5) +
#   labs(y = expression(paste("Scaled abundance (t" %*% 10^3, ")", sep = ""))) +
#   scale_x_continuous(breaks = yrBreaks) +
#   scale_y_continuous(labels = function(x) comma(x / 1000)) +
#   expand_limits(x = c(firstYrFig - 0.5, max(yrRange) + 0.5), y = 0) +
#   facet_wrap(~ Section, labeller = label_both, scales = "free_y") +
#   myTheme +
#   theme(
#     legend.position = "top",
#     axis.text.x = element_text(angle = 45, hjust = 1)) +
#   ggsave(
#     filename = "SectionsCC.png", width = figWidth, height = figWidth*0.67,
#          dpi = figRes
#   )

# Plot percent change in spawn by year
spawnChangePlot <- ggplot(
  data = spawnYr, mapping = aes(x = Year, y = PctChange)
) +
  geom_bar(aes(fill = PctChange >= 0), stat = "identity") +
  annotate(
    geom = "text", x = -Inf, y = Inf, label = "(b)", vjust = 1.3, hjust = -0.1
  ) +
  scale_fill_grey(start = 0, end = 0.5) +
  scale_x_continuous(breaks = yrBreaks) +
  expand_limits(x = c(min(yrRange) - 0.5, max(yrRange) + 0.5), y = 0) +
  labs(y = "Percent change (%)") +
  guides(fill = FALSE) +
  myTheme

# Arrange and save the index and proportion plots
ipPlots <- plot_grid(spawnIndexPlot, spawnPercentSAStackPlot,
                     spawnPercentSecStackPlot,
                     align = "v", ncol = 1,
                     rel_heights = c(2.1, 2.5, 2.5)
) 
ggsave(
  ipPlots, filename = file.path(regName, "SpawnIndexPercent.png"),
  width = figWidth, height = 6.7, dpi = figRes
)

# Arrange and save the spawn index and percent change plots
pctPlots <- plot_grid(spawnIndexPlot, spawnChangePlot,
                      align = "v", ncol = 1, rel_heights = c(2.1, 2.1)
) 
ggsave(
  pctPlots, filename = file.path(regName, "SpawnIndexChange.png"),
  width = figWidth, height = figWidth, dpi = figRes
)

# # Arrange and save the spawn index and percent change plots
# pctPlots <- plot_grid(spawnIndexPlot, spawnChangePlot, spawnPercentSecStackPlot,
#                       align = "v", ncol = 1, rel_heights = c(2.3, 2.1, 2.5)
# ) +
#   ggsave(
#     filename = file.path(regName, "SpawnIndexChange.png"), width = figWidth,
#     height = figWidth, dpi = figRes
#   )

# Plot proportion of spawn from surface vs dive surveys
spawnTypePropPlot <- ggplot(
  data = spawnYrTypeProp, mapping = aes(x = Year, y = Prop, fill = Type)
) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = pars$years$dive - 0.5, linetype = "dashed") +
  labs(y = "Proportion") +
  scale_x_continuous(breaks = yrBreaks) +
  scale_fill_viridis_d() +
  myTheme +
  theme(legend.position = "top") 
ggsave(
  spawnTypePropPlot, filename = file.path(regName, "SpawnTypeProp.png"),
  width = figWidth, height = figWidth * 0.67, dpi = figRes
)

# Plot percent contribution by Section faceted by Stat Area or Group
PlotPCSecSA <- function(dat) {
  # Rename the Statistical Area column
  dat <- rename(.data = dat, SA = StatArea)
  # Start a list to hold plots
  pList <- list()
  # If aggregating by group
  if (exists("spawnYrGrp")) {
    # Group name
    gName <- "Group"
    # Remove NAs
    dat <- dat %>%
      filter(!is.na(Group))
  } else { # End if aggregating by group, otherwise
    # Group name
    gName <- "SA"
    # Remove NAs
    dat <- dat %>%
      filter(!is.na(SA))
  } # End if aggregating by StatArea
  # Get unique group names
  uGroups <- unique(dat[[gName]])[order(unique(dat[[gName]]))]
  # Get the number of plots
  nPlots <- length(uGroups)
  # Get the maximum y range
  yUpper <- dat %>%
    group_by_("Year", gName) %>%
    summarise(Total = SumNA(PercSI, omitNA = TRUE)) %>%
    ungroup()
  # Get the y limits
  yLims <- c(0, max(yUpper$Total, na.rm = TRUE))
  # Loop over groups
  for (i in 1:nPlots) {
    # Subset the data
    datSub <- dat[dat[[gName]] == uGroups[i], ]
    # Make the plot
    pList[[i]] <- ggplot(
      data = datSub,
      mapping = aes_string(x = "Year", y = "PercSI", fill = "Section")
    ) +
      geom_bar(stat = "identity", width = 1, colour = "black", size = 0.1) +
      labs(x = NULL, y = NULL) +
      scale_x_continuous(breaks = yrBreaks) +
      scale_y_continuous(labels = comma) +
      expand_limits(x = c(firstYrFig, max(yrRange)), y = yLims) +
      scale_fill_viridis(
        guide =
          guide_legend(ncol = ceiling(length(unique(datSub$Section)) / 6)),
        discrete = TRUE
      ) +
      facet_grid(paste(gName, "~ ."), labeller = label_both) +
      myTheme
    # Update the plot if it's not the bottom plot
    if (i != nPlots) {
      pList[[i]] <- pList[[i]] +
        theme(axis.text.x = element_blank())
    }
  } # End i loop over groups
  # Arrange into a grid
  pGrid <- plot_grid(plotlist = pList, align = "v", ncol = 1)
  # Make a title for the x-axis
  titleX <- ggdraw() +
    draw_label("Year", size = 12, x = 0.4)
  # Combine the x-axis title
  pGridX <- plot_grid(pGrid, titleX, ncol = 1, rel_heights = c(1, 0.05))
  # Make a title for the y-axes
  titleY <- ggdraw() +
    draw_label("Spawn index (%)", angle = 90, size = 12)
  # Combine the y-axis title, and save the plots
  pGridXY <- plot_grid(titleY, pGridX, ncol = 2, rel_widths = c(0.04, 1))
  ggsave(
    pGridXY, filename = file.path(regName, "SpawnPercentGrid.png"),
    width = figWidth, height = min(7.1, length(pList) * 3), dpi = figRes
  )
  # Return the plot list
  return(pGridXY)
} # End PlotPCSecSA function

# Get the list of plots showing percent contribution
plotGridPCs <- PlotPCSecSA(dat = spawnYrSec)

# Plot proportion of spawn in each section and year
spawnPercentPanelPlot <- ggplot(
  data = spawnYrSec,
  mapping = aes(x = Year, y = PercSI, fill = Year == max(yrRange))
) +
  geom_bar(stat = "identity") +
  labs(y = "Spawn index (%)") +
  scale_x_continuous(breaks = yrBreaks) +
  scale_y_continuous(labels = comma) +
  scale_fill_grey(start = 0.5, end = 0) +
  expand_limits(x = c(firstYrFig, max(yrRange))) +
  facet_wrap(~Section, labeller = label_both, drop = TRUE, ncol = 3) +
  myTheme +
  theme(
    legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)
  ) 
ggsave(
  spawnPercentPanelPlot, filename = file.path(regName, "SpawnPercentPanel.png"),
  width = figWidth, dpi = figRes,
  height = min(7.5, 1.5 * (ceiling(n_distinct(spawnYrSec$Section) / 3)))
)

# Plot index of spawn in each section and year
spawnIndexPanelPlot <- ggplot(
  data = spawnYrSec,
  mapping = aes(x = Year, y = TotalSI, fill = Year == max(yrRange))
) +
  geom_bar(stat = "identity") +
  labs(y = expression(paste("Spawn index (t" %*% 10^3, ")", sep = ""))) +
  scale_x_continuous(breaks = yrBreaks) +
  scale_y_continuous(labels = function(x) comma(x / 1000)) +
  scale_fill_grey(start = 0.5, end = 0) +
  expand_limits(x = c(firstYrFig, max(yrRange))) +
  facet_wrap(~Section, labeller = label_both, drop = TRUE, ncol = 3) +
  myTheme +
  theme(legend.position = "none") 
ggsave(
  spawnIndexPanelPlot, filename = file.path(regName, "SpawnIndexPanel.png"),
  width = figWidth, dpi = figRes,
  height = min(7.9, 1.5 * (ceiling(n_distinct(spawnYrSec$Section) / 3)))
)

# If length by sampling protocol
if (exists("lenAgeSample")) {
  # Compare the two sampling protocols (length)
  lenAgeSamplePlot <- ggplot(
    data = lenAgeSample, mapping = aes(
      x = Age, y = Length, fill = SampleSource2,
      group = interaction(Age, SampleSource2)
    )
  ) +
    geom_boxplot(outlier.colour = "black", size = 0.25) +
    labs(y = "Length (mm)", fill = "Sample") +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
    myTheme +
    theme(legend.position = "top")
  ggsave(
    lenAgeSamplePlot, filename = file.path(regName, "LengthAgeSample.png"),
    width = figWidth,height = figWidth * 0.67, dpi = figRes
  )
  # Compare the two sampling protocols by StatArea (length)
  lenAgeSamplePlotSA <- ggplot(
    data = lenAgeSample, mapping = aes(
      x = Age, y = Length, fill = SampleSource2,
      group = interaction(Age, SampleSource2)
    )
  ) +
    geom_boxplot(outlier.colour = "black", size = 0.25) +
    labs(y = "Length (mm)", fill = "Sample") +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
    myTheme +
    theme(legend.position = "top") +
    facet_grid(SA ~ ., labeller = label_both)
  ggsave(
    lenAgeSamplePlotSA, filename = file.path(regName, "LengthAgeSampleSA.png"),
    width = figWidth, height = figWidth, dpi = figRes
  )
  # Compare the two sampling protocols by StatArea and year (length)
  lenAgeSamplePlotSAYr <- ggplot(
    data = lenAgeSample, mapping = aes(
      x = Age, y = Length, fill = SampleSource2,
      group = interaction(Age, SampleSource2)
    )
  ) +
    geom_boxplot(outlier.colour = "black", size = 0.25) +
    labs(y = "Length (mm)", fill = "Sample") +
    scale_x_continuous(breaks = pretty_breaks()) +
    # scale_fill_viridis_d( alpha=0.5 ) +
    myTheme +
    theme(legend.position = "top") +
    facet_grid(Year ~ SA, labeller = label_both)
  ggsave(
    lenAgeSamplePlotSAYr,
    filename = file.path(regName, "LengthAgeSampleSAYr.png"),
    width = figWidth, height = figWidth, dpi = figRes
  )
  # Compare age distributions by year and stat area
  ageDistSamplePlotSA <- ggplot(
    data = lenAgeSample,
    mapping = aes(
      x = Year, y = Age, fill = SampleSource2,
      group = interaction(Year, SampleSource2)
    )
  ) +
    geom_boxplot(outlier.colour = "black", size = 0.25) +
    labs(fill = "Sample") +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
    myTheme +
    theme(legend.position = "top") +
    facet_grid(SA ~ ., labeller = label_both)
  ggsave(
    ageDistSamplePlotSA, filename = file.path(regName, "AgeDistSampleSA.png"),
    width = figWidth, height = figWidth, dpi = figRes
  )
} # End if length by sampling protocol

# If spawn depth by year and statistical area
if (exists("spawnStatsYrSA") & exists("spawnStatsYrSec")) {
  # Determine the number of sections
  nSecCol <- n_distinct(spawnStatsYrSec$Section)
  # Plot spawn depth (maximum by spawn number) in each statistical area and year
  spawnDepthSAPlot <- ggplot(
    data = spawnStatsYrSA, mapping = aes(
      x = Year, y = Depth, fill = StatArea,
      group = interaction(Year, StatArea)
    )
  ) +
    geom_boxplot(outlier.colour = "black", outlier.size = 0.5, size = 0.25) +
    expand_limits(x = c(firstYrFig, max(yrRange)), y = 0) +
    scale_fill_viridis(discrete = TRUE) +
    labs(x = NULL, y = "Maximum spawn depth (m)", fill = "SA") +
    scale_x_continuous(breaks = yrBreaks) +
    scale_y_reverse() +
    annotate(
      geom = "text", x = -Inf, y = -Inf, label = "(a)", vjust = 1.3,
      hjust = -0.1
    ) +
    myTheme +
    theme(legend.position = "top", axis.text.x = element_blank())
  # Plot spawn depth (maximum by spawn number) in each section and year
  spawnDepthSecPlot <- ggplot(
    data = spawnStatsYrSec, mapping = aes(
      x = Year, y = Depth, fill = Section, group = interaction(Year, Section)
    )
  ) +
    geom_boxplot(outlier.colour = "black", outlier.size = 0.5, size = 0.25) +
    expand_limits(x = c(firstYrFig, max(yrRange)), y = 0) +
    scale_fill_viridis(discrete = TRUE) +
    labs(y = "Maximum spawn depth (m)") +
    scale_x_continuous(breaks = yrBreaks) +
    scale_y_reverse() +
    annotate(
      geom = "text", x = -Inf, y = -Inf, label = "(b)", vjust = 1.3,
      hjust = -0.1
    ) +
    guides(fill = guide_legend(nrow = ceiling(nSecCol / 8))) +
    myTheme +
    theme(legend.position = "bottom")
  # Arrange and save the depth plots
  depthPlots <- plot_grid(spawnDepthSAPlot, spawnDepthSecPlot,
                          align = "v", ncol = 1, rel_heights = c(2, 2)
  )
  ggsave(
    depthPlots, filename = file.path(regName, "SpawnDepthSASec.png"),
    width = figWidth, height = figWidth, dpi = figRes
  )
  # Plot spawn layers in each statistical area and year
  spawnLayersSAPlot <- ggplot(
    data = spawnStatsYrSA, mapping = aes(
      x = Year, y = Layers, fill = StatArea,
      group = interaction(Year, StatArea)
    )
  ) +
    geom_boxplot(outlier.colour = "black", outlier.size = 0.5, size = 0.25) +
    scale_fill_viridis(discrete = TRUE) +
    expand_limits(x = c(firstYrFig, max(yrRange)), y = 0) +
    labs(x = NULL, y = "Number of egg layers", fill = "SA") +
    scale_x_continuous(breaks = yrBreaks) +
    annotate(
      geom = "text", x = -Inf, y = Inf, label = "(a)", vjust = 1.3,
      hjust = -0.1
    ) +
    myTheme +
    theme(legend.position = "top", axis.text.x = element_blank())
  # Plot spawn layers in each section and year
  spawnLayersSecPlot <- ggplot(
    data = spawnStatsYrSec, mapping = aes(
      x = Year, y = Layers, fill = Section,
      group = interaction(Year, Section)
    )
  ) +
    geom_boxplot(outlier.colour = "black", outlier.size = 0.5, size = 0.25) +
    expand_limits(x = c(firstYrFig, max(yrRange)), y = 0) +
    labs(y = "Number of egg layers") +
    scale_x_continuous(breaks = yrBreaks) +
    annotate(
      geom = "text", x = -Inf, y = Inf, label = "(b)", vjust = 1.3,
      hjust = -0.1
    ) +
    guides(fill = guide_legend(nrow = ceiling(nSecCol / 8))) +
    myTheme +
    theme(legend.position = "bottom")
  # Arrange and save the depth plots
  layerPlots <- plot_grid(spawnLayersSAPlot, spawnLayersSecPlot,
                          align = "v", ncol = 1, rel_heights = c(2, 2)
  ) 
  ggsave(
    layerPlots, filename = file.path(regName, "SpawnLayersSASec.png"),
    width = figWidth, height = figWidth, dpi = figRes
  )
} # End if spawn depth by year and statistical area

# Plot length at age if data
if (nrow(lenAge) > 0) {
  lengthAgePlot <- ggplot(data = lenAge, mapping = aes(x = Length)) +
    geom_bar() +
    labs(x = "Length (mm)", y = "Count") +
    facet_wrap(Age ~ ., ncol = 2, dir = "v", scales = "free_y") +
    myTheme 
  ggsave(
    lengthAgePlot, filename = file.path(regName, "LengthAge.png"),
    width = figWidth,height = figWidth * 0.67, dpi = figRes
  )
}

# Show spawn index by locations by year
PlotLocationsYear <- function(dat) {
  # Wrangle data for spawn index by year
  datYr <- dat %>%
    group_by(Year) %>%
    summarise(SITotal = SumNA(SITotal)) %>%
    ungroup() %>%
    mutate(
      Survey = ifelse(Year < pars$years$dive, "Surface", "Dive"),
      Survey = factor(Survey, levels = c("Surface", "Dive"))
    )
  # Get the number of plots
  uPages <- unique(dat$Year)
  # Set up the map
  MapGIF <- BaseMap +
    geom_path(
      data = shapes$secDF, mapping = aes(group = Section), size = 0.25,
      colour = "black"
    ) +
    geom_text(
      data = shapes$secCentDF, alpha = 0.6, size = 2,
      mapping = aes(label = paste("Sec", Section, sep = " "))
    )
  # Start the PDF
  pdf(
    file = file.path(regName, "SpawnIndexAnimation.pdf"), width = figWidth,
    height = min(6.75, 6.5 / shapes$xyRatio)
  )
  # Loop over pages/years
  for (i in 1:length(uPages)) {
    # Get the index (up to 9999)
    iLong <- formatC(uPages[i], width = 4, flag = "0")
    # The plot
    layersPlot <- MapGIF +
      facet_wrap_paginate(~Year,
                          ncol = 1, nrow = 1, page = i, labeller = label_both
      ) +
      geom_point(data = dat, aes(colour = SITotal), size = 4, alpha = 0.75) +
      scale_colour_viridis(labels = comma) +
      labs(colour = "Spawn\nindex (t)") +
      theme(
        legend.justification = c(0, 0),
        legend.position = if (region == "PRD") "right" else c(0.01, 0.01)
      )
    # Inset: spawn index vs year
    subPlot <- ggplot(data = datYr, mapping = aes(x = Year, y = SITotal)) +
      geom_point(size = 0.25, aes(shape = Survey)) +
      geom_point(data = datYr[i, ], aes(shape = Survey), size = 1.5) +
      geom_path(size = 0.2, aes(group = Survey)) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = NULL) +
      guides(shape = FALSE) +
      theme_tufte() +
      theme(
        plot.background = element_rect(fill = alpha("white", 0.5), size = 0.1),
        plot.margin = unit(c(0.3, 0.6, 0.1, 0.1), "lines")
      )
    # Convert to a grob
    subGrob <- ggplotGrob(x = subPlot)
    # Determine the x location for the grob
    grobLeft <- max(shapes$landCropDF$Eastings) -
      diff(range(shapes$landCropDF$Northings)) / 2.5
    # Determine the y location for the grob
    grobBottom <- max(shapes$landCropDF$Northings) -
      diff(range(shapes$landCropDF$Northings)) / 5
    # Add the inset to the map
    finalPlot <- layersPlot +
      annotation_custom(
        grob = subGrob, xmin = grobLeft, xmax = Inf,
        ymin = grobBottom, ymax = Inf
      )
    # Print the main plot
    print(finalPlot)
  } # End i loop over years
  # Turn the device off
  dev.off()
  # Save a copy (for later, if desired)
  file.copy(
    from = file.path(regName, "SpawnIndexAnimation.pdf"),
    to = file.path(
      "Animations",
      paste("SpawnIndexAnimation", regName, "pdf", sep = ".")
    ),
    overwrite = TRUE
  )
} # End PlotLocationsYear

# Make the animation
if (makeAnimation || 
    !paste("SpawnIndexAnimation", regName, "pdf", sep = ".") %in% 
    list.files(path = "Animations")) {
  # Use 64-bit R if you encounter memory issues
  if(system64) {
    # Location of script
    scriptLoc <- "C:/Grinnell/Git/DataSummaries/MakeAnimation.R"
    # Save the workspace image
    save.image(file = "Temp.RData")
    # Call R via system: takes a few mins
    system(paste0(Sys.getenv("R_HOME"), "/bin/x64/Rscript.exe ", scriptLoc), 
           wait = TRUE, invisible = FALSE, intern = TRUE)
    # Remove the image
    file.remove(file = "Temp.RData")
  } else { # End if 64-bit, otherwise
    # Show spawn locations (this takes a few minutes!)
    PlotLocationsYear(dat = siYearLoc)
  } # End if 32-bit
} else { # End if making the animation, otherwise
  # Copy the saved version
  file.copy(
    from = file.path(
      "Animations",
      paste("SpawnIndexAnimation", regName, "pdf", sep = ".")
    ),
    to = file.path(regName, "SpawnIndexAnimation.pdf")
  )
} # End if not making the animation

# Plot food and bait animation
PlotFoodBait <- function(dat) {
  # Get range for catch values
  catRange <- range(dat$Catch)
  # Get the number of plots
  uPages <- unique(dat$Year)
  # Set up the map
  MapGIF <- BaseMap +
    geom_path(
      data = shapes$secDF, mapping = aes(group = Section), size = 0.25,
      colour = "black"
    )
  # Start the PDF
  pdf(
    file = file.path(regName, "FoodBaitAnimation.pdf"), width = figWidth,
    height = min(6.75, 6.5 / shapes$xyRatio)
  )
  # Loop over pages/years
  for (i in 1:length(uPages)) {
    # Get data for ith year
    iDat <- dat %>%
      filter(Year == uPages[i]) %>%
      mutate(Section = as.character(Section)) %>%
      rename(id = Section)
    # Colour sections
    iSection <- shapes$secDF %>%
      left_join(y = iDat, by = "id") %>%
      complete(Year = yrRange)
    # The plot
    layersPlot <- MapGIF +
      facet_wrap_paginate(
        ~Year, ncol = 1, nrow = 1, page = i, labeller = label_both
      ) +
      geom_polygon(
        data = iSection, mapping = aes(group = Section, fill = Catch),
        linewidth = 0.25, colour = "black", alpha = 0.75
      ) +
      scale_fill_viridis(labels = comma, limits = catRange) +
      labs(fill = "Catch (t)") +
      geom_text(
        data = shapes$secCentDF, alpha = 0.6, size = 2,
        mapping = aes(label = paste("Sec", Section, sep = " "))
      ) +
      theme(
        legend.justification = c(0, 0),
        legend.position = if (region == "PRD") "right" else c(0.01, 0.01)
      )
    # Result
    finalPlot <- layersPlot
    # Print the main plot
    print(finalPlot)
  } # End i loop over years
  # Turn the device off
  dev.off()
  # Save a copy (for later, if desired)
  file.copy(
    from = file.path(regName, "FoodBaitAnimation.pdf"),
    to = file.path(
      "Animations",
      paste("FoodBaitAnimation", regName, "pdf", sep = ".")
    ),
    overwrite = TRUE
  )
}

# Plot food and bait
# PlotFoodBait(dat = catchFB)

# Update progress
cat("done\n")

##### xTables #####

# Format regions table
xRegions <- regions %>%
  rename(Name = RegionName, Code = Region) %>%
  select(Name, Code, Type) %>%
  xtable()

# Write regions to disc
print(
  x = xRegions, file = file.path(regName, "Regions.tex"),
  include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
  NA.string = NA
)

# Format commercial catch
xCatchCommUseYr <- catchCommUseYr %>%
  rename("Catch (t)" = Catch) %>%
  xtable()

# Write commercial catch to disc
print(
  x = xCatchCommUseYr, file = file.path(regName, "CatchCommUseYr.tex"),
  include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
  NA.string = NA
)

# Format commercial harvest (recent years)
xHarvestSOK <- harvestSOK %>%
  filter(Year >= firstYrTab) %>%
  arrange(Year) %>%
  mutate(
    Year = as.integer(Year),
    Harvest = format(Harvest, big.mark = ",", digits = 0, scientific = FALSE),
    Biomass = format(Biomass, big.mark = ",", digits = 0, scientific = FALSE)
  ) %>%
  rename("Harvest (lb)" = Harvest, "Spawning biomass (t)" = Biomass) %>%
  xtable()

# Write commercial harvest to disc
print(
  x = xHarvestSOK, file = file.path(regName, "HarvestSOK.tex"),
  include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
  NA.string = NA
)

###### This stuff is for the stock assessment research document #####
# write_csv( x=allHarvSOK, file=paste("allHarvSOK", regName, ".csv", sep="") )
# pSOK1 <- ggplot( data=allHarvSOK, aes(x=Year, y=Harvest) ) +
#    geom_bar( stat="identity", aes(fill=Year==max(yrRange)) ) +
#    scale_fill_grey( start=0.5, end=0 ) +
#    labs( y=expression(paste("Harvest (kg"%*%10^3, ")", sep="")) )  +
#    scale_y_continuous( labels=function(x) comma(x/1000) ) +
#    guides( fill=FALSE ) +
#    myTheme +
#    ggsave( filename=paste("HarvestSOK", regName, ".png", sep=""),
#        width=figWidth, height=figWidth*0.67, dpi=figRes )
# pSOK2 <- ggplot( data=allHarvSOK, aes(x=Year, y=Biomass) ) +
#    geom_bar( stat="identity", aes(fill=Year==max(yrRange)) ) +
#    scale_fill_grey( start=0.5, end=0 ) +
#    labs( y=expression(paste("Spawning biomass (t"%*%10^3, ")", sep="")) )  +
#    scale_y_continuous( labels=function(x) comma(x/1000) ) +
#    guides( fill=FALSE ) +
#    myTheme +
#    ggsave( filename=paste("BiomassSOK", regName, ".png", sep=""),
#        width=figWidth, height=figWidth*0.67, dpi=figRes )

# Format number of biosamples
xBioNum <- bioNum %>%
  mutate(
    Year = as.integer(Year), Commercial = as.integer(Commercial),
    Test = as.integer(Test), Nearshore = as.integer(Nearshore),
    Total = as.integer(Total)
  ) %>%
  xtable()

# Write number of biosamples to disc
print(
  x = xBioNum, file = file.path(regName, "BioNum.tex"),
  include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
  NA.string = NA
)

# Format number of biosamples by type
xBioTypeNum <- bioTypeNum %>%
  mutate(Number = as.integer(Number)) %>%
  rename("Number of samples" = Number) %>%
  xtable()

# Write number of biosamples by type to disc
print(
  x = xBioTypeNum, file = file.path(regName, "BioTypeNum.tex"),
  include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
  NA.string = NA
)

# Number-, proportion-, weight- and length-at-age
if (exists("deltaNumAgeYr") & exists("deltaPropAgeYr") &
    exists("deltaWtAgeYr") & exists("deltaLenAgeYr") & exists("nearNum") &
    exists("nearProp") & exists("nearWt") & exists("nearLen")) {
  # Format number-at-age
  xDeltaNumAgeYr <- deltaNumAgeYr %>%
    xtable(digits = c(0, 0, rep(0, times = length(ageRange))))
  # Write number-at-age to disc
  print(
    x = xDeltaNumAgeYr, file = file.path(regName, "DeltaNumAgeYr.tex"),
    include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
    NA.string = NA
  )
  # Format proportion-at-age
  xDeltaPropAgeYr <- deltaPropAgeYr %>%
    xtable(digits = c(0, 0, rep(3, times = length(ageRange))))
  # Write proportion-at-age to disc
  print(
    x = xDeltaPropAgeYr, file = file.path(regName, "DeltaPropAgeYr.tex"),
    include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
    NA.string = NA
  )
  # Format weight-at-age
  xDeltaWtAgeYr <- deltaWtAgeYr %>%
    xtable(digits = c(0, 0, rep(0, times = length(ageRange))))
  # Write weight-at-age to disc
  print(
    x = xDeltaWtAgeYr, file = file.path(regName, "DeltaWtAgeYr.tex"),
    include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
    NA.string = NA
  )
  # Format length-at-age
  xDeltaLenAgeYr <- deltaLenAgeYr %>%
    xtable(digits = c(0, 0, rep(0, times = length(ageRange))))
  # Write length-at-age to disc
  print(
    x = xDeltaLenAgeYr, file = file.path(regName, "DeltaLenAgeYr.tex"),
    include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
    NA.string = NA
  )
  # Format number-at-age: annuals and total
  xNearNum <- nearNum %>%
    xtable(digits = c(0, 0, rep(0, times = length(ageRange))))
  # Write number-at-age to disc
  print(
    x = xNearNum, file = file.path(regName, "NearNum.tex"),
    include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
    NA.string = NA
  )
  # Format proportion-at-age: annuals and total
  xNearProp <- nearProp %>%
    xtable(digits = c(0, 0, rep(3, times = length(ageRange))))
  # Write proportion-at-age to disc
  print(
    x = xNearProp, file = file.path(regName, "NearProp.tex"),
    include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
    NA.string = NA
  )
  # Format weight-at-age: annuals and total
  xNearWt <- nearWt %>%
    xtable(digits = c(0, 0, rep(0, times = length(ageRange))))
  # Write weight-at-age to disc
  print(
    x = xNearWt, file = file.path(regName, "NearWt.tex"),
    include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
    NA.string = NA
  )
  # Format length-at-age: annuals and total
  xNearLen <- nearLen %>%
    xtable(digits = c(0, 0, rep(0, times = length(ageRange))))
  # Write length-at-age to disc
  print(
    x = xNearLen, file = file.path(regName, "NearLen.tex"),
    include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
    NA.string = NA
  )
} # End if number-, proportion-, weight-, and length-at-age

# Format proportion-at-age
xPropAgedYearTab <- propAgedYearTab %>%
  xtable(digits = c(0, 0, rep(3, times = length(ageRange))))

# Write proportion-at-age to disc
print(
  x = xPropAgedYearTab, file = file.path(regName, "PropAgedYearTab.tex"),
  include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
  NA.string = NA
)

# If weight-at-age by group exists
if (exists("weightAgeGroupN")) {
  # Format weight-at-age
  xWeightAgeGroupN <- weightAgeGroupN %>%
    mutate(
      Previous = format(Previous, big.mark = ",", scientific = FALSE),
      Recent = format(Recent, big.mark = ",", scientific = FALSE)
    ) %>%
    rename("Previous decade" = Previous, "Recent decade" = Recent) %>%
    xtable(digits = c(0, 0, 0, 0, 0))
  # Write weight-at-age to disc
  print(
    x = xWeightAgeGroupN, file = file.path(regName, "WeightAgeGroupN.tex"),
    include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
    NA.string = NA
  )
} # End if weight-at-age exists

# Format spawn summary
xSpawnYrTab <- spawnYrTab %>%
  mutate(
    TotalLength = format(TotalLength,
                         big.mark = ",", digits = 0,
                         scientific = FALSE
    ),
    MeanWidth = format(MeanWidth, big.mark = ",", digits = 0),
    MeanLayers = format(MeanLayers, big.mark = ",", digits = 1),
    TotalSI = format(TotalSI, big.mark = ",", digits = 0, scientific = FALSE)
  ) %>%
  rename(
    "Total length" = TotalLength, "Mean width" = MeanWidth,
    "Mean number of" = MeanLayers, "Spawn index" = TotalSI
  ) %>%
  xtable()

# Write spawn summary to disc
print(
  x = xSpawnYrTab, file = file.path(regName, "SpawnYrTab.tex"),
  include.rownames = FALSE, booktabs = TRUE, only.contents = TRUE,
  NA.string = NA
)

# Wrangle headings
xTemp <- readLines(con = file.path(regName, "SpawnYrTab.tex"))
hRow <- grep(
  pattern = "Year & Total length & Mean width & Mean number of & Spawn index",
  x = xTemp)
xTemp[3] <- paste0(
  "& Total length & Mean width & Mean number of & Spawn index \\\\ ",
  "Year & (m) & (m) & egg layers & (t) \\\\"
)
writeLines(text = xTemp, con = file.path(regName, "SpawnYrTab.tex"))

# If there is spawn reported
if (nrow(spawnByLoc) >= 1) {
  # Format spawn summary
  xSpawnByLoc <- spawnByLoc %>%
    arrange(StatArea, Section, LocationName, Start) %>%
    mutate(
      StatArea = formatC(StatArea, width = 2, format = "d", flag = "0"),
      Section = formatC(Section, width = 3, format = "d", flag = "0"),
      Start = format(Start, format = "%B %d"),
      TotalSI = format(TotalSI, big.mark = ",", digits = 0, scientific = FALSE)
    ) %>%
    rename(
      "Statistical Area" = StatArea, "Location name" = LocationName,
      "Start date" = Start, "Spawn index (t)" = TotalSI
    ) %>%
    xtable()
  # Write spawn summary (longtable) to disc
  WriteLongTable(dat = xSpawnByLoc, fn = file.path(regName, "SpawnByLoc.tex"))
} else { # End if there was spawn, otherwise
  # Make an empty file (required for latex)
  write_csv(x = spawnByLoc, file = file.path(regName, "SpawnByLoc.tex"))
} # End if there was no spawn

# Format the spatial table
xSpatialGroup <- spatialGroup %>%
  rename(Region = RegionName, "Statistical Area" = StatArea) %>%
  xtable()
# Write the spatial table (longtable) to disc
WriteLongTable(dat = xSpatialGroup, fn = file.path(regName, "SpatialGroup.tex"))

##### LaTeX #####

# Number of years in the time series
nYrs <- length(yrRange)

# Current season code
thisSeason <- paste(yrRange[nYrs - 1], yrRange[nYrs], sep = "/")

# Toggle for major vs not major SAR
tfMajor <- ifelse(
  regionType == "major", "\\toggletrue{major}", "\\togglefalse{major}"
)
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
# Turn the toggle to false: special region
tfSpecialRegion <- "\\togglefalse{specialRegion}"
# Turn the toggle to false: catch zoom
tfShowCatchZoom <- "\\togglefalse{showCatchZoom}"

# Group name
spawnIndexGroupName <- list(a = "Statistical Area", b = "Statistical Area (SA)")
# Create an empty legend
spawnIndexGroupLegend <- ""
# Get year ranges (none)
wtAgeYrRanges <- list(old = "", new = "")

# If there are no biosamples to show, set the switch to false
if (nrow(bioLocations) == 0) tfBiosamples <- "\\togglefalse{biosamples}"

# If there was no spawn, set the switch to false
if (nrow(spawnByLoc) == 0) tfSpawnByLoc <- "\\togglefalse{spawnByLoc}"

# If there was no spawn to show on the map, set the switch to false
if (nrow(spawnByLocXY) == 0) tfSpawnByLocXY <- "\\togglefalse{spawnByLocXY}"

# If spawn index is summarized by group: set the group name
if (exists("spawnYrGrp")) {
  spawnIndexGroupName <- list(a = "Group", b = "Group")
}

# If spawn depth by year and statistical area: set toggle to true
if (exists("spawnStatsYrSA") & exists("spawnStatsYrSec")) {
  tfSpawnDepth <- "\\toggletrue{spawnDepth}"
}

# If there is a catch zoom: set toggle to true
if(showCatchZoom) tfShowCatchZoom <- "\\toggletrue{showCatchZoom}"

# If weight by age and group
if (exists("weightAgeGroup")) {
  # Set the toggle to true
  tfWeightGroup <- "\\toggletrue{weightGroup}"
  # Get year ranges
  wtAgeYrRanges <- list(
    recent = weightAgeGroup %>%
      filter(Decade == "Recent") %>%
      select(Year) %>%
      range() %>%
      paste(collapse = " to "),
    previous = weightAgeGroup %>%
      filter(Decade == "Previous") %>%
      select(Year) %>%
      range() %>%
      paste(collapse = " to ")
  )
} # End if weight by age and group

# If weight by catch type: set the toggle to true
if (exists("weightCatchFig")) tfWeightCatch <- "\\toggletrue{weightCatch}"

# If catch by stat area: set toggle to true
if (exists("catchStatArea")) {
  tfCatchStatArea <- "\\toggletrue{catchStatArea}"
}

# If the region is Strait of Georgia
if (region == "SoG") {
  # Turn the toggle to true: show spatial table with group info
  tfSpatialGroup <- "\\toggletrue{spatialGroup}"
  # Create a legend for Groups
  spawnIndexGroupLegend <- "Legend: `14\\&17' is Statistical Areas 14 and 17
      (excluding Section 173); `ESoG' is eastern Strait of Georgia;
      `Lazo' is above Cape Lazo; and `SDodd' is South of Dodd Narrows"
} # End if Strait of Georgia

# If the region is Central Coast
if (region == "CC") {
  # Turn the toggle to true: show spatial table with group info
  tfSpatialGroup <- "\\toggletrue{spatialGroup}"
  # Create a legend for Groups
  spawnIndexGroupLegend <- "Legend: `06\\&07' is Statistical Areas 06 and 07;
       and `08' is Statistical Area 08."
  # Get print friendly values: historic years
  histYrs <- paste(range(yrsRatioHist), collapse = " and ")
  # Get print friendly values: historic ratio
  histRat <- propNumBioHist %>%
    filter(Group == "8") %>%
    mutate(SampWt = round(SampWt * 100, digits = 1)) %>%
    pull(SampWt)
  # Get print friendly values: years to fix
  fixYrs <- PasteNicely(yrsRatioFix)
} else { # End if Central Coast, otherwise
  # Dummy values
  histYrs <- ""
  histRat <- ""
  fixYrs <- ""
} # End if not Central Coast

# Special region
if( regionType == "special") tfSpecialRegion <- "\\toggletrue{specialRegion}"

# If the region is Johnstone Strait
if(region == "JS") {
  sectionsJS <- 
  "In addition, note that sections 132 and 135 are included in Johnstone Strait
  here, but they are officially in the Strait of Georgia SAR."
} else { # End if JS, otherwise
  sectionsJS <- ""
} # End if not JS

# If number-, proportion-, weight-, and length-at-age: set the toggle to true
if (exists("deltaNumAgeYr") & exists("deltaPropAgeYr") & exists("deltaWtAgeYr")
    & exists("deltaLenAgeYr") & exists("nearNum") & exists("nearProp") &
    exists("nearWt") & exists("nearLen")) {
  tfNumPropWtAge <- "\\toggletrue{numPropWtAge}"
}

# Number of samples: this year
nSampThisYr <- bioNum %>%
  filter(Year == max(yrRange)) %>%
  select(Total)

# If there were samples
if (nSampThisYr > 0) {
  # Number aged: this year
  numAgedThisYr <- numAgedYear %>%
    filter(Year == max(yrRange)) %>%
    select(Number) %>%
    sum() %>%
    formatC(big.mark = ",")
} else { # End if there were samples
  # Number aged: this year
  numAgedThisYr <- 0
} # End if there were no samples

# Largest spawn contributor this year
pSpawnThisYr <- spawnYrSec %>%
  filter(Year == max(yrRange)) %>%
  slice(which.max(PercSI)) %>%
  mutate(Percent = round(PercSI)) %>%
  select(Section, Percent)

# If there was spawn this year
if (nrow(spawnByLoc) >= 1) {
  # Number of locations surveyed for herring spawn
  nSpawnLoc <- nrow(xSpawnByLoc)
  # Column names for spawn location table
  namesSpawnLoc <- paste(names(xSpawnByLoc), collapse = " & ")
} else { # End if spawn, otherwise
  # Number of locations surveyed for herring spawn
  nSpawnLoc <- 0
  # No names required
  namesSpawnLoc <- ""
} # End if no spawn

# Get names for spatial groups
namesSpatialGroup <- paste(names(xSpatialGroup), collapse = " & ")

# Formatted year ranges for q1 (surface) and q2 (dive)
qYrs <- list(
  q1 = paste(range(yrRange[yrRange < pars$years$dive]), collapse = " to "),
  q2 = paste(range(yrRange[yrRange >= pars$years$dive]), collapse = " to ")
)

# Justification for age tables
ageTableAlign <- paste(c("{l", rep("r", times = length(ageRange)), "}"),
                       collapse = ""
)

##### Tables #####

# Progress message
cat("Writing tables... ")

## Write areas to a csv
# write_csv( x=areas, file=file.path(regName, "Areas.csv") )
#
## Write raw catch data to a csv
# write_csv( x=catchRaw, file=file.path(regName, "CatchRaw.csv") )
#
## Write raw biological data to a csv
# write_csv( x=bioRaw, file=file.path(regName, "BioRaw.csv") )
#
# If looking at all regions
if (region == "All") {
  # Write spawn data to a csv for Open Data portal (English and French) and FIND
  spawnRaw %>%
    select(
      Region, Year, StatArea, Section, LocationCode, LocationName,
      SpawnNumber, Start, End, Eastings, Northings, Longitude, Latitude,
      Length, Width, Method, SurfSI, MacroSI, UnderSI, Survey
    ) %>%
    mutate(StatArea = formatC(StatArea, width = 2, format = "d", flag = "0"),
           Section = formatC(Section, width = 3, format = "d", flag = "0"),
           Longitude = ifelse(Longitude == 0, NA, Longitude),
           Latitude = ifelse(Latitude == 0, NA, Latitude)) %>%
    rename(StatisticalArea = StatArea, StartDate = Start, EndDate = End,
           Surface = SurfSI, Macrocystis = MacroSI, Understory = UnderSI) %>%
    arrange(
      Region, Year, StatisticalArea, Section, LocationCode, SpawnNumber
    ) %>%
    # Omit data in Stat Area 10 and 28 (change to NA)
    mutate(Surface = ifelse(StatisticalArea %in% c(10, 28), NA, Surface),
           Macrocystis = ifelse(StatisticalArea %in% c(10, 28), NA, Macrocystis),
           Understory = ifelse(StatisticalArea %in% c(10, 28), NA, Understory)) %>%
    write_csv(file = file.path(regName, "FIND.csv")) %>%
    select(-Eastings, -Northings, -Survey) %>%
    write_csv(file = file.path(regName, "OpenDataEng.csv")) %>%
    mutate(Method = ifelse(Method=="Dive", "Plongée", Method),
           Method = ifelse(Method=="Incomplete", "Incomplet", Method)) %>%
    rename('Région' = Region, 'Année' = Year, ZoneStatistique = StatisticalArea,
           CodeLieu = LocationCode, NomLieu = LocationName,
           'NuméroFrai' = SpawnNumber, 'DateDébut' = StartDate,
           DateFin = EndDate, Longeur = Length, Largeur = Width, 
           'Méthode' = Method, 'Sous-étage' = Understory) %>%
    write_csv(file = file.path(regName, "OpenDataFra.csv"))
  # Write the locations with spatial inconsistencies
  spatialInconsistent <- bind_rows(overAreas, overSpawn, overBio) %>%
    distinct() %>%
    arrange(StatAreaLoc, SectionLoc, LocationCode) %>%
    write_csv(file = file.path(regName, "SpatialInconsistent.csv"))
} # End if all regions

# # Write catch data to a csv (same as ADMB input data file)
# catch %>%
#   group_by( Year ) %>%
#   summarise( Catch=sum(Catch)/1000 ) %>%
#   ungroup( ) %>%
#   complete( Year=yrRange, fill=list(Catch=0) ) %>%
#   write_csv( file=file.path(regName, "Catch.csv") )

# # Write spawn index data to a csv (same as ADMB input data file)
# spawnYr %>%
#   select( Year, TotalSI ) %>%
#   rename( Spawn=TotalSI ) %>%
#   mutate( Spawn=Spawn/1000, Gear=ifelse(Year<pars$years$dive, 4, 5),
#           Weight=ifelse(Year<pars$years$dive, 1, 1.1666)) %>%
#   write_csv( file=file.path(regName, "Spawn.csv") )

# # Write number-at-age data to a csv (same as ADMB input data file)
# numAgedADMB %>%
#   select( -Area, -Group, -Sex ) %>%
#   complete( Year=yrRange, Gear=1:3 ) %>%
#   arrange( Gear, Year ) %>%
#   write_csv( file=file.path(regName, "NumAge.csv") )

# # Write weight-at-age data to a csv (same as ADMB input data file)
# bio %>%
#   filter( GearCode == 29 ) %>%
#   select( Year, Age, Weight, SampWt ) %>%
#   na.omit( ) %>%
#   group_by( Year, Age ) %>%
#   summarise( MeanWeight=WtMeanNA(x=Weight, w=SampWt)/1000 ) %>%
#   ungroup( ) %>%
#   arrange( Year, Age ) %>%
#   spread( key=Age, value=MeanWeight ) %>%
#   complete( Year=yrRange ) %>%
#   arrange( Year ) %>%
#   write_csv( file=file.path(regName, "WeightAge.csv") )

## Write catch data to a csv
# write_csv( x=catch, file=file.path(regName, "Catch.csv") )
#
## Write biological data to a csv
# write_csv( x=bio, file=file.path(regName, "Bio.csv") )
#
## Write ADMB catch to a csv
# write_csv( x=catchADMB, file=file.path(regName, "CatchADMB.csv") )
#
## Write ADMB spawn to a csv
# write_csv( x=spawnADMB, file=paste(regName, "SpawnADMB.csv", sep="") )
#
## Write ADMB number aged to a csv
# write_csv( x=numAgedADMB, file=file.path(regName, "NumAgedADMB.csv") )
#
## Write ADMB weight-at-age to a csv
# write_csv( x=weightAgeADMB, file=file.path(regName, "WeightAgeADMB.csv") )
#
## If exists, write weight-at-age by group to a csv
# if( exists("weightAgeGroupN") )
#  write_csv( x=weightAgeGroupN, file=file.path(regName,
#          "WeightAgeGroupN.csv") )

# Write the number of biosamples to a csv
write_csv(
  x = numBiosamples,
  file = file.path(regName, paste("NumBiosamples", regName, ".csv", sep = ""))
)

# Write the spawn distribution to a csv
if(regName %in% c("HG", "PRD", "CC", "SoG", "WCVI", "A27", "A2W", "A10")){
  write_csv(
    x = propSpawn,
    file = file.path(srLoc, paste("prop-spawn-", tolower(regName), ".csv",
                                  sep = ""
    ))
  )
}

# Write the SOK harvest to a csv
if(regName %in% c("HG", "PRD", "CC", "SoG", "WCVI", "A27", "A2W", "A10")){
  write_csv(
    x = allHarvSOK,
    file = file.path(srLoc, paste("harvest-sok-", tolower(regName), ".csv",
                                  sep = ""
    ))
  )
}

# Write spawn by year and section to a csv if requested
if(regName %in% c("CC", "A27")) {
  write_csv(
    x = spawnYrSec %>%
      select(Year, StatArea, Section, Survey, TotalSI) %>%
      rename(Index = TotalSI) %>%
      filter(!is.na(Year)) %>%
      arrange(Year, StatArea, Section),
    file = file.path(srLoc, paste("spawn-yr-sec-", tolower(regName), ".csv",
                                  sep = "")))
}

# write incidental catch to a csv
if(regName %in% c("HG", "PRD", "CC", "SoG", "WCVI", "A27", "A2W", "A10")){
  write_csv(
    x = incidental,
    file = file.path(srLoc, paste("incidental-", tolower(regName), ".csv",
                                  sep = ""))
  )
}

## Format the spawn summary
# spawnYrF <- spawnYr %>%
#    mutate( TotalLength=formatC(TotalLength, digits=0, format="f"),
#        MeanWidth=formatC(MeanWidth, digits=0, format="f"),
#        MeanLayers=formatC(MeanLayers, digits=3, format="f"),
#        TotalSI=formatC(TotalSI, digits=0, format="f"),
#        Region=regName ) %>%
#    select( Region, Year, TotalLength, MeanWidth, MeanLayers, TotalSI )
#
## Write the spawn summary
# write_csv( x=spawnYrF, file=paste("RawIndex", regName, ".csv", sep="") )

# Update progress
cat("done\n")

##### Output #####

# Save the workspace image
save.image(file = file.path(
  regName, paste("Image.", regName, ".RData", sep = "")
))

##### QAQC #####

## Missing spatial groups: sections and statistical areas
# areaNoGroup <- areas %>%
#    filter( is.na(Group) ) %>%
#    select( Region, StatArea, Section ) %>%
#    distinct( ) %>%
#    write_csv( file="AreasNoGroup.csv", append=ifelse(r==1, FALSE, TRUE) )

## Missing spatial groups: spawn
# spawnNoGroup <- spawnRaw %>%
#    filter( is.na(Group) ) %>%
#    group_by( Region, Year, StatArea, Section, LocationCode ) %>%
#    summarise( Spawn=SumNA(c(SurfSI, MacroSI, UnderSI)) ) %>%
#    ungroup( ) %>%
#    mutate( Spawn=round(Spawn, digits=1) ) %>%
#    write_csv( file="SpawnNoGroup.csv", append=ifelse(r==1, FALSE, TRUE) )

## Missing spatial groups: bio
# bioNoGroup <- bio %>%
#    filter( is.na(Group) ) %>%
#    group_by( Region, Year, StatArea, Section, LocationCode, Sample ) %>%
#    summarise( NFish=n() ) %>%
#    ungroup( ) %>%
#    write_csv( file="BioNoGroup.csv", append=ifelse(r==1, FALSE, TRUE) )

## Missing spatial groups: catch
# catchNoGroup <- catch %>%
#    filter( is.na(Group) ) %>%
#    group_by( Region, Year, StatArea, Section, Gear ) %>%
#    summarise( Catch=SumNA(Catch) ) %>%
#    ungroup( ) %>%
#    mutate( Catch=round(Catch, digits=1) ) %>%
#    write_csv( file="CatchNoGroup.csv", append=ifelse(r==1, FALSE, TRUE) )

##### End #####

# Print end of file message and elapsed time
cat("End of file Summary.R: ", sep = "")
print(Sys.time() - sTime)
