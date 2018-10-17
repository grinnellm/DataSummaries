###############################################################################
# 
# Author:       Matthew H. Grinnell
# Affiliation:  Pacific Biological Station, Fisheries and Oceans Canada (DFO) 
# Group:        Quantitative Assessment Methods Section, Science
# Address:      3190 Hammond Bay Road, Nanaimo, BC, Canada, V9T 6N7
# Contact:      e-mail: matt.grinnell@dfo-mpo.gc.ca | tel: (250) 756.7055
# Project:      Herring
# Code name:    SpawnIndex.R
# Version:      2.0
# Date started: Oct 12, 2016
# Date edited:  Aug 30, 2017
# 
# Overview: 
# Calculate the spawn index (SI) for each 'spawn number' by spawn type (i.e., 
# surface, macrocystis, and understory) from raw spawn survey data. The output 
# for the stock assessment is aggregated by stock assessment region and year,
# in tonnes.
# 
# Requirements: 
# Access to the main herring spawn databases on the shared network drive, or
# local copies.  
# 
# Notes: 
# Usually only one stock assessment region is considered at a time, but this 
# script can analyse and aggregate multiple regions if specified. 
#
# References:
# Haegele, C.W., Hourston, A.S., Humphreys, R.D., and Miller, D.C. 1979. Eggs 
#   per unit area in British Columbia herring spawn depositions. Fisheries and 
#   Marine Service Technical Report 894. Department of Fisheries and Oceans. 
# Haegele, C.W., and Schweigert, J.F. 1990. A model which predicts Pacific 
#   herring (Clupea harengus pallasi) egg deposition on giant kelp 
#   (Macrocystis sp.) plants from underwater observations. Canadian Manuscript 
#   Report of Fisheries and Aquatic Sciences 2056. Department of Fisheries and 
#   Oceans.
# Hay, D.E. 1985. Reproductive biology of Pacific herring (Clupea harengus 
#   pallasi) Canadian Journal of Fisheries and Aquatic Sciences S1 (42).
# Hay, D.E., and Brett, J.R. 1988. Maturation and fecundity of Pacific herring 
#   (Clupea harengus pallasi): An experimental study with comparisons to natural 
#   populations. Canadian Journal of Fisheries and Aquatic Sciences 45 (3).
# Hay, D.E., and Kronlund, A.R. 1987. Factors affecting the distribution, 
# 	 abundance, and measurement of Pacific herring (Clupea harengus pallasi) 
#   spawn. Canadian Journal of Fisheries and Aquatic Sciences 44 (6).
# Hay, D.E., and Miller, D.C. 1982. A quantitative assessment of herring spawn
#   lost by storm action in French Creek, 1980. Canadian Manuscript Report of
#   Fisheries and Aquatic Sciences 1636. Department of Fisheries and Oceans.
# Humphreys, R.D., and Haegele, C.W. 1976. An evaluation of herring spawn survey 
#   techniques used in British Columbia waters. Technical Report 613. 
#   Environment Canada, Fisheries and Marine Service.
# Schweigert, J.F., Fort, C., and Hamer, L. 1997. Stock assessment for British 
#   Columbia herring in 1996 and forecasts of the potential catch in 1997.
#   Canadian Technical Report of Fisheries and Aquatic Sciences 2173. Department 
#   of Fisheries and Oceans.
# Schweigert, J. 2005. An assessment framework for Pacific herring (Clupea 
#   pallasi) stocks in British Columbia. Research Document 2005/083. Fisheries 
#   and Oceans Canada.
# Shields, T.L., Jamieson, G.S., and Sprout, P.E. 1985. Spawn-on-kelp fisheries
#   in the Queen Charlotte Islands and northern British Columbia coast - 1982 
#   and 1983. Canadian Technical Report of Fisheries and Aquatic Sciences 1372. 
#   Department of Fisheries and Oceans.
# Whyte, J.N.C., and Englar, J.R. 1977. Aspects of the production of herring roe
#   roe on Macrocystis integrifolia in Georgia Strait locations. Fisheries and
#   marine service technical report 751. Fisheries and Marine Service.
# 
# Versions: Version 1 was a stand-alone script to understand and duplicate the
# calculations in the database. Version 2 is incorporated in the script 
# "Summary.R" to avoid relying on the database.
# 
###############################################################################


################ 
##### Main ##### 
################     

# Calculate egg conversion factor
CalcEggConversion <- function( fecundity, pFemale ) {
  # Calculate the conversion factor for eggs to spawning biomass index in
  # tonnes. Fecundity is the number of eggs per kilogram of female spawners,
  # and pFemale is the proportion of spawners that are female (Hay 1985, Hay
  # and Brett 1988). Divide the number of eggs by the conversion factor to get
  # tonnes of spawners.
  # Eggs per tonne: eggs/kilogram female * proportion female * kilograms/tonne
  eggsPerTonne <- fecundity * pFemale * 1000
  # Return the conversion factor 
  return( eggsPerTonne )
}  # End CalcEggConversion function

# Calculate spawning biomass from SOK harvest
CalcBiomassSOK <- function( SOK, eggKelpProp, eggBrineProp, eggWt, ECF ) {
  # Calculate spawning biomass from spawn on kelp (SOK) harvest. SOK is spawn
  # on kelp harvest in kilograms, eggKelpProp is the proportion of SOK product
  # that is eggs, not kelp (ShieldsEtal1985), eggBrineProp is the proportion of
  # SOK product that is eggs after brining (WhyteEnglar1977), and eggWt is the
  # average weight in kilograms of a fertilized egg (HayMiller1982). Returns
  # spawning biomass in tonnes based on the egg conversion factor.
  # Spawnin biomass in tonnes: (kg SOK * proportion eggs * proportion eggs) / 
  # (kg per egg * eggs per tonne )
  SB <- (SOK * eggKelpProp * eggBrineProp) / (eggWt * ECF)
  # Return the spawning biomass
  return( SB )
}  # End CalcBiomassSOK

# Calculate surface spawn
CalcSurfSpawn <- function( where, a, f ) {
  # Establish connection with access
  accessDB <- odbcConnectAccess( access.file=file.path(where$loc, where$db) )
  # Access the region worksheet and wrangle
  regStd <- sqlFetch( channel=accessDB, sqtable=where$fns$regionStd ) %>%
      rename( SAR=REGION, WidthReg=WIDMED ) %>%
      filter( SAR %in% a$SAR ) %>%
      select( SAR, WidthReg ) %>%
      as_tibble( )
  # Access the section worksheet and wrangle
  secStd <- sqlFetch( channel=accessDB, sqtable=where$fns$sectionStd ) %>%
      rename( Section=SECTION, WidthSec=WIDMED ) %>%
      filter( Section %in% a$Section ) %>%
      mutate( SAR=regStd$SAR ) %>%
      select( Section, WidthSec ) %>%
      as_tibble( )
  # Access the bed worksheet and wrangle
  bedStd <- sqlFetch( channel=accessDB, sqtable=where$fns$poolStd ) %>%
      rename( Section=SECTION, Bed=BED, WidthBed=WIDMED ) %>%
      filter( Section %in% a$Section ) %>%
      mutate( SAR=regStd$SAR ) %>%
      select( Section, Bed, WidthBed ) %>%
      as_tibble( )
  # Load intensity categories and egg layers (Humphreys and Haegele 1976, Hay 
  # and Kronlund 1987)
  intensity <- sqlFetch( channel=accessDB, sqtable=where$fns$intensity ) %>%
      rename( Layers=AvgLayersFromIntensity ) %>%
      as_tibble( )
  # Load all spawn
  spawn <- sqlFetch( channel=accessDB, sqtable=where$fns$allSpawn ) %>%
      rename( LocationCode=Loc_Code, SpawnNumber=Spawn_Number, 
          WidthObs=Width ) %>%
      mutate( Method=str_title_case(Method) ) %>%
      filter( Year %in% yrRange, LocationCode %in% a$LocationCode ) %>%
      select( Year, LocationCode, SpawnNumber, Length, WidthObs, Method ) %>%
      as_tibble( )
  # Get a small subset of area data
  areasSm <- a %>%
      select( Region, StatArea, Section, LocationCode, Bed ) %>%
      distinct( ) %>%
      as_tibble( )
  # Extract relevant surface data
  surface <- sqlFetch( channel=accessDB, sqtable=where$fns$surface ) %>%
      rename( LocationCode=Loc_Code, SpawnNumber=Spawn_Number ) %>%
      filter( Year %in% yrRange, LocationCode %in% a$LocationCode ) %>%
      left_join( y=areasSm, by="LocationCode" ) %>%
      left_join( y=spawn, by=c("Year", "LocationCode", "SpawnNumber") ) %>%
      replace_na( replace=list(Lay_Grass=0, Grass_Percent=0, Lay_Rockweed=0, 
              Rockweed_Percent=0, Lay_Kelp=0, Kelp_Percent=0, Lay_Brown_Algae=0,
              Brown_Algae_Percent=0, Lay_Leafy_Red=0, Leafy_Red_Percent=0,
              Lay_Stringy_Red=0, Stringy_Red_Percent=0, Lay_Rock=0, 
              Rock_Percent=0, Lay_Other=0, Other_Percent=0) ) %>%
      mutate( Grass=Lay_Grass*Grass_Percent/100,
          Rockweed=Lay_Rockweed*Rockweed_Percent/100,
          Kelp=Lay_Kelp*Kelp_Percent/100, 
          BrownAlgae=Lay_Brown_Algae*Brown_Algae_Percent/100,
          LeafyRed=Lay_Leafy_Red*Leafy_Red_Percent/100, 
          StringyRed=Lay_Stringy_Red*Stringy_Red_Percent/100, 
          Rock=Lay_Rock*Rock_Percent/100, 
          Other=Lay_Other*Other_Percent/100,
          EggLyrs=Grass + Rockweed + Kelp + BrownAlgae + LeafyRed + 
              StringyRed + Rock + Other,
          Intensity=ifelse(Year %in% rescaleYrs & Intensity > 0, 
              Intensity * 2 - 1, Intensity) ) %>%
      filter( Method %in% c("Surface", "Dive") ) %>%
      select( Year, Region, StatArea, Section, LocationCode, Bed, SpawnNumber, 
          Length, WidthObs, Intensity, EggLyrs ) %>%
      as_tibble( )
  # Fill-in missing egg layers manually and calculate egg density
  eggs <- surface %>%
      left_join( y=intensity, by="Intensity" ) %>%
      mutate( EggLyrs=ifelse(Year %in% intenseYrs, Layers, EggLyrs),
          # (SoG) 1 record: update Intensity from 0 to 1 and use Intensity
          # to fill-in missing values (spawn surveyed, layers not reported)
          EggLyrs=ifelse(Year==1962 & StatArea==14 & Section==142 & 
                  LocationCode==820 & EggLyrs==0, 0.5529, EggLyrs),
          # Egg density in thousands (eggs * 10^3 / m^2; Schweigert et al. 1997)
          # Yes, thousands: the report is wrong (J. Schweigert, personal 
          # communication, 21 February 2017)
          EggDens=EggLyrs * 212.218 + 14.698 )
  # These are the 'original' manual updates that were in the Microsoft Access
  # database: some overwrite good data with no documented reason and have been 
  # omitted, others have been omitted because the spawn survey was incomplete.
  # However, they (and others) are still present in the Microsoft Access
  # database, causing discrepancies for HG 1979, as well as WCVI 1982 and 1984.
  # They should get removed from the Microsoft Access database (especially
  # updates 1, 4, and 5 which cause errros). Update 2 is still relevant; 
  # updates 3 and 6 no longer have an effect.
  # 1. HG (15 records): Year 1979, SA 2, Intensity 4 (update EggLyrs to 
  #    2.1496 using intensity table; 14 records overwrite good data)
  # 2. SoG (1 record): Year 1962, SA 14, Intensity 0 (update EggLyrs to 
  #    0.5529 using intensity 1: spawn was surveyed but not reported)
  # 3. WCVI (4 records): Year 1981, SA 24, EggLyrs 0 (update EggLyrs to
  #    0.5529 using intensity table)
  # 4. WCVI (7 records): Year 1982, SA 23, Intensity 3 (update EggLyrs to
  #    1.3360 using intensity table; 7 records overwrite good data)
  # 5. WCVI (41 records): Year 1984, SA 24, Intensity 0 (update EggLyrs to
  #    2.33 -- not sure why/how; 41 records overwrite good data)
  # 6. A27 (14 records): Year 1982, SA 27, EggLyrs 0 (update EggLyrs to
  #    2.98 using a historical average)
  # Get the number of records with no egg layer info
  noLayers <- eggs %>% filter( EggLyrs==0 )
  # Error if there are missing values
  if( nrow(noLayers) > 0 )  stop( "Missing egg layers for ", nrow(noLayers), 
        " record(s):",  print(noLayers), sep="" )
  # Output egg layer info
  eggLyrs <- eggs %>%
      group_by( Year, Region, StatArea, Section, LocationCode, SpawnNumber ) %>%
      summarise( SurfLyrs=MeanNA(EggLyrs) ) %>%
      ungroup( )
  # Calculate egg density per spawn number/bed
  eggsSpawn <- eggs %>%
      group_by( Year, Region, StatArea, Section, LocationCode, SpawnNumber, 
          Bed ) %>%
      summarise( EggDens=MeanNA(EggDens) ) %>%
      ungroup( )
  # Calculate annual fish biomass by spawn number/bed
  biomassSpawn <- eggsSpawn %>%
      left_join( y=spawn, by=c("Year", "LocationCode", "SpawnNumber") ) %>%
      mutate( WidthReg=regStd$WidthReg ) %>%
      left_join( y=secStd, by="Section" ) %>%
      left_join( y=bedStd, by=c("Section", "Bed") ) %>%
      # Width is set to bed, section, region, or observed width (in that order)
      mutate( Width=WidthBed,
          Width=ifelse(is.na(Width), WidthSec, Width),
          Width=ifelse(is.na(Width), WidthReg, Width),
          Width=ifelse(is.na(Width), WidthObs, Width),
          # Biomass in tonnes, based on Hay (1985), and Hay and Brett (1988)
          SurfSI=EggDens * Length * Width * 1000 / f ) %>%
      group_by( Year, Region, StatArea, Section, LocationCode, SpawnNumber ) %>%
      summarise( SurfSI=SumNA(SurfSI) ) %>%
      ungroup( ) %>%
      full_join( y=eggLyrs, by=c("Year", "Region", "StatArea", "Section", 
              "LocationCode", "SpawnNumber") )
  # Calculate annual SI by spawn number
  SI <- biomassSpawn %>%
      select( Year, Region, StatArea, Section, LocationCode, SpawnNumber, 
          SurfSI )
  # Close the connection
  odbcClose( accessDB )
  # Return the data
  return( list(surface=surface, eggs=eggs, eggsSpawn=eggsSpawn, 
          biomassSpawn=biomassSpawn, SI=SI) )
}  # End CalcSurfSpawn function

# Calculate macrocystis spawn
CalcMacroSpawn <- function( where, a, f ) {
  # Establish connection with access
  accessDB <- odbcConnectAccess( access.file=file.path(where$loc, where$db) )
  # Load all spawn
  spawn <- sqlFetch( channel=accessDB, sqtable=where$fns$allSpawn ) %>%
      rename( LocationCode=Loc_Code, SpawnNumber=Spawn_Number,
          LengthMacro=Length_Macrocystis ) %>%
      mutate( Method=str_title_case(Method) ) %>%
      filter( Year %in% yrRange, LocationCode %in% a$LocationCode ) %>%
      select( Year, LocationCode, SpawnNumber, LengthMacro, Length, Method ) %>%
      as_tibble( )
  # Get plant-level data
  plants <-  sqlFetch( channel=accessDB, sqtable=where$fns$plants ) %>%
      rename( LocationCode=Loc_Code, SpawnNumber=Spawn_Number ) %>%
      filter( Year %in% yrRange, LocationCode %in% a$LocationCode,
          !is.na(Mature) )  %>%
      select( Year, LocationCode, SpawnNumber, Transect, Mature ) %>%
      as_tibble( )
  # Get a small subset of area data
  areasSm <- a %>%
      select( Region, StatArea, Section, LocationCode, Bed ) %>%
      distinct( ) %>%
      as_tibble( )
  # Get transect-level data
  transects <- sqlFetch( channel=accessDB, sqtable=where$fns$transects ) %>%
      rename( LocationCode=Loc_Code, SpawnNumber=Spawn_Number ) %>%
      filter( Year %in% yrRange, LocationCode %in% a$LocationCode ) %>%
      left_join( y=areasSm, by="LocationCode" ) %>%
      select( Year, Region, StatArea, Section, LocationCode, SpawnNumber, 
          Transect, Height, Width, Layers ) %>%
      as_tibble( )
  # Merge the data
  dat <- transects %>%
      left_join( y=plants, by=c("Year", "LocationCode", "SpawnNumber", 
              "Transect") ) %>%
      replace_na( replace=list(Mature=0) ) %>%
      mutate( Swath=transectSwath )
  # Calculate transect-level data
  datTrans <- dat %>%
      group_by( Year, Region, StatArea, Section, LocationCode, SpawnNumber, 
          Transect ) %>%
      summarise( 
          Width=unique(Width), 
          Swath=unique(Swath), 
          Area=Width*Swath,
          # Plant metrics for mature plants only
          Height=MeanNA(Height[Mature > 0]),
          EggLyrs=MeanNA(Layers[Mature > 0]),
          Stalks=SumNA(Mature[Mature > 0]),
          Plants=length(Mature[Mature > 0]) ) %>%
      ungroup( )
  # Calculate spawn-level data
  biomassSpawn <- datTrans %>%
      left_join( y=spawn, by=c("Year", "LocationCode", "SpawnNumber") ) %>%
      mutate(  
          LengthMacro=ifelse(is.na(LengthMacro), Length, LengthMacro) ) %>%
      filter( Method %in% c("Surface", "Dive") ) %>%
      group_by( Year, Region, StatArea, Section, LocationCode, SpawnNumber ) %>%
      summarise( LengthMacro=unique(LengthMacro),
          Width=MeanNA(Width),
          Area=SumNA(Area), 
          Plants=SumNA(Plants),
          Stalks=SumNA(Stalks), 
          Height=MeanNA(Height),  
          EggLyrs=MeanNA(EggLyrs),
          StalksPerPlant=Stalks/Plants,
          # Eggs per plant in thousands (egtgs * 10^3 / plant; Haegele and  
          # Schweigert 1990)
          EggsPerPlant=0.073 * EggLyrs^0.673 * Height^0.932* 
              StalksPerPlant^0.703 * 1000,
          # Eggs density in thousands (eggs * 10^3 / m^2)
          EggDens=EggsPerPlant * Plants / Area,
          # Biomass in tonnes, based on Hay (1985), and Hay and Brett (1988)
          MacroSI=EggDens * LengthMacro * Width * 1000 / f ) %>%
      rename( MacroLyrs=EggLyrs ) %>%
      ungroup( )
  # Return the macrocystis spawn
  SI <- biomassSpawn %>%
      select( Year, Region, StatArea, Section, LocationCode, SpawnNumber, 
          MacroSI )
  # Close the connection
  odbcClose( accessDB )
  # Return the data
  return( list(dat=dat, datTrans=datTrans, biomassSpawn=biomassSpawn, SI=SI) )
}  # End CalcMacroSpawn function

# Calculate understory spawn
CalcUnderSpawn <- function( where, a, f ) {
  # Establish connection with access
  accessDB <- odbcConnectAccess( access.file=file.path(where$loc, where$db) )
  # Load all spawn
  spawn <- sqlFetch( channel=accessDB, sqtable=where$fns$allSpawn ) %>%
      rename( LocationCode=Loc_Code, SpawnNumber=Spawn_Number,
          LengthAlgae=Length_Vegetation ) %>%
      mutate( Method=str_title_case(Method) ) %>%
      filter( Year %in% yrRange, LocationCode %in% a$LocationCode ) %>%
      select( Year, LocationCode, SpawnNumber, LengthAlgae, Length, Method ) %>%
      as_tibble( )
  # Load algae transects
  algTrans <- sqlFetch( channel=accessDB, sqtable=where$fns$algTrans ) %>%
#      select( -Width ) %>%
      rename( LocationCode=Loc_Code, SpawnNumber=Spawn_Number, 
          QuadratSize=Quadrat_Size ) %>%  # , Width=Width_Recorded
      filter( Year %in% yrRange, LocationCode %in% a$LocationCode ) %>%
      # TODO: This is a temporary ugly cludge -- MT may fix it in the database
      mutate( QuadratSize=ifelse(QuadratSize == 0, 0.5, QuadratSize) ) %>%
      select( Year, LocationCode, SpawnNumber, Transect, Width, 
          QuadratSize ) %>%
      as_tibble( )
  # TODO: Adjust the width -- not quite sure what value to use for 'adjWidth'
  # Adjust widths if requested (lead line shrinkage issue)
  if( doAdjWidth ) {
    # Adjust the spawn width (i.e., transect length) for specified years
    algTrans <- algTrans %>%
        mutate( Width=ifelse(Year %in% adjWidthYrs, Width*(1+adjWidthFrac), 
                Width) )
  }  # End if adjusting widths
  # Error if any quadrats are not 0.5 m^2
  if( any(algTrans$QuadratSize != 0.5) ) 
    stop( "All quadrats must be 0.5m^2", call.=FALSE )
  # Load station data
  stations <- sqlFetch( channel=accessDB, sqtable=where$fns$stations ) %>%
      rename( LocationCode=Loc_Code, SpawnNumber=Spawn_Number,
          SubLyrs=Layers_Bottom ) %>%
      filter( Year %in% yrRange, LocationCode %in% a$LocationCode ) %>%
      mutate( SubProp=Percent_Bottom/100 ) %>%
      select( Year, LocationCode, SpawnNumber, Transect, Station, SubLyrs,
          SubProp ) %>%
      as_tibble( )
  # Get egg layer info: substrate
  eggLyrsSub <- stations %>%
      group_by( Year, LocationCode, SpawnNumber, Transect ) %>%
      summarise( Layers=MeanNA(SubLyrs) ) %>%
      ungroup( ) %>%
      mutate( Source="Substrate" ) 
  # Load algae
  algae <- sqlFetch( channel=accessDB, sqtable=where$fns$algae ) %>%
      rename( LocationCode=Loc_Code, SpawnNumber=Spawn_Number,
          AlgType=Type_Vegetation, AlgLyrs=Layers_Vegetation ) %>%
      filter( Year %in% yrRange, LocationCode %in% a$LocationCode ) %>%
      mutate( AlgType=str_uppercase(AlgType), AlgProp=Percent_Vegetation/100, 
          AlgProp=ifelse(AlgProp>1, 1, AlgProp) ) %>%
      select( Year, LocationCode, SpawnNumber, Transect, Station, AlgType,
          AlgLyrs, AlgProp ) %>%
      as_tibble( )
  # Get egg layer info: algae
  eggLyrsAlg <- algae %>%
      group_by( Year, LocationCode, SpawnNumber, Transect ) %>%
      summarise( Layers=MeanNA(AlgLyrs) ) %>%
      ungroup( ) %>%
      mutate( Source="Algae" )
  # Combine egg layer info
  eggLyrs <- bind_rows( eggLyrsSub, eggLyrsAlg ) %>%
      group_by( Year, LocationCode, SpawnNumber, Transect ) %>%
      summarise( Layers=MeanNA(Layers) ) %>%
      group_by( Year, LocationCode, SpawnNumber ) %>%
      summarise( UnderLyrs=MeanNA(Layers) ) %>%
      ungroup( )
  # Load algae types and coefficients (Schweigert 2005)
  algType <- sqlFetch( channel=accessDB, sqtable=where$fns$typeAlg ) %>%
      rename( AlgType=Type_Vegetation, Coef=VParameter ) %>%
      mutate( AlgType=str_uppercase(AlgType) ) %>%
      select( AlgType, Coef ) %>%
      arrange( AlgType ) %>%
      as_tibble( )
  # If there are missing algae types
  if( any(!algae$AlgType %in% algType$AlgType) ) {
    # Get missing algae type(s)
    missAlg <- unique( algae$AlgType[!algae$AlgType %in% 
                algType$AlgType] )
    # Error, and show missing type(s)
    stop( "Missing algae type(s): ", paste(missAlg, collapse=", "),
        call.=FALSE )
  }  # End if there are missing algae types
  # Get a small subset of area data
  areasSm <- a %>%
      select( Region, StatArea, Section, LocationCode ) %>%
      distinct( ) %>%
      as_tibble( )
  # Calculate substrate egg density
  eggsSub <- stations %>%
      full_join( y=algTrans, by=c("Year", "LocationCode", "SpawnNumber",
              "Transect") ) %>%
      left_join( y=areasSm, by="LocationCode" ) %>%
      # Egg density in thousands (eggs x 10^3 / m^2; Haegele et al. 1979)
      mutate( EggDensSub=340 * SubLyrs * SubProp ) %>%
      replace_na( replace=list(EggDensSub=0) ) %>%
      select( Year, Region, StatArea, Section, LocationCode, SpawnNumber, 
          Transect, Station, Width, EggDensSub )
  # Calculate substrate egg density by quadrat/station
  eggsAlg <- algae %>%
      left_join( y=algType, by="AlgType" ) %>%
      left_join( y=areasSm, by="LocationCode" ) %>%
      left_join( y=select(.data=algTrans, -Width), 
          by=c("Year", "LocationCode", "SpawnNumber", "Transect") ) %>%
      # Egg density in thousands (eggs * 10^3 / m^2; Schweigert 2005); quadrat 
      # size coefficients not required because all quadrats are 0.5m^2 (1.0512)
      mutate( EggDensAlg=600.567 * AlgLyrs^0.6355 * AlgProp^1.413 * Coef * 
              1.0512 ) %>%
      group_by( Year, Region, StatArea, Section, LocationCode, SpawnNumber, 
          Transect, Station ) %>%
      summarise( EggDensAlg=SumNA(EggDensAlg) ) %>%
      replace_na( replace=list(EggDensAlg=0) ) %>%
      ungroup( )
  # Combine eggs
  eggs <- eggsSub %>%
      full_join( y=eggsAlg, by=c("Year", "Region", "StatArea", "Section", 
              "LocationCode", "SpawnNumber", "Transect", "Station") ) %>%
      replace_na( replace=list(Width=0, EggDensSub=0, EggDensAlg=0) ) %>%
      mutate( EggDensSub=ifelse(Width > 0, EggDensSub, 0) )
  # Calculate total egg density by station/quadrat
  eggsStation <- eggs %>%
      # Total egg density in thousands (eggs * 10^3 / m)
      mutate( EggDens=EggDensSub + EggDensAlg ) %>%
      filter( !is.na(Station) )
  # Widths
  widths <- eggsStation %>%
      group_by( Year, Region, StatArea, Section, LocationCode, SpawnNumber, 
          Transect ) %>%
      summarise( Width=unique(Width) ) %>%
      group_by( Year, Region, StatArea, Section, LocationCode, SpawnNumber ) %>%
      summarise( WidthTot=SumNA(Width), WidthBar=MeanNA(Width) ) %>%
      ungroup( )
  # Calculate transect-level metrics
  eggsTrans <- eggsStation %>%
      filter( Width > 0 ) %>%
      group_by( Year, Region, StatArea, Section, LocationCode, SpawnNumber, 
          Transect ) %>%
      summarise( EggDensL=MeanNA(EggDens) * unique(Width) ) %>%
      ungroup( )
  # Calculate spawn number-level metrics
  eggsSpawn <- eggsTrans %>%
      left_join( y=spawn, by=c("Year", "LocationCode", "SpawnNumber") ) %>%
      mutate( LengthAlgae=ifelse(is.na(LengthAlgae), Length, LengthAlgae) ) %>%
      filter( Method %in% c("Surface", "Dive") ) %>%
      left_join( y=widths, by=c("Year", "Region", "StatArea", "Section", 
              "LocationCode", "SpawnNumber") ) %>%
      group_by( Year, Region, StatArea, Section, LocationCode, SpawnNumber ) %>%
      summarise( WidthBar=unique(WidthBar), 
          WidthTot=unique(WidthTot), LengthAlgae=unique(LengthAlgae), 
          EggDensL=SumNA(EggDensL) ) %>%
      ungroup( )
  # Calculate understory biomass by spawn number
  biomassSpawn <- eggsSpawn %>% 
      mutate( 
          # Weighted egg density in thousands (eggs * 10^3 / m^2)
          EggDens=EggDensL / WidthTot,
          # Biomass in tonnes, based on Hay (1985), and Hay and Brett (1988)
          UnderSI=EggDens * LengthAlgae * WidthBar * 1000 / f ) %>%
      left_join( y=eggLyrs, by=c("Year", "LocationCode", "SpawnNumber") )
  # Calculate understory SI by spawn number
  SI <- biomassSpawn %>%
      select( Year, Region, StatArea, Section, LocationCode, SpawnNumber, 
          UnderSI )
  # Close the connection
  odbcClose( accessDB )
  # Return the data
  return( list(stations=stations, algae=algae, eggs=eggs, 
          eggsStation=eggsStation, eggsTrans=eggsTrans, eggsSpawn=eggsSpawn, 
          biomassSpawn=biomassSpawn, SI=SI) )
}  # End CalcUnderSpawn function

# Get the all spawn data
GetAllSpawn <- function( where, a ) {
  # Establish connection with access
  accessDB <- odbcConnectAccess( access.file=file.path(where$loc, where$db) )
  # Extract relevant spawn data
  spawn <- sqlFetch( channel=accessDB, sqtable=where$fns$allSpawn ) %>%
      rename( LocationCode=Loc_Code, SpawnNumber=Spawn_Number ) %>%
      mutate( Start=as_date(Start), End=as_date(End), 
          Method=str_to_title(Method) ) %>%
      filter( Year %in% yrRange, LocationCode %in% a$LocationCode ) %>%
      select( Year, LocationCode, SpawnNumber, Start, End, Length, Width,
          Method ) %>%
      as_tibble( )
  # Extrac relevant stations data
  stations <- sqlFetch( channel=accessDB, sqtable=where$fns$stations) %>%
      rename( LocationCode=Loc_Code, SpawnNumber=Spawn_Number ) %>%
      filter( LocationCode %in% areas$LocationCode ) %>%
      mutate( DepthM=Depth*convFac$ft2m*-1 ) %>%
      group_by( Year, LocationCode, SpawnNumber ) %>%
      summarise( Depth=MaxNA(DepthM) ) %>%
      ungroup( ) %>%
      arrange( Year, LocationCode, SpawnNumber )
  # Combine spawn and station data
  spawnStation <- full_join( x=spawn, y=stations, 
      by=c("Year", "LocationCode", "SpawnNumber") )
  # Get a small subset of area data
  areasSm <- a %>%
      select( Region, StatArea, Group, Section, LocationCode, LocationName, 
          Eastings, Northings ) %>%
      distinct( ) %>%
      as_tibble( )
  # Combine spawn and station data with area data
  res <- spawnStation %>%
      left_join( y=areasSm, by=c("LocationCode") ) %>%
      select( Year, Region, StatArea, Group, Section, LocationCode, 
          LocationName, SpawnNumber, Eastings, Northings, Start, End, Length, 
          Width, Depth, Method ) %>%
      arrange( Year, Region, StatArea, Section, LocationCode, SpawnNumber,
          Start )
  # Close the connection
  odbcClose( accessDB )
  # Return the table
  return( res )
}  # End GetAllSpawn function


############### 
##### End ##### 
############### 

# Print end of file message
cat( "Loaded spawn index functions: 'SpawnIndex.R'\n" )
