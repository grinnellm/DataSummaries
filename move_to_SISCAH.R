#sarah.power@dfo-mpo.gc.gov

#Move HerringXXyear.dat files for SISCA, will overwrite existing files

move_to_SISCAH <- function(region, year = 2022) {
  #region <- "PRD"
  #year <- 2022 # year of latest data, should match file name
  move_file <- paste0("Herring", region, year, ".dat")
  
  file.copy(from = paste0(region, "/", move_file),
            to = paste0("../SISCAH/Data/", region, "/", move_file),
            overwrite = TRUE)
}

move_to_SISCAH("CC")
move_to_SISCAH("PRD") 
move_to_SISCAH("HG")
move_to_SISCAH("WCVI")
# Because both SOG and SoG are used, use the below for SOG.
# (In the future stick to one naming convention.)

file.copy(from = paste0("SoG/HerringSoG", year, ".dat"),
          to = paste0("../SISCAH/Data/SOG/HerringSoG", year, ".dat"),
          overwrite = TRUE
)
          
