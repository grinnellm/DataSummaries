#bio_sample_size
#sarah.power@dfo-mpo.gc.ca
#April 2023

# this creates a csv of biological sample sizes
#Note this can be incorporated into dataSummaries, 
#setup
librarian::shelf(tidyverse, magrittr)

#data
#region is one fo the following: "CC", "HG", "PRD", "SOG", "WCVI". 
bio_sample_sizes <- function(region = "CC"){
  df <- read.csv(paste0("Summaries/AgeData", region, ".csv")) %>%
    mutate(total = a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10,
           gear = case_when(Gear == 1 ~ 'reduction & food-bait',
                            Gear == 2 ~ 'seine',
                            Gear == 3 ~ 'gillnet',
                            TRUE ~ 'unknown')) %>%
    select(Year, gear, a2, a3, a4, a5, a6, a7, a8, a9, a10, total) %>%
    write_csv(file = paste0("Summaries/bio_sample_sizes", region, ".csv"))
  
  file.copy(from = paste0("Summaries/bio_sample_sizes", region, ".csv"),
            to = paste0("../SISCAH_resDoc2023/table/AppA/bio_sample_sizes", region, ".csv"),
            overwrite = TRUE
  )
}

bio_sample_sizes("CC")
bio_sample_sizes("HG")
bio_sample_sizes("PRD")
bio_sample_sizes("SOG")
bio_sample_sizes("WCVI")
