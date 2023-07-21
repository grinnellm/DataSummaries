#sarah.power@dfo-mpo.gc.ca
# split index for SISCAH
# This code is now redundant as it has been incorporated into Summary.R

#setup
librarian::shelf(tidyverse, magrittr)


#data

df <- read.csv("Summaries/SpawnWCVI.csv") %>%
  mutate(Surf = as.numeric(Surf), Macro = as.numeric(Macro), Under = as.numeric(Under)) %>%
  replace(is.na(.), 0) %>%
  mutate(Dive    = (Macro + Under)/1000,
         Surface = Surf/1000) %>%
  select(Year, Dive, Surface) %>% 
  write_csv(file = "Summaries/WCVI_BlendedIdx.csv")


file.copy(from = "Summaries/WCVI_BlendedIdx.csv",
          to = paste0("../SISCAH/Data/WCVI/SplitIdx/WCVI_BlendedIdx.csv"),
          overwrite = TRUE
)
