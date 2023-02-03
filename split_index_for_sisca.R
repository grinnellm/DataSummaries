#sarah.power@dfo-mpo.gc.ca
# split index for SISCAH

#setup
librarian::shelf(tidyverse, magrittr)


#data

df <- read.csv("Summaries/SpawnCC.csv") %>%
  mutate(Surf = as.numeric(Surf), Macro = as.numeric(Macro), Under = as.numeric(Under)) %>%
  replace(is.na(.), 0) %>%
  mutate(Dive    = (Macro + Under)/1000,
         Surface = Surf/1000) %>%
  select(Year, Dive, Surface) %>% 
  write_csv(file = "Summaries/CC_BlendedIdx.csv")


file.copy(from = "Summaries/CC_BlendedIdx.csv",
          to = paste0("../SISCAH/Data/CC/SplitIdx/CC_BlendedIdx.csv"),
          overwrite = TRUE
)
