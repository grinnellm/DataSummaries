#sarah.power@dfo-mpo.gc.ca
# split index for SISCAH

#setup
librarian::shelf(tidyverse, magrittr)


#data
df <- read.csv("Summaries/SpawnHG.csv") |>
  mutate(dive = sum(Macro + Under)

