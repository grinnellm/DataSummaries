#SpawnOnKelpHG
#sarah.power@dfo-mpo.gc.ca
#Code reads in a SOK HG data that includes data that includes area 20 and "unknown location" data
#Data file created by Ashley Burton pulling from DFO Access data base.
#This code is using 2022 data and earlier and should be replaced by 2024 
#either by sorting out which "unknown location" data belongs in the major area
#or by incorporating this code into DataSummaries Code. 

library("librarian")
shelf("tidyverse")

df <- read_csv("Summaries/SpawnOnKelpHG.csv") %>%
  select(Season, SpawnOnKelp) %>%
  group_by(Season) %>%
  summarise(Value = sum(SpawnOnKelp))%>%
  mutate(Year = as.character(Season),
        Year = paste0(substr(Year, 1,3), substr(Year,5,5)),
        Gear = 6, 
        Area = 1, 
        Type = 2, 
        Value = Value*0.00000045359237, #converts form short tonnes to kilotonnes
        Stock = "HG") %>%
  select(-Season)

df2 <- read_csv("Summaries/catchDataHG.csv") %>%
  filter(Gear != 6) %>% #removes any SOL data, helps to prevent doubling up of any SOK values
  rbind(df)

write_csv(df2, "Summaries/catchDataHG.csv")

write_csv(df2, "../SISCAH/Data/HG/catchDataHG.csv")
