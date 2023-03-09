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
  filter(Section %in% c("006", "020","021", "022","023","024", "025")) %>% #Certain sections to include as per JC
  select(Season, ProductLanded) %>% #Product Landed column not SpawnOnKelp column
  group_by(Season) %>%
  summarise(Value = sum(ProductLanded))%>%
  mutate(Year = as.character(Season),
        Year = as.numeric(substr(Year, 1, 4)) + 1,
        Gear = 6, 
        Area = 1, 
        Type = 2, 
        Value = Value*0.00000045359237, #converts from short tonnes to kilotons
        Stock = "HG") %>%
  select(-Season)

df2 <- read_csv("Summaries/catchDataHG.csv") %>%
  filter(Gear != 6) %>% #removes any SOL data, helps to prevent doubling up of any SOK values
  rbind(df)

write_csv(df2, "Summaries/catchDataHG.csv")
write_csv(df2, "../SISCAH/Data/HG/catchData.csv")
