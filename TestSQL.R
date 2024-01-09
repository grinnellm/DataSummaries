# Packages
require(odbc)
require(DBI)
require(tidyverse)

# Database connection
cnn <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server Native Client 11.0",
                 Server = "DFBCV9TWVASP001\\SQLEXPRESS16",
                 Database = "Herring",
                 Trusted_Connection = "Yes")

# Select gear lookup table
sql <- "SELECT * FROM Biosample.Lookup.Gear"

# Execute the query
df <- dbGetQuery(cnn, sql) %>%
  as_tibble()

# Close the connection
dbDisconnect(cnn)

# Print the table
print(df)
